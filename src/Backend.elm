module Backend exposing (..)

import DateUtils exposing (isWorkday, oneDay, oneYear)
import Dict
import Fifo
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import PlanNexus
import Task
import Time
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions =
            \m ->
                Sub.batch
                    [ Time.every (3600 * 1000) HourTicker
                    , Time.every (8 * 1000) SevenSecondTicker
                    ]
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { applications = Dict.empty
      , lastError = Nothing
      , lastFetch = Time.millisToPosix 0
      , currentTime = Time.millisToPosix 0
      , queuedFetches = Fifo.empty
      , pendingFetch = Nothing
      }
    , Task.perform HourTicker Time.now
    )


isSummary : String -> Application -> Bool
isSummary id application =
    case application of
        ApplicationSummary summary ->
            True

        ApplicationDetail detail ->
            False


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    let
        batchOfFetches since =
            [ Types.GreenBelt
            , Types.FloodRisk
            , Types.ConservationArea
            , Types.TreePreservation
            , Types.ListedBuilding
            , Types.Article4
            , Types.AONB
            , Types.SSSI
            ]
                |> List.foldl
                    (\constraint fifo ->
                        Fifo.insert
                            { sinceDate = since
                            , constraint = constraint
                            , page = 1
                            }
                            fifo
                    )
                    Fifo.empty
    in
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        HourTicker now ->
            --Fetch after 24 hours on working days only.
            --Use query queue to throttle and debounce.
            --Do nothing if we're already loading!
            let
                moreThanOneDaySinceLastFetch =
                    Time.posixToMillis now - Time.posixToMillis model.lastFetch > oneDay

                noQueuedFetches =
                    List.isEmpty (Fifo.toList model.queuedFetches)

                fetchSince =
                    DateUtils.mostRecent
                        (DateUtils.oneYearBefore now)
                        model.lastFetch
            in
            if moreThanOneDaySinceLastFetch && isWorkday now && noQueuedFetches then
                ( { model
                    | queuedFetches = batchOfFetches fetchSince
                  }
                , Cmd.none
                )

            else
                ( { model | currentTime = now }
                , Cmd.none
                )

        SevenSecondTicker now ->
            --Throttle and debounce API calls.
            --If we have pending fetches, dispatch exactly one of them.
            --If we have some summaries, fetch detail for any one.
            --If we have no Applications, queue up fetches for them all.
            --Message needs to have fetch context for paging to work.
            --Note that queueing avoids the situation where a response
            --taking > 7 seconds leads to a repeat query.
            case ( model.pendingFetch, Fifo.remove model.queuedFetches ) of
                ( Just pending, _ ) ->
                    -- Just wait, no point stressing the API.
                    ( { model | currentTime = now }
                    , Cmd.none
                    )

                ( Nothing, ( Just firstFetch, remainingFetches ) ) ->
                    ( { model
                        | currentTime = now
                        , lastFetch = now
                        , queuedFetches = remainingFetches
                        , pendingFetch = Just firstFetch
                      }
                    , PlanNexus.pagedConstrainedSummaries
                        (Debug.log "fetching" firstFetch)
                        (GotSummaries firstFetch)
                    )

                ( Nothing, ( Nothing, _ ) ) ->
                    if Dict.isEmpty model.applications then
                        -- Put all the query params into our new queue.
                        -- In another seven seconds, the queries start executing.
                        ( { model
                            | queuedFetches = batchOfFetches (DateUtils.oneYearBefore now)
                            , currentTime = now
                          }
                        , Cmd.none
                        )

                    else
                        -- We have some applications; make sure they all have details (slowly).
                        --TODO: Don't want filter, want find (though this is optimisation)!
                        case
                            model.applications
                                |> Dict.filter isSummary
                                |> Dict.keys
                        of
                            summaryApplication :: _ ->
                                -- We don't queue these requests, only the bulk load.
                                ( { model | currentTime = now }
                                , PlanNexus.requestDetail summaryApplication GotDetail
                                )

                            [] ->
                                -- We have all the details, take a rest.
                                ( { model | currentTime = now }
                                , Cmd.none
                                )

        GotSummaries fetch result ->
            case result of
                Ok value ->
                    --If there's another page, queue up the next query.
                    --Note that an updated Summary will displace an existing Detail;
                    --this is what we want as details only may have changed.
                    let
                        updatedApplications =
                            List.foldl
                                (\summary dict ->
                                    Dict.insert
                                        summary.id
                                        (ApplicationSummary summary)
                                        dict
                                )
                                model.applications
                                value.data

                        followUp : Maybe QueuedQuery
                        followUp =
                            if value.meta.page < value.meta.total_pages then
                                Just { fetch | page = fetch.page + 1 }

                            else
                                Nothing
                    in
                    ( { model
                        | applications = updatedApplications
                        , lastError = Nothing
                        , lastFetch = model.currentTime
                        , queuedFetches =
                            case followUp of
                                Just nextPageQuery ->
                                    Fifo.insert nextPageQuery model.queuedFetches

                                Nothing ->
                                    model.queuedFetches
                        , pendingFetch = Nothing
                      }
                    , Lamdera.broadcast (CachedApplications updatedApplications)
                    )

                Err error ->
                    ( { model | lastError = Just error }
                    , Cmd.none
                    )

        GotDetail result ->
            case result of
                Ok detail ->
                    ( { model
                        | applications =
                            Dict.insert detail.id
                                (ApplicationDetail detail)
                                model.applications
                        , lastError = Nothing
                      }
                    , Lamdera.broadcast (CachedApplication (ApplicationDetail detail))
                    )

                Err error ->
                    ( { model | lastError = Just error }
                    , Cmd.none
                    )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        NewClient ->
            ( model
            , sendToFrontend clientId (CachedApplications model.applications)
            )
