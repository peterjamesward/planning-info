module Backend exposing (..)

import Applications exposing (detailsOnly, isPurgeable)
import DateUtils exposing (isWorkday, oneDay, oneYear)
import Dict
import Iso8601
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import PlanNexus
import Queue
import Set
import Task
import Time
import Types exposing (..)


type alias Model =
    BackendModel


app =
    --NB Reducing interval to 1.2 seconds to allow for at 60/minute.
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions =
            \m ->
                if Queue.isEmpty m.queryQueue then
                    Sub.batch
                        [ Time.every DateUtils.oneHour BackgroundFetchTicker
                        , Time.every DateUtils.oneDay BackgroundPurgeTicker
                        ]

                else
                    Time.every 1200 TickerToThrottleApiCalls
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { applications = Dict.empty
      , lastError = Nothing
      , lastFetch = Time.millisToPosix 0
      , currentTime = Time.millisToPosix 0
      , queryQueue = Queue.empty
      }
    , Task.perform BackgroundFetchTicker Time.now
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
        moreThanOneDaySinceLastFetch =
            Time.posixToMillis model.currentTime - Time.posixToMillis model.lastFetch > oneDay

        fetchSince current =
            DateUtils.mostRecent
                (DateUtils.oneYearBefore current)
                model.lastFetch
    in
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        BackgroundPurgeTicker now ->
            --Each day, remove applications that are in some closed state for more than one month (say).
            let
                ( purged, remaining ) =
                    Dict.partition (isPurgeable now) model.applications
            in
            ( { model | applications = remaining }
            , Lamdera.broadcast (PurgeApplications (Dict.keys purged))
            )

        BackgroundFetchTicker now ->
            if Dict.isEmpty model.applications || moreThanOneDaySinceLastFetch then
                --Incremental fetch after 24 hours or if we have none at all.
                ( { model
                    | currentTime = now
                    , queryQueue =
                        Queue.enqueue
                            (SummaryQuery { sinceDate = fetchSince now, page = 1 })
                            model.queryQueue
                  }
                , Cmd.none
                )

            else
                ( { model | currentTime = now }
                , Cmd.none
                )

        TickerToThrottleApiCalls now ->
            -- All we do here is dispatch the next queued query.
            let
                ( query, tail ) =
                    Queue.dequeue model.queryQueue

                action =
                    case query of
                        Just (SummaryQuery sq) ->
                            PlanNexus.pagedSummaries sq (GotSummaries sq)

                        Just (DetailQuery dq) ->
                            PlanNexus.requestDetail dq GotDetail

                        Just (HistoryQuery hq) ->
                            PlanNexus.requestHistory hq (GotHistory hq)

                        Nothing ->
                            Cmd.none
            in
            ( { model | queryQueue = tail }
            , action
            )

        GotSummaries fetch result ->
            case result of
                Ok value ->
                    let
                        filteredResults =
                            --Preemptively remove less contentious stuff.
                            List.filter
                                (\summary ->
                                    not <|
                                        Set.member summary.application_type
                                            (Set.fromList
                                                [ "tree_works"
                                                , "discharge_conditions"
                                                , "lawful_development"
                                                , "other"
                                                ]
                                            )
                                )
                                value.data

                        updatedApplications =
                            --Note that an updated Summary will displace an existing Detail;
                            --this is what we want as details only may have changed.
                            List.foldl
                                (\summary dict ->
                                    Dict.insert
                                        summary.id
                                        (ApplicationSummary summary)
                                        dict
                                )
                                model.applications
                                filteredResults

                        queueWithOptionalNextPageQuery =
                            if value.meta.page < value.meta.total_pages then
                                Queue.enqueue
                                    (SummaryQuery { fetch | page = fetch.page + 1 })
                                    model.queryQueue

                            else
                                model.queryQueue

                        queueWithDetailQueries =
                            List.foldl
                                (\summary queue ->
                                    queue
                                        |> Queue.enqueue (DetailQuery summary.id)
                                        |> Queue.enqueue (HistoryQuery summary.id)
                                )
                                queueWithOptionalNextPageQuery
                                filteredResults
                    in
                    ( { model
                        | applications = updatedApplications
                        , lastError = Nothing
                        , lastFetch = model.currentTime
                        , queryQueue = queueWithDetailQueries
                      }
                    , Cmd.none
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
                    , Cmd.none
                    )

                Err error ->
                    ( { model | lastError = Just error }
                    , Cmd.none
                    )

        GotHistory id result ->
            case result of
                Ok history ->
                    -- We use the history to get a reliable timestamp for updated details,
                    -- because council portal does not reliably timestamp all state changes.
                    case Dict.get id model.applications of
                        Just (ApplicationDetail detail) ->
                            let
                                mostRecentEntry =
                                    history
                                        |> List.filterMap (.effective >> Iso8601.toTime >> Result.toMaybe)
                                        |> List.map Time.posixToMillis
                                        |> List.maximum
                                        |> Maybe.withDefault 0
                                        |> Time.millisToPosix

                                timestampedDetail =
                                    { detail | lastChangeDate = mostRecentEntry }
                            in
                            ( { model
                                | lastError = Nothing
                                , applications =
                                    Dict.insert id
                                        (ApplicationDetail timestampedDetail)
                                        model.applications
                              }
                            , Lamdera.broadcast (CachedApplication timestampedDetail)
                            )

                        _ ->
                            ( model, Cmd.none )

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
            , sendToFrontend clientId (CachedApplications <| detailsOnly model.applications)
            )
