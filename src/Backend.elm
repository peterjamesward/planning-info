module Backend exposing (..)

import Dict
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import PlanNexus
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
                    [ Time.every (60 * 1000) MinuteTicker
                    , Time.every (8 * 1000) SevenSecondTicker
                    ]
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { applications = Dict.empty
      , lastError = Nothing
      , lastFetch = Time.millisToPosix 0
      , currentTime = Time.millisToPosix 0
      , pendingDetail = []
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        MinuteTicker now ->
            -- Is it time for a new total fetch yet?
            ( { model | currentTime = now }
            , Cmd.none
            )

        SevenSecondTicker now ->
            -- Can we do a detail incremental fetch?
            case model.pendingDetail of
                first :: rest ->
                    ( { model | pendingDetail = rest }
                    , PlanNexus.requestDetail first GotDetail
                    )

                [] ->
                    ( model, Cmd.none )

        GotSummaries result ->
            case result of
                Ok value ->
                    let
                        applications =
                            PlanNexus.summariesAsDict value.data
                    in
                    ( { model
                        | applications = applications
                        , lastError = Nothing
                        , lastFetch = model.currentTime
                        , pendingDetail = Dict.keys applications
                      }
                    , Cmd.batch
                        [ Lamdera.broadcast (CachedApplications applications) ]
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
            , if Dict.isEmpty model.applications then
                PlanNexus.requestSummaries "2026-05-01" GotSummaries

              else
                sendToFrontend clientId (CachedApplications model.applications)
            )
