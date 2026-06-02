module Backend exposing (..)

import Delay
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
        , subscriptions = \m -> Time.every (60 * 1000) TheTimeIs
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { summaries = Dict.empty
      , details = Dict.empty
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

        TheTimeIs now ->
            -- Is it time for a fetch yet?
            ( { model | currentTime = now }
            , Cmd.none
            )

        GotSummaries result ->
            case result of
                Ok value ->
                    let
                        summaries =
                            PlanNexus.summariesAsDict value.data
                    in
                    ( { model
                        | summaries = summaries
                        , lastError = Nothing
                        , lastFetch = model.currentTime
                        , pendingDetail = Dict.keys summaries
                      }
                    , Cmd.batch
                        [ Lamdera.broadcast (CachedSummaries summaries)
                        , Delay.after 7000 GetNextDetail
                        ]
                    )

                Err error ->
                    ( { model | lastError = Just error }
                    , Cmd.none
                    )

        GetNextDetail ->
            case model.pendingDetail of
                first :: rest ->
                    ( { model | pendingDetail = rest }
                    , Cmd.batch
                        [ PlanNexus.requestDetail first GotDetail
                        , Delay.after 7000 GetNextDetail
                        ]
                    )

                [] ->
                    ( model, Cmd.none )

        GotDetail result ->
            case result of
                Ok detail ->
                    ( { model
                        | details = Dict.insert detail.id detail model.details
                        , lastError = Nothing
                      }
                    , Lamdera.broadcast (CachedDetail detail)
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
            , if Dict.isEmpty model.summaries then
                PlanNexus.requestSummaries GotSummaries

              else
                sendToFrontend clientId (CachedSummaries model.summaries)
            )
