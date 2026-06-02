module Backend exposing (..)

import Dict
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import PlanNexus
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { summaries = Dict.empty
      , details = Dict.empty
      , lastError = Nothing
      }
    , PlanNexus.requestSummaries GotSummaries
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

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
                      }
                    , Lamdera.broadcast (CachedSummaries summaries)
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
            , sendToFrontend clientId (CachedSummaries model.summaries)
            )
