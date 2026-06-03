module Backend exposing (..)

import Dict
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
      , pendingDetail = []
      }
    , Task.perform HourTicker Time.now
    )


oneDay =
    -- A day's worth of milliseconds.
    24 * 3600 * 1000


fourWeeks =
    28 * oneDay


monthNumberInfix : Time.Month -> String
monthNumberInfix m =
    case m of
        Time.Jan ->
            "-01-"

        Time.Feb ->
            "-02-"

        Time.Mar ->
            "-03-"

        Time.Apr ->
            "-04-"

        Time.May ->
            "-05-"

        Time.Jun ->
            "-06-"

        Time.Jul ->
            "-07-"

        Time.Aug ->
            "-08-"

        Time.Sep ->
            "-09-"

        Time.Oct ->
            "-10-"

        Time.Nov ->
            "-11-"

        Time.Dec ->
            "-12-"


dateFromPosix p =
    let
        year =
            Time.toYear Time.utc p

        month =
            Time.toMonth Time.utc p

        day =
            Time.toDay Time.utc p
    in
    String.fromInt year
        ++ monthNumberInfix month
        ++ (if day < 10 then
                "0"

            else
                ""
           )
        ++ String.fromInt day


isWorkday : Time.Posix -> Bool
isWorkday p =
    case Time.toWeekday Time.utc p of
        Time.Mon ->
            True

        Time.Tue ->
            True

        Time.Wed ->
            True

        Time.Thu ->
            True

        Time.Fri ->
            True

        Time.Sat ->
            False

        Time.Sun ->
            False


fetchSummariesForFourWeeks now =
    let
        oneMonthAgo =
            Time.posixToMillis now
                - fourWeeks
                |> Time.millisToPosix
                |> dateFromPosix
    in
    PlanNexus.requestSummaries oneMonthAgo GotSummaries


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        HourTicker now ->
            -- Is it time for a new total fetch yet?
            if Time.posixToMillis now > Time.posixToMillis model.lastFetch + oneDay && isWorkday now then
                ( { model
                    | currentTime = now
                    , lastFetch = now
                  }
                , fetchSummariesForFourWeeks now
                )

            else
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
                fetchSummariesForFourWeeks model.currentTime

              else
                sendToFrontend clientId (CachedApplications model.applications)
            )
