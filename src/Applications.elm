module Applications exposing (..)

import DateUtils exposing (olderThanFourWeeks)
import Set exposing (Set)
import Time
import Types exposing (..)



-- A place to put functions for Applications other than Encode/Decode but perhaps those also.


terminalStates : Set String
terminalStates =
    Set.fromList
        [ "decided"
        , "withdrawn"
        , "returned"
        , "Approved"
        , "Granted"
        , "Permitted"
        , "Refused"
        ]


isTerminated : Application -> Bool
isTerminated application =
    let
        status =
            case application of
                ApplicationSummary summary ->
                    summary.status

                ApplicationDetail detail ->
                    detail.status
    in
    Set.member status terminalStates


isPurgeable : Time.Posix -> String -> Application -> Bool
isPurgeable now id application =
    let
        oneMonthSinceLastChange =
            case application of
                ApplicationDetail detail ->
                    olderThanFourWeeks now detail.lastChangeDate

                ApplicationSummary _ ->
                    False
    in
    isTerminated application && oneMonthSinceLastChange
