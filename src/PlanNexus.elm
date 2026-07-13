module PlanNexus exposing (..)

import DateUtils
import Dict exposing (Dict)
import Env
import Http
import Iso8601
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Time
import Types
import Url.Builder as Builder


plannexus =
    "https://api.plannexus.io"


harrowUid =
    "ef340ad8-1a60-43a8-b741-2483f6919d3f"


pagedSummaries :
    Types.QueuedSummaryQuery
    -> (Result Http.Error Types.Root -> msg)
    -> Cmd msg
pagedSummaries query msg =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-Api-Key" Env.plannexusApiKey ]
        , url =
            Debug.log "URL" <|
                Builder.crossOrigin plannexus
                    [ "v1", "applications" ]
                    [ Builder.string "postcode" "HA7"
                    , Builder.string "authority_id" harrowUid
                    , Builder.string "changed_since" (DateUtils.dateFromPosix query.sinceDate)
                    , Builder.string "date_received_from" (DateUtils.dateFromPosix <| DateUtils.oneYearBefore query.sinceDate)
                    , Builder.int "per_page" 100
                    , Builder.int "page" query.page
                    ]
        , body = Http.emptyBody
        , expect = Http.expectJson msg rootDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


requestDetail : String -> (Result Http.Error Types.Detail -> msg) -> Cmd msg
requestDetail id msg =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-Api-Key" Env.plannexusApiKey ]
        , url =
            Builder.crossOrigin plannexus
                [ "v1", "applications", id ]
                []
        , body = Http.emptyBody
        , expect = Http.expectJson msg detailDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


requestHistory : String -> (Result Http.Error (List Types.StateChange) -> msg) -> Cmd msg
requestHistory id msg =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-Api-Key" Env.plannexusApiKey ]
        , url =
            Builder.crossOrigin plannexus
                [ "v1", "applications", id, "history" ]
                []
        , body = Http.emptyBody
        , expect = Http.expectJson msg historyDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


rootDecoder : Decode.Decoder Types.Root
rootDecoder =
    Decode.succeed Types.Root
        |> Pipeline.required "data" (Decode.list dataDecoder)
        |> Pipeline.required "meta" metaDecoder


metaDecoder : Decode.Decoder Types.Meta
metaDecoder =
    Decode.succeed Types.Meta
        |> Pipeline.required "total" Decode.int
        |> Pipeline.required "page" Decode.int
        |> Pipeline.required "per_page" Decode.int
        |> Pipeline.required "total_pages" Decode.int


dataDecoder : Decode.Decoder Types.Summary
dataDecoder =
    Decode.succeed Types.Summary
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "reference" Decode.string
        |> Pipeline.required "address" Decode.string
        |> Pipeline.required "application_type" Decode.string
        |> Pipeline.required "status" Decode.string
        |> Pipeline.required "authority_name" Decode.string
        |> Pipeline.optional "date_received" Decode.string ""
        |> Pipeline.optional "latitude" Decode.float 0.0
        |> Pipeline.optional "longitude" Decode.float 0.0


detailDecoder : Decode.Decoder Types.Detail
detailDecoder =
    --N.B. Rely on subsequent history call to set lastChangeDate.
    Decode.succeed Types.Detail
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "reference" Decode.string
        |> Pipeline.required "address" Decode.string
        |> Pipeline.required "description" Decode.string
        |> Pipeline.required "application_type" Decode.string
        |> Pipeline.required "status" Decode.string
        |> Pipeline.optional "decision" Decode.string ""
        |> Pipeline.optional "date_received" Decode.string ""
        |> Pipeline.optional "decision_date" Decode.string ""
        |> Pipeline.optional "latitude" Decode.float 0.0
        |> Pipeline.optional "longitude" Decode.float 0.0
        |> Pipeline.optional "source_url" Decode.string ""
        |> Pipeline.optional "ward" Decode.string ""
        |> Pipeline.optionalAt [ "constraints", "summary", "green_belt", "present" ] Decode.bool False
        |> Pipeline.optionalAt [ "constraints", "summary", "flood_risk_zone", "label" ] Decode.string ""
        |> Pipeline.optionalAt [ "constraints", "summary", "conservation_area", "label" ] Decode.string ""
        |> Pipeline.optionalAt [ "constraints", "summary", "tree_preservation_zone", "present", "label" ] Decode.bool False
        |> Pipeline.optionalAt [ "constraints", "summary", "listed_building_outline", "label" ] Decode.string ""
        |> Pipeline.optionalAt [ "constraints", "summary", "article_4_direction_area", "present" ] Decode.bool False
        |> Pipeline.optionalAt [ "constraints", "summary", "area_of_outstanding_natural_beauty", "present" ] Decode.bool False
        |> Pipeline.optionalAt [ "constraints", "summary", "site_of_special_scientific_interest", "present" ] Decode.bool False
        |> Pipeline.hardcoded (Time.millisToPosix 0)


historyDecoder : Decode.Decoder (List Types.StateChange)
historyDecoder =
    Decode.list stateChangeDecoder


stateChangeDecoder : Decode.Decoder Types.StateChange
stateChangeDecoder =
    Decode.succeed Types.StateChange
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "status" Decode.string
        |> Pipeline.required "detected_at" Decode.string
