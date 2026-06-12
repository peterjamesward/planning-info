module PlanNexus exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Types
import Url.Builder as Builder


plannexus =
    "https://api.plannexus.io"


apiKey =
    --Should not be here, I know.
    "pn_live_dd5a90f71013ea2de78bec1c48349d0d6a9dd7bf35e07099"


harrowUid =
    "ef340ad8-1a60-43a8-b741-2483f6919d3f"


requestSummaries : String -> (Result Http.Error Types.Root -> msg) -> Cmd msg
requestSummaries sinceDate msg =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-Api-Key" apiKey ]
        , url =
            Builder.crossOrigin plannexus
                [ "v1", "applications" ]
                [ Builder.string "postcode" "HA7"
                , Builder.string "authority_id" harrowUid
                , Builder.string "date_received_from" sinceDate
                , Builder.int "per_page" 100
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
        , headers = [ Http.header "X-Api-Key" apiKey ]
        , url =
            Builder.crossOrigin plannexus
                [ "v1", "applications", id ]
                []
        , body = Http.emptyBody
        , expect = Http.expectJson msg detailDecoder
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
        |> Pipeline.required "date_received" Decode.string
        |> Pipeline.optional "latitude" Decode.float 0.0
        |> Pipeline.optional "longitude" Decode.float 0.0


detailDecoder : Decode.Decoder Types.Detail
detailDecoder =
    Decode.succeed Types.Detail
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "reference" Decode.string
        |> Pipeline.required "address" Decode.string
        |> Pipeline.required "description" Decode.string
        |> Pipeline.required "application_type" Decode.string
        |> Pipeline.required "status" Decode.string
        |> Pipeline.optional "decision" Decode.string ""
        |> Pipeline.required "date_received" Decode.string
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
        |> Pipeline.optionalAt [ "constraints", "summary", "article_4_direction_area", "label" ] Decode.string ""
        |> Pipeline.optionalAt [ "constraints", "summary", "area_of_outstanding_natural_beauty", "present" ] Decode.bool False
        |> Pipeline.optionalAt [ "constraints", "summary", "site_of_special_scientific_interest", "present" ] Decode.bool False


summariesAsDict : List Types.Summary -> Dict String Types.Application
summariesAsDict summaryList =
    summaryList
        |> List.map (\summary -> ( summary.id, Types.ApplicationSummary summary ))
        |> Dict.fromList
