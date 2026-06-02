module PlanNexus exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Types
import Url.Builder as Builder


plannexus =
    --"https://api.plannexus.io/v1/applications?postcode=HA7&authority_id=ef340ad8-1a60-43a8-b741-2483f6919d3f&per_page=100"
    "https://api.plannexus.io"


apiKey =
    --Should not be here, I know.
    "pn_live_dd5a90f71013ea2de78bec1c48349d0d6a9dd7bf35e07099"


harrowUid =
    "ef340ad8-1a60-43a8-b741-2483f6919d3f"


requestSummaries : (Result Http.Error Types.Root -> msg) -> Cmd msg
requestSummaries msg =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-Api-Key" apiKey ]
        , url =
            Builder.crossOrigin plannexus
                [ "v1", "applications" ]
                [ Builder.string "postcode" "HA7"
                , Builder.string "authority_id" harrowUid
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
        |> Pipeline.required "latitude" Decode.float
        |> Pipeline.required "longitude" Decode.float


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
        |> Pipeline.required "latitude" Decode.float
        |> Pipeline.required "longitude" Decode.float
        |> Pipeline.required "source_url" Decode.string
        |> Pipeline.required "ward" Decode.string
        |> Pipeline.optionalAt
            [ "constraints"
            , "summary"
            , "conservation_area"
            ]
            Decode.string
            ""


summariesAsDict : List Types.Summary -> Dict String Types.Summary
summariesAsDict summaryList =
    let
        addSummary : Types.Summary -> Dict String Types.Summary -> Dict String Types.Summary
        addSummary summary dict =
            Dict.insert summary.id summary dict
    in
    List.foldl addSummary Dict.empty summaryList
