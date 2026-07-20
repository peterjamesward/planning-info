module IpInfo exposing (..)

import Http
import Url.Builder as Builder


apiRoot =
    "https://ipinfo.io"


requestIpInformation : (Result Http.Error () -> msg) -> Cmd msg
requestIpInformation msg =
    Http.request
        { method = "GET"
        , headers = []
        , url =
            Builder.crossOrigin apiRoot
                []
                [ Builder.string "token" "d8eaf22a340613" ]
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        , timeout = Nothing
        , tracker = Nothing
        }
