module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Url


type alias Summary =
    { id : String
    , reference : String
    , address : String
    , application_type : String
    , status : String
    , authority_name : String
    , date_received : String
    , latitude : Float
    , longitude : Float
    }


type alias Detail =
    { id : String
    , authority_id : String
    , authority_name : String
    , reference : String
    , address : String
    , description : String
    , application_type : String
    , status : String
    , decision : Maybe String
    , date_received : String
    , decision_date : Maybe String
    , latitude : Float
    , longitude : Float
    , source_url : Maybe String
    , ward : String
    , conservation_area : Maybe String
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , summaries : Dict.Dict String Summary
    , detail : Maybe Detail
    }


type alias BackendModel =
    { summaries : Dict.Dict String Summary
    , details : Dict.Dict String Detail
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend
    | NewClient


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
    | CachedSummaries (Dict.Dict String Summary)
