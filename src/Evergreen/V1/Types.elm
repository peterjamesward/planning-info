module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Http
import Time
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
    , reference : String
    , address : String
    , description : String
    , application_type : String
    , status : String
    , decision : String
    , date_received : String
    , decision_date : String
    , latitude : Float
    , longitude : Float
    , source_url : String
    , ward : String
    , green_belt : Bool
    , flood_risk_zone : String
    , conservation_area : String
    , tree_preservation_zone : Bool
    , listed_building_outline : String
    , article_4_direction_area : String
    , area_of_outstanding_natural_beauty : Bool
    , site_of_special_scientific_interest : Bool
    }


type Application
    = ApplicationSummary Summary
    | ApplicationDetail Detail


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , applications : Dict.Dict String Application
    , selected : Maybe String
    }


type alias BackendModel =
    { applications : Dict.Dict String Application
    , lastError : Maybe Http.Error
    , lastFetch : Time.Posix
    , currentTime : Time.Posix
    , pendingDetail : List String
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | Select String


type ToBackend
    = NoOpToBackend
    | NewClient


type alias Meta =
    { total : Int
    , page : Int
    , per_page : Int
    , total_pages : Int
    }


type alias Root =
    { data : List Summary
    , meta : Meta
    }


type BackendMsg
    = NoOpBackendMsg
    | GotSummaries (Result Http.Error Root)
    | HourTicker Time.Posix
    | SevenSecondTicker Time.Posix
    | GotDetail (Result Http.Error Detail)


type ToFrontend
    = NoOpToFrontend
    | CachedApplications (Dict.Dict String Application)
    | CachedApplication Application
