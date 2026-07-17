module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Http
import Queue
import Set
import Time
import Url


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
    , article_4_direction_area : Bool
    , area_of_outstanding_natural_beauty : Bool
    , site_of_special_scientific_interest : Bool
    , lastChangeDate : Time.Posix
    }


type FrontEndMode
    = FullDisplay
    | Embedded


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , applications : Dict.Dict String Detail
    , selected : Maybe String
    , mode : FrontEndMode
    , typeFilters : Set.Set String
    , statusFilters : Set.Set String
    , decisionFilters : Set.Set String
    , currentTime : Time.Posix
    , green_belt : Bool
    , flood_risk_zone : Bool
    , conservation_area : Bool
    , tree_preservation_zone : Bool
    , listed_building_outline : Bool
    , article_4_direction_area : Bool
    , area_of_outstanding_natural_beauty : Bool
    , site_of_special_scientific_interest : Bool
    }


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


type Application
    = ApplicationSummary Summary
    | ApplicationDetail Detail


type alias QueuedSummaryQuery =
    { sinceDate : Time.Posix
    , page : Int
    }


type QueuedQuery
    = SummaryQuery QueuedSummaryQuery
    | DetailQuery String
    | HistoryQuery String


type alias BackendModel =
    { applications : Dict.Dict String Application
    , lastError : Maybe Http.Error
    , lastFetch : Time.Posix
    , currentTime : Time.Posix
    , queryQueue : Queue.Queue QueuedQuery
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | Select String
    | ToggleTypeFilter String Bool
    | ToggleStatusFilter String Bool
    | ToggleDecisionFilter String Bool
    | Green_belt_Toggle Bool
    | Flood_risk_zone_Toggle Bool
    | Conservation_area_Toggle Bool
    | Tree_preservation_zone_Toggle Bool
    | Listed_building_outline_Toggle Bool
    | Article_4_direction_area_Toggle Bool
    | Area_of_outstanding_natural_beauty_Toggle Bool
    | Site_of_special_scientific_interest_Toggle Bool
    | TimeTicker Time.Posix


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


type alias StateChange =
    { id : String
    , status : String
    , effective : String
    }


type BackendMsg
    = NoOpBackendMsg
    | GotSummaries QueuedSummaryQuery (Result Http.Error Root)
    | BackgroundFetchTicker Time.Posix
    | BackgroundPurgeTicker Time.Posix
    | TickerToThrottleApiCalls Time.Posix
    | GotDetail (Result Http.Error Detail)
    | GotHistory String (Result Http.Error (List StateChange))


type ToFrontend
    = NoOpToFrontend
    | CachedApplications (Dict.Dict String Detail)
    | CachedApplication Detail
    | PurgeApplications (List String)
