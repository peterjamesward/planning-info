module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Http
import Time
import Url exposing (Url)



--TODO: Use "Application" type in backend as well, so new client gets all the details!
--TODO: Fetch from plannexus once each work day.
--TODO: Detail view.
--TODO: Detail map view.
--TODO: API key in config.


type alias Root =
    { data : List Summary
    , meta : Meta
    }


type alias Meta =
    { total : Int
    , page : Int
    , per_page : Int
    , total_pages : Int
    }


type alias Summary =
    {- e.g.
       "id": "8ec88f47-58a1-4371-9e10-233aff471aa5",
       "reference": "PL/1431/26",
       "address": "22 Derwent Crescent, Stanmore, Harrow, HA7 2NF",
       "application_type": "other",
       "status": "received",
       "authority_name": "Harrow",
       "date_received": "2026-05-28",
       "latitude": 51.599354,
       "longitude": -0.308165
    -}
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
    {- e.g.

       "id": "e8eaffa6-9eb1-4515-b225-833a93fb673b",
       "authority_id": "ef340ad8-1a60-43a8-b741-2483f6919d3f",
       "authority_name": "Harrow",
       "reference": "PL/0271/26",
       "address": "MARTINSELL Green Lane, Stanmore, Harrow, HA7 3AB",
       "postcode": "HA7 3AB",
       "description": "Single storey rear extension; front porch; dormer to each side and rear roof slopes; external alterations including replacement of windows, installation of garage door, installation of CCTV cameras and installation of AC unit (part retrospective)",
       "application_type": "householder",
       "status": "received",
       "decision": null,
       "date_received": "2026-05-03",
       "decision_date": null,
       "latitude": 51.620276,
       "longitude": -0.316661,
       "source_url": "https://planningsearch.harrow.gov.uk/planning/index.html?fa=search",
       "ward": "Stanmore",
       "parish": "Harrow, unparished area",
       "easting": null,
       "northing": null,
       "uprn": null,
       "development_type": null,
       "applicant_name": null,
       "agent_name": null,
       "agent_company": null,
       "case_officer": null,
       "appeal_status": null,
       "appeal_decision": null,
       "date_validated": null,
       "consultation_start_date": null,
       "consultation_end_date": null,
       "target_decision_date": null,
       "committee_date": null,
       "constraints": {
           "details": {
               "conservation_area": [
                   {
                       "name": "Stanmore Hill Conservation Area",
                       "entity": 44008807,
                       "reference": "COA00000170",
                       "documentation_url": ""
                   }
               ]
           },
           "summary": {
               "green_belt": null,
               "flood_risk_zone": null,
               "conservation_area": "Stanmore Hill Conservation Area",
               "scheduled_monument": null,
               "world_heritage_site": null,
               "tree_preservation_zone": null,
               "listed_building_outline": null,
               "article_4_direction_area": null,
               "area_of_outstanding_natural_beauty": null,
               "site_of_special_scientific_interest": null
           }
       },
       "first_scraped_at": "2026-04-29T14:55:25.414219Z",
       "last_scraped_at": "2026-05-03T02:00:17.263313Z",
       "created_at": "2026-04-29T14:55:25.458065Z",
       "updated_at": "2026-05-03T02:00:17.317844Z",
       "redacted_fields": [
           "applicant_name",
           "agent_name",
           "agent_company",
           "case_officer"
       ]

    -}
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
    , conservation_area : String
    }


type Application
    = ApplicationSummary Summary
    | ApplicationDetail Detail


type alias FrontendModel =
    { key : Key
    , applications : Dict String Application
    , selected : Maybe String
    }


type alias BackendModel =
    { summaries : Dict String Summary
    , details : Dict String Detail
    , lastError : Maybe Http.Error
    , lastFetch : Time.Posix
    , currentTime : Time.Posix
    , pendingDetail : List String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend
    | NewClient


type BackendMsg
    = NoOpBackendMsg
    | GotSummaries (Result Http.Error Root)
    | TheTimeIs Time.Posix
    | GetNextDetail
    | GotDetail (Result Http.Error Detail)


type ToFrontend
    = NoOpToFrontend
    | CachedSummaries (Dict String Summary)
    | CachedDetail Detail
