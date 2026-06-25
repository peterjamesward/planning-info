module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Fifo
import Http
import Time
import Url exposing (Url)


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
       unfortunately, the type of these specials seems indeterminate.
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
    , green_belt : Bool
    , flood_risk_zone : String
    , conservation_area : String
    , tree_preservation_zone : Bool
    , listed_building_outline : String
    , article_4_direction_area : Bool
    , area_of_outstanding_natural_beauty : Bool
    , site_of_special_scientific_interest : Bool
    }


type ConstraintType
    = GreenBelt
    | FloodRisk
    | ConservationArea
    | TreePreservation
    | ListedBuilding
    | Article4
    | AONB
    | SSSI



{- These are all possible status values from applications/facets API, across councils.
   May help with retention.
      "value": "decided",
      "value": "received",
      "value": "withdrawn",
      "value": "pending_consideration",
      "value": "validated",
      "value": "consultation",
      "value": "pending_decision",
      "value": "appeal",
      "value": "returned",
      "value": "Approved",
      "value": "Granted",
      "value": "Permitted",
      "value": "Refused",
      "value": "Pending Consideration",
      "value": "Registered",
      "value": "Under Consideration",
      "value": "Awaiting Decision",
      "value": "Appealed",

   Standard Processing States (according to Haiku)

   Received:	Application has been submitted and registered on the council's system
   Invalid:	Application lacks required information or documentation and has been rejected for resubmission
   Validation in Progress:	Council is checking whether the application is complete and valid
   Under Consideration:	Application is being assessed by planning officers (the main processing period)
   Consultation:	Application is out for public consultation or third-party responses
   Determined:	A planning decision has been made (either approved, refused, or approved with conditions)
   Withdrawn:	Applicant has requested to withdraw the application
   Lapsed:	Application has expired without determination

   Harrow Council's Process Overview

   Submission and Registration – Application is received and registered once all documents and payments are confirmed
   Validation – Harrow has 8 weeks to determine if your application is valid (13 weeks for large or complex applications)
   Assessment – Planning officers assess the application and may conduct site visits
   Consultation – Public consultation feedback is considered
   Recommendation and Decision – The case officer recommends approval or refusal, and the decision is made either by delegated powers (99% of applications) or the planning committee
-}


type ApplicationStatus
    = Submission Preamble
    | Process Processing
    | Decided Outcome


type Preamble
    = Submitted
    | Accepted
    | Returned
    | Withdrawn


type Processing
    = UnderConsideration
    | InConsultation


type Outcome
    = Approved
    | Refused
    | Conditional


type Application
    = ApplicationSummary Summary
    | ApplicationDetail Detail


type alias FrontendModel =
    { key : Key
    , applications : Dict String Application
    , selected : Maybe String
    }


type alias BackendModel =
    { applications : Dict String Application
    , lastError : Maybe Http.Error
    , lastFetch : Time.Posix
    , currentTime : Time.Posix
    , queuedFetches : Fifo.Fifo QueuedQuery
    , pendingFetch : Maybe QueuedQuery
    }


type alias QueuedQuery =
    -- Use this to throttle and debounce the backend HTTP calls to Plannexus.
    { sinceDate : Time.Posix
    , constraint : ConstraintType
    , page : Int
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | Select String


type ToBackend
    = NoOpToBackend
    | NewClient


type BackendMsg
    = NoOpBackendMsg
    | GotSummaries QueuedQuery (Result Http.Error Root)
    | HourTicker Time.Posix
    | SevenSecondTicker Time.Posix
    | GotDetail (Result Http.Error Detail)


type ToFrontend
    = NoOpToFrontend
    | CachedApplications (Dict String Application)
    | CachedApplication Application
