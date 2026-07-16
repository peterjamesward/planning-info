module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import DateUtils
import Dict exposing (Dict)
import Element exposing (Element, alignLeft, alignRight, centerX, centerY, el, fill, fillPortion, padding, rgb255, rgba255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font
import Element.HexColor as HexColor exposing (hex)
import Element.Input as Input
import Html
import Lamdera exposing (sendToBackend)
import Set exposing (Set)
import Types exposing (..)
import Url
import Url.Builder as Builder


type alias Model =
    FrontendModel


lightGreen =
    rgba255 75 107 70 0.2


stanmoreGreen =
    rgb255 20 165 20


stanmoreWhite =
    rgba255 255 255 255 1.0


grey =
    rgb255 200 200 200


paleRed =
    rgb255 180 120 120


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , applications = Dict.empty
      , selected = Nothing
      , mode =
            if Debug.log "URL" url.path == "/embed" then
                Embedded

            else
                FullDisplay
      , typeFilters = Set.empty
      , statusFilters = Set.empty
      , decisionFilters = Set.empty
      , green_belt = False
      , flood_risk_zone = False
      , conservation_area = False
      , tree_preservation_zone = False
      , listed_building_outline = False
      , article_4_direction_area = False
      , area_of_outstanding_natural_beauty = False
      , site_of_special_scientific_interest = False
      }
    , sendToBackend NewClient
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        Select string ->
            ( { model | selected = Just string }
            , Cmd.none
            )

        ToggleTypeFilter string bool ->
            ( { model | typeFilters = toggleHelper string bool model.typeFilters }
            , Cmd.none
            )

        ToggleStatusFilter string bool ->
            ( { model | statusFilters = toggleHelper string bool model.statusFilters }
            , Cmd.none
            )

        ToggleDecisionFilter string bool ->
            ( { model | decisionFilters = toggleHelper string bool model.decisionFilters }
            , Cmd.none
            )

        Green_belt_Toggle bool ->
            ( { model | green_belt = bool }, Cmd.none )

        Flood_risk_zone_Toggle bool ->
            ( { model | flood_risk_zone = bool }, Cmd.none )

        Conservation_area_Toggle bool ->
            ( { model | conservation_area = bool }, Cmd.none )

        Tree_preservation_zone_Toggle bool ->
            ( { model | tree_preservation_zone = bool }, Cmd.none )

        Listed_building_outline_Toggle bool ->
            ( { model | listed_building_outline = bool }, Cmd.none )

        Article_4_direction_area_Toggle bool ->
            ( { model | article_4_direction_area = bool }, Cmd.none )

        Area_of_outstanding_natural_beauty_Toggle bool ->
            ( { model | area_of_outstanding_natural_beauty = bool }, Cmd.none )

        Site_of_special_scientific_interest_Toggle bool ->
            ( { model | site_of_special_scientific_interest = bool }, Cmd.none )


toggleHelper : String -> Bool -> Set String -> Set String
toggleHelper value state set =
    if state then
        Set.insert value set

    else
        Set.remove value set


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        CachedApplications applications ->
            ( { model | applications = applications }
            , Cmd.none
            )

        CachedApplication application ->
            ( { model
                | applications = Dict.insert application.id application model.applications
              }
            , Cmd.none
            )

        PurgeApplications ids ->
            ( { model
                | applications =
                    List.foldl (\purge dict -> Dict.remove purge dict) model.applications ids
                , selected =
                    case model.selected of
                        Just select ->
                            if List.member select ids then
                                Nothing

                            else
                                model.selected

                        Nothing ->
                            Nothing
              }
            , Cmd.none
            )


showId : ( String, Summary ) -> Html.Html msg
showId ( id, summary ) =
    Html.text summary.id


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        List.singleton <|
            Element.layout
                [ Font.family
                    [ Font.typeface "Helvetica"
                    , Font.sansSerif
                    ]
                , Font.size 14
                ]
            <|
                case model.mode of
                    FullDisplay ->
                        fullView model

                    Embedded ->
                        Element.text "Hello"
    }


fullView model =
    Element.column
        [ Element.width fill
        , Element.alignTop
        , padding 10
        , spacing 10
        , Element.alignLeft
        ]
        [ Element.row [ width fill ]
            [ el [ width <| fillPortion 1 ] <| viewFilters model
            , el [ width <| fillPortion 3 ] <| viewApplications model
            , el [ width <| fillPortion 3 ] <| viewSelected model.selected model.applications
            ]
        , Element.paragraph [ spacing 5, Font.italic, Element.centerX ]
            [ Element.text "There are currently "
            , Element.text (String.fromInt <| Dict.size model.applications)
            , Element.text " visible applications in Harrow HA7 postcodes. "
            ]
        ]


viewFilters : FrontendModel -> Element FrontendMsg
viewFilters model =
    -- Get some insights from data.
    let
        filterCheckbox msg currentSet label =
            Input.checkbox
                []
                { onChange = msg label
                , icon = Input.defaultCheckbox
                , checked = Set.member label currentSet
                , label = Input.labelRight [] <| text label
                }

        applicationTypes =
            collectVariants .application_type
                |> checkBoxes ToggleTypeFilter model.typeFilters

        statuses =
            collectVariants .status
                |> checkBoxes ToggleStatusFilter model.statusFilters

        decisions =
            collectVariants .decision
                |> checkBoxes ToggleDecisionFilter model.decisionFilters

        collectVariants : (Detail -> String) -> Set String
        collectVariants field =
            model.applications
                |> Dict.values
                |> List.map field
                |> Set.fromList
                |> Set.remove ""

        checkBoxes : (String -> Bool -> FrontendMsg) -> Set String -> Set String -> Element FrontendMsg
        checkBoxes msg current contents =
            Element.column [ spacing 2 ]
                (contents
                    |> Set.toList
                    |> List.map (filterCheckbox msg current)
                )

        constraintFilters =
            Element.column [ spacing 2 ]
                [ constraintFilter "Green belt" Green_belt_Toggle model.green_belt
                , constraintFilter "Flood risk" Flood_risk_zone_Toggle model.flood_risk_zone
                , constraintFilter "Conservation area" Conservation_area_Toggle model.conservation_area
                , constraintFilter "Tree preservation zone" Tree_preservation_zone_Toggle model.tree_preservation_zone
                , constraintFilter "Listed building" Listed_building_outline_Toggle model.listed_building_outline
                , constraintFilter "Article 4" Article_4_direction_area_Toggle model.article_4_direction_area
                , constraintFilter "AONB" Area_of_outstanding_natural_beauty_Toggle model.area_of_outstanding_natural_beauty
                , constraintFilter "SSSI" Site_of_special_scientific_interest_Toggle model.site_of_special_scientific_interest
                ]

        constraintFilter label msg current =
            Input.checkbox
                []
                { onChange = msg
                , icon = Input.defaultCheckbox
                , checked = current
                , label = Input.labelRight [] <| text label
                }
    in
    Element.column
        [ Element.width fill
        , Element.alignTop
        , padding 10
        , spacing 10
        , Element.alignLeft
        , Font.family
            [ Font.typeface "Helvetica"
            , Font.sansSerif
            ]
        , Font.light
        , Font.size 14
        ]
        [ applicationTypes
        , statuses
        , decisions
        , constraintFilters
        ]


linkToCouncil : Detail -> Element FrontendMsg
linkToCouncil detail =
    --e.g. https://planningsearch.harrow.gov.uk/planning/index.html?fa=getApplication&id=229024
    if detail.source_url == "" then
        Element.el
            [ Font.italic
            , Border.width 1
            , Border.rounded 5
            , padding 5
            , Border.color (rgb255 200 200 200)
            ]
            (Element.text "Link to application not available.")

    else
        Element.newTabLink
            [ Font.bold
            , Border.width 2
            , Border.rounded 5
            , padding 5
            , Border.color lightGreen
            ]
            { url = detail.source_url
            , label =
                Element.paragraph []
                    [ Element.text "Click here to view details or search on council planning portal" ]
            }


viewSelected : Maybe String -> Dict String Detail -> Element FrontendMsg
viewSelected id applications =
    case id of
        Just string ->
            Element.column
                [ padding 10
                , spacing 10
                ]
                [ case Dict.get string applications of
                    Just application ->
                        Element.column [ spacing 10 ]
                            [ viewOnMap application
                            , viewApplication application
                            , linkToCouncil application
                            ]

                    Nothing ->
                        Element.text "gone!"
                , Element.el [ Font.italic, alignRight ] <| Element.text string
                ]

        Nothing ->
            Element.el
                [ width (Element.px 400)
                , padding 10
                , spacing 10
                ]
            <|
                text "Please select from the list on the left."


mapsApiKey =
    "AIzaSyAZkdWkG0jidxa1gRF3MmY4GvbtaAWMpDY"


mapApiRoot =
    "https://maps.googleapis.com"


mapApiPath =
    [ "maps", "api", "staticmap" ]


viewOnMap : Detail -> Element FrontendMsg
viewOnMap detail =
    {- e.g.
       https://maps.googleapis.com/maps/api/staticmap
       ?center=40.714728,-73.998672&zoom=12&size=400x400&
       key=YOUR%5C_API%5C_KEY%5C%60
    -}
    let
        ( lat, long ) =
            ( detail.latitude, detail.longitude )

        coordString =
            String.fromFloat lat ++ "," ++ String.fromFloat long

        staticMapUrl =
            Builder.crossOrigin mapApiRoot
                mapApiPath
                [ Builder.string "center" coordString
                , Builder.string "markers" coordString
                , Builder.int "zoom" 16
                , Builder.string "size" "400x400"
                , Builder.string "key" mapsApiKey
                ]
    in
    Element.image []
        { src = staticMapUrl
        , description = "Google Map"
        }


viewApplications : FrontendModel -> Element FrontendMsg
viewApplications model =
    let
        activeConstraintFilters =
            [ model.green_belt
            , model.flood_risk_zone
            , model.conservation_area
            , model.tree_preservation_zone
            , model.listed_building_outline
            , model.article_4_direction_area
            , model.area_of_outstanding_natural_beauty
            , model.site_of_special_scientific_interest
            ]

        constraintFilter application =
            if List.any identity activeConstraintFilters then
                let
                    constraints =
                        [ application.green_belt
                        , application.flood_risk_zone /= ""
                        , application.conservation_area /= ""
                        , application.tree_preservation_zone
                        , application.listed_building_outline /= ""
                        , application.article_4_direction_area
                        , application.area_of_outstanding_natural_beauty
                        , application.site_of_special_scientific_interest
                        ]
                in
                List.any identity <| List.map2 (&&) constraints activeConstraintFilters

            else
                True
    in
    Element.column
        [ Element.height (Element.px 600)
        , Element.padding 10
        , Element.spacing 10
        , Element.scrollbarY
        , Background.color lightGreen
        ]
    <|
        List.map viewApplication <|
            List.filter constraintFilter <|
                (if Set.isEmpty model.typeFilters then
                    identity

                 else
                    List.filter (\a -> Set.member a.application_type model.typeFilters)
                )
                <|
                    (if Set.isEmpty model.statusFilters then
                        identity

                     else
                        List.filter (\a -> Set.member a.status model.statusFilters)
                    )
                    <|
                        (if Set.isEmpty model.decisionFilters then
                            identity

                         else
                            List.filter (\a -> Set.member a.decision model.decisionFilters)
                        )
                            (Dict.values model.applications)


viewApplication : Detail -> Element FrontendMsg
viewApplication detail =
    let
        asDetail =
            Element.column [ spacing 4 ]
                [ Element.el [ Font.bold ] <| Element.text detail.reference
                , Element.paragraph [] [ Element.text detail.address ]
                , case specials of
                    [] ->
                        Element.none

                    some ->
                        Element.wrappedRow [ spacing 4 ] some
                , Element.paragraph [] [ Element.text detail.description ]
                , Element.wrappedRow
                    [ Font.light, spacing 10 ]
                    [ Element.text detail.application_type
                    , case detail.decision of
                        "" ->
                            Element.text detail.status

                        decision ->
                            Element.el
                                [ Font.family
                                    [ Font.typeface "Impact"
                                    , Font.sansSerif
                                    ]
                                , Font.size 18
                                , Font.color (decisionColour decision)

                                --, Element.rotate 0.1
                                ]
                                (Element.text decision)
                    , Element.text <| DateUtils.dateFromPosix detail.lastChangeDate
                    ]
                ]

        decisionColour decision =
            case decision of
                "approved" ->
                    stanmoreGreen

                "refused" ->
                    paleRed

                _ ->
                    grey

        safeHex x =
            x |> hex |> Maybe.withDefault (rgb255 140 140 140)

        specials : List (Element FrontendMsg)
        specials =
            [ if detail.green_belt then
                Just <| constraintWidget "Green Belt" (safeHex "#48742E")

              else
                Nothing
            , if detail.flood_risk_zone /= "" then
                Just <| constraintWidget ("Flood risk " ++ detail.flood_risk_zone) (safeHex "#529AB0")

              else
                Nothing
            , if detail.conservation_area /= "" then
                Just <| constraintWidget detail.conservation_area (safeHex "#938057")

              else
                Nothing
            , if detail.tree_preservation_zone then
                Just <| constraintWidget "Tree preservation order" (safeHex "#848484")

              else
                Nothing
            , if detail.listed_building_outline /= "" then
                Just <| constraintWidget detail.listed_building_outline (safeHex "#AC247C")

              else
                Nothing
            , if detail.article_4_direction_area then
                Just <| constraintWidget "Article 4" (safeHex "#D77053")

              else
                Nothing
            , if detail.area_of_outstanding_natural_beauty then
                Just <| constraintWidget "AONB" (safeHex "#6474BC")

              else
                Nothing
            , if detail.site_of_special_scientific_interest then
                Just <| constraintWidget "SSSI" (safeHex "#4E82C3")

              else
                Nothing
            ]
                |> List.filterMap identity

        constraintWidget label colour =
            Element.el
                [ Border.rounded 5
                , padding 5
                , Background.color colour
                , Font.color (rgb255 255 255 255)
                ]
                (Element.text label)
    in
    Input.button
        [ Background.color stanmoreWhite
        , rounded 8
        , padding 5
        , width fill
        ]
        { onPress = Just <| Select <| detail.id
        , label = asDetail
        }
