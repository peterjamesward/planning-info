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
import Types exposing (..)
import Url
import Url.Builder as Builder


type alias Model =
    FrontendModel


stanmoreGreen =
    rgba255 75 107 70 0.2


stanmoreWhite =
    rgba255 255 255 255 1.0


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
            let
                id =
                    case application of
                        ApplicationDetail detail ->
                            detail.id

                        ApplicationSummary summary ->
                            summary.id
            in
            ( { model
                | applications =
                    Dict.insert id application model.applications
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
                Element.column [ Element.alignTop, padding 10, spacing 10, Element.alignLeft ]
                    [ Element.row []
                        [ viewApplications model.applications
                        , viewSelected model.selected model.applications
                        ]
                    , Element.paragraph [ spacing 5, Font.italic, Element.centerX ]
                        [ Element.text "There are currently "
                        , Element.text (String.fromInt <| Dict.size model.applications)
                        , Element.text " visible applications with special conditions in Harrow HA7 postcodes. "
                        ]
                    ]
    }


linkToCouncil : Application -> Element FrontendMsg
linkToCouncil application =
    case application of
        ApplicationDetail detail ->
            Element.newTabLink
                [ Font.bold
                , Border.width 2
                , Border.rounded 5
                , padding 5
                , Border.color stanmoreGreen
                ]
                { url = detail.source_url
                , label =
                    Element.paragraph []
                        [ Element.text "Click here to view details or search on council planning portal" ]
                }

        ApplicationSummary _ ->
            Element.none


viewSelected : Maybe String -> Dict String Application -> Element FrontendMsg
viewSelected id applications =
    case id of
        Just string ->
            Element.column
                [ width (Element.px 400)
                , padding 10
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


viewOnMap : Application -> Element FrontendMsg
viewOnMap application =
    {- e.g.
       https://maps.googleapis.com/maps/api/staticmap
       ?center=40.714728,-73.998672&zoom=12&size=400x400&
       key=YOUR%5C_API%5C_KEY%5C%60
    -}
    let
        ( lat, long ) =
            case application of
                ApplicationSummary summary ->
                    ( summary.latitude, summary.longitude )

                ApplicationDetail detail ->
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


viewApplications : Dict String Application -> Element FrontendMsg
viewApplications applications =
    Element.column
        [ Element.height (Element.px 600)
        , Element.width (Element.px 400)
        , Element.padding 10
        , Element.spacing 10
        , Element.scrollbarY
        , Background.color stanmoreGreen
        ]
    <|
        List.map viewApplication <|
            Dict.values applications


viewApplication : Application -> Element FrontendMsg
viewApplication application =
    let
        applicationId =
            case application of
                ApplicationSummary summary ->
                    summary.id

                ApplicationDetail detail ->
                    detail.id

        asSummary summary =
            Element.column [ spacing 4 ]
                [ Element.el [ Font.bold ] <| Element.text summary.reference
                , Element.paragraph [] [ Element.text summary.address ]
                , Element.row
                    [ Font.light, spacing 10 ]
                    [ Element.text summary.application_type
                    , Element.text summary.status
                    , Element.text summary.date_received
                    ]
                ]

        asDetail : Detail -> Element FrontendMsg
        asDetail detail =
            --TODO: Decision!
            Element.column [ spacing 4 ]
                [ Element.el [ Font.bold ] <| Element.text detail.reference
                , Element.paragraph [] [ Element.text detail.address ]
                , case specials detail of
                    [] ->
                        Element.none

                    some ->
                        Element.wrappedRow [ spacing 4 ] some
                , Element.paragraph [] [ Element.text detail.description ]
                , Element.row
                    [ Font.light, spacing 10 ]
                    [ Element.text detail.application_type
                    , Element.text detail.status
                    , case detail.decision of
                        "" ->
                            Element.none

                        _ ->
                            Element.el
                                [ Font.family
                                    [ Font.typeface "Impact"
                                    , Font.sansSerif
                                    ]
                                , Font.size 24
                                , Element.rotate 0.1
                                ]
                                (Element.text detail.decision)
                    , Element.text <| DateUtils.dateFromPosix detail.lastChangeDate
                    ]
                ]

        safeHex x =
            x |> hex |> Maybe.withDefault (rgb255 140 140 140)

        specials : Detail -> List (Element FrontendMsg)
        specials detail =
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
        { onPress = Just <| Select <| applicationId
        , label =
            case application of
                ApplicationSummary summary ->
                    asSummary summary

                ApplicationDetail detail ->
                    asDetail detail
        }
