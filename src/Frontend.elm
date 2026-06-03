module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (Element, alignLeft, alignRight, centerY, el, fill, fillPortion, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font
import Element.Input as Input
import Html
import Lamdera exposing (sendToBackend)
import Types exposing (..)
import Url
import Url.Builder as Builder


type alias Model =
    FrontendModel


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
                        [ el [ width <| fillPortion 1 ] (viewApplications model.applications)
                        , el [ width <| fillPortion 1 ] (viewSelected model.selected model.applications)
                        ]
                    , Element.paragraph [ spacing 5, Font.italic, Element.centerX ]
                        [ Element.text "There are currently "
                        , Element.text (String.fromInt <| Dict.size model.applications)
                        , Element.text " visible applications in Harrow for HA7 postcodes. "
                        , Element.el [ Font.italic ] <|
                            Element.text "We refresh our data once each work day."
                        ]
                    ]
    }


viewSelected : Maybe String -> Dict String Application -> Element FrontendMsg
viewSelected id applications =
    case id of
        Just string ->
            Element.column [ padding 10, spacing 10, alignLeft, centerY ]
                [ case Dict.get string applications of
                    Just application ->
                        Element.column [ spacing 10 ]
                            [ viewOnMap application
                            , viewApplication application
                            ]

                    Nothing ->
                        Element.text "gone!"
                , Element.el [ Font.italic, alignRight ] <| Element.text string
                ]

        Nothing ->
            text "Nothing selected"


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
                , Builder.string "size" "600x400"
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
        , Element.padding 20
        , Element.spacing 10
        ]
        [ Element.column
            [ Element.height (Element.px 600)
            , Element.spacing 10
            , Element.scrollbarY
            , Element.scrollbars
            ]
          <|
            List.map viewApplication <|
                Dict.values applications
        ]


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
                , Element.text summary.address
                , Element.row
                    [ Font.light, spacing 10 ]
                    [ Element.text summary.application_type
                    , Element.text summary.status
                    , Element.text summary.date_received
                    ]
                ]

        asDetail : Detail -> Element FrontendMsg
        asDetail detail =
            Element.column [ spacing 4 ]
                [ Element.el [ Font.bold ] <| Element.text detail.reference
                , Element.text detail.address
                , case specials detail of
                    [] ->
                        Element.none

                    some ->
                        Element.wrappedRow [ spacing 4 ] some
                , Element.row
                    [ Font.light, spacing 10 ]
                    [ Element.text detail.application_type
                    , Element.text detail.status
                    , Element.text detail.date_received
                    ]
                ]

        specials : Detail -> List (Element FrontendMsg)
        specials detail =
            [ if detail.green_belt then
                "Green Belt"

              else
                ""
            , case detail.flood_risk_zone of
                "" ->
                    ""

                content ->
                    "Flood risk " ++ content
            , detail.conservation_area
            , if detail.tree_preservation_zone then
                "Tree preservation order"

              else
                ""
            , detail.listed_building_outline
            , detail.article_4_direction_area
            , if detail.area_of_outstanding_natural_beauty then
                "AONB"

              else
                ""
            , if detail.site_of_special_scientific_interest then
                "SSSI"

              else
                ""
            ]
                |> List.filter (\s -> s /= "")
                |> List.map
                    (\s ->
                        Element.el
                            [ Border.rounded 5
                            , padding 5
                            , Background.color (rgb255 255 100 100)
                            , Font.color (rgb255 255 255 255)
                            ]
                            (Element.text s)
                    )
    in
    Input.button
        [ Background.color <| Element.rgb255 220 220 250
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
