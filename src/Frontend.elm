module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (Element, alignRight, centerY, el, fill, fillPortion, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Lamdera exposing (sendToBackend)
import Types exposing (..)
import Url


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
                Element.row [ width fill ]
                    [ el [ width <| fillPortion 1 ] (viewApplications model.applications)
                    , el [ width <| fillPortion 1 ] (viewSelected model.selected model.applications)
                    ]
    }


viewSelected : Maybe String -> Dict String Application -> Element FrontendMsg
viewSelected id applications =
    case id of
        Just string ->
            text string

        Nothing ->
            text "Nothing selected"


viewApplications : Dict String Application -> Element FrontendMsg
viewApplications applications =
    Element.column
        [ Element.height (Element.px 600)
        , Element.padding 20
        , Element.spacing 10
        ]
        [ Element.row [ spacing 5, Font.italic ]
            [ Element.text "There are currently"
            , Element.text (String.fromInt <| Dict.size applications)
            , Element.text "visible applications in HA7."
            ]
        , Element.el [ Font.italic ] <|
            Element.text "This is refreshed once each work day."
        , Element.column
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
    case application of
        ApplicationSummary summary ->
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

        ApplicationDetail detail ->
            Element.column [ spacing 4 ]
                [ Input.button
                    [ Background.color <| Element.rgb255 180 180 250
                    , padding 4
                    ]
                    { onPress = Just (Select detail.id)
                    , label = text detail.reference
                    }
                , Element.text detail.address
                , Element.row
                    [ Font.light, spacing 10 ]
                    [ Element.text detail.application_type
                    , Element.text detail.status
                    , Element.text detail.date_received
                    , Element.text "DETAIL"
                    ]
                ]
