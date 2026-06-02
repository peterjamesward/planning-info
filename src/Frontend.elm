module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (Element, alignRight, centerY, el, fill, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html
import Html.Attributes as Attr
import Lamdera exposing (sendToBackend)
import Time
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
      , summaries = Dict.empty
      , detail = Nothing
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


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        CachedSummaries dict ->
            ( { model | summaries = dict }
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
                viewSummaries model.summaries
    }


viewSummaries : Dict String Summary -> Element FrontendMsg
viewSummaries summaries =
    Element.column
        [ Element.height (Element.px 600)
        , Element.padding 20
        , Element.spacing 10
        ]
        [ Element.row [ spacing 5, Font.italic ]
            [ Element.text "There are currently"
            , Element.text (String.fromInt <| Dict.size summaries)
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
            List.map viewSummary <|
                Dict.values summaries
        ]


viewSummary : Summary -> Element FrontendMsg
viewSummary summary =
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
