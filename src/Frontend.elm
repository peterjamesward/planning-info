module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict
import Html
import Html.Attributes as Attr
import Lamdera exposing (sendToBackend)
import Types exposing (..)
import Url


plannexus =
    --"https://api.plannexus.io/v1/applications?postcode=HA7&authority_id=ef340ad8-1a60-43a8-b741-2483f6919d3f&per_page=100"
    "https://api.plannexus.io/v1/"


apiKey =
    --Should not be here, I know.
    "pn_live_dd5a90f71013ea2de78bec1c48349d0d6a9dd7bf35e07099"


harrowUid =
    "ef340ad8-1a60-43a8-b741-2483f6919d3f"


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
      -- Really requesting a set of summaries.
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
            ( { model | summaries = dict }, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
            [ Html.img [ Attr.src "https://lamdera.app/lamdera-logo-black.png", Attr.width 150 ] []
            , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "40px"
                ]
                [ Html.text "Ehllo orldw" ]
            ]
        ]
    }
