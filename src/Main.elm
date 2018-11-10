module Main exposing (Model, Msg(..), constructURL, getImages, getImagesDecoder, init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
import Key exposing (key)
import Url.Builder as Url



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--TYPES


type alias Image =
    { link : String
    }



-- MODEL


type alias Model =
    { term : String
    , url : String
    , images : List Image
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "apollo" "waiting.gif" []
    , getImages "apollo"
    )



-- UPDATE


type Msg
    = FetchImages
    | NewImages (Result Http.Error (List Image))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchImages ->
            ( model
            , getImages model.term
            )

        NewImages result ->
            case result of
                Ok images ->
                    ( { model | images = images }
                    , Cmd.none
                    )

                Err _ ->
                    ( model
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.term ]
        , button [ onClick FetchImages ] [ text "More Please!" ]
        , br [] []
        , img [ src model.url ] []
        ]



-- HTTP


getImages : String -> Cmd Msg
getImages term =
    Http.send NewImages (Http.get (constructURL term) getImagesDecoder)


constructURL : String -> String
constructURL term =
    Url.crossOrigin "https://images-api.nasa.gov"
        [ "search" ]
        [ Url.string "q" term
        , Url.string "media_type" "image"
        ]


getImagesDecoder : Decode.Decoder (List Image)
getImagesDecoder =
    Decode.at [ "collection", "items" ] (Decode.list imageDecoder)


imageDecoder : Decode.Decoder Image
imageDecoder =
    Decode.succeed Image
        |> required "href" Decode.string
