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


type alias Item =
    { href : String
    , links : List Link
    , data : List ImageData
    }


type alias Link =
    { href : String
    }


type alias ImageData =
    { title : String
    }



-- MODEL


type alias Model =
    { term : String
    , url : String
    , images : List Item
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        term =
            "rocket"
    in
    ( Model term "waiting.gif" []
    , getImages term
    )



-- UPDATE


type Msg
    = FetchImages
    | NewImages (Result Http.Error (List Item))


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
    div [ class "container" ]
        [ h2 [] [ text model.term ]
        , button [ onClick FetchImages ] [ text "More Please!" ]
        , br [] []
        , img [ src model.url ]
            []
        , div
            [ class "resultsGroup" ]
            (List.map renderItem model.images)
        ]


renderItem : Item -> Html msg
renderItem item =
    --Unwrap records from lists
    case getFirstItem item.links of
        Just link ->
            case getFirstItem item.data of
                Just imageData ->
                    --we know we have both img src url and image meta data
                    div [ class "item" ]
                        [ img [ src link.href ] []
                        , div [ class "itemDetails" ] [ text imageData.title ]
                        ]

                Nothing ->
                    text ""

        Nothing ->
            text ""


getFirstItem : List a -> Maybe a
getFirstItem list =
    List.head list



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


getImagesDecoder : Decode.Decoder (List Item)
getImagesDecoder =
    Decode.at [ "collection", "items" ] (Decode.list itemDecoder)


imageDecoder : Decode.Decoder Image
imageDecoder =
    Decode.succeed Image
        |> required "href" Decode.string


itemDecoder : Decode.Decoder Item
itemDecoder =
    Decode.succeed Item
        |> required "href" Decode.string
        |> required "links" (Decode.list linkDecoder)
        |> required "data" (Decode.list imageDataDecoder)


linkDecoder : Decode.Decoder Link
linkDecoder =
    Decode.succeed Link
        |> required "href" Decode.string


imageDataDecoder : Decode.Decoder ImageData
imageDataDecoder =
    Decode.succeed ImageData
        |> required "title" Decode.string
