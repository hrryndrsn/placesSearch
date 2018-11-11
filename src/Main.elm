module Main exposing (Model, Msg(..), constructURL, getImages, getImagesDecoder, init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, href, id, placeholder, src, value)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
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
    , description : String
    , keywords : List String
    }


type GridSetting
    = OneCol
    | TwoCol
    | FourCol



-- MODEL


type alias Model =
    { term : String
    , url : String
    , images : List Item
    , isLoading : Bool
    , gridSetting : GridSetting
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        term =
            "black hole"
    in
    ( Model term
        "waiting.gif"
        []
        False
        TwoCol
    , getImages term
    )



-- UPDATE


type Msg
    = FetchImages
    | NewImages (Result Http.Error (List Item))
    | UpdateSearchTerm String
    | KeyDown Int
    | SetGrid GridSetting


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
                    ( { model
                        | images = images
                        , isLoading = False
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model
                    , Cmd.none
                    )

        UpdateSearchTerm term ->
            ( { model | term = term }
            , Cmd.none
            )

        KeyDown key ->
            if key == 13 then
                ( { model
                    | images = []
                    , isLoading = True
                  }
                , getImages model.term
                )

            else
                ( model, Cmd.none )

        SetGrid newSetting ->
            case newSetting of
                OneCol ->
                    ( { model
                        | gridSetting = newSetting
                      }
                    , Cmd.none
                    )

                TwoCol ->
                    ( { model
                        | gridSetting = newSetting
                      }
                    , Cmd.none
                    )

                FourCol ->
                    ( { model
                        | gridSetting = newSetting
                      }
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
        -- page title
        [ h2 [ class "siteTitle", id "top" ] [ a [ href "https://images.nasa.gov/" ] [ text "Explore images-api.nasa.gov" ] ]

        -- search input and button
        , div [ class "searchGroup" ]
            [ input
                [ class "searchInput"
                , value model.term
                , onInput UpdateSearchTerm
                , onKeyDown KeyDown
                , placeholder "Search for images"
                , autofocus
                    True
                ]
                []
            ]

        -- results
        , div
            [ class (resultsGroupClassList model.gridSetting) ]
            -- if isLoading is true show the spinner, else show the results
            (case model.isLoading of
                True ->
                    [ div [ class "loader" ]
                        [ div [ class "line" ] []
                        , div [ class "line" ] []
                        , div [ class "line" ] []
                        ]
                    ]

                False ->
                    List.map renderItem model.images
            )
        , renderGridControls
        ]


renderGridControls : Html Msg
renderGridControls =
    div [ class "gridSettings" ]
        [ a [ href "#top" ]
            [ button [ class "gridSettingButton" ] [ text "Top" ]
            ]
        , button [ class "gridSettingButton", onClick (SetGrid OneCol) ] [ text "1" ]
        , button [ class "gridSettingButton", onClick (SetGrid TwoCol) ] [ text "2" ]
        , button [ class "gridSettingButton", onClick (SetGrid FourCol) ] [ text "3" ]
        ]


resultsGroupClassList : GridSetting -> String
resultsGroupClassList gridSetting =
    case gridSetting of
        OneCol ->
            "resultsGroup oneCol"

        TwoCol ->
            "resultsGroup twoCol"

        FourCol ->
            "resultsGroup fourCol"


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Decode.map tagger keyCode)


renderItem : Item -> Html msg
renderItem item =
    --Unwrap records from lists
    case getFirstItem item.links of
        Just link ->
            case getFirstItem item.data of
                Just imageData ->
                    --we know we have both img src url and image meta data
                    div [ class "item" ]
                        [ div [ class "itemImage" ]
                            [ img [ src link.href ] []
                            ]
                        , div [ class "itemDetails" ]
                            [ p [ class "itemName" ] [ text imageData.title ]
                            ]
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
        |> optional "description" Decode.string "Classified"
        |> hardcoded [ "keywords", "keywords" ]
