module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)



---- MODEL ----


type alias Model =
    { searchTerm : String }


init : ( Model, Cmd Msg )
init =
    ( { searchTerm = "thai food" }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | SearchTermChange String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchTermChange term ->
            ( { model | searchTerm = term }, Cmd.none )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        -- Nav bar
        [ div [ class "nav" ]
            [ img
                [ src "/logo.svg" ]
                []
            , div [ class "siteTitle" ]
                [ text "Zording with elm "
                ]
            ]

        -- Main body
        , div
            [ class "container" ]
            [ input
                [ class "searchInput"
                , value model.searchTerm
                , onInput SearchTermChange
                ]
                []
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
