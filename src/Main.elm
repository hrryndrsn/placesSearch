module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, h2, img, input, p, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)



---- MODEL ----


type alias Model =
    { searchTerm : String
    , results : List Result
    }


type alias Result =
    { name : String
    , description : String
    , address : String
    }


init : ( Model, Cmd Msg )
init =
    ( { searchTerm = "thai food"
      , results =
            -- sample data
            [ { name = "sampleResult"
              , description = "A sample zord result"
              , address = " 101 fake street"
              }
            , { name = "sampleResult"
              , description = "A sample zord result"
              , address = " 101 fake street"
              }
            , { name = "sampleResult"
              , description = "A sample zord result"
              , address = " 101 fake street"
              }
            , { name = "sampleResult"
              , description = "A sample zord result"
              , address = " 101 fake street"
              }
            ]
      }
    , Cmd.none
    )



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
            -- Search box
            [ div [ class "search" ]
                [ input
                    [ class "searchInput"
                    , value model.searchTerm
                    , onInput SearchTermChange
                    , placeholder "Search for places"
                    ]
                    []
                ]

            -- results
            , ul [ class "results" ]
                -- replace with array of results
                (List.map
                    renderResult
                    model.results
                )
            ]
        ]



---- VIEW HELPERS ---


renderResult : Result -> Html msg
renderResult res =
    div [ class "result" ]
        [ h2 [] [ text res.name ]
        , p [] [ text res.description ]
        , p [] [ text res.address ]
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
