module Main exposing (init, main, update, view)

import Board exposing (..)
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Types exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }


init : Model
init =
    { board = startingBoard }


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddPieces ->
            model


view : Model -> Html Msg
view model =
    div []
        [ div [ class "mt4" ] [ renderBoard model.board ]
        , button [ onClick AddPieces ] [ text "add pieces" ]
        ]
