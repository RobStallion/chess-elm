module Main exposing (init, main, update, view)

import Board exposing (..)
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra
import Types exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }


init : Model
init =
    { board = createBoard
    , pieces = createLightPawns <| List.range 22 29
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        CheckAvailableMoves piece ->
            { model | pieces = List.Extra.updateAt 0 movePiece model.pieces }


view : Model -> Html Msg
view model =
    div []
        [ div [ class "mt4" ] [ renderBoard model.pieces model.board ]
        ]
