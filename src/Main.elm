module Main exposing (init, main, update, view)

import Array
import Board exposing (createBoard, renderBoard)
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra
import Move exposing (getPossibleMoves)
import Piece exposing (createAllPieces)
import Types exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }


init : Model
init =
    { board = createBoard
    , pieces = createAllPieces
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        CheckAvailableMoves piece ->
            let
                _ =
                    Debug.log "" <| getPossibleMoves piece
            in
            -- { model | pieces = List.Extra.updateAt 0 movePiece model.pieces }
            model


view : Model -> Html Msg
view model =
    div []
        [ div [ class "mt4 flex justify-center" ]
            [ renderBoard model.pieces model.board
            ]
        ]
