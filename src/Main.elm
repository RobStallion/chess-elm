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
                possMoves =
                    getPossibleMoves piece

                poss =
                    List.filter (\index -> not <| doesTileContainPiece piece index model) possMoves

                _ =
                    Debug.log "" poss
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


doesTileContainPiece : Piece -> Int -> Model -> Bool
doesTileContainPiece currentPiece index model =
    case List.filter (\piece -> piece.index == index) model.pieces of
        [ piece ] ->
            currentPiece.colour == piece.colour

        _ ->
            False
