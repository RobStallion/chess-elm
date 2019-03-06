module State exposing (init, update)

import Board exposing (startingBoard)
import Dict
import Move exposing (getPossibleMoves)
import Types exposing (..)


init : Model
init =
    { board = startingBoard }


update : Msg -> Model -> Model
update msg model =
    case msg of
        CheckAvailableMoves tileIndex ->
            let
                possMoves =
                    getPossibleMoves tileIndex model.board
            in
            { model | board = updatePossibleMoves possMoves model.board }


updatePossibleMoves : List Int -> Board -> Board
updatePossibleMoves possMoves board =
    List.foldl updatePossibleMove board possMoves


updatePossibleMove : Int -> Board -> Board
updatePossibleMove index board =
    Dict.update index (Maybe.map (\tile -> { tile | status = PossilbeMove })) board
