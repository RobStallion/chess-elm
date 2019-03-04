module State exposing (init, update)

import Board exposing (startingBoard)
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

                _ =
                    Debug.log "" possMoves
            in
            model
