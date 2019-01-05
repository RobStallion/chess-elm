module Move exposing (getPossibleMoves)

import Board exposing (outOfBoundsList)
import List.Extra
import Types exposing (..)


getPossibleMoves : Piece -> List Int
getPossibleMoves piece =
    case piece.piece of
        King ->
            kingMoves piece

        Queen ->
            queenMoves piece

        Rook ->
            rookMoves piece

        Bishop ->
            bishopMoves piece

        Knight ->
            knightMoves piece

        Pawn ->
            pawnMoves piece


kingMoves : Piece -> List Int
kingMoves piece =
    [ 1, 9, 10, 11, -1, -9, -10, -11 ]
        |> List.map (\i -> piece.index + i)
        |> List.filter (\i -> not <| List.member i outOfBoundsList)


queenMoves : Piece -> List Int
queenMoves piece =
    rookMoves piece ++ bishopMoves piece


knightMoves : Piece -> List Int
knightMoves piece =
    [ 21, 19, 12, 8, -21, -19, -12, -8 ]
        |> List.map (\i -> piece.index + i)
        |> List.filter (\i -> not <| List.member i outOfBoundsList)
        |> List.filter (\i -> i > 1 && i < 100)


rookMoves : Piece -> List Int
rookMoves piece =
    addMovesToList piece.index 1 []
        ++ addMovesToList piece.index -1 []
        ++ addMovesToList piece.index 10 []
        ++ addMovesToList piece.index -10 []


bishopMoves : Piece -> List Int
bishopMoves piece =
    addMovesToList piece.index 9 []
        ++ addMovesToList piece.index -9 []
        ++ addMovesToList piece.index 11 []
        ++ addMovesToList piece.index -11 []


pawnMoves : Piece -> List Int
pawnMoves piece =
    let
        moves =
            case piece.colour of
                Light ->
                    [ 9, 10, 11, 20 ]

                Dark ->
                    [ -9, -10, -11, -20 ]
    in
    moves
        |> List.map (\i -> piece.index + i)
        |> List.filter (\i -> not <| List.member i outOfBoundsList)
        |> checkTwoTileMove piece



-- if pawn is on home square, (for now) remove the plus 20 move


checkTwoTileMove : Piece -> List Int -> List Int
checkTwoTileMove piece moves =
    if isPawnOnStartingPosition piece then
        moves

    else
        List.filter (\i -> not <| i == piece.index + 20) moves


isPawnOnStartingPosition : Piece -> Bool
isPawnOnStartingPosition piece =
    List.member piece.index <| pawnHomeIndexes piece


pawnHomeIndexes : Piece -> List Int
pawnHomeIndexes piece =
    case piece.colour of
        Light ->
            List.range 22 29

        Dark ->
            List.range 71 79


addMovesToList : Int -> Int -> List Int -> List Int
addMovesToList index count acc =
    if List.member (index + count) outOfBoundsList then
        acc

    else
        addMovesToList (index + count) count (index + count :: acc)
