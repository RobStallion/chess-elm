module Move exposing (getPossibleMoves)

import Board exposing (getTile, outOfBoundsList)
import Dict
import Types exposing (..)


getPossibleMoves : Int -> Board -> List Int
getPossibleMoves tileIndex board =
    let
        maybePiece =
            board
                |> getTile tileIndex
                |> .piece
    in
    case maybePiece of
        Nothing ->
            []

        Just piece ->
            case piece.pieceType of
                King ->
                    kingMoves tileIndex piece board

                Queen ->
                    queenMoves tileIndex piece board

                Rook ->
                    rookMoves tileIndex piece board

                Bishop ->
                    bishopMoves tileIndex piece board

                Knight ->
                    knightMoves tileIndex piece board

                Pawn ->
                    pawnMoves tileIndex piece board


kingMoves : Int -> Piece -> Board -> List Int
kingMoves tileIndex piece board =
    [ 1, 9, 10, 11, -1, -9, -10, -11 ]
        |> List.map (\i -> tileIndex + i)
        |> List.filter (\i -> not <| List.member i outOfBoundsList)
        |> List.filter (\i -> not <| doesTileContainSameColourPiece i piece board)


queenMoves : Int -> Piece -> Board -> List Int
queenMoves tileIndex piece board =
    rookMoves tileIndex piece board ++ bishopMoves tileIndex piece board


rookMoves : Int -> Piece -> Board -> List Int
rookMoves tileIndex piece board =
    addMovesToList tileIndex piece board 1 []
        ++ addMovesToList tileIndex piece board -1 []
        ++ addMovesToList tileIndex piece board 10 []
        ++ addMovesToList tileIndex piece board -10 []


bishopMoves : Int -> Piece -> Board -> List Int
bishopMoves tileIndex piece board =
    addMovesToList tileIndex piece board 9 []
        ++ addMovesToList tileIndex piece board -9 []
        ++ addMovesToList tileIndex piece board 11 []
        ++ addMovesToList tileIndex piece board -11 []


knightMoves : Int -> Piece -> Board -> List Int
knightMoves tileIndex piece board =
    [ 21, 19, 12, 8, -21, -19, -12, -8 ]
        |> List.map (\i -> tileIndex + i)
        |> List.filter (\i -> not <| List.member i outOfBoundsList)
        |> List.filter (\i -> i > 1 && i < 100)
        |> List.filter (\i -> not <| doesTileContainSameColourPiece i piece board)


pawnMoves : Int -> Piece -> Board -> List Int
pawnMoves tileIndex piece board =
    case piece.colour of
        Light ->
            lightPawnMoves tileIndex piece board

        Dark ->
            darkPawnMoves tileIndex piece board


lightPawnMoves : Int -> Piece -> Board -> List Int
lightPawnMoves tileIndex clickedPiece board =
    let
        possMoves =
            if isTileFree (tileIndex + 10) board && isTileFree (tileIndex + 20) board then
                [ tileIndex + 10, tileIndex + 20 ]

            else if isTileFree (tileIndex + 10) board then
                [ tileIndex + 10 ]

            else
                []
    in
    possMoves
        |> checkTile tileIndex clickedPiece board 9
        |> checkTile tileIndex clickedPiece board 11


darkPawnMoves : Int -> Piece -> Board -> List Int
darkPawnMoves tileIndex clickedPiece board =
    let
        possMoves =
            if isTileFree (tileIndex - 10) board && isTileFree (tileIndex - 20) board then
                [ tileIndex - 10, tileIndex - 20 ]

            else if isTileFree (tileIndex - 10) board then
                [ tileIndex - 10 ]

            else
                []
    in
    possMoves
        |> checkTile tileIndex clickedPiece board -9
        |> checkTile tileIndex clickedPiece board -11


allowTwoSpaceMove : Int -> Piece -> Int -> List Int -> List Int
allowTwoSpaceMove tileIndex clickedPiece count intList =
    if List.member tileIndex (pawnHomeIndexes clickedPiece) then
        (tileIndex + count) :: intList

    else
        intList


checkTile : Int -> Piece -> Board -> Int -> List Int -> List Int
checkTile tileIndex clickedPiece board count intList =
    case getTilePiece (tileIndex + count) board of
        Nothing ->
            intList

        Just piece ->
            if sameTeam clickedPiece piece then
                intList

            else
                (tileIndex + count) :: intList


pawnHomeIndexes : Piece -> List Int
pawnHomeIndexes piece =
    case piece.colour of
        Light ->
            List.range 22 29

        Dark ->
            List.range 71 79



-- pawnMoves : Int -> Piece -> Board -> List Int
-- pawnMoves tileIndex piece board =
--     let
--         moves =
--             case piece.colour of
--                 Light ->
--                     [ 9, 10, 11, 20 ]
--
--                 Dark ->
--                     [ -9, -10, -11, -20 ]
--     in
--     moves
--         |> List.map (\i -> tileIndex + i)
--         |> checkTwoTileMove tileIndex piece
--         |> List.filter (\i -> not <| List.member i outOfBoundsList)
--
--
-- checkTwoTileMove : Int -> Piece -> List Int -> List Int
-- checkTwoTileMove tileIndex piece moves =
--     if isPawnOnStartingPosition tileIndex piece then
--         moves
--
--     else
--         List.filter (\i -> not <| i == tileIndex + 20) moves
--
--
-- isPawnOnStartingPosition : Int -> Piece -> Bool
-- isPawnOnStartingPosition tileIndex piece =
--     List.member tileIndex <| pawnHomeIndexes piece
--
--


addMovesToList : Int -> Piece -> Board -> Int -> List Int -> List Int
addMovesToList tileIndex piece board count acc =
    if doesTileContainOpposingPiece (tileIndex + count) piece board then
        tileIndex + count :: acc

    else if List.member (tileIndex + count) outOfBoundsList || doesTileContainSameColourPiece (tileIndex + count) piece board then
        acc

    else
        addMovesToList (tileIndex + count) piece board count (tileIndex + count :: acc)


getTilePiece : Int -> Board -> Maybe Piece
getTilePiece tileIndex board =
    board |> getTile tileIndex |> .piece


isTileFree : Int -> Board -> Bool
isTileFree tileIndex board =
    board
        |> getTilePiece tileIndex
        |> (\maybePiece ->
                case maybePiece of
                    Nothing ->
                        True

                    Just piece ->
                        False
           )


doesTileContainOpposingPiece : Int -> Piece -> Board -> Bool
doesTileContainOpposingPiece tileIndex clickedPiece board =
    board
        |> getTilePiece tileIndex
        |> (\maybePiece ->
                case maybePiece of
                    Nothing ->
                        False

                    Just piece ->
                        not <| sameTeam piece clickedPiece
           )


doesTileContainSameColourPiece : Int -> Piece -> Board -> Bool
doesTileContainSameColourPiece tileIndex clickedPiece board =
    board
        |> getTilePiece tileIndex
        |> (\tilePiece ->
                case tilePiece of
                    Nothing ->
                        False

                    Just piece ->
                        sameTeam piece clickedPiece
           )


sameTeam : Piece -> Piece -> Bool
sameTeam piece piece2 =
    if piece.colour == piece2.colour then
        True

    else
        False
