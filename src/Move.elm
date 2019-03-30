module Move exposing (getPossibleMoves)

import Board exposing (getTile, outOfBoundsList)
import Dict
import Types exposing (..)


getPossibleMoves : Int -> Board -> List Int
getPossibleMoves tileIndex board =
    board
        |> getTilePiece tileIndex
        |> Maybe.withDefault (Piece King White Alive)
        |> (\piece ->
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

                    Pawn _ ->
                        pawnMoves tileIndex piece board
           )


kingMoves : Int -> Piece -> Board -> List Int
kingMoves tileIndex piece board =
    [ 1, 9, 10, 11, -1, -9, -10, -11 ]
        |> List.map (\i -> tileIndex + i)
        |> filterOutOfBoundsMoves
        |> filterMovesWhereTileContainsSameTeam piece board


queenMoves : Int -> Piece -> Board -> List Int
queenMoves tileIndex piece board =
    rookMoves tileIndex piece board ++ bishopMoves tileIndex piece board


rookMoves : Int -> Piece -> Board -> List Int
rookMoves tileIndex piece board =
    List.foldl (\i acc -> addMovesToList tileIndex piece board i acc) [] [ 1, -1, 10, -10 ]


bishopMoves : Int -> Piece -> Board -> List Int
bishopMoves tileIndex piece board =
    List.foldl (\i acc -> addMovesToList tileIndex piece board i acc) [] [ 9, -9, 11, -11 ]


knightMoves : Int -> Piece -> Board -> List Int
knightMoves tileIndex piece board =
    [ 21, 19, 12, 8, -21, -19, -12, -8 ]
        |> List.map (\i -> tileIndex + i)
        |> filterOutOfBoundsMoves
        |> filterMovesWhereTileContainsSameTeam piece board


pawnMoves : Int -> Piece -> Board -> List Int
pawnMoves tileIndex piece board =
    case piece.team of
        White ->
            checkEnPassant tileIndex piece board :: getPawnMoves Light 10 20 9 11 tileIndex piece board

        Black ->
            getPawnMoves Dark -10 -20 -9 -11 tileIndex piece board


getPawnMoves : Colour -> Int -> Int -> Int -> Int -> Int -> Piece -> Board -> List Int
getPawnMoves colour oneSq twoSq capL capR tileIndex clickedPiece board =
    let
        possMoves =
            if allowTwoSpaceMove tileIndex clickedPiece && isTileFree (tileIndex + oneSq) board && isTileFree (tileIndex + twoSq) board then
                [ tileIndex + oneSq, tileIndex + twoSq ]

            else if isTileFree (tileIndex + oneSq) board then
                [ tileIndex + oneSq ]

            else
                []
    in
    possMoves
        |> checkTile tileIndex clickedPiece board capL
        |> checkTile tileIndex clickedPiece board capR


allowTwoSpaceMove : Int -> Piece -> Bool
allowTwoSpaceMove tileIndex piece =
    List.member tileIndex (pawnHomeIndexes piece)


pawnHomeIndexes : Piece -> List Int
pawnHomeIndexes piece =
    case piece.team of
        White ->
            List.range 22 29

        Black ->
            List.range 71 79


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


checkEnPassant : Int -> Piece -> Board -> Int
checkEnPassant tileIndex clickedPiece board =
    let
        neighbouringTilePieces =
            [ 1, -1 ]
                |> List.map (\i -> getTilePiece (tileIndex + i) board)
                |> List.filter (\piece -> piece /= Nothing)
    in
    case clickedPiece.team of
        Black ->
            0

        White ->
            0


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
    if piece.team == piece2.team then
        True

    else
        False


filterOutOfBoundsMoves : List Int -> List Int
filterOutOfBoundsMoves possMoves =
    possMoves
        |> List.filter (\i -> not <| List.member i outOfBoundsList)
        |> List.filter (\i -> i > 1 && i < 100)


filterMovesWhereTileContainsSameTeam : Piece -> Board -> List Int -> List Int
filterMovesWhereTileContainsSameTeam piece board possMoves =
    List.filter (\i -> not <| doesTileContainSameColourPiece i piece board) possMoves
