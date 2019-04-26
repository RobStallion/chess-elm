module Move exposing (getPossibleMoves)

import Board exposing (getTile, outOfBoundsList)
import Dict
import Types exposing (..)


getPossibleMoves : Int -> Model -> List Int
getPossibleMoves tileIndex model =
    model
        |> .boardWithoutPossibleMoves
        |> getTilePiece tileIndex
        |> Maybe.withDefault (Piece King White)
        |> (\piece ->
                case piece.pieceType of
                    King ->
                        kingMoves tileIndex piece model.boardWithoutPossibleMoves

                    Queen ->
                        queenMoves tileIndex piece model.boardWithoutPossibleMoves

                    Rook ->
                        rookMoves tileIndex piece model.boardWithoutPossibleMoves

                    Bishop ->
                        bishopMoves tileIndex piece model.boardWithoutPossibleMoves

                    Knight ->
                        knightMoves tileIndex piece model.boardWithoutPossibleMoves

                    Pawn ->
                        let
                            enPassentCap =
                                canPawnCaptureEP tileIndex piece model
                        in
                        enPassentCap ++ pawnMoves tileIndex piece model.boardWithoutPossibleMoves
           )


didPawnMoveTwoSpaces : Int -> Piece -> Model -> Bool
didPawnMoveTwoSpaces currentIndex piece model =
    case model.previousMove of
        Just ( p, i1, i2 ) ->
            i2 == currentIndex && i1 == currentIndex + 20

        Nothing ->
            False


canPawnCaptureEP : Int -> Piece -> Model -> List Int
canPawnCaptureEP tileIndex piece model =
    case piece.team of
        Black ->
            []

        White ->
            if List.member tileIndex (List.range 52 59) then
                let
                    tile_int =
                        [ 1, -1 ]
                            |> List.map (\i -> ( tileIndex + i, getTilePiece (tileIndex + i) model.board ))
                            |> List.foldl
                                (\mp acc ->
                                    case mp of
                                        ( _, Nothing ) ->
                                            acc

                                        ( i, Just p ) ->
                                            if p.pieceType == Pawn && True then
                                                ( i, p ) :: acc

                                            else
                                                acc
                                )
                                []
                            |> List.foldl
                                (\( i, p ) acc ->
                                    if didPawnMoveTwoSpaces i p model then
                                        i + 10 :: acc

                                    else
                                        acc
                                )
                                []
                in
                tile_int

            else
                []


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
            getPawnMoves 10 20 9 11 tileIndex piece board

        Black ->
            getPawnMoves -10 -20 -9 -11 tileIndex piece board


getPawnMoves : Int -> Int -> Int -> Int -> Int -> Piece -> Board -> List Int
getPawnMoves oneSq twoSq capL capR tileIndex clickedPiece board =
    let
        possMoves =
            if
                isPawnOnStartingTile tileIndex clickedPiece
                    && isTileFree (tileIndex + oneSq) board
                    && isTileFree (tileIndex + twoSq) board
            then
                [ tileIndex + oneSq, tileIndex + twoSq ]

            else if isTileFree (tileIndex + oneSq) board then
                [ tileIndex + oneSq ]

            else
                []
    in
    possMoves
        |> checkTile tileIndex clickedPiece board capL
        |> checkTile tileIndex clickedPiece board capR


isPawnOnStartingTile : Int -> Piece -> Bool
isPawnOnStartingTile tileIndex piece =
    List.member tileIndex (pawnHomeIndexes piece)


pawnHomeIndexes : Piece -> List Int
pawnHomeIndexes piece =
    case piece.team of
        White ->
            List.range 22 29

        Black ->
            List.range 71 79


pawnPossCapEPIndexes : Piece -> List Int
pawnPossCapEPIndexes piece =
    case piece.team of
        Black ->
            List.range 42 49

        White ->
            List.range 52 59


checkTile : Int -> Piece -> Board -> Int -> List Int -> List Int
checkTile tileIndex clickedPiece board move intList =
    case getTilePiece (tileIndex + move) board of
        Nothing ->
            intList

        Just piece ->
            if sameTeam clickedPiece piece then
                intList

            else
                (tileIndex + move) :: intList


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
