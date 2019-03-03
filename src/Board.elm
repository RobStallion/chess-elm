module Board exposing (getTile, outOfBoundsList, startingBoard)

import Dict
import Types exposing (..)


startingBoard : Board
startingBoard =
    List.range 1 100
        |> List.map (\i -> ( i, createTiles i ))
        |> Dict.fromList


outOfBoundsList : List Int
outOfBoundsList =
    List.range 1 10
        ++ List.range 91 100
        ++ (List.map (\n -> n * 10 + 1) <| List.range 1 8)
        ++ (List.map (\n -> n * 10) <| List.range 2 9)


getTile : Int -> Board -> Tile
getTile tileIndex board =
    board
        |> Dict.get tileIndex
        |> Maybe.withDefault (Tile WithinBounds Nothing)


createTiles : Int -> Tile
createTiles int =
    if isPiece int lightPawnIndexes then
        addPieceToTile <| Piece Pawn Light Alive

    else if isPiece int lightRookIndexes then
        addPieceToTile <| Piece Rook Light Alive

    else if isPiece int lightKnightIndexes then
        addPieceToTile <| Piece Knight Light Alive

    else if isPiece int lightBishopIndexes then
        addPieceToTile <| Piece Bishop Light Alive

    else if int == lightKingIndex then
        addPieceToTile <| Piece King Light Alive

    else if int == lightQueenIndex then
        addPieceToTile <| Piece Queen Light Alive

    else if isPiece int darkPawnIndexes then
        addPieceToTile <| Piece Pawn Dark Alive

    else if isPiece int darkRookIndexes then
        addPieceToTile <| Piece Rook Dark Alive

    else if isPiece int darkKnightIndexes then
        addPieceToTile <| Piece Knight Dark Alive

    else if isPiece int darkBishopIndexes then
        addPieceToTile <| Piece Bishop Dark Alive

    else if int == darkKingIndex then
        addPieceToTile <| Piece King Dark Alive

    else if int == darkQueenIndex then
        addPieceToTile <| Piece Queen Dark Alive

    else
        createEmptyTile int


createEmptyTile : Int -> Tile
createEmptyTile index =
    Tile (emptyTileStatus index) Nothing


addPieceToTile : Piece -> Tile
addPieceToTile piece =
    Tile WithinBounds <| Just piece


isPiece : Int -> List Int -> Bool
isPiece int intList =
    List.member int intList


lightPawnIndexes : List Int
lightPawnIndexes =
    List.range 22 29


lightRookIndexes : List Int
lightRookIndexes =
    [ 12, 19 ]


lightKnightIndexes : List Int
lightKnightIndexes =
    [ 13, 18 ]


lightBishopIndexes : List Int
lightBishopIndexes =
    [ 14, 17 ]


lightQueenIndex : Int
lightQueenIndex =
    15


lightKingIndex : Int
lightKingIndex =
    16


darkPawnIndexes : List Int
darkPawnIndexes =
    List.range 72 79


darkRookIndexes : List Int
darkRookIndexes =
    [ 82, 89 ]


darkKnightIndexes : List Int
darkKnightIndexes =
    [ 83, 88 ]


darkBishopIndexes : List Int
darkBishopIndexes =
    [ 84, 87 ]


darkQueenIndex : Int
darkQueenIndex =
    85


darkKingIndex : Int
darkKingIndex =
    86


emptyTileStatus : Int -> TileStatus
emptyTileStatus int =
    if isIndexOutOfBounds int then
        OutOfBounds

    else
        WithinBounds


isIndexOutOfBounds : Int -> Bool
isIndexOutOfBounds int =
    List.member int outOfBoundsList
