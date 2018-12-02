module Board exposing (renderBoard, startingBoard)

import Browser
import Html exposing (Html, div, img, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Types exposing (..)



-- Update board functions


updateBoard : Int -> Piece -> Board -> Board
updateBoard index piece board =
    let
        updatedTile =
            getTile index board |> updateTilePiece piece

        beforeTile =
            List.take (index - 1) board

        afterTile =
            List.drop index board
    in
    beforeTile ++ updatedTile :: afterTile


updateTilePiece : Piece -> Tile -> Tile
updateTilePiece piece tile =
    ( tileCoordinate tile, tileStatus tile, Just piece )


getTile : Int -> Board -> Tile
getTile int board =
    board
        |> List.drop (int - 1)
        |> List.head
        |> Maybe.withDefault ( 0, Legal, Nothing )



-- Render board functions


renderBoard : Board -> Html Msg
renderBoard tileList =
    div [ class "" ] <| List.map renderRow <| splitBoardIntoRows tileList


renderRow : List Tile -> Html Msg
renderRow tileList =
    div [ class "flex justify-center" ] <| List.map renderTile tileList


renderTile : Tile -> Html Msg
renderTile tile =
    div [ class <| tileClasses tile ]
        [ displayInTile tile
        ]


displayInTile : Tile -> Html Msg
displayInTile tile =
    let
        piece =
            Maybe.withDefault (Piece King Light) (tilePiece tile)

        inTileHtml =
            if tilePiece tile == Nothing then
                p [ class "tc" ] [ text <| String.fromInt <| tileCoordinate tile ]

            else
                div [ class "h3 w3 flex items-center justify-center", onClick AddPieces ]
                    [ img [ src <| makePieceImgStr piece, class "w2-5" ] []
                    ]
    in
    inTileHtml


makePieceImgStr : Piece -> String
makePieceImgStr piece =
    "images/" ++ colorToText piece.color ++ pieceToText piece.piece ++ ".svg"


splitBoardIntoRows : Board -> List (List Tile)
splitBoardIntoRows tileList =
    chunk 10 tileList []


chunk : Int -> List a -> List (List a) -> List (List a)
chunk int list acc =
    if List.length list <= int then
        acc ++ [ list ]

    else
        chunk int (List.drop int list) acc ++ [ List.take int list ]



-- pieceToText and colorToText will be replaced with images in later commit


pieceToText : PieceType -> String
pieceToText piece =
    case piece of
        King ->
            "k"

        Queen ->
            "q"

        Rook ->
            "r"

        Bishop ->
            "b"

        Knight ->
            "n"

        Pawn ->
            "p"


colorToText : Color -> String
colorToText color =
    case color of
        Light ->
            "l"

        Dark ->
            "d"



-- Create board functions


addDarkRooks : Board -> Board
addDarkRooks board =
    addPiecesToBoard [ 82, 89 ] (Piece Rook Dark) board


addDarkPawns : Board -> Board
addDarkPawns board =
    addPiecesToBoard (List.range 72 79) (Piece Pawn Dark) board


startingBoard : Board
startingBoard =
    createBoard
        -- Light pieces
        |> addPiecesToBoard (List.range 22 29) (Piece Pawn Light)
        |> addPiecesToBoard [ 12, 19 ] (Piece Rook Light)
        |> addPiecesToBoard [ 13, 18 ] (Piece Knight Light)
        |> addPiecesToBoard [ 14, 17 ] (Piece Bishop Light)
        |> addPiecesToBoard [ 15 ] (Piece Queen Light)
        |> addPiecesToBoard [ 16 ] (Piece King Light)
        -- Dark pieces
        |> addPiecesToBoard (List.range 72 79) (Piece Pawn Dark)
        |> addPiecesToBoard [ 82, 89 ] (Piece Rook Dark)
        |> addPiecesToBoard [ 83, 88 ] (Piece Knight Dark)
        |> addPiecesToBoard [ 84, 87 ] (Piece Bishop Dark)
        |> addPiecesToBoard [ 85 ] (Piece Queen Dark)
        |> addPiecesToBoard [ 86 ] (Piece King Dark)


addPiecesToBoard : List Int -> Piece -> Board -> Board
addPiecesToBoard intList piece board =
    List.foldl (\i acc -> updateBoard i piece acc) board intList


createBoard : Board
createBoard =
    List.map createTile <| List.range 1 100


createTile : Int -> Tile
createTile int =
    ( int, getTileStatus int, Nothing )


getTileStatus : Int -> Status
getTileStatus int =
    if isIndexOutOfBounds int then
        OutOfBounds

    else
        Legal


isIndexOutOfBounds : Int -> Bool
isIndexOutOfBounds int =
    List.member int outOfBoundsList


outOfBoundsList : List Int
outOfBoundsList =
    List.range 1 10
        ++ List.range 91 100
        ++ (List.map (\n -> n * 10 + 1) <| List.range 1 8)
        ++ (List.map (\n -> n * 10) <| List.range 2 9)


tileClasses : Tile -> String
tileClasses tile =
    case tileStatus tile of
        Legal ->
            lightOrDarkTile tile ++ "h3 w3 flex items-center justify-center"

        Illegal ->
            "bg-red "

        OutOfBounds ->
            -- will be dn in future
            "h3 w3 flex items-center justify-center bg-gray"


lightOrDarkTile : Tile -> String
lightOrDarkTile tile =
    if isEven <| sumOfCoordinate tile then
        "c-bg-light "

    else
        "c-bg-dark "


isEven : Int -> Bool
isEven int =
    0 == modBy 2 int


sumOfCoordinate : Tile -> Int
sumOfCoordinate tile =
    tile
        |> tileCoordinate
        |> String.fromInt
        |> String.split ""
        |> List.map String.toInt
        |> List.foldl (\maybeN acc -> Maybe.withDefault 0 maybeN + acc) 0



-- Tile helpers


tileCoordinate : Tile -> Int
tileCoordinate tile =
    let
        ( coordinate, _, _ ) =
            tile
    in
    coordinate


tileStatus : Tile -> Status
tileStatus tile =
    let
        ( _, status, _ ) =
            tile
    in
    status


tilePiece : Tile -> Maybe Piece
tilePiece tile =
    let
        ( _, _, piece ) =
            tile
    in
    piece
