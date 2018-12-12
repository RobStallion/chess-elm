module Board exposing (createBoard, createLightPawns, movePiece, renderBoard)

import Browser
import Html exposing (Html, div, img, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import List.Extra
import Types exposing (..)


createLightPawn : Int -> Piece
createLightPawn index =
    Piece Pawn Light index Alive


createLightPawns : List Int -> List Piece
createLightPawns indexList =
    List.map createLightPawn indexList


renderBoard : List Piece -> Board -> Html Msg
renderBoard pieces board =
    let
        pieceIndexes =
            List.map (\p -> p.index) pieces
    in
    div [] <|
        List.map
            (\tile ->
                if List.member tile.index pieceIndexes then
                    let
                        piece =
                            getPieceByIndex tile.index pieces
                    in
                    if piece.status == Alive then
                        div [ class <| tileClasses tile, onClick <| CheckAvailableMoves piece ] [ pieceImgTag piece ]

                    else
                        div [ class <| tileClasses tile ] [ text <| String.fromInt tile.index ]

                else
                    div [ class <| tileClasses tile ] [ text <| String.fromInt tile.index ]
            )
            board


movePiece : Piece -> Piece
movePiece piece =
    let
        _ =
            Debug.log "--------" piece
    in
    { piece | index = piece.index + 10 }


getPieceByIndex : Int -> List Piece -> Piece
getPieceByIndex int pieceList =
    List.filter (\piece -> piece.index == int) pieceList
        |> List.head
        |> Maybe.withDefault (Piece King Light 0 Alive)


pieceImgTag : Piece -> Html Msg
pieceImgTag piece =
    div [ class "h3 w3 flex items-center justify-center" ]
        [ img [ src <| pieceImgStr piece, class "w2-5" ] []
        ]



-- Render board functions


pieceImgStr : Piece -> String
pieceImgStr piece =
    "images/" ++ colorToText piece.colour ++ pieceToText piece.piece ++ ".svg"


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


colorToText : Colour -> String
colorToText colour =
    case colour of
        Light ->
            "l"

        Dark ->
            "d"


createBoard : Board
createBoard =
    List.map createEmptyTile <| List.range 1 100


createEmptyTile : Int -> Tile
createEmptyTile index =
    Tile index (emptyTileStatus index)


emptyTileStatus : Int -> TileStatus
emptyTileStatus int =
    if isIndexOutOfBounds int then
        OutOfBounds

    else
        Legal


tileClasses : Tile -> String
tileClasses tile =
    case tile.status of
        Legal ->
            lightOrDarkTile tile ++ "h3 w3 flex items-center justify-center"

        OutOfBounds ->
            "h3 w3 flex items-center justify-center bg-gray"



-- Helpers


isEven : Int -> Bool
isEven int =
    0 == modBy 2 int


isIndexOutOfBounds : Int -> Bool
isIndexOutOfBounds int =
    List.member int outOfBoundsList


outOfBoundsList : List Int
outOfBoundsList =
    List.range 1 10
        ++ List.range 91 100
        ++ (List.map (\n -> n * 10 + 1) <| List.range 1 8)
        ++ (List.map (\n -> n * 10) <| List.range 2 9)


lightOrDarkTile : Tile -> String
lightOrDarkTile tile =
    if isEven <| sumOfIndex tile then
        "c-bg-light "

    else
        "c-bg-dark "


sumOfIndex : Tile -> Int
sumOfIndex tile =
    tile.index
        |> String.fromInt
        |> String.split ""
        |> List.map String.toInt
        |> List.foldl (\maybeN acc -> Maybe.withDefault 0 maybeN + acc) 0
