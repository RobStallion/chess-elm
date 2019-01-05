module Board exposing (createBoard, outOfBoundsList, renderBoard)

import Array exposing (Array)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Piece exposing (getPieceByIndex, pieceImgTag)
import Types exposing (..)



-- Render


renderBoard : List Piece -> Board -> Html Msg
renderBoard pieceList board =
    div [] <| List.map (renderRow pieceList) <| chunk 10 board []


renderRow : List Piece -> List Tile -> Html Msg
renderRow pieceList row =
    div [ class "flex" ] <| List.map (renderTile pieceList) row


renderTile : List Piece -> Tile -> Html Msg
renderTile pieceList tile =
    let
        pieceIndexes =
            List.map (\p -> p.index) pieceList
    in
    if List.member tile.index pieceIndexes then
        let
            piece =
                getPieceByIndex tile.index pieceList
        in
        if piece.status == Alive then
            div [ class <| tileClasses tile, onClick <| CheckAvailableMoves piece ] [ pieceImgTag piece ]

        else
            div [ class <| tileClasses tile ] [ text <| String.fromInt tile.index ]

    else
        div [ class <| tileClasses tile ] [ text <| String.fromInt tile.index ]



-- Create


createBoard : Board
createBoard =
    List.range 1 100
        |> List.map createEmptyTile


createEmptyTile : Int -> Tile
createEmptyTile index =
    Tile index (emptyTileStatus index)


emptyTileStatus : Int -> TileStatus
emptyTileStatus int =
    if isIndexOutOfBounds int then
        OutOfBounds

    else
        Legal



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


tileClasses : Tile -> String
tileClasses tile =
    case tile.status of
        Legal ->
            lightOrDarkTile tile ++ "h3 w3 flex items-center justify-center"

        OutOfBounds ->
            "h3 w3 flex items-center justify-center bg-gray"


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


chunk : Int -> List a -> List (List a) -> List (List a)
chunk int list acc =
    if List.length list <= int then
        acc ++ [ list ]

    else
        chunk int (List.drop int list) acc ++ [ List.take int list ]
