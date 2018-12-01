module Board exposing (createBoard, renderBoard)

import Browser
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Types exposing (..)


boardSize : List Int
boardSize =
    List.range 1 100


outOfBounds : List Int
outOfBounds =
    List.range 1 10
        ++ List.range 91 100
        ++ (List.map (\n -> n * 10 + 1) <| List.range 1 8)
        ++ (List.map (\n -> n * 10) <| List.range 2 9)


isTileOutOfBounds : Int -> Bool
isTileOutOfBounds int =
    List.member int outOfBounds


getTileStatus : Int -> Status
getTileStatus int =
    if isTileOutOfBounds int then
        OutOfBounds

    else
        Legal


createTile : Int -> Tile
createTile int =
    ( int, getTileStatus int, Nothing )


createBoard : List Tile
createBoard =
    List.map createTile boardSize


tileStatus : Tile -> Status
tileStatus tile =
    let
        ( _, status, _ ) =
            tile
    in
    status


tileColor : Tile -> String
tileColor tile =
    let
        status =
            tileStatus tile
    in
    case status of
        Legal ->
            "bg-green"

        Illegal ->
            "bg-red"

        OutOfBounds ->
            "bg-gray"


tileDiv : Tile -> Html msg
tileDiv tile =
    div [ class <| "w3 h3 dib " ++ tileColor tile ] [ p [] [] ]


chunk : Int -> List a -> List (List a) -> List (List a)
chunk int list acc =
    if List.length list <= int then
        acc ++ [ list ]

    else
        chunk int (List.drop int list) acc ++ [ List.take int list ]


splitBoardIntoRows : List Tile -> List (List Tile)
splitBoardIntoRows tileList =
    chunk 10 tileList []


renderRow : List Tile -> Html msg
renderRow tileList =
    div [] <| List.map tileDiv tileList


renderBoard : List Tile -> Html msg
renderBoard tileList =
    div [] <| List.map renderRow <| splitBoardIntoRows tileList
