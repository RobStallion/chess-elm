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


sumOfCoordinate : Tile -> Int
sumOfCoordinate tile =
    tile
        |> tileCoordinate
        |> String.fromInt
        |> String.split ""
        |> List.map String.toInt
        |> List.foldl (\maybeN acc -> Maybe.withDefault 0 maybeN + acc) 0


lightOrDarkTile : Tile -> String
lightOrDarkTile tile =
    if 0 == (modBy 2 <| sumOfCoordinate tile) then
        "c-bg-light "

    else
        "c-bg-dark "


tileColor : Tile -> String
tileColor tile =
    case tileStatus tile of
        Legal ->
            lightOrDarkTile tile

        Illegal ->
            "bg-red "

        OutOfBounds ->
            "bg-gray "


tileDiv : Tile -> Html msg
tileDiv tile =
    div [ class <| tileColor tile ++ "flex h3 items-center justify-center w3" ]
        [ p [ class "tc" ] [ text <| String.fromInt <| tileCoordinate tile ]
        ]


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
    div [ class "flex justify-center" ] <| List.map tileDiv tileList


renderBoard : List Tile -> Html msg
renderBoard tileList =
    div [ class "blah" ] <| List.map renderRow <| splitBoardIntoRows tileList
