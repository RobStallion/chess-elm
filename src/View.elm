module View exposing (view)

import Dict
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Types exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ div [ class "mt4 flex justify-center" ]
            [ renderBoard model.board
            ]
        ]


renderBoard : Board -> Html Msg
renderBoard board =
    board
        |> Dict.toList
        |> chunk 10 []
        |> List.reverse
        |> List.map renderRow
        |> div []


renderRow : List ( Int, Tile ) -> Html Msg
renderRow tileIntList =
    tileIntList
        |> List.map renderTile
        |> div [ class "flex" ]


renderTile : ( Int, Tile ) -> Html Msg
renderTile ( int, tile ) =
    case tile.piece of
        Just piece ->
            div
                [ class <| tileClasses int tile
                , onMouseDown <| CheckPossibleMoves int
                , onMouseUp <| RemovePossilbeMoves
                ]
                [ pieceImgTag piece ]

        Nothing ->
            div [ class <| tileClasses int tile ] [ text <| String.fromInt int ]


tileClasses : Int -> Tile -> String
tileClasses int tile =
    case tile.status of
        WithinBounds ->
            lightOrDarkTile int ++ "h3 w3 flex items-center justify-center"

        OutOfBounds ->
            "h3 w3 flex items-center justify-center bg-gray"

        PossilbeMove ->
            lightOrDarkTile int ++ "h3 w3 flex items-center justify-center bg-green"


lightOrDarkTile : Int -> String
lightOrDarkTile int =
    if isEven <| sumOfIndex int then
        "c-bg-light "

    else
        "c-bg-dark "


sumOfIndex : Int -> Int
sumOfIndex int =
    int
        |> String.fromInt
        |> String.split ""
        |> List.map String.toInt
        |> List.foldl (\maybeN acc -> Maybe.withDefault 0 maybeN + acc) 0


pieceImgTag : Piece -> Html Msg
pieceImgTag piece =
    div [ class "h3 w3 flex items-center justify-center" ]
        [ img [ src <| pieceImgStr piece, class "w2-5" ] []
        ]


pieceImgStr : Piece -> String
pieceImgStr piece =
    "images/" ++ colourToText piece.colour ++ pieceToText piece.pieceType ++ ".svg"


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


colourToText : Colour -> String
colourToText colour =
    case colour of
        Light ->
            "l"

        Dark ->
            "d"


chunk : Int -> List (List a) -> List a -> List (List a)
chunk int acc list =
    if List.length list <= int then
        acc ++ [ list ]

    else
        chunk int (acc ++ [ List.take int list ]) (List.drop int list)


isEven : Int -> Bool
isEven int =
    0 == modBy 2 int
