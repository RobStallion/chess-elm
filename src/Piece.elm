module Piece exposing (createAllPieces, getPieceByIndex, pieceImgTag)

import Array exposing (Array)
import Html exposing (Html, div, img, p, text)
import Html.Attributes exposing (class, src)
import Types exposing (..)


createPiece : PieceType -> Colour -> Int -> Piece
createPiece pieceType colour int =
    { piece = pieceType, colour = colour, index = int, status = Alive }


createAllPieces : List Piece
createAllPieces =
    List.map (createPiece Pawn Light) (List.range 22 29)
        ++ List.map (createPiece Rook Light) [ 12, 19 ]
        ++ List.map (createPiece Knight Light) [ 13, 18 ]
        ++ List.map (createPiece Bishop Light) [ 14, 17 ]
        ++ [ createPiece Queen Light 15 ]
        ++ [ createPiece King Light 16 ]
        ++ List.map (createPiece Pawn Dark) (List.range 72 79)
        ++ List.map (createPiece Rook Dark) [ 82, 89 ]
        ++ List.map (createPiece Knight Dark) [ 83, 88 ]
        ++ List.map (createPiece Bishop Dark) [ 84, 87 ]
        ++ [ createPiece Queen Dark 85 ]
        ++ [ createPiece King Dark 86 ]



-- Helpers


updatePieceIndex : Int -> Piece -> Piece
updatePieceIndex index piece =
    { piece | index = index }


pieceImgTag : Piece -> Html Msg
pieceImgTag piece =
    div [ class "h3 w3 flex items-center justify-center" ]
        [ img [ src <| pieceImgStr piece, class "w2-5" ] []
        ]


getPieceByIndex : Int -> List Piece -> Piece
getPieceByIndex int pieceList =
    List.filter (\piece -> piece.index == int) pieceList
        |> List.head
        |> Maybe.withDefault (Piece King Light 0 Alive)


pieceImgStr : Piece -> String
pieceImgStr piece =
    "images/" ++ colorToText piece.colour ++ pieceToText piece.piece ++ ".svg"


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
