module Main exposing (Model)

-- import Board exposing (..)

import Board exposing (createBoard, renderBoard)
import Browser
import Html exposing (Html, button, div, li, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Types exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { board : Board }


type Msg
    = AddPieces


init : Model
init =
    { board = createBoard }


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddPieces ->
            model


createPiece : Piece -> String
createPiece piece =
    case piece of
        King ->
            "K"

        Queen ->
            "Q"

        Rook ->
            "R"

        Bishop ->
            "B"

        Knight ->
            "N"

        Pawn ->
            "P"


view : Model -> Html Msg
view model =
    div []
        [ div [ class "tc mt4" ] [ renderBoard model.board ]
        , button [ onClick AddPieces ] [ text "add pieces" ]
        ]
