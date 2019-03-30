module View exposing (view)

import Dict
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, draggable, src)
import Html.Events exposing (on, onClick, onMouseDown, onMouseUp, preventDefaultOn)
import Json.Decode exposing (succeed)
import Types exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ div [ class "mt4 flex justify-center" ]
            [ renderBoard model
            ]
        ]


renderBoard : Model -> Html Msg
renderBoard model =
    model.board
        |> Dict.toList
        |> chunk 10 []
        |> List.reverse
        |> List.map (\row -> renderRow model row)
        |> div []


renderRow : Model -> List ( Int, Tile ) -> Html Msg
renderRow model tileIntList =
    tileIntList
        |> List.map (\tile -> renderTile model tile)
        |> div [ class "flex" ]


renderTile : Model -> ( Int, Tile ) -> Html Msg
renderTile model ( int, tile ) =
    case tile.status of
        WithinBounds ->
            case tile.piece of
                Just piece ->
                    if piece.team == model.turn then
                        div
                            [ class <| tileClasses ++ lightOrDarkTile int ]
                            [ pieceImgTag piece <| movePieceEventListeners piece int ]

                    else
                        div [ class <| tileClasses ++ lightOrDarkTile int ] [ pieceImgTag piece [] ]

                Nothing ->
                    div [ class <| tileClasses ++ lightOrDarkTile int ] [ text <| String.fromInt int ]

        OutOfBounds ->
            div [ class <| tileClasses ++ "bg-gray" ] [ text <| String.fromInt int ]

        PossilbeMove ->
            case tile.piece of
                Just piece ->
                    div
                        [ class <| tileClasses ++ lightOrDarkTile int
                        , onDragOver DragOver
                        , onDrop <| Drop int
                        ]
                        [ pieceImgTag piece []
                        , div [ class "bg-red w1 h1 br4 absolute" ] []
                        ]

                Nothing ->
                    div
                        [ class <| tileClasses ++ lightOrDarkTile int
                        , onDragOver DragOver
                        , onDrop <| Drop int
                        ]
                        [ div [ class "bg-green w1 h1 br4" ] []
                        ]


movePieceEventListeners : Piece -> Int -> List (Html.Attribute Msg)
movePieceEventListeners piece int =
    [ onMouseDown <| CheckPossibleMoves int
    , onMouseUp RemovePossilbeMoves
    , draggable "true"
    , onDragStart <| Drag ( piece, int )
    , onDragEnd DragEnd
    ]


tileClasses : String
tileClasses =
    "h3 w3 flex items-center justify-center "


lightOrDarkTile : Int -> String
lightOrDarkTile int =
    if isEven <| sumOfIndex int then
        "c-bg-light"

    else
        "c-bg-dark"


sumOfIndex : Int -> Int
sumOfIndex int =
    int
        |> String.fromInt
        |> String.split ""
        |> List.map String.toInt
        |> List.foldl (\maybeN acc -> Maybe.withDefault 0 maybeN + acc) 0


pieceImgTag : Piece -> List (Html.Attribute Msg) -> Html Msg
pieceImgTag piece attributes =
    img ([ src <| pieceImgStr piece, class "w2-5" ] ++ attributes) []


pieceImgStr : Piece -> String
pieceImgStr piece =
    "images/" ++ colourToText piece.team ++ pieceToText piece.pieceType ++ ".svg"


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

        Pawn _ ->
            "p"


colourToText : Team -> String
colourToText team =
    case team of
        White ->
            "l"

        Black ->
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


onDragStart : Msg -> Html.Attribute Msg
onDragStart msg =
    on "dragstart" <| succeed msg


onDragEnd : Msg -> Html.Attribute Msg
onDragEnd msg =
    on "dragend" <| succeed msg


onDragOver : Msg -> Html.Attribute Msg
onDragOver msg =
    preventDefaultOn "dragover" <| succeed ( msg, True )


onDrop : Msg -> Html.Attribute Msg
onDrop msg =
    preventDefaultOn "drop" <| succeed ( msg, True )
