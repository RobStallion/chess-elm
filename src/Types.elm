module Types exposing (Board, Color(..), Model, Msg(..), Piece, PieceType(..), Status(..), Tile)


type alias Board =
    List Tile


type alias Tile =
    { index : Int, status : Status, piece : Maybe Piece }


type Status
    = Legal
    | Illegal
    | OutOfBounds


type alias Piece =
    { piece : PieceType
    , color : Color
    }


type PieceType
    = King
    | Queen
    | Rook
    | Bishop
    | Knight
    | Pawn


type Color
    = Light
    | Dark


type alias Model =
    { board : Board }


type Msg
    = AddPieces
