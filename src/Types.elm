module Types exposing (Board, Colour(..), Model, Msg(..), Piece, PieceType(..), Status(..), Tile)


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
    , colour : Colour
    }


type PieceType
    = King
    | Queen
    | Rook
    | Bishop
    | Knight
    | Pawn


type Colour
    = Light
    | Dark


type alias Model =
    { board : Board }


type Msg
    = AddPieces
