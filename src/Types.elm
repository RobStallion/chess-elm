module Types exposing (Board, Color(..), Piece(..), Status(..), Tile)


type alias Board =
    List Tile


type alias Tile =
    ( Int, Status, Maybe Piece )


type Status
    = Legal
    | Illegal
    | OutOfBounds


type Piece
    = King
    | Queen
    | Rook
    | Bishop
    | Knight
    | Pawn


type Color
    = Light
    | Dark
