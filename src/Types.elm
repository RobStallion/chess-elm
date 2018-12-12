module Types exposing (Board, Colour(..), Model, Msg(..), Piece, PieceStatus(..), PieceType(..), Tile, TileStatus(..))


type alias Board =
    List Tile


type alias Tile =
    { index : Int, status : TileStatus }


type TileStatus
    = Legal
    | OutOfBounds



-- Think about adding a piece number to piece
-- This can be used as an easy way to make sure we are always moving the
-- correct piece.


type alias Piece =
    { piece : PieceType
    , colour : Colour
    , index : Int
    , status : PieceStatus
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


type PieceStatus
    = Alive
    | Captured


type alias Model =
    { board : Board
    , pieces : List Piece
    }


type Msg
    = CheckAvailableMoves Piece
