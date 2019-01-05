module Types exposing (Board, Colour(..), Model, Msg(..), Piece, PieceStatus(..), PieceType(..), Tile, TileStatus(..))

import Array exposing (Array)


type alias Model =
    { board : Board
    , pieces : List Piece
    }


type alias Board =
    List Tile


type alias Tile =
    { index : Int, status : TileStatus }


type TileStatus
    = Legal
    | OutOfBounds


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


type Msg
    = CheckAvailableMoves Piece
