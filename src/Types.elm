module Types exposing (Board, Colour(..), Model, Msg(..), Piece, PieceStatus(..), PieceType(..), Tile, TileStatus(..))

import Array exposing (Array)
import Dict exposing (Dict)


type alias Model =
    { board : Board
    }


type alias Board =
    Dict Int Tile


type alias Tile =
    { status : TileStatus, piece : Maybe Piece }


type TileStatus
    = WithinBounds
    | OutOfBounds
    | PossilbeMove


type alias Piece =
    { pieceType : PieceType
    , colour : Colour
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
    = CheckPossibleMoves Int
    | RemovePossilbeMoves
