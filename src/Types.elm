module Types exposing (Board, Colour(..), Model, Msg(..), Piece, PieceType(..), Team(..), Tile, TileStatus(..))

import Array exposing (Array)
import Dict exposing (Dict)


type alias Model =
    { board : Board
    , boardWithoutPossibleMoves : Board
    , beingDragged : Maybe ( Piece, Int )
    , turn : Team
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
    , team : Team
    }


type PieceType
    = King
    | Queen
    | Rook
    | Bishop
    | Knight
    | Pawn


type Team
    = Black
    | White


type Colour
    = Light
    | Dark


type Msg
    = CheckPossibleMoves Int
    | RemovePossilbeMoves
    | Drag ( Piece, Int )
    | DragEnd
    | DragOver
    | Drop Int
