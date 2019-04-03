module Types exposing (Board, Colour(..), King, KingState, Knight, Model, Msg(..), Pawn, PawnState, Piece(..), PieceState, Queen, Rook, Team(..), Tile, TileStatus(..))

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


type Piece
    = King KingState
    | Queen PieceState
    | Rook PieceState
    | Knight PieceState
    | Bishop PieceState
    | Pawn PawnState


type alias KingState =
    { team : Team
    , check : Bool
    , doubleCheck : Bool
    }


type alias PieceState =
    { team : Team }


type alias PawnState =
    { team : Team
    , onHomeTile : Bool
    , movedTwoTiles : Bool
    , capableOfEnPassentCap : Bool
    }


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
