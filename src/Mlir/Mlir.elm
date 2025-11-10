module Mlir.Mlir exposing
    ( DenseF64, Dim(..), MlirAttr(..), MlirBlock, MlirModule
    , MlirOp, MlirRegion(..), MlirType(..), Visibility(..)
    , OpBuilderFns, OpBuilder, opBuilder
    , DenseF64Value(..), mlirOp
    )

{-| MLIR models the MLIR SSA compiler representation. This can be used to build MLIR code for further
processing by the MLIR tool-chain and development system.


# MLIR Model

@docs DenseF64, Dim, Loc, MlirAttr, MlirBlock, MlirModule
@docs MlirOp, MlirRegion, MlirType, Visibility


# Builders

@docs OpBuilderFns, OpBuilder, opBuilder

-}

import Dict exposing (Dict)
import Loc exposing (Loc)
import OrderedDict exposing (OrderedDict)


type Visibility
    = Public
    | Private
    | Nested


type Dim
    = Static Int
    | Dynamic


type MlirType
    = I1
    | I8
    | I16
    | I32
    | I64
    | F32
    | F64
    | Index
    | MemRef { dims : List Dim, elem : MlirType }
    | StructType (List MlirType)
    | NamedStruct String
    | FunctionType { inputs : List MlirType, results : List MlirType }
    | RankedTensor (List Dim) MlirType
    | UnrankedTensor MlirType


type DenseF64Value
    = Scalar Float
    | Tensor (List DenseF64Value)


type alias DenseF64 =
    { shape : List Dim
    , values : DenseF64Value
    }


type MlirAttr
    = StringAttr String
    | BoolAttr Bool
    | IntAttr Int
    | FloatAttr Float
    | TypeAttr MlirType
    | ArrayAttr (List MlirAttr)
    | DenseF64Attr { type_ : MlirType, payload : DenseF64 }
    | SymbolRefAttr String
    | VisibilityAttr Visibility
    | UnitAttr


type alias MlirOp =
    { name : String
    , id : String
    , operands : List String
    , results : List ( String, MlirType )
    , attrs : Dict String MlirAttr
    , regions : List MlirRegion
    , isTerminator : Bool
    , loc : Loc
    , successors : List String
    }


type alias MlirModule =
    { body : List MlirOp
    , loc : Loc
    }


type MlirRegion
    = MlirRegion
        { entry : MlirBlock
        , blocks : OrderedDict String MlirBlock
        }


type alias MlirBlock =
    { args : List ( String, MlirType )
    , body : List MlirOp
    , terminator : MlirOp
    }



--=== Builders


unknownLoc : Loc
unknownLoc =
    Loc.unknown


type alias OpBuilderFns e =
    { withOperands : List String -> OpBuilder e -> OpBuilder e
    , withResults : List ( String, MlirType ) -> OpBuilder e -> OpBuilder e
    , withAttrs : Dict String MlirAttr -> OpBuilder e -> OpBuilder e
    , withRegions : List MlirRegion -> OpBuilder e -> OpBuilder e
    , isTerminator : Bool -> OpBuilder e -> OpBuilder e
    , withLoc : Loc -> OpBuilder e -> OpBuilder e
    , withSuccessors : List String -> OpBuilder e -> OpBuilder e
    , build : OpBuilder e -> ( e, MlirOp )
    }


type OpBuilder e
    = OpBuilder e MlirOp


opBuilder : OpBuilderFns e
opBuilder =
    { withOperands =
        \operands (OpBuilder e op) ->
            OpBuilder e { op | operands = operands }
    , withResults =
        \results (OpBuilder e op) ->
            OpBuilder e { op | results = results }
    , withAttrs =
        \attrs (OpBuilder e op) ->
            OpBuilder e { op | attrs = attrs }
    , withRegions =
        \regions (OpBuilder e op) ->
            OpBuilder e { op | regions = regions }
    , isTerminator =
        \flag (OpBuilder e op) ->
            OpBuilder e { op | isTerminator = flag }
    , withLoc =
        \loc (OpBuilder e op) ->
            OpBuilder e { op | loc = loc }
    , withSuccessors =
        \succs (OpBuilder e op) ->
            OpBuilder e { op | successors = succs }
    , build = \(OpBuilder e op) -> ( e, op )
    }


{-| Creates a builder for a named MlirOp with a generated id that is unique within a given environment.
The remaining parameters can be filled in using the builder functions in OpBuilderFn. This is designed for
convenience of allowing an id generation function to be specified, and to pass through a new environment
(with a particular id consumed).
-}
mlirOp : (e -> ( e, String )) -> e -> String -> OpBuilder e
mlirOp idFn env name =
    let
        ( nextEnv, id ) =
            idFn env
    in
    OpBuilder
        nextEnv
        { name = name
        , id = id
        , operands = []
        , results = []
        , attrs = Dict.empty
        , regions = []
        , isTerminator = False
        , loc = unknownLoc
        , successors = []
        }
