module Mlir.Mlir exposing
    ( MlirAttr(..)
    , Visibility(..)
    , MlirType(..)
    , MlirOp
    , MlirBlock, MlirRegion(..), MlirModule
    , OpBuilderFns, OpBuilder, opBuilder, mlirOp
    )

{-| An Elm program builds an MLIR program as an ordinary value of the types in
this module, then writes it out, as MLIR text with `Mlir.Pretty` or as MLIR
bytecode with `Mlir.Bytecode.StreamEncode`. This module is the form of that
value, which both writers read, and a builder for its operations.

MLIR is a compiler framework whose programs are built from _operations_. An
operation has a name of the form `dialect.op`, where a _dialect_ is a named
family of operations, types and attributes, such as `func`, `arith` or `llvm`.
It reads values, its _operands_, and defines new ones, its _results_. It
carries _attributes_, which are constant data such as a function's name or an
integer flag, and it may contain _regions_ of nested code. A region is a list
of _blocks_, and a block is a straight run of operations that ends in a
_terminator_, the operation that passes control on. The blocks a terminator
may pass control to are its _successors_. Values are in SSA form:
each is defined exactly once, by an operation's result or a block's argument,
and is referred to everywhere else by its name.

The model is generic, and nothing in it is checked. An operation is a name
string and some fields, not a type of its own, so nothing here knows which
operations exist or checks that an operation's operands, results, attributes,
successors or regions make sense for it. Values and blocks are named by plain
strings. The attributes and types are a small fixed set, with a form written
as raw MLIR text for an attribute outside it (`OpaqueAttr`) and for a type
defined by a dialect other than builtin (`NamedStruct`). Every constructor
except the op builder's is exposed, and `MlirOp` is a record, so any value
can be built directly.

A few names do mean something to the writers, though neither checks them: the
attribute names `_operand_types` and `callee` (see `MlirOp`), the block label
`bb0` and the names of operations whose regions cannot see values defined
outside them (see `MlirRegion`), and a location named `"unknown"` that starts
at row 0, column 0 (see `Mlir.Loc`).

Both writers read the same model, but not always in the same way, so a value
that comes out right from one can come out differently from the other. A
successor label without MLIR's leading `^`, for example, is found by the
bytecode writer but printed without the `^` by the text writer. The type
docstrings note the main differences.

The rest of the module is the _op builder_: `mlirOp` starts an operation and
draws its `id` from an environment of the caller's, the functions in
`opBuilder` fill it in, and `build` returns the operation with the environment
left after the `id` was drawn.


# MLIR Model


## Attributes

@docs MlirAttr
@docs Visibility


## Types

@docs MlirType


## Operations

@docs MlirOp


## Blocks, Regions and Modules

@docs MlirBlock, MlirRegion, MlirModule


# Builders

@docs OpBuilderFns, OpBuilder, opBuilder, mlirOp

-}

import Dict exposing (Dict)
import Mlir.Loc as Loc exposing (Loc)
import OrderedDict exposing (OrderedDict)



-- Attributes


{-| A constant value attached to an operation under a name in its `attrs`, or
held inside another attribute.

`StringAttr` holds its text with escape sequences written out, not decoded: a
newline is held as a backslash and an `n`, as in `StringAttr "a\\nb"` in Elm
source. Nothing checks that the text is in this form, and each writer decodes
it its own way. Both understand `\n`, `\t`, `\r`, `\\`, `\"`, `\'` and `\u`
followed by four hexadecimal digits: the text writer re-spells each for an
MLIR string literal, and the bytecode writer writes the character it stands
for.

Other escapes, including MLIR's own `\0A` form, can come out differently from
the two writers, so keep to the escapes above.

`BoolAttr` is printed as `true` or `false`, and written to bytecode as the
`i1` integer 1 or 0.

`IntAttr` carries the integer's type, or `Nothing` for an integer with none,
which the text writer prints with no type and the bytecode writer writes as an
`i64`. Compiled to JavaScript, an Elm `Int` is exact only within ±2^53, so not
every 64-bit value outside that range can be held there.

`TypedFloatAttr` is a floating-point number with its type, which should be
`F64`, the one float type here: the bytecode writer writes the number as a
64-bit float whatever the type, and the text writer prints a NaN or an infinity
as a 64-bit bit pattern.

`TypeAttr` is a type used as a value.

`ArrayAttr` with no type is a list of attributes, of any kinds, mixed. With
`Just t` it is a dense array of elements of type `t`, which should be `I1`,
`I8`, `I32` or `I64`, and whose elements should all be untyped `IntAttr`s.
The bytecode writer keeps only the integer of each `IntAttr` element and
drops any other element; the text writer prints every element, a typed
`IntAttr` with its type.

`SymbolRefAttr` refers to a symbol, such as a function, by its name without
MLIR's leading `@`. The text writer prints the name after an `@` as it is,
unquoted, so for text output it must be a name MLIR accepts without quotes.

`VisibilityAttr` gives a symbol's visibility.

`UnitAttr` carries no value; its presence under a name is the whole of what it
says.

`DictAttr` is a dictionary of named attributes used as a value. Both writers
write its entries in name order.

`OpaqueAttr` is any other attribute, as its full MLIR text, such as
`#llvm.linkage<internal>`. Both writers write that text as it is, so nothing
here checks it; it is parsed only by whatever reads the output. The bytecode
writer records it as belonging to the dialect named after the leading `#`, up
to the first `.` or `<`, or to `builtin` when the text does not start with `#`.

-}
type MlirAttr
    = StringAttr String
    | BoolAttr Bool
    | IntAttr (Maybe MlirType) Int
    | TypedFloatAttr Float MlirType
    | TypeAttr MlirType
    | ArrayAttr (Maybe MlirType) (List MlirAttr)
    | SymbolRefAttr String
    | VisibilityAttr Visibility
    | UnitAttr
    | DictAttr (Dict String MlirAttr)
    | OpaqueAttr String


{-| The visibility a `VisibilityAttr` can give a symbol. `Private` is the only
one modelled, and both writers write it as the string `"private"`.
-}
type Visibility
    = Private



-- Types


{-| The type of an SSA value, or of an attribute's contents.

MLIR has no fixed list of types: each dialect may define its own. This is a
handful of builtin types, plus a way to name a type of any dialect other than
builtin. A builtin type not listed here, such as `f32` or `tensor<4xi32>`,
cannot be expressed.

`I1`, `I8`, `I32` and `I64` are signless integers of that many bits, and `F64`
is a 64-bit float. `SignedIntegerType w` is the signed integer of `w` bits,
such as `si64`, and `IndexType` is MLIR's `index`.

`NamedStruct` names a type defined by a dialect, in full and without MLIR's
leading `!`, such as `llvm.ptr` for `!llvm.ptr`; both writers add the `!`. The
part before the first `.` is the dialect, or the whole name if it has no `.`.
The type need not be a struct. Both writers write it as its text, so nothing
here checks it; it is parsed only by whatever reads the output.

`FunctionType` is the type of a function, from its input types to its result
types.

-}
type MlirType
    = I1
    | I8
    | I32
    | I64
    | F64
    | NamedStruct String
    | FunctionType { inputs : List MlirType, results : List MlirType }
    | IndexType
    | SignedIntegerType Int



-- Operations


{-| One MLIR operation.

`name` is the full `dialect.op` name. `id` is a name for the operation, which
`mlirOp` takes from its `idFn`; neither writer reads it.

`operands` are the names of the values the operation reads, and `results` the
names and types of the values it defines. The text writer prints these names
as they are, so for text output each must carry MLIR's leading `%`, as in
`%0`. The bytecode writer uses them only to match each operand to the value
it names.

An operation holds no types for its operands. Bytecode does not need them, but
the text writer prints them. It prints an operation in MLIR's _generic form_,
the syntax in which MLIR can print any operation, which ends in a signature,
`(operand types) -> result types`, and it finds the operand types in one of two
ways. If `attrs` holds an `ArrayAttr` named `_operand_types`, its `TypeAttr`
elements are the operand types, in order, and any other element is skipped.
That attribute is a convention of this package, not of MLIR: the text writer
also prints it with the other attributes, and the bytecode writer leaves it
out. Without it, each operand is looked up among the values in scope, as
`Mlir.Pretty` describes, and one not found is left out without warning, so the
signature lists fewer types than there are operands. That happens to every
operand of a top-level operation; to an operand in a labelled block, or in a
region nested inside one, whose value is defined in another block of the
labelled block's region; and to an operand whose value is defined later.
`_operand_types` is the way to give the types in those cases.

If `attrs` holds an attribute named `callee`, whatever the operation, the text
writer prints all of `attrs`, `_operand_types` included, as properties
(`<{...}>`) before the regions, instead of as a dictionary (`{...}`) after
them. The bytecode writer has no such rule.

`isTerminator` is read only for the operations in a block's `body`: both
writers skip one that has it set, since a block's terminator is written from
the block's `terminator` field.

`loc` is the operation's source location. The text writer prints no location,
and the bytecode writer writes it as `Mlir.Loc` describes.

`successors` name, by their labels, the blocks of the enclosing region that a
terminator may pass control to. The text writer prints them as they are, so
for text output each must carry MLIR's leading `^`, as in `^bb1`. The bytecode
writer removes one leading `^` if there is one, looks the rest up among the
region's block labels, `bb0` being the entry block, and writes a label it does
not find as the entry block.

-}
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



-- Blocks, Regions and Modules


{-| A block: arguments, a run of operations, and the terminator that ends it.

`args` are the values the block defines on entry, as names and types; like
any value's name, each needs MLIR's leading `%` for text output.

`terminator` is the operation that ends the block. `body` may also hold
operations whose `isTerminator` is set; both writers skip those and write only
`terminator`, at the end.

-}
type alias MlirBlock =
    { args : List ( String, MlirType )
    , body : List MlirOp
    , terminator : MlirOp
    }


{-| A region of nested code inside an operation: an entry block, where control
enters, followed by any further blocks.

Building one needs `OrderedDict`, from the `j-maas/elm-ordered-containers`
package. The further blocks are kept in `blocks` under their labels, without
MLIR's leading `^`, in the order they were inserted, and those labels are what
`successors` refer to. The entry block has no label here, and both writers call
it `bb0`. So no block in `blocks` should be labelled `bb0`: the text writer
prints such a block with no header when it has no arguments, so it runs on
from the block before it, and the bytecode writer sends successors labelled
`bb0` to it instead of to the entry block.

A region is _isolated_ (from above) when it cannot see values defined outside
it. The text writer ignores isolation. The bytecode writer goes by the name of
the operation holding the region: the regions of `func.func` and
`builtin.module` are isolated, and those of any operation whose name is passed
to `Mlir.Bytecode.StreamEncode.isolating`.

-}
type MlirRegion
    = MlirRegion
        { entry : MlirBlock
        , blocks : OrderedDict String MlirBlock
        }


{-| A whole MLIR program: its top-level operations, such as function
definitions, in order, and a location for the module itself.

No function in this package takes one. `Mlir.Pretty` prints top-level
operations one at a time, and `Mlir.Bytecode.StreamEncode` takes them in
batches and takes the module's location as an argument of `assembleModule`.

-}
type alias MlirModule =
    { body : List MlirOp
    , loc : Loc
    }



-- Builders


{-| The location every operation started by `mlirOp` has, `Mlir.Loc.unknown`.
-}
unknownLoc : Loc
unknownLoc =
    Loc.unknown


{-| The functions of the op builder, gathered in a record so that they are
reached through the one value `opBuilder`, as in `opBuilder.withAttrs`.

Each `with...` function, and `isTerminator`, replaces the field of the
operation it names; none adds to what is already there, so a second
`withAttrs` discards the attributes the first one set. `build` finishes the
operation and returns it with the environment left after its `id` was drawn.

There is no function for `successors` or `loc`. Set them by record update on
the operation `build` returns, as in `{ op | successors = [ "^bb1" ] }`.

-}
type alias OpBuilderFns e =
    { withOperands : List String -> OpBuilder e -> OpBuilder e
    , withResults : List ( String, MlirType ) -> OpBuilder e -> OpBuilder e
    , withAttrs : Dict String MlirAttr -> OpBuilder e -> OpBuilder e
    , withRegions : List MlirRegion -> OpBuilder e -> OpBuilder e
    , isTerminator : Bool -> OpBuilder e -> OpBuilder e
    , build : OpBuilder e -> ( e, MlirOp )
    }


{-| An operation under construction, whose `id` has already been drawn from the
caller's environment `e`.

The only way to get one is `mlirOp`. The functions in `opBuilder` fill in the
operation, and `build` returns it together with the environment that drawing
its `id` left.

-}
type OpBuilder e
    = OpBuilder e MlirOp


{-| The op builder's functions, as `OpBuilderFns` describes them.
-}
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
    , build = \(OpBuilder e op) -> ( e, op )
    }


{-| Starts building an operation called `name`, taking its `id` and the next
environment from `idFn env`.

The operation starts with no operands, results, attributes, regions or
successors, is not a terminator, and has the location `Mlir.Loc.unknown`.
Nothing in the package makes ids unique, and the same `env` gives the same
`id`, so pass the environment `build` returns to the next call. Here the
environment is a counter:

    constant : Int -> ( Int, MlirOp )
    constant nextId =
        mlirOp (\i -> ( i + 1, "op" ++ String.fromInt i )) nextId "arith.constant"
            |> opBuilder.withResults [ ( "%c", I32 ) ]
            |> opBuilder.withAttrs (Dict.singleton "value" (IntAttr (Just I32) 7))
            |> opBuilder.build

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
