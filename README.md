# elm-mlir

Build [MLIR](https://mlir.llvm.org) programs in Elm, and write them out as MLIR text or as MLIR
bytecode, the binary form MLIR tools load directly. It is pure Elm: no LLVM, no MLIR library and
no ports are needed to produce the output, only to consume it.

```
elm install the-sett/elm-mlir
```

## An example

A function that returns the 64-bit integer 42, written as text and as bytecode:

```elm
import Bytes exposing (Bytes)
import Dict
import Mlir.Bytecode.StreamEncode as StreamEncode
import Mlir.Loc as Loc
import Mlir.Mlir exposing (MlirAttr(..), MlirOp, MlirRegion(..), MlirType(..))
import Mlir.Pretty as Pretty
import OrderedDict


op : String -> MlirOp
op name =
    { name = name
    , id = ""
    , operands = []
    , results = []
    , attrs = Dict.empty
    , regions = []
    , isTerminator = False
    , loc = Loc.unknown
    , successors = []
    }


answer : MlirOp
answer =
    let
        ( constantOp, returnOp, funcOp ) =
            ( op "arith.constant", op "func.return", op "func.func" )

        constant =
            { constantOp
                | results = [ ( "%c", I64 ) ]
                , attrs = Dict.fromList [ ( "value", IntAttr (Just I64) 42 ) ]
            }

        return =
            { returnOp | operands = [ "%c" ], isTerminator = True }
    in
    { funcOp
        | attrs =
            Dict.fromList
                [ ( "sym_name", StringAttr "answer" )
                , ( "function_type", TypeAttr (FunctionType { inputs = [], results = [ I64 ] }) )
                ]
        , regions =
            [ MlirRegion
                { entry = { args = [], body = [ constant ], terminator = return }
                , blocks = OrderedDict.empty
                }
            ]
    }


text : String
text =
    Pretty.ppModuleHeader ++ Pretty.ppTopLevelOp answer ++ Pretty.ppModuleFooter


bytecode : Bytes
bytecode =
    StreamEncode.assembleModule "answer-example"
        (StreamEncode.collectAndEncodeOps [ answer ] StreamEncode.emptyStreamTables)
        Loc.unknown
```

`mlir-opt` reads either form, so `mlir-opt answer.mlirbc` prints the module back as text. The
first argument of `assembleModule` names the program that wrote the file; MLIR shows it with any
error it reports while reading.
Building `OrderedDict` regions needs `j-maas/elm-ordered-containers`, which this package depends
on.

## The modules

- **`Mlir.Mlir`** is the program: operations, regions, blocks, attributes and types, as plain
  Elm values, plus a small builder that draws each operation's `id` from an environment you
  thread through.
- **`Mlir.Pretty`** writes MLIR's generic text form, which MLIR reads for any dialect without
  knowing its custom syntax.
- **`Mlir.Bytecode.StreamEncode`** writes bytecode, taking the top-level operations a batch at a
  time: each batch is encoded as soon as it is added and only its bytes are kept, so a large
  program need not be held in memory at once.
- **`Mlir.Loc`** is the source location every operation carries.

## The model is generic

An operation is its name (`dialect.op`), the names of the SSA values it reads and defines, a
dictionary of attributes, its regions and its location. Nothing in the package knows which
operations a dialect has, so any dialect can be written, and nothing checks that an operation
makes sense: that is MLIR's verifier's job when it reads the output.

The attributes and types are a fixed set: integers, floats, strings, booleans, types, arrays and
dense integer arrays, dictionaries, symbol references and unit attributes; `i1`, `i8`, `i32`,
`i64`, signed integers, `index`, `f64` and function types. Anything else is written as its MLIR
text: a type of any dialect other than builtin by its name, as `NamedStruct "llvm.ptr"` is
written `!llvm.ptr`, and any other attribute as `OpaqueAttr "#llvm.linkage<internal>"`. MLIR
parses that text when it reads the program. A builtin type outside the set, such as `f32` or
`tensor<4xi32>`, cannot be expressed.

Value names are written as given, so they carry MLIR's `%` (`"%c"`), and successors their `^`.

## Which MLIR reads the output

The bytecode declares format version 4. MLIR's bytecode reader accepts every format version up
to its own, and the builtin attribute and type encodings written here are the same from LLVM 21
to at least LLVM 24, so the same bytes load in both. What does change between LLVM releases is
the dialects themselves, operation names and attributes, which is a matter for the program being
built, not for this package.

## Things to know

- **Lookups are not checked.** The bytecode writer refers to names, types, attributes and values
  by index. Anything it cannot find, such as an operand that names no value in scope, is written
  as an index that points nowhere: MLIR rejects the file, and the writer itself reports nothing.
  Each top-level operation is encoded with nothing in scope, so one top-level operation cannot
  use another's results.
- **Use `Loc.unknown` for bytecode.** The text writer prints no locations. The bytecode writer
  writes file locations, but a file location's name is only found if the same text is also used
  as an attribute name or string attribute value, and the module's own location must be
  `Loc.unknown` or one an operation carries; otherwise the file is rejected.
- **Operand types in text.** The text writer looks up each operand's type among the values in
  scope. Where it cannot see the definition, such as across blocks of a region, give the types
  with an `ArrayAttr` of `TypeAttr`s named `_operand_types`, a convention of this package.
- **`callee`.** An operation with an attribute named `callee` has all its attributes printed as
  `<{...}>` properties in text.
- **Isolated regions.** The bytecode writer marks the regions of `func.func` and
  `builtin.module` as isolated from above (counting values again from 0), and of any operation
  named with `StreamEncode.isolating`. Naming is optional; naming an operation with more than one
  region, or one whose regions use outer values, gives a file MLIR rejects.
- **Strings.** A `StringAttr` holds its text with escape sequences still written out
  (`StringAttr "a\\nb"`). Keep to `\n`, `\t`, `\\`, `\"` and `\uXXXX`, which both writers
  understand.

The module documentation states each of these precisely, with the remaining edge cases.
