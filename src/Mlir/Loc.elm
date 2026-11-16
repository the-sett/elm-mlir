module Mlir.Loc exposing (Loc(..), Pos, unknown)

{-| Every operation and module in `Mlir.Mlir` carries a location, which says
where in the source program it came from, or that this is not known. This
module defines the type of that location.

A location names a file and spans a range within it, from a start position to
an end position, each a row and a column. When there is no source position to
give, the location is `unknown`, which is where every operation built with
`Mlir.Mlir.mlirOp` starts.

The two writers use a location differently. The text writer, `Mlir.Pretty`,
prints no locations at all. The bytecode writer, `Mlir.Bytecode.StreamEncode`,
writes operations' locations. It does not read an `MlirModule`'s `loc`. It
looks up the location passed to `assembleModule` for the module but does not
add it, so unless that location has the name and start of `unknown` or of the
location of an operation given to the writer or nested in one, the module's
location is written as a reference to no entry.

The bytecode writer writes `unknown`, and any location named `"unknown"` that
starts at row 0, column 0, as MLIR's unknown location, with no file name, line
or column. It writes any other location of an operation as a file name, a line
and a column, taken from the name and the start. The end is not written, so two
locations that differ only in their end are written as one.

`unknown` is always written to bytecode correctly, but an operation's file
location may not be. Its file name is written as a reference to an entry in the
bytecode's attribute table holding a string attribute with that text, and the
writer does not add that entry for the location. The entry is there only if
the same text was added for another reason, such as an attribute name or a
`StringAttr` attribute value of an operation given to the writer; otherwise the
reference points to no entry.

@docs Loc, Pos, unknown

-}


{-| A source location: a range within the file called `name`, from `start` to
`end`, or the placeholder `unknown`, which says that no source position is
known.

The constructor is exposed, so a location is made directly, as in

    Loc
        { name = "input.mlir"
        , start = { row = 3, col = 1 }
        , end = { row = 3, col = 12 }
        }

Nothing checks that the file exists or that `start` comes before `end`.

-}
type Loc
    = Loc
        { name : String
        , start : Pos
        , end : Pos
        }


{-| A position in a source file, as a row and a column.

Nothing in this package says whether rows and columns count from zero or from
one, and nothing converts them. Where the bytecode writer writes a line and a
column, they are the row and the column, each unchanged if it is from 0 to
2^63 - 1. A negative row or column is not rejected; one from -2^63 to -1 is
written as the unsigned 64-bit number 2^64 plus its value.

-}
type alias Pos =
    { row : Int
    , col : Int
    }


{-| The location of an operation or module that has no source position.

It is an ordinary `Loc`, with the file name `"unknown"` and both positions at
row 0, column 0, not a constructor of its own. The bytecode writer writes any
location with that name and that start as MLIR's unknown location, whatever its
end, so a location in a real file called `unknown` that starts at row 0,
column 0 cannot be written to bytecode as one.

-}
unknown : Loc
unknown =
    { name = "unknown"
    , start = { row = 0, col = 0 }
    , end = { row = 0, col = 0 }
    }
        |> Loc
