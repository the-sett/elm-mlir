module Mlir.Bytecode.DialectSection exposing
    ( DialectRegistry, opIndex, dialectIndex, encode
    , OpGroup, buildRegistry, registryFromOpMap
    )

{-| MLIR bytecode does not write an operation's name with the operation. It
refers to the name by an index into a list of operation names kept in the
dialect section, and this module holds that numbering and writes that section.

An MLIR operation name has the form `dialect.op`, such as `func.func`. The
_dialect_ is the text before the first `.` and the _suffix_ is the rest. The
dialect section lists the dialects, then the operation names in _op groups_.
An op group is one dialect, given by its index in the dialect list, followed
by suffixes of that dialect's operation names. The section holds no index for
an operation name: its index is the position of its suffix among the suffixes
listed in all the op groups, counting from 0 and continuing across groups. A
dialect's index is likewise its position in the dialect list.

A `DialectRegistry` holds two things apart: the numbering that `opIndex` and
`dialectIndex` answer from, and the dialect list and op groups that `encode`
writes. This module works out no numbering of its own. `buildRegistry` and
`registryFromOpMap` take what they are given, and nothing checks that the two
parts agree.

Dialect names and suffixes are written as indices into the string section, so
each must already be in the `StringTable` given to `encode`. No lookup in this
module is checked. `Mlir.Bytecode.StringTable.indexOf` gives -1 for a string
that was never added, and `Mlir.Bytecode.VarInt.encodeVarInt` writes a negative
number in nine bytes rather than rejecting it, so a missed suffix is written as
2^64 − 1 and a missed dialect name, whose index is doubled first, as 2^64 − 2.
`opIndex` and `dialectIndex` likewise return -1 for a name the registry has no
index for, and report no miss as an error.

@docs DialectRegistry, opIndex, dialectIndex, encode
@docs OpGroup, buildRegistry, registryFromOpMap

-}

import Bytes.Encode as BE
import Dict exposing (Dict)
import Mlir.Bytecode.StringTable as StringTable exposing (StringTable)
import Mlir.Bytecode.VarInt exposing (encodeVarInt)


{-| A numbering of dialects and operation names, together with a dialect list
and op groups.

`opIndex` and `dialectIndex` answer from the numbering, and `encode` writes
the dialect list and op groups. The two are held apart, so an index looked up
names the right entry of the written section only where they agree. A
registry is made by `buildRegistry`, which holds exactly what it is given, or
by `registryFromOpMap`, which answers only `opIndex`.

-}
type DialectRegistry
    = DialectRegistry
        { dialects : List String
        , dialectIndices : Dict String Int
        , opGroups : List OpGroup
        , opIndexMap : Dict String Int
        }


{-| One op group of the dialect section: a dialect, given by its index in the
dialect list, and suffixes of that dialect's operation names, in the order they
are written.

`opNames` holds suffixes, not full names: `func` for `func.func`. `encode`
writes whatever groups a registry holds, and one from `buildRegistry` may hold
several groups for the same dialect.

-}
type alias OpGroup =
    { dialectIdx : Int
    , opNames : List String
    }


{-| Returns the registry's index for the operation name `name`, written in
full as `dialect.op`, or -1 if the registry has no index for it.
-}
opIndex : String -> DialectRegistry -> Int
opIndex name (DialectRegistry reg) =
    case Dict.get name reg.opIndexMap of
        Just idx ->
            idx

        Nothing ->
            -1


{-| Returns the registry's index for the dialect `name`, or -1 if the registry
has no index for it.
-}
dialectIndex : String -> DialectRegistry -> Int
dialectIndex name (DialectRegistry reg) =
    case Dict.get name reg.dialectIndices of
        Just idx ->
            idx

        Nothing ->
            -1


{-| Creates an encoder for the contents of the dialect section, with every name
written as its index in `stringTable`.

It writes the number of dialects; for each dialect, its name's string index
times two, so that the low bit, which would say a dialect version follows, is
clear; the number of operation names, summed over all the op groups; and then
each op group as its dialect index, its number of suffixes, and each suffix's
string index, not doubled. Every number is a PrefixVarInt, as
`Mlir.Bytecode.VarInt` describes. This is the layout MLIR reads for bytecode
version 4, the version `Mlir.Bytecode.StreamEncode` writes in the file header.

A dialect name missing from `stringTable` is written as 2^64 − 2, and a
missing suffix as 2^64 − 1.

-}
encode : StringTable -> DialectRegistry -> BE.Encoder
encode stringTable (DialectRegistry reg) =
    let
        numDialects =
            List.length reg.dialects

        dialectNameEncoders =
            reg.dialects
                |> List.map
                    (\name ->
                        let
                            strIdx =
                                StringTable.indexOf name stringTable
                        in
                        encodeVarInt (strIdx * 2)
                    )

        totalOpNames =
            List.foldl (\group total -> total + List.length group.opNames) 0 reg.opGroups

        opGroupEncoders =
            reg.opGroups
                |> List.map (encodeOpGroup stringTable)
    in
    BE.sequence
        (encodeVarInt numDialects
            :: dialectNameEncoders
            ++ [ encodeVarInt totalOpNames ]
            ++ opGroupEncoders
        )


{-| Creates an encoder for one op group: its dialect index, its number of
suffixes, and each suffix's index in `stringTable`, written as it is with no
flag bit, unlike a dialect name's index in `encode`.
-}
encodeOpGroup : StringTable -> OpGroup -> BE.Encoder
encodeOpGroup stringTable group =
    let
        numOps =
            List.length group.opNames

        opEncoders =
            group.opNames
                |> List.map
                    (\name ->
                        let
                            strIdx =
                                StringTable.indexOf name stringTable
                        in
                        encodeVarInt strIdx
                    )
    in
    BE.sequence
        (encodeVarInt group.dialectIdx
            :: encodeVarInt numOps
            :: opEncoders
        )


{-| Creates a registry holding exactly the given numberings, dialect list and op
groups.

Nothing is checked. `opIndexMap`, keyed by full operation name, and
`dialectIndices` are what `opIndex` and `dialectIndex` return, and `dialects`
and `opGroups` are what `encode` writes. For a looked-up index to name the
right entry of the written section, each operation name's index must be the
position of its suffix, counting from 0 through `opGroups` in order, in a group
whose `dialectIdx` is the index of the operation's dialect, and each dialect's
index its position in `dialects`.

-}
buildRegistry :
    { dialects : List String
    , dialectIndices : Dict String Int
    , opGroups : List OpGroup
    , opIndexMap : Dict String Int
    }
    -> DialectRegistry
buildRegistry r =
    DialectRegistry r


{-| Creates a registry that answers only `opIndex`, from a map of full
operation names to their indices.

`dialectIndex` on it returns -1 for every name, and `encode` writes a section
with no dialects and no operation names.

-}
registryFromOpMap : Dict String Int -> DialectRegistry
registryFromOpMap opMap =
    DialectRegistry
        { dialects = []
        , dialectIndices = Dict.empty
        , opGroups = []
        , opIndexMap = opMap
        }
