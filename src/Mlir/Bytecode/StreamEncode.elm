module Mlir.Bytecode.StreamEncode exposing (StreamTables, emptyStreamTables, isolating, collectAndEncodeOps, assembleModule)

{-| Writes an MLIR program built with `Mlir.Mlir` as an MLIR bytecode file. It
takes the program's top-level operations a batch at a time, so that a caller
need not hold them all at once: each batch is encoded to bytes as soon as it is
added, and only the bytes are kept.

MLIR bytecode does not write an operation's name, attributes, types, location
or most strings where the operation uses them. It writes each once, in tables
near the start of the file, and the operation refers to it by its _index_ in a
table. An operation can therefore be encoded before the tables are complete
only because every index here is _append-only_: an index never changes once
given. A _batch_ is the list of top-level operations given to one call of
`collectAndEncodeOps`. What its operations use is added to the tables, with
some exceptions, among them those described below, and then each operation is
encoded against them. `StreamTables` holds the tables and the bytes between
batches, and `assembleModule` writes the tables and then every operation, in the
order they were added, inside one `builtin.module` operation. The file declares
bytecode version 4.

In bytecode, an operation refers to the values it reads by number, and the
reader works the numbers out by counting values as it reads them. A region
marked in the file as _isolated from above_ starts counting again from 0.
MLIR's reader goes by that mark, not by the operation's definition. This
writer marks a region isolated by the name of the operation holding it, as
`emptyStreamTables` and `isolating` describe.

Nothing is checked as it is written. A name, type, location or attribute
dictionary missing from the tables, and an operand that names no value in
scope, are written as the index -1, which comes out as a nine-byte number that
points at no entry. The file is then corrupt, and the writer reports no error.
Besides the cases `collectAndEncodeOps` and `assembleModule` describe, one case
that arises in ordinary use is a file location, which `Mlir.Loc` describes. Its
file name is written as a reference to a string attribute with the same text,
which is in the tables only if that text was collected as a string attribute
for another reason, such as an attribute's name or an operation's `StringAttr`
value.

Some mistakes are not written as -1 and are not reported either. For example, a
successor label that names no block of its region is written as the region's
entry block.

An attribute or type used more than once is written once, as an entry that
every use refers to, except a use as an element of a typed `ArrayAttr`, which
refers to no entry. Some values that are written alike share an entry, such
as `BoolAttr True` and the `i1` integer 1, but not every pair written alike
does. A `TypedFloatAttr` of 0 and one of -0 of the same type also share an
entry, though they are not written alike, so both are written with the sign of
whichever was met first; so do two attribute dictionaries that differ only
there.

A `NamedStruct` type and an `OpaqueAttr` are written as their MLIR text, under
the dialect read from that text as `Mlir.Mlir.MlirType` and
`Mlir.Mlir.MlirAttr` describe. The text is not checked here; it is parsed only
by whatever reads the file.

@docs StreamTables, emptyStreamTables, isolating, collectAndEncodeOps, assembleModule

-}

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Encode as BE
import Dict exposing (Dict)
import Mlir.Bytecode.AttrType as AttrType exposing (AttrTypeTable)
import Mlir.Bytecode.DialectSection as DialectSection exposing (DialectRegistry)
import Mlir.Bytecode.IrSection as IrSection
import Mlir.Bytecode.Section as Section
import Mlir.Bytecode.StringTable as StringTable exposing (StringTable)
import Mlir.Bytecode.VarInt exposing (encodeVarInt, varIntWidth)
import Mlir.Loc exposing (Loc)
import Mlir.Mlir
    exposing
        ( MlirBlock
        , MlirOp
        , MlirRegion(..)
        )
import OrderedDict
import Set exposing (Set)



-- ==== Stream Tables (accumulator) ====


{-| A bytecode file being written a batch at a time.

Start from `emptyStreamTables`, add batches with `collectAndEncodeOps`, and
write the file with `assembleModule`. An index never changes once given, so
the bytes of an early batch are still right when `assembleModule` writes the
tables they refer to.

-}
type StreamTables
    = StreamTables
        { stringTable : StringTable
        , dialectBuilder : StreamDialectBuilder
        , attrAccum : AttrType.StreamAccum
        , encodedOps : List Bytes -- newest first
        , numOps : Int
        , isolated : Set String
        }


{-| A file to which no operation has been added yet.

The writer marks the regions of `func.func` and `builtin.module` operations as
isolated from above, and those of no other operation until `isolating` names
it.

-}
emptyStreamTables : StreamTables
emptyStreamTables =
    StreamTables
        { stringTable =
            StringTable.empty
                |> StringTable.addString "builtin"
                |> StringTable.addString "module"
        , dialectBuilder = emptyDialectBuilder |> addDialectOp "builtin.module"
        , attrAccum = AttrType.initStreamAccum
        , encodedOps = []
        , numOps = 0
        , isolated = Set.fromList [ "func.func", "builtin.module" ]
        }


{-| Adds operations, by name, to those whose regions the writer marks as
isolated from above, wherever in the program they appear. A name applies only
to batches added after the call.

Naming an operation is optional. What it changes is that each of the
operation's regions starts counting values again from 0 and is written in a
section of the file of its own.

Naming an operation gives a file that MLIR rejects when its regions use a
value defined outside them, because such an operand is written as -1. So does
naming an operation that has more than one region, because each region of a
named operation is written in a section of the file of its own, where MLIR's
reader expects all of them in one.

    StreamEncode.emptyStreamTables
        |> StreamEncode.isolating [ "llvm.func" ]

-}
isolating : List String -> StreamTables -> StreamTables
isolating names (StreamTables st) =
    StreamTables { st | isolated = List.foldl Set.insert st.isolated names }


{-| Adds `ops`, a batch of top-level operations, to the file, after those of
earlier batches.

What the operations of the batch use, and what the operations nested in them
use, is added to the tables, with some exceptions, among them those the module
documentation describes. The batch is encoded as it is added, and only its
bytes are kept. They are final: later batches do not change them.

Each top-level operation is encoded on its own, with no values in scope. So an
operand of the operation itself is written as -1, and nothing can refer to its
results.

-}
collectAndEncodeOps : List MlirOp -> StreamTables -> StreamTables
collectAndEncodeOps ops (StreamTables st) =
    let
        newStringTable =
            List.foldl StringTable.collectOp st.stringTable ops

        newDialectBuilder =
            List.foldl walkOpForDialects st.dialectBuilder ops

        newAttrAccum =
            List.foldl AttrType.streamCollectOp st.attrAccum ops

        -- Lookup-only views of the updated tables: they answer indices but
        -- hold nothing to write.
        encDialectReg =
            DialectSection.registryFromOpMap (dialectOpMap newDialectBuilder)

        encAttrTable =
            AttrType.streamAccumEncodingView newAttrAccum

        newEncodedOps =
            List.foldl
                (\op acc ->
                    BE.encode (IrSection.encodeFuncOp st.isolated encDialectReg encAttrTable op) :: acc
                )
                st.encodedOps
                ops
    in
    StreamTables
        { stringTable = newStringTable
        , dialectBuilder = newDialectBuilder
        , attrAccum = newAttrAccum
        , encodedOps = newEncodedOps
        , numOps = st.numOps + List.length ops
        , isolated = st.isolated
        }


{-| Returns the whole bytecode file: every operation added to the
`StreamTables`, in the order they were added, held in one `builtin.module`
operation whose location is `moduleLoc`.

`producer` names the program that wrote the file, such as `"my-compiler 1.2"`.
MLIR keeps it only to report it alongside any error it meets while reading the
file. It is written as given, followed by a zero byte that ends it, so a
`producer` holding a zero character is cut short there.

The file starts with a header of MLIR's magic number, bytecode version 4 and
the producer. Then come the string, dialect,
and attribute and type sections, the IR section holding the operations, and
resource sections that declare no resources.

`moduleLoc` is looked up in the tables but not added to them. It is found if
it has the name and start of `Mlir.Loc.unknown`, which the tables always hold,
or of the location of an operation added or nested in one, whatever its end;
otherwise it is written as -1, and the file is corrupt.

-}
assembleModule : String -> StreamTables -> Loc -> Bytes
assembleModule producer (StreamTables st) moduleLoc =
    let
        stringTable =
            st.stringTable

        attrTypeTable =
            AttrType.finalizeStreamAccum st.attrAccum

        -- A dialect that only attributes or types bring in, such as an
        -- `OpaqueAttr`'s, is numbered here, after every dialect of an
        -- operation name.
        dialectRegistry =
            finalizeDialectBuilder
                (List.foldl addDialect st.dialectBuilder (AttrType.entryDialects attrTypeTable))

        stringSectionBody =
            StringTable.encode stringTable

        dialectSectionBody =
            DialectSection.encode stringTable dialectRegistry

        ( attrTypeSectionBody, attrTypeOffsetSectionBody ) =
            AttrType.encodeDataAndOffsets stringTable dialectRegistry attrTypeTable
    in
    BE.encode <|
        BE.sequence
            [ -- Magic number: "MLïR"
              BE.unsignedInt8 0x4D
            , BE.unsignedInt8 0x4C
            , BE.unsignedInt8 0xEF
            , BE.unsignedInt8 0x52
            , encodeVarInt bytecodeVersion

            -- Producer string, written inline and NUL-terminated
            , BE.string producer
            , BE.unsignedInt8 0x00
            , Section.encodeSection Section.sectionId.string stringSectionBody
            , Section.encodeSection Section.sectionId.dialect dialectSectionBody
            , Section.encodeSection Section.sectionId.attrType attrTypeSectionBody
            , Section.encodeSection Section.sectionId.attrTypeOffset attrTypeOffsetSectionBody
            , irSection dialectRegistry attrTypeTable st.numOps st.encodedOps moduleLoc

            -- Resource sections declaring no resources
            , Section.encodeSection Section.sectionId.resource (BE.sequence [])
            , Section.encodeSection Section.sectionId.resourceOffset (encodeVarInt 0)
            ]


{-| The version of the MLIR bytecode format that the file header declares.

The dialect and IR sections are always written in layouts that assume
version 4, whatever the value here, so they must be changed together.

-}
bytecodeVersion : Int
bytecodeVersion =
    4



-- ==== IR Section Assembly ====


{-| Creates an encoder for the whole IR section, id and length included: a
`builtin.module` operation located at `moduleLoc` whose one block holds the
operations already encoded in `encodedOpsNewestFirst`, written oldest first.
`numOps` must be the number of those operations.

The module operation has one isolated region, written in an IR section of its
own. The region has one block with no arguments, and is written as defining no
values, which matches each operation having been encoded with no values in
scope. Laid out, the section is:

    u8 ir, varint irBodyLen,
        blockHeader, moduleNameIdx, 0x10, moduleLocIdx, regionEncoding,
        u8 ir, varint regionLen,
            varint 1, varint 0, bodyBlockHeader, op bytes...

Both lengths are worked out from `varIntWidth` and the widths of the operation
bytes, instead of by encoding the contents and measuring them as
`Section.encodeSection`, which frames the other sections, does. The bytes are
those `Section.encodeSection` would write, since it puts nothing between a
section's length and its contents.

-}
irSection : DialectRegistry -> AttrTypeTable -> Int -> List Bytes -> Loc -> BE.Encoder
irSection dialectReg attrTypeTable numOps encodedOpsNewestFirst moduleLoc =
    let
        -- Block header (numOps << 1) | hasArgs: one op, the module, with no
        -- arguments.
        blockHeaderValue =
            Bitwise.shiftLeftBy 1 1

        moduleNameIdx =
            DialectSection.opIndex "builtin.module" dialectReg

        moduleLocIdx =
            AttrType.locIndex moduleLoc attrTypeTable

        -- regionEncoding: (numRegions << 1) | isIsolated = (1 << 1) | 1 = 3
        regionEncodingValue =
            Bitwise.or (Bitwise.shiftLeftBy 1 1) 1

        -- The region's one block: every encoded op, no arguments.
        bodyBlockHeaderValue =
            Bitwise.shiftLeftBy 1 numOps

        -- Folding the newest-first list onto the front reverses it to oldest
        -- first.
        ( opEncoders, opsWidth ) =
            List.foldl
                (\b ( acc, w ) -> ( BE.bytes b :: acc, w + Bytes.width b ))
                ( [], 0 )
                encodedOpsNewestFirst

        regionLen =
            varIntWidth 1
                + varIntWidth 0
                + varIntWidth bodyBlockHeaderValue
                + opsWidth

        irBodyLen =
            varIntWidth blockHeaderValue
                + varIntWidth moduleNameIdx
                + 1
                + varIntWidth moduleLocIdx
                + varIntWidth regionEncodingValue
                + 1
                + varIntWidth regionLen
                + regionLen
    in
    BE.sequence
        [ BE.sequence
            [ -- IR section header
              BE.unsignedInt8 Section.sectionId.ir
            , encodeVarInt irBodyLen

            -- builtin.module op
            , encodeVarInt blockHeaderValue
            , encodeVarInt moduleNameIdx
            , BE.unsignedInt8 0x10 -- kHasInlineRegions
            , encodeVarInt moduleLocIdx
            , encodeVarInt regionEncodingValue

            -- Region section header + region content header
            , BE.unsignedInt8 Section.sectionId.ir
            , encodeVarInt regionLen
            , encodeVarInt 1 -- number of blocks
            , encodeVarInt 0 -- number of values the blocks define
            , encodeVarInt bodyBlockHeaderValue
            ]
        , BE.sequence opEncoders
        ]



-- ==== Streaming Dialect Builder ====


{-| The numbering of dialects and operation names for the dialect section,
built up as operations are met.

Each new dialect and each new operation name takes the next free index in its
own numbering, and an index never changes once given, so operations can be
encoded against `dialectOpMap` before every name is known. Each name's dialect
index and suffix are kept, newest first, for `finalizeDialectBuilder` to turn
into op groups.

-}
type StreamDialectBuilder
    = StreamDialectBuilder
        { dialectList : List String -- newest first
        , dialectSet : Dict String Int -- dialect -> its index
        , numDialects : Int
        , opEntries : List { dialectIdx : Int, opSuffix : String } -- newest first
        , opSet : Dict String Int -- full operation name -> its index
        , nextOpIndex : Int
        }


{-| A builder that has numbered no dialect and no operation name.
-}
emptyDialectBuilder : StreamDialectBuilder
emptyDialectBuilder =
    StreamDialectBuilder
        { dialectList = []
        , dialectSet = Dict.empty
        , numDialects = 0
        , opEntries = []
        , opSet = Dict.empty
        , nextOpIndex = 0
        }


{-| Numbers the operation named `fullName` with the next free operation index,
and its dialect with the next free dialect index if the dialect is new. A name
already numbered leaves the builder unchanged.

A name with no `.` is a dialect with an empty suffix, and keeps its index under
the name itself.

-}
addDialectOp : String -> StreamDialectBuilder -> StreamDialectBuilder
addDialectOp fullName (StreamDialectBuilder b) =
    case Dict.get fullName b.opSet of
        Just _ ->
            StreamDialectBuilder b

        Nothing ->
            let
                ( dialect, suffix ) =
                    StringTable.splitDialect fullName

                dIdx =
                    Dict.get dialect b.dialectSet |> Maybe.withDefault b.numDialects

                isNewDialect =
                    not (Dict.member dialect b.dialectSet)
            in
            StreamDialectBuilder
                { dialectList =
                    if isNewDialect then
                        dialect :: b.dialectList

                    else
                        b.dialectList
                , dialectSet =
                    if isNewDialect then
                        Dict.insert dialect b.numDialects b.dialectSet

                    else
                        b.dialectSet
                , numDialects =
                    if isNewDialect then
                        b.numDialects + 1

                    else
                        b.numDialects
                , opEntries = { dialectIdx = dIdx, opSuffix = suffix } :: b.opEntries
                , opSet = Dict.insert fullName b.nextOpIndex b.opSet
                , nextOpIndex = b.nextOpIndex + 1
                }


{-| Numbers `dialect` with the next free dialect index, without any operation
name, unless it is numbered already.
-}
addDialect : String -> StreamDialectBuilder -> StreamDialectBuilder
addDialect dialect (StreamDialectBuilder b) =
    if Dict.member dialect b.dialectSet then
        StreamDialectBuilder b

    else
        StreamDialectBuilder
            { b
                | dialectList = dialect :: b.dialectList
                , dialectSet = Dict.insert dialect b.numDialects b.dialectSet
                , numDialects = b.numDialects + 1
            }


{-| Returns every operation name numbered so far, written in full, with its
index.
-}
dialectOpMap : StreamDialectBuilder -> Dict String Int
dialectOpMap (StreamDialectBuilder b) =
    b.opSet


{-| Returns the registry the dialect section is written from: the dialects in
the order they were numbered, and the operation names in op groups, one for
each run of consecutive names of one dialect, so that each name's position in
the section is the index it was given.
-}
finalizeDialectBuilder : StreamDialectBuilder -> DialectRegistry
finalizeDialectBuilder (StreamDialectBuilder b) =
    let
        dialects =
            List.reverse b.dialectList

        entries =
            List.reverse b.opEntries

        opGroups =
            buildRunLengthOpGroups entries
    in
    DialectSection.buildRegistry
        { dialects = dialects
        , dialectIndices = b.dialectSet
        , opGroups = opGroups
        , opIndexMap = b.opSet
        }


{-| Splits `entries` into op groups, one for each run of consecutive entries of
the same dialect, keeping their order. A dialect whose entries are interleaved
with another's gets one group per run.
-}
buildRunLengthOpGroups : List { dialectIdx : Int, opSuffix : String } -> List DialectSection.OpGroup
buildRunLengthOpGroups entries =
    case entries of
        [] ->
            []

        first :: rest ->
            let
                ( groupNames, remaining ) =
                    spanByDialect first.dialectIdx [ first.opSuffix ] rest
            in
            { dialectIdx = first.dialectIdx, opNames = groupNames }
                :: buildRunLengthOpGroups remaining


{-| Splits off the run at the head of `entries` whose dialect is `dIdx`.
Returns the suffixes of `acc`, which holds them newest first, followed by those
of the run, in order, and the entries left after the run.
-}
spanByDialect : Int -> List String -> List { dialectIdx : Int, opSuffix : String } -> ( List String, List { dialectIdx : Int, opSuffix : String } )
spanByDialect dIdx acc entries =
    case entries of
        [] ->
            ( List.reverse acc, [] )

        e :: rest ->
            if e.dialectIdx == dIdx then
                spanByDialect dIdx (e.opSuffix :: acc) rest

            else
                ( List.reverse acc, entries )



-- ==== Walk ops for dialect op names ====


{-| Numbers the name of `op` and then those of the operations nested in its
regions.
-}
walkOpForDialects : MlirOp -> StreamDialectBuilder -> StreamDialectBuilder
walkOpForDialects op builder =
    let
        b1 =
            addDialectOp op.name builder
    in
    List.foldl walkRegionForDialects b1 op.regions


{-| Numbers the operation names in a region: its entry block first, then its
other blocks in their stored order.
-}
walkRegionForDialects : MlirRegion -> StreamDialectBuilder -> StreamDialectBuilder
walkRegionForDialects (MlirRegion r) builder =
    let
        b1 =
            walkBlockForDialects r.entry builder
    in
    OrderedDict.foldl (\_ blk acc -> walkBlockForDialects blk acc) b1 r.blocks


{-| Numbers the operation names in a block: its body in order, then its
terminator.
-}
walkBlockForDialects : MlirBlock -> StreamDialectBuilder -> StreamDialectBuilder
walkBlockForDialects blk builder =
    let
        b1 =
            List.foldl walkOpForDialects builder blk.body
    in
    walkOpForDialects blk.terminator b1
