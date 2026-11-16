module Mlir.Bytecode.IrSection exposing (encodeFuncOp)

{-| An MLIR bytecode file keeps a program's operations in its IR section, and
this module writes them there, with the regions and blocks nested inside them.

In bytecode an operation does not name the SSA values it reads. It gives each
one's _value index_, a number that the reader works out for itself by counting
values as it reads them, so the encoder has to count them the same way or an
operand will point at the wrong value. It assumes MLIR's reader counts like
this:

  - Within one region, the values its blocks define are numbered one after
    another: the entry block's first, then each labelled block's in the order
    of `blocks`, and within a block its arguments, then the results of its
    operations in order, ending with its `terminator`. Regions nested inside
    those operations are not part of this count.
  - A region is _isolated_ when it cannot see values defined outside it. Its
    numbering starts at 0.
  - Any other region carries on from where the numbering of the region around
    it stopped, after all of that region's values. Sibling regions of one
    operation, such as the two branches of an `scf.if`, each start from that
    same number.

So each region is handled in two passes. The first, `numberRegion`, builds its
_value environment_, the map from SSA name to value index. The second,
`encodeRegion`, writes the region using it. Because a region is numbered in
full before any of it is written, an operand may name a value defined later in
the region, and a name defined twice among the values a region can see refers
to the definition numbered last. A nested region is numbered only when the
operation holding it is written.

This module treats a region as isolated by the name of the operation holding
it and nothing else: the regions of the operations named in the `isolated` set
the caller gives are treated as isolated, and no others.
`Mlir.Bytecode.StreamEncode` describes that set. Each region treated as
isolated is written inside an IR section of its own.

Operations in a block's `body` whose `isTerminator` is set are left out, both
of the numbering and of what is written.

Blocks are referred to by _block index_ within their region: the entry block
is 0 and is called `bb0`, and the labelled blocks follow from 1. A labelled
block that is also called `bb0` takes that name over from the entry block.

Nothing here is checked. An operand name missing from the value environment
is written as -1, and so is a type, location, attribute dictionary or
operation name the tables do not hold, as `Mlir.Bytecode.AttrType` and
`Mlir.Bytecode.DialectSection` describe. `Mlir.Bytecode.VarInt.encodeVarInt`
writes -1 as 2^64 − 1, so the file is corrupt rather than the encoding
failing. A successor label that names no block is written not as -1 but as
block 0, the entry block.

@docs encodeFuncOp

-}

import Bitwise
import Bytes.Encode as BE
import Dict exposing (Dict)
import Mlir.Bytecode.AttrType as AttrType exposing (AttrTypeTable)
import Mlir.Bytecode.DialectSection as DialectSection exposing (DialectRegistry)
import Mlir.Bytecode.Section as Section
import Mlir.Bytecode.VarInt exposing (encodeVarInt)
import Mlir.Loc
import Mlir.Mlir
    exposing
        ( MlirBlock
        , MlirOp
        , MlirRegion(..)
        , MlirType
        )
import OrderedDict
import Set exposing (Set)



-- ==== Encoding mask bits ====


{-| The bit of an operation's encoding mask saying that it has an attribute
dictionary.
-}
kHasAttrs : Int
kHasAttrs =
    0x01


{-| The bit of an operation's encoding mask saying that it has results.
-}
kHasResults : Int
kHasResults =
    0x02


{-| The bit of an operation's encoding mask saying that it has operands.
-}
kHasOperands : Int
kHasOperands =
    0x04


{-| The bit of an operation's encoding mask saying that it has successors.
-}
kHasSuccessors : Int
kHasSuccessors =
    0x08


{-| The bit of an operation's encoding mask saying that it has regions.
-}
kHasInlineRegions : Int
kHasInlineRegions =
    0x10



-- ==== SSA Value Environment ====


{-| A value environment: the value index of each SSA name numbered so far, and
the index the next value numbered will get.
-}
type alias ValueEnv =
    { valueMap : Dict String Int
    , nextValueIndex : Int
    }


{-| A value environment with no names in it, whose numbering starts at 0.
-}
emptyValueEnv : ValueEnv
emptyValueEnv =
    { valueMap = Dict.empty
    , nextValueIndex = 0
    }


{-| Gives `name` the next value index in `env`.

The index is used up even when `name` already has one, and `name` then refers
to the new index.

-}
registerValue : String -> ValueEnv -> ValueEnv
registerValue name env =
    { valueMap = Dict.insert name env.nextValueIndex env.valueMap
    , nextValueIndex = env.nextValueIndex + 1
    }


{-| Returns the value index of `name` in `env`, or -1 if it has none.
-}
lookupValue : String -> ValueEnv -> Int
lookupValue name env =
    case Dict.get name env.valueMap of
        Just idx ->
            idx

        Nothing ->
            -1


{-| Gives each of the named values in `pairs` the next value index in `env`,
in list order. The types are ignored.
-}
registerValues : List ( String, MlirType ) -> ValueEnv -> ValueEnv
registerValues pairs env =
    List.foldl (\( name, _ ) acc -> registerValue name acc) env pairs



-- ==== Pass 1: Value Numbering ====


{-| Returns `env` extended with value indices for the values defined in the
blocks of a region, numbered on from `env`'s next index: the entry block, then
the labelled blocks in order.

The results of operations in `body` whose `isTerminator` is set are not
numbered, and nor are the regions nested inside the region's operations.

-}
numberRegion : ValueEnv -> MlirRegion -> ValueEnv
numberRegion env (MlirRegion r) =
    let
        env1 =
            numberBlock env r.entry
    in
    OrderedDict.foldl (\_ blk acc -> numberBlock acc blk) env1 r.blocks


{-| Returns `env` extended with value indices for a block's arguments, then for
the results of its operations in order, ending with its terminator.

Operations in `body` whose `isTerminator` is set are skipped, as they are when
the block is written.

-}
numberBlock : ValueEnv -> MlirBlock -> ValueEnv
numberBlock env blk =
    let
        env1 =
            registerValues blk.args env

        nonTermBody =
            List.filter (\op -> not op.isTerminator) blk.body

        env2 =
            List.foldl numberOp env1 nonTermBody
    in
    numberOp blk.terminator env2


{-| Returns `env` extended with value indices for the results of `op`.
-}
numberOp : MlirOp -> ValueEnv -> ValueEnv
numberOp op env =
    registerValues op.results env



-- ==== Block index environment ====


{-| The block indices of one region, by label: each labelled block's under its
label, and the entry block's under `bb0` unless a labelled block is also called
`bb0`.
-}
type alias BlockEnv =
    Dict String Int


{-| Returns the block indices of a region: 0 for the entry block, as `bb0`, then
1, 2, ... for the labelled blocks in the order of `blocks`.

A labelled block that is itself called `bb0` takes that name over from the
entry block.

-}
buildBlockEnv : MlirRegion -> BlockEnv
buildBlockEnv (MlirRegion r) =
    let
        namedBlocks =
            OrderedDict.toList r.blocks

        indexed =
            namedBlocks
                |> List.indexedMap (\i ( label, _ ) -> ( label, i + 1 ))
    in
    Dict.fromList (( "bb0", 0 ) :: indexed)



-- ==== Pass 2: Encoding ====


{-| Creates an encoder for a region, given `valueEnv`, its value environment,
which must number the region's values as `numberRegion` does.

It writes the number of blocks, the number of values `countRegionValues` counts
in them, then the entry block and the labelled blocks in order. Successors are
resolved against this region's blocks.

-}
encodeRegion : Set String -> DialectRegistry -> AttrTypeTable -> ValueEnv -> MlirRegion -> BE.Encoder
encodeRegion isolated dialectReg attrTypeTable valueEnv (MlirRegion r) =
    let
        namedBlocks =
            OrderedDict.toList r.blocks

        numBlocks =
            1 + List.length namedBlocks

        blockEnv =
            buildBlockEnv (MlirRegion r)

        numValues =
            countRegionValues (MlirRegion r)

        entryEncoder =
            encodeBlock isolated dialectReg attrTypeTable valueEnv blockEnv r.entry

        namedEncoders =
            namedBlocks
                |> List.map (\( _, blk ) -> encodeBlock isolated dialectReg attrTypeTable valueEnv blockEnv blk)
    in
    BE.sequence
        (encodeVarInt numBlocks
            :: encodeVarInt numValues
            :: entryEncoder
            :: namedEncoders
        )


{-| Creates an encoder for a block: a header giving its number of operations
and whether it has arguments, the arguments if it has any, then its operations.

The operations written are those of `body` whose `isTerminator` is not set,
followed by `terminator`.

-}
encodeBlock : Set String -> DialectRegistry -> AttrTypeTable -> ValueEnv -> BlockEnv -> MlirBlock -> BE.Encoder
encodeBlock isolated dialectReg attrTypeTable valueEnv blockEnv blk =
    let
        hasBlockArgs =
            not (List.isEmpty blk.args)

        allOps =
            List.filter (\op -> not op.isTerminator) blk.body ++ [ blk.terminator ]

        numOps =
            List.length allOps

        blockHeader =
            Bitwise.or
                (Bitwise.shiftLeftBy 1 numOps)
                (if hasBlockArgs then
                    1

                 else
                    0
                )

        blockArgsEncoder =
            if hasBlockArgs then
                encodeBlockArgs attrTypeTable blk.args

            else
                BE.sequence []

        opEncoders =
            allOps
                |> List.map (encodeOp isolated dialectReg attrTypeTable valueEnv blockEnv)
    in
    BE.sequence
        (encodeVarInt blockHeader
            :: blockArgsEncoder
            :: opEncoders
        )


{-| Creates an encoder for a block's arguments: their number, then for each one
its type index shifted left one bit with the low bit set, saying a location
follows, and the index of `Mlir.Loc.unknown`, then a zero byte.

The model holds no location for a block argument, so every argument is given
the unknown location. The zero byte is always written; this module assumes
the reader takes it as saying the block has no use-list orders.

-}
encodeBlockArgs : AttrTypeTable -> List ( String, MlirType ) -> BE.Encoder
encodeBlockArgs attrTypeTable args =
    let
        numArgs =
            List.length args

        argEncoders =
            args
                |> List.map
                    (\( _, ty ) ->
                        let
                            tyIdx =
                                AttrType.typeIndex ty attrTypeTable

                            typeAndLoc =
                                Bitwise.or (Bitwise.shiftLeftBy 1 tyIdx) 1

                            locIdx =
                                AttrType.locIndex Mlir.Loc.unknown attrTypeTable
                        in
                        BE.sequence
                            [ encodeVarInt typeAndLoc
                            , encodeVarInt locIdx
                            ]
                    )
    in
    BE.sequence
        (encodeVarInt numArgs
            :: argEncoders
            ++ [ BE.unsignedInt8 0 ]
        )


{-| Creates an encoder for `op`, given `valueEnv`, the value environment of the
region it is in, and `blockEnv`, the block indices of that region.

It writes the operation name's index, an encoding mask byte saying which of
the optional parts follow, and the location's index. Then come those parts that
are present, in this order: the index of the attribute dictionary as
`Mlir.Bytecode.AttrType.bytecodeAttrs` returns it, present only when that is
not empty; the number of results and each result's type index; the number of
operands and each one's value index; the number of successors and each one's
block index, looked up with one leading `^` removed; and the regions. No
properties and no use-list orders are written.

The regions start with one number holding their count shifted left one bit,
with the low bit set when the operation's name is in `isolated`. Each region of
such an operation is numbered from 0 and wrapped in an IR section of its own,
so one with several regions has one section per region. Every
other region is numbered on from `valueEnv`, each separately, so siblings start
from the same index and see the names in `valueEnv` but not each other's
values.

-}
encodeOp : Set String -> DialectRegistry -> AttrTypeTable -> ValueEnv -> BlockEnv -> MlirOp -> BE.Encoder
encodeOp isolated dialectReg attrTypeTable valueEnv blockEnv op =
    let
        nameIdx =
            DialectSection.opIndex op.name dialectReg

        locIdx =
            AttrType.locIndex op.loc attrTypeTable

        -- Without _operand_types, as collected, so the lookup can find it.
        attrs =
            AttrType.bytecodeAttrs op.attrs

        hasAttrs =
            not (Dict.isEmpty attrs)

        hasResults =
            not (List.isEmpty op.results)

        hasOperands =
            not (List.isEmpty op.operands)

        hasSuccessors =
            not (List.isEmpty op.successors)

        hasRegions =
            not (List.isEmpty op.regions)

        encodingMask =
            (if hasAttrs then
                kHasAttrs

             else
                0
            )
                |> Bitwise.or
                    (if hasResults then
                        kHasResults

                     else
                        0
                    )
                |> Bitwise.or
                    (if hasOperands then
                        kHasOperands

                     else
                        0
                    )
                |> Bitwise.or
                    (if hasSuccessors then
                        kHasSuccessors

                     else
                        0
                    )
                |> Bitwise.or
                    (if hasRegions then
                        kHasInlineRegions

                     else
                        0
                    )

        attrEncoder =
            if hasAttrs then
                [ encodeVarInt (AttrType.dictAttrIndex attrs attrTypeTable) ]

            else
                []

        resultsEncoder =
            if hasResults then
                let
                    numResults =
                        List.length op.results

                    typeEncoders =
                        op.results
                            |> List.map (\( _, t ) -> encodeVarInt (AttrType.typeIndex t attrTypeTable))
                in
                encodeVarInt numResults :: typeEncoders

            else
                []

        operandsEncoder =
            if hasOperands then
                let
                    numOperands =
                        List.length op.operands

                    valEncoders =
                        op.operands
                            |> List.map (\name -> encodeVarInt (lookupValue name valueEnv))
                in
                encodeVarInt numOperands :: valEncoders

            else
                []

        successorsEncoder =
            if hasSuccessors then
                let
                    numSuccessors =
                        List.length op.successors

                    succEncoders =
                        op.successors
                            |> List.map
                                (\label ->
                                    let
                                        cleanLabel =
                                            if String.startsWith "^" label then
                                                String.dropLeft 1 label

                                            else
                                                label

                                        blockIdx =
                                            Dict.get cleanLabel blockEnv
                                                |> Maybe.withDefault 0
                                    in
                                    encodeVarInt blockIdx
                                )
                in
                encodeVarInt numSuccessors :: succEncoders

            else
                []

        regionsEncoder =
            if hasRegions then
                let
                    numRegions =
                        List.length op.regions

                    isIsolated =
                        Set.member op.name isolated

                    regionEncoding =
                        Bitwise.or
                            (Bitwise.shiftLeftBy 1 numRegions)
                            (if isIsolated then
                                1

                             else
                                0
                            )

                    regionBaseEnv =
                        valueEnv

                    regionEncoders =
                        op.regions
                            |> List.map
                                (\region ->
                                    if isIsolated then
                                        let
                                            isoEnv =
                                                numberRegion emptyValueEnv region
                                        in
                                        Section.encodeSection Section.sectionId.ir
                                            (encodeRegion isolated dialectReg attrTypeTable isoEnv region)

                                    else
                                        let
                                            altEnv =
                                                numberRegion regionBaseEnv region
                                        in
                                        encodeRegion isolated dialectReg attrTypeTable altEnv region
                                )
                in
                encodeVarInt regionEncoding :: regionEncoders

            else
                []
    in
    BE.sequence
        ([ encodeVarInt nameIdx
         , BE.unsignedInt8 encodingMask
         , encodeVarInt locIdx
         ]
            ++ attrEncoder
            ++ resultsEncoder
            ++ operandsEncoder
            ++ successorsEncoder
            ++ regionsEncoder
        )


{-| Creates an encoder for a top-level operation, whatever its name, on its own,
so that a module's operations can be encoded one at a time. `isolated` names
the operations, at any depth, whose regions are treated as isolated.

The operation is encoded with an empty value environment and no block
indices. So an operand of the operation itself is written as -1 and a
successor as block 0, and each of its regions is numbered from 0, isolated or
not. The operation's own results get no value index.

-}
encodeFuncOp : Set String -> DialectRegistry -> AttrTypeTable -> MlirOp -> BE.Encoder
encodeFuncOp isolated dialectReg attrTypeTable op =
    encodeOp isolated dialectReg attrTypeTable emptyValueEnv Dict.empty op


{-| Returns the number of values defined in a region's blocks: block arguments
and operation results, skipping operations in `body` whose `isTerminator` is
set, as numbering does. Values defined in nested regions are not counted.
-}
countRegionValues : MlirRegion -> Int
countRegionValues (MlirRegion r) =
    let
        countBlock blk =
            List.length blk.args
                + List.foldl (\op acc -> acc + List.length op.results)
                    0
                    (List.filter (\op -> not op.isTerminator) blk.body)
                + List.length blk.terminator.results
    in
    countBlock r.entry
        + (OrderedDict.toList r.blocks
            |> List.foldl (\( _, blk ) acc -> acc + countBlock blk) 0
          )
