module Mlir.Bytecode.Section exposing (encodeSection, sectionId)

{-| After its header, an MLIR bytecode file is made of sections, and a section
can also sit inside the contents of another. This module writes the frame that
goes around a section.

A _section_ is one part of the file with a single job, such as holding the
string table or the operations. It starts with a byte saying which kind of
section it is, its _id_, followed by the length of its contents in bytes as a
PrefixVarInt (`Mlir.Bytecode.VarInt` describes that form), so that a reader can
find where this section's contents end without decoding them. The ids are fixed
by MLIR's bytecode format, and `sectionId` lists them.

The format also lets a section ask for its contents to be aligned: the high bit
of the id byte then says that an alignment value and padding bytes follow the
length. `encodeSection` writes the low 8 bits of the id it is given as the id
byte and puts nothing between the length and the contents. So with an id from
0 to 127, which every id in `sectionId` is, the section is unaligned and is
exactly the id byte, the length and the contents.

@docs encodeSection, sectionId

-}

import Bytes
import Bytes.Encode as BE
import Mlir.Bytecode.VarInt exposing (encodeVarInt)


{-| The id of each kind of section, as MLIR's bytecode format numbers them.
-}
sectionId :
    { string : Int
    , dialect : Int
    , attrType : Int
    , attrTypeOffset : Int
    , ir : Int
    , resource : Int
    , resourceOffset : Int
    , dialectVersions : Int
    , properties : Int
    }
sectionId =
    { string = 0
    , dialect = 1
    , attrType = 2
    , attrTypeOffset = 3
    , ir = 4
    , resource = 5
    , resourceOffset = 6
    , dialectVersions = 7
    , properties = 8
    }


{-| Creates an encoder that writes `contentEncoder` as a section with the id
`id`, for an `id` from 0 to 127: the id byte, the length of the contents in
bytes as a PrefixVarInt, then the contents.

The contents are encoded when this encoder is created, to measure their
length, and the resulting bytes are what the section carries, so
`contentEncoder` is run once.

-}
encodeSection : Int -> BE.Encoder -> BE.Encoder
encodeSection id contentEncoder =
    let
        contentBytes =
            BE.encode contentEncoder

        contentLen =
            Bytes.width contentBytes
    in
    BE.sequence
        [ BE.unsignedInt8 id
        , encodeVarInt contentLen
        , BE.bytes contentBytes
        ]
