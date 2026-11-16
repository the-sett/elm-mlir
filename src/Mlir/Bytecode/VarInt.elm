module Mlir.Bytecode.VarInt exposing (encodeVarInt, encodeSignedVarInt, varIntWidth)

{-| MLIR bytecode holds numbers such as counts, lengths and table indices in a
variable-length form called PrefixVarInt, and this module writes that form.

A _PrefixVarInt_ is a little-endian unsigned integer of one to nine bytes whose
first byte says how long it is: the number of trailing zero bits in the first
byte, plus one, is the total number of bytes. The bits above the lowest set bit
hold the low bits of the value, and the following bytes hold the rest. A first
byte of zero therefore means eight more bytes, which hold the whole value as a
64-bit integer.

    xxxxxxx1:  7 value bits, 1 byte
    xxxxxx10: 14 value bits, 2 bytes
    xxxxx100: 21 value bits, 3 bytes
    xxxx1000: 28 value bits, 4 bytes
    xxx10000: 35 value bits, 5 bytes
    xx100000: 42 value bits, 6 bytes
    x1000000: 49 value bits, 7 bytes
    10000000: 56 value bits, 8 bytes
    00000000: 64 value bits, 9 bytes

This module writes only the 1-, 2-, 3-, 4- and 9-byte forms. A short form is
made by shifting the value left beside its length bits, and under JavaScript
`Bitwise` works on 32 bits (`Mlir.Bytecode.Int64` describes the two hosts), so
28 value bits are the most such a shift can place. When `encodeVarInt` is given
a value at or above 2^28, it writes the 9-byte form instead, which needs no such
shift: its eight bytes are the word pair that `Mlir.Bytecode.Int64.toWords`
gives. A negative value given to `encodeVarInt` is written in the 9-byte form
too, as its 64-bit two's complement, so -1 is written as 2^64 − 1. Nothing is
rejected.

The 9-byte form is exact for a value from −2^63 to 2^63 − 1, the range over
which `toWords` is exact. Under JavaScript a whole number outside that range is
written as its low 64 bits.

_Zigzag encoding_ maps a signed integer to an unsigned one so that values of
small magnitude stay small: 0, -1, 1, -2, 2 become 0, 1, 2, 3, 4.
`encodeSignedVarInt` writes a value zigzag-encoded as a PrefixVarInt.

@docs encodeVarInt, encodeSignedVarInt, varIntWidth

-}

import Bitwise
import Bytes
import Bytes.Encode as BE
import Mlir.Bytecode.Int64 as Int64


{-| Creates an encoder that writes `value` as a PrefixVarInt.

A `value` from 0 to below 2^28 is written in the shortest of the 1-, 2-, 3-
and 4-byte forms that holds it, and one at or above 2^28 in the 9-byte form.

A negative `value` is not rejected. It is written in the 9-byte form as its
64-bit two's complement, that is, as the unsigned integer 2^64 + `value`, so
`-1` is written as 2^64 − 1. This holds down to −2^63; under JavaScript a whole
number outside the range −2^63 to 2^63 − 1 is written as its low 64 bits.

-}
encodeVarInt : Int -> BE.Encoder
encodeVarInt value =
    if value < 0 then
        encode9Bytes value

    else if value < 0x80 then
        BE.unsignedInt8 (Bitwise.or (Bitwise.shiftLeftBy 1 value) 1)

    else if value < 0x4000 then
        let
            tagged =
                Bitwise.or (Bitwise.shiftLeftBy 2 value) 2
        in
        BE.sequence
            [ BE.unsignedInt8 (Bitwise.and tagged 0xFF)
            , BE.unsignedInt8 (Bitwise.and (Bitwise.shiftRightZfBy 8 tagged) 0xFF)
            ]

    else if value < 0x00200000 then
        let
            tagged =
                Bitwise.or (Bitwise.shiftLeftBy 3 value) 4
        in
        BE.sequence
            [ BE.unsignedInt8 (Bitwise.and tagged 0xFF)
            , BE.unsignedInt8 (Bitwise.and (Bitwise.shiftRightZfBy 8 tagged) 0xFF)
            , BE.unsignedInt8 (Bitwise.and (Bitwise.shiftRightZfBy 16 tagged) 0xFF)
            ]

    else if value < 0x10000000 then
        let
            -- Under JavaScript this is negative from 2^27 up; only its bytes are used.
            tagged =
                Bitwise.or (Bitwise.shiftLeftBy 4 value) 8
        in
        BE.sequence
            [ BE.unsignedInt8 (Bitwise.and tagged 0xFF)
            , BE.unsignedInt8 (Bitwise.and (Bitwise.shiftRightZfBy 8 tagged) 0xFF)
            , BE.unsignedInt8 (Bitwise.and (Bitwise.shiftRightZfBy 16 tagged) 0xFF)
            , BE.unsignedInt8 (Bitwise.and (Bitwise.shiftRightZfBy 24 tagged) 0xFF)
            ]

    else
        encodeLargeVarInt value


{-| Returns the number of bytes `encodeVarInt value` writes: 1 to 4 for a
`value` from 0 to below 2^28, and 9 for one at or above 2^28 or negative.

It repeats the thresholds of `encodeVarInt` rather than sharing them, so the
two must be changed together.

-}
varIntWidth : Int -> Int
varIntWidth value =
    if value < 0 then
        9

    else if value < 0x80 then
        1

    else if value < 0x4000 then
        2

    else if value < 0x00200000 then
        3

    else if value < 0x10000000 then
        4

    else
        9


{-| Creates an encoder that writes `value` in the 9-byte form, exactly as
`encode9Bytes` does.
-}
encodeLargeVarInt : Int -> BE.Encoder
encodeLargeVarInt value =
    encode9Bytes value


{-| Creates an encoder that writes `value` in the 9-byte form: a zero byte, then
`value` as a 64-bit little-endian two's complement integer.

This is exact for a `value` from −2^63 to 2^63 − 1, which on a 64-bit host is
every `Int`. Under JavaScript a whole number outside that range, although the
host may hold it exactly, is written as its low 64 bits.

-}
encode9Bytes : Int -> BE.Encoder
encode9Bytes value =
    let
        ( hi, lo ) =
            Int64.toWords value
    in
    encodeWords9 hi lo


{-| Creates an encoder that writes the 9-byte form of the 64-bit value whose word
pair is `( hi, lo )`: a zero byte, then `lo` and `hi`, each as four
little-endian bytes.
-}
encodeWords9 : Int -> Int -> BE.Encoder
encodeWords9 hi lo =
    BE.sequence
        [ BE.unsignedInt8 0x00
        , BE.unsignedInt32 Bytes.LE lo
        , BE.unsignedInt32 Bytes.LE hi
        ]


{-| Creates an encoder that writes `value` zigzag-encoded as a PrefixVarInt. A
`value` from −2^63 to 2^63 − 1 becomes `2 * value` if it is zero or more and
`-2 * value - 1` if it is negative, a number from 0 to 2^64 − 1.

On either host, a `value` whose magnitude is below 2^52 is zigzag-encoded by
that arithmetic, which keeps the result below 2^53 and so exact under
JavaScript too. Any other `value` is written in the 9-byte form, computed from
its word pair.

-}
encodeSignedVarInt : Int -> BE.Encoder
encodeSignedVarInt value =
    if value >= 0 && value < 0x0010000000000000 then
        encodeVarInt (value * 2)

    else if value < 0 && value > -0x0010000000000000 then
        encodeVarInt (-2 * value - 1)

    else
        let
            ( hi, lo ) =
                Int64.toWords value

            shiftedHi : Int
            shiftedHi =
                Bitwise.or (Bitwise.shiftLeftBy 1 hi) (Bitwise.shiftRightZfBy 31 lo)

            shiftedLo : Int
            shiftedLo =
                Bitwise.shiftLeftBy 1 lo

            sign : Int
            sign =
                if value < 0 then
                    0xFFFFFFFF

                else
                    0
        in
        encodeWords9 (u32 (Bitwise.xor shiftedHi sign)) (u32 (Bitwise.xor shiftedLo sign))


{-| Returns the low 32 bits of `x` as a word, from 0 to 2^32 − 1, on either
host.
-}
u32 : Int -> Int
u32 x =
    Bitwise.shiftRightZfBy 0 (Bitwise.and x 0xFFFFFFFF)
