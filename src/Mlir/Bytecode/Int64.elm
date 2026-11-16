module Mlir.Bytecode.Int64 exposing (toWords)

{-| MLIR bytecode holds some integers in 64 bits, but an Elm `Int` is not 64 bits on every _host_,
the platform the Elm code runs on. This module splits an `Int` into the two 32-bit words of its
64-bit two's complement, giving the same words on every host.

Compiled to native code with a 64-bit `Int`, an `Int` is a signed 64-bit integer and `Bitwise`
works on 64 bits. Compiled to JavaScript, an `Int` is a double, exact only within ±2^53, and
`Bitwise` works on 32 bits. The widest unit both hosts' `Bitwise` can work on is a _word_, a
whole number from 0 to 2^32 − 1, so a 64-bit value is handled as a _word pair_ `( hi, lo )`: its
upper 32 bits and its lower 32 bits, each as a word.

Which host the code is on is found out when it runs, by trying a shift, and `toWords` takes a
different path on each. Under JavaScript, an integer beyond ±2^53 may already differ from the
number that was meant before it reaches this module, and splitting it cannot restore what was lost.

@docs toWords

-}

import Bitwise


{-| Whether the host's `Int` is a 64-bit integer rather than a JavaScript double.

JavaScript's shifts count modulo 32, so shifting 1 left by 32 gives 1 there. The test takes any
other answer to mean a 64-bit `Int`.

-}
hostIs64 : Bool
hostIs64 =
    Bitwise.shiftLeftBy 32 1 /= 1


{-| The weight of the high word: `hi` times this, plus `lo`, gives a word pair's 64 bits.
-}
two32 : Int
two32 =
    4294967296


{-| Returns the low 32 bits of `x` as a word, on either host.

Under JavaScript, `Bitwise.and` gives a signed 32-bit result, and the zero-fill shift by 0 turns it
into an unsigned one. On a 64-bit host the shift changes nothing.

-}
u32 : Int -> Int
u32 x =
    Bitwise.shiftRightZfBy 0 (Bitwise.and x 0xFFFFFFFF)


{-| Returns the 64-bit two's complement of `value` as a word pair `( hi, lo )`, `hi` holding the
upper 32 bits and `lo` the lower 32. For example, `-1` gives `( 0xFFFFFFFF, 0xFFFFFFFF )` and
2^32 gives `( 1, 0 )`.

This holds for every `value` from −2^63 to 2^63 − 1, which on a 64-bit host is every `Int`.

-}
toWords : Int -> ( Int, Int )
toWords value =
    if hostIs64 then
        ( u32 (Bitwise.shiftRightBy 32 value), u32 value )

    else
        let
            -- Exact on a double: power-of-two scaling, floor, and a difference below 2^32.
            hiSigned =
                floor (toFloat value / toFloat two32)
        in
        ( u32 hiSigned, value - hiSigned * two32 )
