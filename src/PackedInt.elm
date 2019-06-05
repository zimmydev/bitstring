module PackedInt exposing (PackedInt, combine, discard, getBit, msb, setBit, shiftLeftBy, shiftRightBy)

{-| THIS IS AN INTERNAL MODULE.

A `PackedInt` comprises a maximum of either 16 or 24 bits, depending on the
implementation. Currently, it is implemented as an integer packing 16 bits. This
could change, but luckily, it is a hidden implementation detail.

A `PackedInt`s size has a trade-off. Although using 16 bits per integer (or 24)
over 8 bits saves on the space taken of the internal representation dramatically
(especially for large data inputs), it does halve (or worse) the frequency of
_"easy appends."_ This is what I like to call the case when the left bitstring
argument to `Bitstring.append` is of a size which an even multiple of the size
of its internal `PackedInt` representation, which allows those `append`
operations to be a simple `Array.append` operation on the `PackedArray`, since
you won't need any shift operations.

Note that 32-bit `PackedInt`s are not seemingly possible at the time of writing,
possibly because of Javascript being elm's compilation target and js's single
floating-point number type. It seems that messing with bit 31 and 30 (the two
MSBs) has strange and undesired effects on the internal representation related
to signing/two's complement, perhaps. Only enough testing was done to conclude
that storing 32 bits has an undesired affect.

-}

import Bitstring.Index as Index exposing (Index)
import Bitwise



--- TYPES ---


{-| A 16-bit `Int`.
-}
type alias PackedInt =
    Int



--- TYPES FOR INTERNAL DOCUMENTATION ---


type alias Bit =
    Int



--- GETTING AND SETTING BITS ---


{-| Get the bit within a packed integer at the given index.
-}
getBit : Index -> PackedInt -> Bit
getBit i packedInt =
    let
        bitmask =
            msb 1 |> Bitwise.shiftRightZfBy (Index.bit i)
    in
    case Bitwise.and packedInt bitmask of
        0x00 ->
            0

        _ ->
            1


{-| Set the bit within a packed integer at the given index to the given bit. If
the index is out of range (i.e., over `15`), the integer is unaltered.
-}
setBit : Index -> Bit -> PackedInt -> PackedInt
setBit i bit int =
    let
        bitmask =
            msb bit |> Bitwise.shiftRightZfBy (Index.bit i)
    in
    Bitwise.or int bitmask
        |> discard



--- BITWISE OPERATIONS AND MASKING ---


{-| Generate a mask with only the left-most (most-significant) bit set or unset
depending on its `bit` argument. Used for creating a simple right-shiftable
mask.
-}
msb : Bit -> PackedInt
msb bit =
    case bit of
        0x00 ->
            -- 0b0000… (16 bits)
            0x00

        _ ->
            -- 0b1000… (16 bits)
            0x8000


{-| Mask an integer such that only its rightmost 16 bits are kept, while the
rest of the leftmost bits get discarded (i.e., set to `0`).
-}
discard : PackedInt -> PackedInt
discard int =
    int |> Bitwise.and 0xFFFF


{-| Shift left, filling with zeros
-}
shiftLeftBy : Int -> PackedInt -> PackedInt
shiftLeftBy n =
    Bitwise.shiftLeftBy n >> discard


{-| Shift right, filling with zeros.
-}
shiftRightBy : Int -> PackedInt -> PackedInt
shiftRightBy =
    Bitwise.shiftRightZfBy


{-| Performs a bitwise _or_ between the two packed integers.
-}
combine : PackedInt -> PackedInt -> PackedInt
combine =
    Bitwise.or
