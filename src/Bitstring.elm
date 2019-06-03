module Bitstring exposing
    ( Bitstring
    , Bit(..)
    , empty
    , repeat
    , initialize
    , SizingMethod(..), and, append, complement, concat, defaultSizing, dropLeft, dropRight, foldl, foldr, fromBytes, fromList, get, indexedMap, isEmpty, left, or, padding, pop, push, right, size, sizeInBytes, slice, toBytes, toList, xor
    )

{-| A `Bitstring` is a [data structure](https://en.wikipedia.org/wiki/Bit_array)
for dealing with data at the bit level; it is also commonly referred to as a
_bit array_. It can be very useful when you need to manipulate binary data, but
your data is not byte-shaped (e.g., compressed or encoded data tends to be
shaped this way).

If you need to deal with data on the level of bytes, [this library](https://package.elm-lang.org/packages/elm/bytes/latest/) is probably more suited to your needs.


# Bitstring

@docs Bitstring
@docs Bit


# Creating a Bitstring

@docs empty
@docs repeat
@docs initialize

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as BDecode exposing (Decoder, Step(..))
import Maybe



--- TYPES ---


{-| `Bitstring` is a variable-length string of bits.
-}
type Bitstring
    = Bitstring Size (Array PackedInt)


{-| A `Bit` (_bit_ being short for _binary digit_) is either `One` or `Zero`.
-}
type Bit
    = One
    | Zero


{-| A `SizingMethod` is needed to determine how bitstrings should be resized
before performing a bitwise operation, in the event that the sizes are
mismatched.

Here are some examples:

  - `PadRight Zero` will pad the shorter bitstring with `0`'s on the right to
    match the length of the longer one.
  - `PadLeft One` will pad the shorter bitstring with `1`'s on the left to
    match the length of the longer one.
  - `TruncateRight` will truncate the right end of the longer bitstring to match
    the length of the shorter one.
  - `TruncateLeft` will truncate the left end of the longer bitstring to match
    the length of the shorter one.

-}
type SizingMethod
    = PadRight Bit
    | PadLeft Bit
    | TruncateLeft
    | TruncateRight


{-| The default sizing behavior for bitwise operations, which is `PadRight Zero`.

This is basically what you would expect if you're building your bitstring up
from left to right, in which case you would expect the MSBs (most-significant
bits) to be aligned. I imagine this is desirable and the most straightforward
in _most_ cases.

This may not be what you would expect if your bitstring is a representation
of a binary number, in which case you would expect the LSBs (least-significant
bits) to be aligned.

-}
defaultSizing : SizingMethod
defaultSizing =
    PadRight Zero



--- TYPES FOR DOCUMENTATION ---


{-| The size (in bits) of the `Bitstring`. This happens to be the same as the
index of the next bit to be written with a `push`. This type only appears in
this library for documentation purposes.
-}
type alias Size =
    Int


{-| A `PackedInt` comprises two 8-bit integers packed into 16 bits. This type
only appears in this library for documentation purposes.
-}
type alias PackedInt =
    Int



--- CREATING A BITSTRING ---


{-| Return an empty bitstring.

    size empty == 0

-}
empty : Bitstring
empty =
    Bitstring 0 Array.empty


{-| Initialize a bitstring using a default element. `repeat n bit` simply
returns a bitstring of size `n` with all bits set to `bit`. This results in a
bitstring with all bits either set or unset, which can be very useful for
creating bitmasks.

    repeat 4 One == fromList [ One, One, One, One ]

-}
repeat : Int -> Bit -> Bitstring
repeat n bit =
    initialize n (always bit)


{-| Initalize a bitstring in a custom way. `initialize n f` creates a bitstring
of size `n` with the bit at each index being initialized using the function `f`,
which takes an index and produces a bit (`Int -> Bit`).

    pattern : Int -> Bit
    pattern i =
        if i |> modBy 2 == 0 then One else Zero

    initialize 4 pattern == fromList [One, Zero, One, Zero]

-}
initialize : Int -> (Int -> Bit) -> Bitstring
initialize _ _ =
    Debug.todo "initialize a bitstring with a closure"



--- ACCESSING PROPERTIES OF A BITSTRING ---


{-| Return the size of a bitstring, in terms of bits.

    size (fromList [ One, Zero, One ]) == 3

-}
size : Bitstring -> Int
size (Bitstring sz _) =
    sz


{-| Return the size of some bitstring, in terms of the minimum number of bytes
that would be needed to represent the it when converting from a `Bitstring` to
`Bytes` using `toBytes` (padding the remaining space with zeros when necessary).

    sizeInBytes empty == 0

    sizeInBytes (repeat 2 Zero) == 1

    sizeInBytes (repeat 8 Zero) == 1

    sizeInBytes (repeat 9 Zero) == 2

-}
sizeInBytes : Bitstring -> Int
sizeInBytes (Bitstring sz _) =
    if sz > 0 then
        (sz - 1) // 8 + 1

    else
        0


{-| Determine if a bitstring is empty.

    isEmpty empty == True

-}
isEmpty : Bitstring -> Bool
isEmpty bits =
    size bits == 0


{-| Return the size that the padding will be if the bitstring is converted to
bytes using `toBytes`. Another way to think about padding is as the amount of
bits that you can push onto the end of a bitstring until it aligns with the next
byte boundary.

    padding empty == 0

    padding (repeat 2 One) == 6

    padding (repeat 8 One) == 0

    padding (repeat 9 One) == 7

Notice that the empty bitstring is considered aligned (i.e., the padding is 0).

-}
padding : Bitstring -> Int
padding bitstring =
    size bitstring |> paddingBy 8



--- CONVERTING A BITSTRING ---


{-| Create a bitstring from some `Bytes`.

Note that if these are bytes that you've created using `toBytes`, your bitstring
will initially contain the padding that was initially encoded, if any. If you
need to delete unwanted padding from the end of a bitstring, use `left` or
`dropRight`.

Also note that the decoding method ignores any endianness in the data. Your
bytes are read one-by-one left-to-right and the bitstring is constructed in the
exact same order.

-}
fromBytes : Bytes -> Bitstring
fromBytes bytes =
    let
        sz =
            Bytes.width bytes
    in
    bytes
        |> BDecode.decode (packedInt16ArrayDecoder sz)
        |> Maybe.withDefault Array.empty
        |> Bitstring (sz * 8)


{-| Create some `Bytes` from a bitstring.

Note that converting to bytes will likely result in some extra `0` being added
to the end as padding. There will be no padding added in the event that your
bitstring is empty or its size is a multiple of 8 bits.

-}
toBytes : Bitstring -> Bytes
toBytes _ =
    Debug.todo "convert from bits to bytes, padding with 0 where necessary"


{-| Create a bitstring from a list of bits.

    size (fromList [ Zero, One, One, Zero ]) == 4

-}
fromList : List Bit -> Bitstring
fromList bitList =
    bitList
        |> List.foldl
            (\bit acc ->
                acc |> push bit
            )
            empty


{-| Create a list of bits from a bitstring.

    toList (repeat 3 Zero) == [ Zero, Zero, Zero ]

-}
toList : Bitstring -> List Bit
toList bitstring =
    List.range 0 (size bitstring - 1)
        |> List.map (\i -> bitstring |> get i |> Maybe.withDefault Zero)



--- GETTING AND SETTING BITS IN A BITSTRING ---


{-| Return `Just` the bit at the given index in the bitstring, or `Nothing` if
the index is out of range.

    get 1 (fromList [ One, Zero, One, One ]) == Zero

-}
get : Int -> Bitstring -> Maybe Bit
get index (Bitstring sizeInBits array) =
    if index < 0 || index >= sizeInBits then
        Nothing

    else
        array
            |> getBitInArrayAt index


{-| Set or unset the individual bit at the given index in the bitstring. If the
index is out of range, the bitstring is unaltered.

    set 2 (repeat 5 One) == fromList [ One, One, Zero, One, One ]

-}
set : Int -> Bit -> Bitstring -> Bitstring
set index bit (Bitstring sizeInBits array) =
    array
        |> setBitInArrayAt index bit
        |> Bitstring sizeInBits



--- GETTING SUB-BITSTRINGS ---


{-| Take a substring of the bitstring using the given `start` and `end` indices.
Negative indices are calculated by subtracting backwards from the end of the
bitstring.

    slice 2 4 (fromList [ One, One, Zero, Zero ]) == fromList [ Zero, Zero ]

    slice 1 -1 (fromList [ One, One, Zero, Zero ]) == fromList [ One, Zero ]

-}
slice : Int -> Int -> Bitstring -> Bitstring
slice _ _ _ =
    Debug.todo "bits to a slice of bits"


{-| Take the leftmost `n` bits of a bitstring.
-}
left : Int -> Bitstring -> Bitstring
left _ _ =
    Debug.todo "grab the left n bits (can be written in terms of dropRight)"


{-| Take the rightmost `n` bits of a bitstring.
-}
right : Int -> Bitstring -> Bitstring
right _ _ =
    Debug.todo "grab the right n bits (can be written in terms of dropLeft)"


{-| Drop `n` bits from the left side of a bitstring.
-}
dropLeft : Int -> Bitstring -> Bitstring
dropLeft _ _ =
    Debug.todo "drop the left n bits"


{-| Drop `n` bits from the right side of a bitstring.
-}
dropRight : Int -> Bitstring -> Bitstring
dropRight _ _ =
    Debug.todo "drop the right n bits"



--- TRANSFORMING A BITSTRING ---


{-| Push a bit onto the end of a bitstring. This is probably the interface
you'll want to use if you're constructing bitstrings at runtime.

    (empty |> push One |> push Zero) == fromList [ One, Zero ]

-}
push : Bit -> Bitstring -> Bitstring
push bit (Bitstring index array) =
    let
        nextIndex =
            index + 1
    in
    if index == 0 || (index - 1) // 16 < (nextIndex - 1) // 16 then
        -- We're crossing a 16-bit "boundary", so we need to make room.
        array
            |> Array.push (msbMask bit)
            |> Bitstring nextIndex

    else
        array
            |> setBitInArrayAt index bit
            |> Bitstring nextIndex


{-| Pop a bit from the end of a bitstring. Return a tuple containing `Just` the
popped bit (or `Nothing` if the bitstring was empty) and the modified bitstring.

    pop (fromList [ One, One, Zero ]) == ( Just Zero, fromList [ One, One ] )

    pop empty == ( Nothing, empty )

-}
pop : Bitstring -> ( Maybe Bit, Bitstring )
pop bitstring =
    case bitstring |> get (size bitstring - 1) of
        Nothing ->
            ( Nothing, bitstring )

        Just bit ->
            ( Just bit, bitstring |> slice 0 -1 )


{-| Append two bitstrings together.

Note that the first argument to the function is the bitstring to be appended
onto the _end_. This may seem backwards, but it is to facilitate appending in a
pipeline.

    (fromList [ Zero, One ] |> append (fromList [ One, One ])) == fromList [ Zero, One, One, One ]

-}
append : Bitstring -> Bitstring -> Bitstring
append _ _ =
    Debug.todo "append two Bitstring"


{-| Concatenate a list of bitstrings into one.

    concat [ fromList [ One ], fromList [ Zero, Zero ], fromList [ One ] ] == fromList [ One, Zero, Zero, One ]

-}
concat : List Bitstring -> Bitstring
concat _ =
    Debug.todo "concat a list of bitstrings"


{-| Map over a bitstring, supplying an index argument to the mapping function.
Using this function, you could, for example, set or unset bits at specific
indices in a bitstring, without having to use `set` for each bit.
-}
indexedMap : (Int -> Bit -> Bit) -> Bitstring -> Bitstring
indexedMap _ _ =
    Debug.todo "map over a bitstring with a bit index"


{-| Reduce a bitstring from the left. `foldl` is conventional nomenclature
meaning _fold from the left_.
-}
foldl : (Bit -> acc -> acc) -> acc -> Bitstring -> acc
foldl _ _ _ =
    Debug.todo "fold left over a bitstring"


{-| Reduce a bitstring from the right. `foldr` is conventional nomenclature
meaning _fold from the right_.
-}
foldr : (Bit -> acc -> acc) -> acc -> Bitstring -> acc
foldr _ _ _ =
    Debug.todo "fold right over a bitstring"



--- BITWISE OPERATIONS ---


{-| Return a bitstring with each bit flipped.
-}
complement : Bitstring -> Bitstring
complement _ =
    Debug.todo "invert the bits"


{-| Perform a bitwise _and_ operation between two bitstrings.

Note that a `SizingMethod` is required to perform this and all other bitwise
binary operations. See the relevant documentation for `SizingMethod` to get a
better idea of what this means.

-}
and : SizingMethod -> Bitstring -> Bitstring -> Bitstring
and _ _ =
    Debug.todo "bitwise and between 2 bitstrings"


{-| Perform a bitwise _or_ operation between two bitstrings.
-}
or : SizingMethod -> Bitstring -> Bitstring -> Bitstring
or _ _ =
    Debug.todo "bitwise or between 2 bitstrings"


{-| Perform a bitwise _exclusive-or_ operation between two bitstrings.
-}
xor : SizingMethod -> Bitstring -> Bitstring -> Bitstring
xor _ _ =
    Debug.todo "bitwise xor between 2 bitstrings"



--- HELPER FUNCTIONS ---


paddingBy : Int -> Int -> Int
paddingBy width sz =
    (width - 1) - (sz - 1 |> modBy width)


{-| A 16-bit mask with only the left-most (most-significant) bit set or unset,
depending on its argument. Used for creating all kinds of masks (e.g. and, or,
xor).
-}
msbMask : Bit -> PackedInt
msbMask bit =
    case bit of
        One ->
            -- 0b1000… (16 bits)
            0x8000

        Zero ->
            -- 0b0000… (16 bits)
            0x00


getBitAt : Int -> PackedInt -> Bit
getBitAt globalOffset int =
    let
        bitOffset =
            globalOffset |> modBy 16

        bitmask =
            msbMask One |> Bitwise.shiftRightZfBy bitOffset
    in
    case Bitwise.and int bitmask of
        0x00 ->
            Zero

        _ ->
            One


getIntInArrayAt : Int -> Array PackedInt -> Maybe PackedInt
getIntInArrayAt globalOffset array =
    let
        intOffset =
            globalOffset // 16
    in
    array
        |> Array.get intOffset


getBitInArrayAt : Int -> Array PackedInt -> Maybe Bit
getBitInArrayAt globalOffset array =
    array
        |> getIntInArrayAt globalOffset
        |> Maybe.map (getBitAt globalOffset)


setBitAt : Int -> Bit -> PackedInt -> PackedInt
setBitAt globalOffset bit int =
    let
        bitOffset =
            globalOffset |> modBy 16

        bitmask =
            msbMask bit |> Bitwise.shiftRightZfBy bitOffset
    in
    Bitwise.or int bitmask


setIntInArrayAt : Int -> PackedInt -> Array PackedInt -> Array PackedInt
setIntInArrayAt globalOffset int array =
    let
        intOffset =
            globalOffset // 16
    in
    array
        |> Array.set intOffset int


setBitInArrayAt : Int -> Bit -> Array PackedInt -> Array PackedInt
setBitInArrayAt globalOffset bit array =
    case array |> getIntInArrayAt globalOffset of
        Nothing ->
            array

        Just int ->
            array
                |> setIntInArrayAt globalOffset (int |> setBitAt globalOffset bit)


packedInt16ArrayDecoder : Int -> Decoder (Array PackedInt)
packedInt16ArrayDecoder sz =
    BDecode.loop
        ( 0, Array.initialize ((sz + 1) // 2) (always 0) )
        (\( n, acc ) ->
            if n >= sz then
                BDecode.succeed (Done acc)

            else
                BDecode.unsignedInt8
                    |> BDecode.map
                        (\x ->
                            let
                                arrayIndex =
                                    n // 2

                                nextInt16 =
                                    if (n |> modBy 2) == 0 then
                                        x |> Bitwise.shiftLeftBy 8

                                    else
                                        acc
                                            |> Array.get arrayIndex
                                            |> Maybe.withDefault 0x00
                                            |> Bitwise.or x
                            in
                            Loop ( n + 1, acc |> Array.set arrayIndex nextInt16 )
                        )
        )
