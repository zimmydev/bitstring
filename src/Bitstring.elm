module Bitstring exposing
    ( Bitstring, Bit(..)
    , empty, repeat, initialize
    , size, sizeInBytes, isEmpty, padding
    , fromBytes, toBytes, fromList, toList, fromString, toString
    , get, set
    , push, pop, append, concat, indexedMap
    , slice, left, right, dropLeft, dropRight
    , foldl, foldr
    , SizingMethod(..), defaultSizing, complement, and, or, xor
    )

{-| A `Bitstring` is a [data structure](https://en.wikipedia.org/wiki/Bit_array)
for dealing with data at the bit level; it is also commonly referred to as a
_bit array_. It can be very useful when you need to manipulate binary data, but
your data is not byte-shaped (e.g., compressed or encoded data tends to be
shaped this way).

If you need to deal with data on the level of bytes, the [`elm/bytes`](https://bit.ly/2wByQX8)
library is probably more suited to your needs.


# Bitstring

@docs Bitstring, Bit


# Creating a Bitstring

@docs empty, repeat, initialize


# Dimensions

@docs size, sizeInBytes, isEmpty, padding


# Converting

@docs fromBytes, toBytes, fromList, toList, fromString, toString


# Getting and setting bits

@docs get, set


# Transforming

@docs push, pop, append, concat, indexedMap


# Substrings

@docs slice, left, right, dropLeft, dropRight


# Reducing

@docs foldl, foldr


# Bitwise operations

@docs SizingMethod, defaultSizing, complement, and, or, xor

-}

import Array exposing (Array)
import Bytes exposing (Bytes)
import Bytes.Decode as BDecode exposing (Decoder, Step(..))
import Index as Index exposing (Index)
import Maybe
import PackedArray exposing (PackedArray)
import PackedInt exposing (PackedInt)
import Size exposing (Size)



--- TYPES ---


{-| `Bitstring` is a variable-length string of bits.
-}
type Bitstring
    = Bitstring Size PackedArray


{-| A `Bit` is either `One` or `Zero`.
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


{-| The default sizing method for bitwise operations, which is `PadRight Zero`.

This is basically what you would expect if you're building your bitstring up
from left to right, in which case you would expect the MSBs (most-significant
bits) to be aligned. I imagine that this is sensible behavior in _most_ cases.

This may not be the behavior you expect if your bitstring is a representation
of a binary number, in which case you might expect the LSBs (least-significant
bits) to be aligned.

-}
defaultSizing : SizingMethod
defaultSizing =
    PadRight Zero



--- CREATING A BITSTRING ---


{-| Return an empty bitstring.

    size empty
    --> 0

-}
empty : Bitstring
empty =
    Bitstring 0 Array.empty


{-| Initialize a bitstring using a default element. `repeat n bit` simply
returns a bitstring of size `n` with all bits set to `bit`. This results in a
bitstring with all bits either set or unset, which can be very useful for
creating bitmasks.

    repeat 4 One
    --> fromList [ One, One, One, One ]

-}
repeat : Int -> Bit -> Bitstring
repeat n bit =
    initialize n (always bit)


{-| Initalize a bitstring in a custom way. `initialize n f` creates a bitstring
of size `n` with the bit at each index being initialized using the function `f`,
which takes an index and produces a bit (`Int -> Bit`).

    pattern : Int -> Bit
    pattern i =
        if (i |> modBy 2) == 0 then One else Zero

    initialize 4 pattern
    --> fromList [One, Zero, One, Zero]

-}
initialize : Int -> (Int -> Bit) -> Bitstring
initialize n f =
    List.range 0 (n - 1)
        |> List.foldl
            (\i acc -> acc |> PackedArray.setBit i (f i |> bitToInt))
            (PackedArray.sizedFor n)
        |> Bitstring n



--- DIMENSIONS OF A BITSTRING ---


{-| Return the size of a bitstring, in terms of bits.

    size (fromList [ One, Zero, One ])
    --> 3

-}
size : Bitstring -> Int
size (Bitstring sizeInBits _) =
    sizeInBits


{-| Return the size of some bitstring, in terms of the minimum number of bytes
that would be needed to represent the it when converting from a `Bitstring` to
`Bytes` using `toBytes` (padding the remaining space with zeros when necessary).

    sizeInBytes empty --> 0

    sizeInBytes (repeat 2 Zero) --> 1

    sizeInBytes (repeat 8 Zero) --> 1

    sizeInBytes (repeat 9 Zero) --> 2

-}
sizeInBytes : Bitstring -> Int
sizeInBytes bitstring =
    size bitstring |> Size.by 8


{-| Determine if a bitstring is empty.

    isEmpty empty
    --> True

-}
isEmpty : Bitstring -> Bool
isEmpty bitstring =
    size bitstring == 0


{-| Return the size that the padding will be if the bitstring is converted to
bytes using `toBytes`. Another way to think about padding is as the amount of
bits that you can push onto the end of a bitstring until it aligns with the next
byte boundary.

    padding empty
    --> 0
    padding (repeat 2 One)
    --> 6
    padding (repeat 8 One)
    --> 0
    padding (repeat 9 One)
    --> 7

Notice that the empty bitstring is considered aligned (i.e., the padding is 0).

-}
padding : Bitstring -> Int
padding bitstring =
    size bitstring |> Size.paddingBy 8



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
        width =
            Bytes.width bytes
    in
    bytes
        |> BDecode.decode (PackedArray.decoder width)
        |> Maybe.withDefault Array.empty
        |> Bitstring (width * 8)


{-| Create some `Bytes` from a bitstring.

Note that converting to bytes will likely result in some extra `0` being added
to the end as padding. There will be no padding added in the event that your
bitstring is empty or its size is a multiple of 8 bits.

-}
toBytes : Bitstring -> Bytes
toBytes _ =
    Debug.todo "convert from bits to bytes, padding with 0 where necessary"


{-| Create a bitstring from a list of bits.

    fromList [ Zero, One, One, Zero ] |> size
    --> 4

-}
fromList : List Bit -> Bitstring
fromList list =
    let
        sizeInBits =
            List.length list
    in
    list
        |> List.map bitToInt
        |> List.foldl
            (\bit ( i, acc ) -> ( i + 1, acc |> PackedArray.setBit i bit ))
            ( 0, PackedArray.sizedFor sizeInBits )
        |> Tuple.second
        |> Bitstring sizeInBits


{-| Create a list of bits from a bitstring.

    toList (repeat 3 Zero)
    --> [ Zero, Zero, Zero ]

-}
toList : Bitstring -> List Bit
toList bitstring =
    List.range 0 (size bitstring - 1)
        |> List.map (\i -> bitstring |> get i |> Maybe.withDefault Zero)


{-| Create `Just` a bitstring from a string like "1001011", or `Nothing` if the
string contains anything but `'1'` or `'0'`.

    fromString "1001011"
    --> Just (fromList [One, Zero, Zero, One, Zero, One, One])
    fromString ""
    --> Just empty
    fromString "he110 w0r1d"
    --> Nothing

-}
fromString : String -> Maybe Bitstring
fromString string =
    let
        sizeInBits =
            String.length string
    in
    string
        |> String.foldl
            (\c ( i, acc ) ->
                case c of
                    '1' ->
                        ( i + 1, acc |> Maybe.map (PackedArray.setBit i 1) )

                    '0' ->
                        ( i + 1, acc |> Maybe.map (PackedArray.setBit i 0) )

                    _ ->
                        ( i + 1, Nothing )
            )
            ( 0, Just <| PackedArray.sizedFor sizeInBits )
        |> Tuple.second
        |> Maybe.map (Bitstring sizeInBits)


{-| Create a string representation of a bitstring.

    toString (repeat 5 One)
    --> "11111"
    toString (fromList [One, One, Zero])
    --> "110"
    toString empty
    --> ""

This is very useful mostly for debugging but could be used for anything that
involves displaying the bits. You can use `toString >> (++) "0b"` to create a
valid binary literal for many programming applications.

-}
toString : Bitstring -> String
toString (Bitstring sizeInBits array) =
    array
        |> PackedArray.fold
            (sizeInBits - 1)
            (\i -> i >= 0)
            decrement
            (\_ b acc ->
                if b == 0 then
                    String.cons '0' acc

                else
                    String.cons '1' acc
            )
            ""



--- GETTING AND SETTING BITS IN A BITSTRING ---


{-| Return `Just` the bit at the given index in the bitstring, or `Nothing` if
the index is out of range.

    fromList [ One, Zero, One, One ] |> get 1
    --> Zero

-}
get : Int -> Bitstring -> Maybe Bit
get i (Bitstring sizeInBits array) =
    if i < 0 || i >= sizeInBits then
        Nothing

    else
        array
            |> PackedArray.getBit i
            |> Maybe.map intToBit


{-| Set or unset the individual bit at the given index in the bitstring. If the
index is out of range, the bitstring is unaltered.

    repeat 5 One |> set 2 Zero
    --> fromList [ One, One, Zero, One, One ]

-}
set : Int -> Bit -> Bitstring -> Bitstring
set i bit (Bitstring sizeInBits array) =
    array
        |> PackedArray.setBit i (bitToInt bit)
        |> Bitstring sizeInBits



--- TRANSFORMING A BITSTRING ---


{-| Push a bit onto the end of a bitstring. This is probably the interface
you'll want to use if you're constructing bitstrings at runtime.

    empty |> push One |> push Zero |> push Zero
    --> fromList [ One, Zero, Zero ]

-}
push : Bit -> Bitstring -> Bitstring
push bit (Bitstring sizeInBits array) =
    if sizeInBits |> Size.isAlignedBy 16 then
        -- We're crossing a `PackedInt` "boundary", so we need to make room.
        array
            |> Array.push (PackedInt.msb (bitToInt bit))
            |> Bitstring (sizeInBits + 1)

    else
        array
            |> PackedArray.setBit sizeInBits (bitToInt bit)
            |> Bitstring (sizeInBits + 1)


{-| Pop a bit from the end of a bitstring. Return a tuple containing `Just` the
popped bit (or `Nothing` if the bitstring was empty) and the modified bitstring.

    pop (fromList [ One, One, Zero ])
    --> ( Just Zero, fromList [ One, One ] )

    pop empty
    --> ( Nothing, empty )

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

    fromList [ Zero, One ]
        |> append (fromList [ One, Zero, Zero ])
    --> fromList [ Zero, One, One, Zero, Zero ]

-}
append : Bitstring -> Bitstring -> Bitstring
append (Bitstring size2 array2) (Bitstring size1 array1) =
    let
        shiftAmount =
            size1 |> Size.paddingBy 16
    in
    if size1 == 0 then
        -- Short-circuit if bitstring1 is empty
        Bitstring size2 array2

    else if size2 == 0 then
        -- Short-circuit if bitstring2 is empty
        Bitstring size1 array1

    else if shiftAmount == 0 then
        -- There are bits, but no padding in bitstring1, so we can do a simple
        -- array append operation
        Array.append array1 array2
            |> Bitstring (size1 + size2)

    else
        -- Do some bitmasking after shifting part of the first `PackedInt` from
        -- bitstring2 to the last `PackedInt` of bitstring1
        let
            newLast =
                PackedArray.firstOrZero array2
                    |> PackedInt.shiftRightBy (16 - shiftAmount)
                    |> PackedInt.combine (PackedArray.lastOrZero array1)

            shiftedArray1 =
                array1 |> PackedArray.setLast newLast

            shiftedArray2 =
                array2 |> PackedArray.dropLeft shiftAmount size2
        in
        Array.append shiftedArray1 shiftedArray2
            |> Bitstring (size1 + size2)


{-| Concatenate a list of bitstrings into one.

    concat
        [ fromList [ One ]
        , fromList [ Zero, Zero ]
        , fromList [ One, Zero, Zero ]
        ]
    --> fromList [ One, Zero, Zero, One, Zero, Zero ]

-}
concat : List Bitstring -> Bitstring
concat =
    List.foldl (\bs acc -> acc |> append bs) empty


{-| Map over a bitstring, supplying an index argument to the mapping function.
Using this function, you could, for example, set or unset bits at specific
indices in a bitstring, without having to use `set` for each bit.
-}
indexedMap : (Int -> Bit -> Bit) -> Bitstring -> Bitstring
indexedMap f (Bitstring sizeInBits array) =
    array
        |> PackedArray.fold
            Index.zero
            (\i -> i < sizeInBits)
            increment
            (\i b acc ->
                acc |> PackedArray.setBit i (intToBit b |> f i |> bitToInt)
            )
            (PackedArray.sizedFor sizeInBits)
        |> Bitstring sizeInBits



--- TAKING SUBSTRINGS ---


{-| Take a substring of the bitstring using the given `start` and `end` indices.
Negative indices are calculated by subtracting backwards from the end of the
bitstring.

    fromList [ One, One, Zero, Zero, One ] |> slice 2 4
    --> fromList [ Zero, Zero ]

    -- The is how pop is defined
    fromList [ One, One, Zero, Zero, One ] |> slice 0 -1
    --> fromList [ One, One, Zero, Zero ]

-}
slice : Int -> Int -> Bitstring -> Bitstring
slice start end (Bitstring sizeInBits array) =
    let
        ( correctStart, correctEnd ) =
            ( start |> translateIndex sizeInBits
            , end |> translateIndex sizeInBits
            )

        newSizeInBits =
            correctEnd - correctStart |> clampPositive
    in
    array
        |> PackedArray.slice correctStart correctEnd
        |> Bitstring newSizeInBits


{-| Take the leftmost `n` bits of a bitstring.

    fromList [ One, One, Zero, Zero, One ] |> left 3
    --> fromList [ One, One, Zero ]

-}
left : Int -> Bitstring -> Bitstring
left n bitstring =
    bitstring
        |> dropRight (size bitstring - n)


{-| Take the rightmost `n` bits of a bitstring.

    fromList [ One, One, Zero, Zero, One ] |> right 3
    --> fromList [ Zero, Zero, One ]

-}
right : Int -> Bitstring -> Bitstring
right n bitstring =
    bitstring
        |> dropLeft (size bitstring - n)


{-| Drop `n` bits from the left side of a bitstring.

    fromList [ One, One, Zero, Zero, One ] |> dropLeft 3
    --> fromList [ Zero, One ]

-}
dropLeft : Int -> Bitstring -> Bitstring
dropLeft n (Bitstring sizeInBits array) =
    if n <= 0 then
        Bitstring sizeInBits array

    else
        array
            |> PackedArray.dropLeft n sizeInBits
            |> Bitstring (sizeInBits - n |> clampPositive)


{-| Drop `n` bits from the right side of a bitstring.

    fromList [ One, One, Zero, Zero, One ] |> dropRight 3
    --> fromList [ One, One ]

-}
dropRight : Int -> Bitstring -> Bitstring
dropRight n (Bitstring sizeInBits array) =
    if n <= 0 then
        Bitstring sizeInBits array

    else
        array
            |> PackedArray.dropRight n sizeInBits
            |> Bitstring (sizeInBits - n |> clampPositive)



--- REDUCING A BITSTRING ---


{-| Reduce a bitstring from the left. `foldl` is conventional nomenclature
meaning _fold from the left_.
-}
foldl : (Bit -> acc -> acc) -> acc -> Bitstring -> acc
foldl f acc (Bitstring sizeInBits array) =
    array
        |> PackedArray.fold
            Index.zero
            (\i -> i < sizeInBits)
            increment
            (\i b acc_ -> f (intToBit b) acc_)
            acc


{-| Reduce a bitstring from the right. `foldr` is conventional nomenclature
meaning _fold from the right_.
-}
foldr : (Bit -> acc -> acc) -> acc -> Bitstring -> acc
foldr f acc (Bitstring sizeInBits array) =
    array
        |> PackedArray.fold
            sizeInBits
            (\i -> i >= 0)
            decrement
            (\i b acc_ -> f (intToBit b) acc_)
            acc



--- BITWISE OPERATIONS ---


{-| Return a bitstring with each bit flipped.
-}
complement : Bitstring -> Bitstring
complement (Bitstring sizeInBits array) =
    array
        |> PackedArray.fold
            Index.zero
            (\i -> i < sizeInBits)
            increment
            (\i b acc -> acc |> PackedArray.setBit i (flip b))
            (PackedArray.sizedFor sizeInBits)
        |> Bitstring sizeInBits


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


{-| Given a bitstring's size, convert a relative index for that bitstring into
an absolute one.
-}
translateIndex : Size -> Index -> Index
translateIndex sizeInBits i =
    let
        adjustedIndex =
            if i < 0 then
                sizeInBits + i

            else
                i
    in
    adjustedIndex |> clamp 0 sizeInBits


bitToInt : Bit -> Int
bitToInt bit =
    case bit of
        Zero ->
            0x00

        One ->
            0x01


intToBit : Int -> Bit
intToBit int =
    case int of
        0x00 ->
            Zero

        _ ->
            One


flip : Int -> Int
flip bit =
    case bit of
        0 ->
            1

        _ ->
            0


increment : number -> number
increment x =
    x + 1


decrement : number -> number
decrement x =
    x - 1


clampPositive : number -> number
clampPositive =
    max 0
