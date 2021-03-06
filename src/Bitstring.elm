module Bitstring exposing
    ( Bitstring, Bit(..)
    , empty, repeat, initialize
    , size, sizeInBytes, isEmpty, padding, isAligned
    , fromBytes, toBytes, fromList, toList, fromString, toString
    , get, set
    , push, pop, append, concat, indexedMap
    , slice, left, right, dropLeft, dropRight
    , foldl, foldr
    , complement, and, or, xor
    , SizingMethod(..), defaultSizing, andBy, orBy, xorBy
    , equals
    )

{-| A `Bitstring` is a [data structure](https://en.wikipedia.org/wiki/Bit_array)
for dealing with data at the bit level; it is also commonly referred to as a
_bit array_. It can be very useful when you need to manipulate binary data, but
your data is not byte-shaped (e.g., compressed or encoded data tends to be
this way).

If you need to deal with data on the level of bytes, the [`elm/bytes`](https://bit.ly/2wByQX8)
library is probably more suited to your needs.


# Bitstring

@docs Bitstring, Bit


# Creating a Bitstring

@docs empty, repeat, initialize


# Dimensions

@docs size, sizeInBytes, isEmpty, padding, isAligned


# Converting

Bitstrings can by converted to and from `Bytes`, `List Bit`, and `String`.

@docs fromBytes, toBytes, fromList, toList, fromString, toString


# Getting and setting bits

You can `get` and `set` individual bits in a bitstring.

@docs get, set


# Transforming

Like with
[`Basics.Array`](https://package.elm-lang.org/packages/elm/core/latest/Array),
`push`, `append`, and `concat` are relatively expensive operations compared to
`get` and `set`. This means that these APIs really should only be used if you're
constructing bitstrings of indeterminate length at runtime. If the bitstring's
length can be known ahead of time, it's probably better to use `initialize` or a
similar interface that can allocate space for your bitstring ahead of time.

@docs push, pop, append, concat, indexedMap


# Substrings

@docs slice, left, right, dropLeft, dropRight


# Reducing

You can reduce a bitstring from the left or right side.

@docs foldl, foldr


# Bitwise operations

You can perform bitwise operations between bitstrings. By default, these
operations will truncate the right side of the larger bitstring to make it match
the length of the shorter bitstring. If you need different behavior, see the
next section: _Bitwise operations with different behavior_.

@docs complement, and, or, xor


# Bitwise operations with different behavior

These operations take an additional `SizingMethod` argument which determines
exactly how bitstrings of disparate sizes will be resized before the operation
is performed.

@docs SizingMethod, defaultSizing, andBy, orBy, xorBy


# Comparisons

@docs equals

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as BDecode exposing (Decoder, Step(..))
import Bytes.Encode as BEncode
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


{-| There are four different sizing methods for four different use-cases. Here
are examples of each method:

  - `TruncateRight` will truncate the right end of the longer bitstring to match
    the length of the shorter one.
  - `TruncateLeft` will truncate the left end of the longer bitstring to match
    the length of the shorter one.
  - `PadRight Zero` will pad the shorter bitstring with `0`s on the right to
    match the length of the longer one.
  - `PadLeft One` will pad the shorter bitstring with `1`s on the left to match
    the length of the longer one.

-}
type SizingMethod
    = TruncateRight
    | TruncateLeft
    | PadRight Bit
    | PadLeft Bit


{-| The default sizing method for bitwise operations, which is `TruncateRight`.

Why truncate? Generally, zipping operations between lists and list-like data
structures truncate the longer list to match the length of the shorter list.
This library chooses to mimic this kind of behavior by default.

Why right? This is basically what you would expect if you're building your
bitstring up from left to right, in which case you would expect the MSBs
(most-significant bits) to be aligned.

Note that this may not be the behavior you expect if your bitstring is a
representation of a binary integer and your bitwise operation is arithmetic in
its intention, in which case you might expect the LSBs (least-significant bits)
to be aligned.

-}
defaultSizing : SizingMethod
defaultSizing =
    TruncateRight



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

    sizeInBytes empty
    --> 0

    sizeInBytes (repeat 8 Zero)
    --> 1

    sizeInBytes (repeat 11 Zero)
    --> 2

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
bytes using `toBytes`. Another way to think about padding is as the number of
bits that you can push onto the end of a bitstring before it aligns with the
next byte boundary.

    padding empty
    --> 0

    padding (repeat 8 One)
    --> 0

    padding (repeat 11 One)
    --> 5

Notice that the empty bitstring is considered aligned (i.e., the padding is 0).

-}
padding : Bitstring -> Int
padding bitstring =
    size bitstring |> Size.paddingBy 8


{-| Determine if a bitstring is byte-aligned.

    isAligned empty
    --> True

    isAligned (repeat 8 One)
    --> True

    isAligned (repeat 11 One)
    --> False

This is really just shorthand for `padding bitstring == 0`, but is more clear in
its intention.

-}
isAligned : Bitstring -> Bool
isAligned bitstring =
    size bitstring |> Size.isAlignedBy 8



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
toBytes (Bitstring sizeInBits array) =
    let
        paddingAmount =
            sizeInBits |> Size.paddingBy 16

        shiftedLast =
            array
                |> PackedArray.lastOrZero
                |> PackedInt.shiftRightBy paddingAmount

        isLastIndex i =
            i == (Array.length array - 1)
    in
    array
        |> Array.indexedMap
            (\i int ->
                if isLastIndex i && paddingAmount >= 8 then
                    BEncode.unsignedInt8 shiftedLast

                else
                    BEncode.unsignedInt16 BE int
            )
        |> Array.toList
        |> BEncode.sequence
        |> BEncode.encode


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
            (\bit ( i, acc ) -> ( Index.inc i, acc |> PackedArray.setBit i bit ))
            ( Index.zero, PackedArray.sizedFor sizeInBits )
        |> Tuple.second
        |> Bitstring sizeInBits


{-| Create a list of bits from a bitstring.

    toList (repeat 3 Zero)
    --> [ Zero, Zero, Zero ]

-}
toList : Bitstring -> List Bit
toList bitstring =
    bitstring |> foldr (::) []


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
                        ( Index.inc i, acc |> Maybe.map (PackedArray.setBit i 1) )

                    '0' ->
                        ( Index.inc i, acc |> Maybe.map (PackedArray.setBit i 0) )

                    _ ->
                        ( Index.inc i, Nothing )
            )
            ( Index.zero, Just <| PackedArray.sizedFor sizeInBits )
        |> Tuple.second
        |> Maybe.map (Bitstring sizeInBits)


{-| Create a string representation of a bitstring. This **does not** elide any
leading zeroes.

    toString (repeat 5 One)
    --> "11111"

    toString (fromList [Zero, One, Zero])
    --> "010"

    toString empty
    --> ""

-}
toString : Bitstring -> String
toString bitstring =
    bitstring
        |> foldr
            (\b acc ->
                case b of
                    Zero ->
                        String.cons '0' acc

                    One ->
                        String.cons '1' acc
            )
            ""



--- GETTING AND SETTING BITS IN A BITSTRING ---


{-| Return `Just` the bit at the given index in the bitstring, or `Nothing` if
the index is out of range.

    fromList [ One, Zero, One, One ]
        |> get 1
    --> Just Zero

    fromList [ One, Zero, One, One ]
        |> get 15
    --> Nothing

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

    repeat 5 One
        |> set 2 Zero
    --> fromList [ One, One, Zero, One, One ]

-}
set : Int -> Bit -> Bitstring -> Bitstring
set i bit (Bitstring sizeInBits array) =
    array
        |> PackedArray.setBit i (bitToInt bit)
        |> Bitstring sizeInBits



--- TRANSFORMING A BITSTRING ---


{-| Push a bit onto the end of a bitstring.

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

    fromList [ Zero, One, One, Zero, Zero, One, Zero, Zero ]
        |> indexedMap (\i b -> if i < 4 then b else One)
    --> fromList [ Zero, One, One, Zero, One, One, One, One ]

-}
indexedMap : (Int -> Bit -> Bit) -> Bitstring -> Bitstring
indexedMap f (Bitstring sizeInBits array) =
    array
        |> PackedArray.fold
            Index.zero
            (\i -> i < sizeInBits)
            Index.inc
            (\i b acc ->
                acc |> PackedArray.setBit i (intToBit b |> f i |> bitToInt)
            )
            (PackedArray.sizedFor sizeInBits)
        |> Bitstring sizeInBits



--- TAKING SUBSTRINGS ---


{-| Take a substring of the bitstring using the given `start` and `end` indices.
Negative indices are calculated by subtracting backwards from the end of the
bitstring.

    fromList [ One, One, Zero, Zero, One ]
        |> slice 2 4
    --> fromList [ Zero, Zero ]

    -- The is how pop is defined
    fromList [ One, One, Zero, Zero, One ]
        |> slice 0 -1
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

    fromList [ One, One, Zero, Zero, One ]
        |> left 3
    --> fromList [ One, One, Zero ]

-}
left : Int -> Bitstring -> Bitstring
left n bitstring =
    bitstring
        |> dropRight (size bitstring - n)


{-| Take the rightmost `n` bits of a bitstring.

    fromList [ One, One, Zero, Zero, One ]
        |> right 3
    --> fromList [ Zero, Zero, One ]

-}
right : Int -> Bitstring -> Bitstring
right n bitstring =
    bitstring
        |> dropLeft (size bitstring - n)


{-| Drop `n` bits from the left side of a bitstring.

    fromList [ One, One, Zero, Zero, One ]
        |> dropLeft 3
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

    fromList [ One, One, Zero, Zero, One ]
        |> dropRight 3
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


{-| Fold a bitstring from the left.

    -- This fold operation counts the number of `1`s in the bitstring.
    fromList [ Zero, One, One, Zero, Zero, One, Zero, Zero ]
        |> foldl (\b acc -> if b == One then acc + 1 else acc) 0
    --> 3

-}
foldl : (Bit -> acc -> acc) -> acc -> Bitstring -> acc
foldl f acc (Bitstring sizeInBits array) =
    array
        |> PackedArray.fold
            Index.zero
            (\i -> i < sizeInBits)
            Index.inc
            (\i b acc_ -> f (intToBit b) acc_)
            acc


{-| Fold a bitstring from the right.
-}
foldr : (Bit -> acc -> acc) -> acc -> Bitstring -> acc
foldr f acc (Bitstring sizeInBits array) =
    array
        |> PackedArray.fold
            (sizeInBits - 1)
            (\i -> i >= 0)
            Index.dec
            (\i b acc_ -> f (intToBit b) acc_)
            acc



--- BITWISE OPERATIONS ---


{-| Return a bitstring with each bit flipped.
-}
complement : Bitstring -> Bitstring
complement (Bitstring sizeInBits array) =
    -- TODO We could accomplish this less verbosely with a PackedArray.map
    array
        |> PackedArray.map sizeInBits flip
        |> Bitstring sizeInBits


{-| Perform a bitwise _and_ between two bitstrings using the `defaultSizing`.
-}
and : Bitstring -> Bitstring -> Bitstring
and =
    andBy defaultSizing


{-| Perform a bitwise _or_ between two bitstrings using the `defaultSizing`.
-}
or : Bitstring -> Bitstring -> Bitstring
or =
    orBy defaultSizing


{-| Perform a bitwise _exclusive-or_ between two bitstrings using the
`defaultSizing`.
-}
xor : Bitstring -> Bitstring -> Bitstring
xor =
    xorBy defaultSizing


{-| Perform a bitwise _and_ between two bitstrings by using the given
`SizingMethod`.
-}
andBy : SizingMethod -> Bitstring -> Bitstring -> Bitstring
andBy method bitstring2 bitstring1 =
    bitstring1 |> bitwiseBy method Bitwise.and bitstring2


{-| Perform a bitwise _or_ between two bitstrings by using the given
`SizingMethod`.
-}
orBy : SizingMethod -> Bitstring -> Bitstring -> Bitstring
orBy method bitstring2 bitstring1 =
    bitstring1 |> bitwiseBy method Bitwise.or bitstring2


{-| Perform a bitwise _exclusive-or_ between two bitstrings by using the given
`SizingMethod`.
-}
xorBy : SizingMethod -> Bitstring -> Bitstring -> Bitstring
xorBy method bitstring2 bitstring1 =
    bitstring1 |> bitwiseBy method Bitwise.xor bitstring2


bitwiseBy :
    SizingMethod
    -> (PackedInt -> PackedInt -> PackedInt)
    -> Bitstring
    -> Bitstring
    -> Bitstring
bitwiseBy method logic (Bitstring size2 array2) (Bitstring size1 array1) =
    Debug.todo "bitwise logic"



--- COMPARING BITSTRINGS ---
{- TODO It may be possible to do an extremely simple `compare` function, if
   the ordering of the packed-ints corresponds to exactly what you would expect
   for bit-ordering (I have an feeling it is). I'll need to do a little testing.
-}


{-| Determine if two bitstrings are equal.
-}
equals : Bitstring -> Bitstring -> Bool
equals (Bitstring size2 array2) (Bitstring size1 array1) =
    -- This short-circuit is probably already implemented in array-equality, but
    -- oh well; better safe than sorry (;
    size1 == size2 && array2 == array1



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
    adjustedIndex |> clamp Index.zero sizeInBits


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


clampPositive : number -> number
clampPositive =
    max 0
