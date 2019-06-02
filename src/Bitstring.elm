module Bitstring exposing
    ( Bitstring
    , Bit(..)
    , empty
    , initialize
    , repeat
    , and, append, complement, concat, foldl, foldr, fromBytes, fromList, get, indexedMap, isEmpty, map, or, padding, pop, push, size, sizeInBytes, slice, toBytes, toList, xor
    )

{-| A `Bitstring` is a [data structure](https://en.wikipedia.org/wiki/Bit_array) for dealing with data at the bit level; it is also commonly referred to as a _bit array_. It can be very useful when you need to manipulate binary data, but your data is not byte-shaped (e.g., compressed or encoded data tends to be shaped this way).

If you need to deal with data on the level of bytes, [this library](https://package.elm-lang.org/packages/elm/bytes/latest/) is probably more suited to your needs.


# Bitstring

@docs Bitstring
@docs Bit


# Creating a bitstring

@docs empty
@docs initialize
@docs repeat

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



--- TYPES FOR DOCUMENTATION ---


{-| The size (in bits) of the `Bitstring`. This happens to be the same as the index of the next bit to be written with a `push`. This type only appears in this library for documentation purposes.
-}
type alias Size =
    Int


{-| A `PackedInt` comprises two 8-bit integers packed into 16 bits. This type only appears in this library for documentation purposes.
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


initialize : Int -> (Int -> Bit) -> Bitstring
initialize _ _ =
    Debug.todo "initialize a bitstring with a closure"


repeat : Int -> Bit -> Bitstring
repeat _ _ =
    Debug.todo "initialize a bitstring with a constant bit"



--- ACCESSING PROPERTIES OF A BITSTRING ---


{-| Return the size of a bitstring, in terms of bits.

    size (fromList [ One, Zero, One ]) == 3

-}
size : Bitstring -> Int
size (Bitstring sz _) =
    sz


{-| Return the size of some bitstring, in terms of the minimum number of bytes that would be needed to represent the bitstring (padding the remaining space with zeros when necessary). For example, the `empty` bitstring is 0 bytes in size, but a 1-bit bitstring is 1 byte in size (with 7 zeros for padding).
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


{-| Return the size that the padding will be if the bitstring is converted to `Bytes`. Another way to think about padding is as the amount of bits you can push onto the end of a bitstring until it aligns with the next byte boundary. Note that the empty bitstring is considered aligned (i.e., the padding is 0).
-}
padding : Bitstring -> Int
padding bitstring =
    7 - (size bitstring - 1 |> modBy 8)



--- CONVERTING A BITSTRING ---


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


toBytes : Bitstring -> Bytes
toBytes _ =
    Debug.todo "convert from bits to bytes, padding with 0 where necessary"


fromList : List Bit -> Bitstring
fromList bitList =
    bitList
        |> List.foldl
            (\bit acc ->
                acc |> push bit
            )
            empty


toList : Bitstring -> List Bit
toList bitstring =
    List.range 0 (size bitstring - 1)
        |> List.map (\i -> bitstring |> get i |> Maybe.withDefault Zero)



--- GETTING AND SETTING BITS IN A BITSTRING ---


get : Int -> Bitstring -> Maybe Bit
get index (Bitstring sizeInBits array) =
    if index < 0 || index >= sizeInBits then
        Nothing

    else
        array
            |> getBitInArrayAt index


set : Int -> Bit -> Bitstring -> Bitstring
set index bit (Bitstring sizeInBits array) =
    array
        |> setBitInArrayAt index bit
        |> Bitstring sizeInBits



--- TRANSFORMING A BITSTRING ---


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


pop : Bitstring -> ( Maybe Bit, Bitstring )
pop _ =
    Debug.todo "pop a bit (might as well have a mirror of push)"


append : Bitstring -> Bitstring -> Bitstring
append _ _ =
    Debug.todo "append two Bitstring using"


concat : List Bitstring -> Bitstring
concat _ =
    Debug.todo "concat a list of bitstrings"


slice : Int -> Int -> Bitstring -> Bitstring
slice _ _ _ =
    Debug.todo "bits to a slice of bits"


map : (Bit -> Bit) -> Bitstring -> Bitstring
map _ _ =
    Debug.todo "map over a bitstring"


indexedMap : (Int -> Bit -> Bit) -> Bitstring -> Bitstring
indexedMap _ _ =
    Debug.todo "map over a bitstring with a bit index"


foldl : (Bit -> acc -> acc) -> acc -> Bitstring -> acc
foldl _ _ _ =
    Debug.todo "fold left over a bitstring"


foldr : (Bit -> acc -> acc) -> acc -> Bitstring -> acc
foldr _ _ _ =
    Debug.todo "fold right over a bitstring"



--- BITWISE OPERATIONS ---


complement : Bitstring -> Bitstring
complement _ =
    Debug.todo "invert the bits"


and : Bitstring -> Bitstring -> Bitstring
and _ _ =
    Debug.todo "bitwise and between 2 bitstrings"


or : Bitstring -> Bitstring -> Bitstring
or _ _ =
    Debug.todo "bitwise or between 2 bitstrings"


xor : Bitstring -> Bitstring -> Bitstring
xor _ _ =
    Debug.todo "bitwise xor between 2 bitstrings"



--- HELPER FUNCTIONS ---


{-| A 16-bit mask with only the left-most (most-significant) bit set or unset, depending on its argument. Used for creating all kinds of masks (e.g. or, and, xor).
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
