module PackedArray exposing (PackedArray, decoder, dropLeft, firstOrZero, getBit, lastOrZero, setBit, setFirst, setLast, sizedFor)

import Array exposing (Array)
import Bitstring.Index as Index exposing (Index)
import Bitstring.Size as Size exposing (Size)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as BDecode exposing (Decoder, Step(..))
import PackedInt exposing (PackedInt)



--- TYPES ---


{-| A `PackedArray` is just an array of `PackedInt`s. It is most of
`Bitstring`'s internal representation.
-}
type alias PackedArray =
    Array PackedInt



--- TYPES FOR INTERNAL DOCUMENTATION ---


type alias Bit =
    Int


type alias ArrayIndex =
    Int



--- OBTAINING A PACKED-ARRAY ---


{-| Decode bytes into bitstring's internal representation; i.e., an array of
`PackedInt`s encoding multiple bytes each of the input bytes. If the size of the
input is not an even integer, the last `PackedInt` will contain 8 bits worth of
zero-padding on its right-hand (least-significant) side.
-}
decoder : Int -> Decoder PackedArray
decoder byteSize =
    BDecode.loop
        ( 0, sizedFor (byteSize * 8) )
        (\( n, acc ) ->
            if n >= byteSize then
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


{-| Create a blank packed array that is sized large enough to bit the given
number of bits.
-}
sizedFor : Size -> PackedArray
sizedFor size =
    Array.repeat (size |> Size.by 16) 0x00



--- GETTING INDIVIDUAL BITS IN A PACKED-ARRAY ---


{-| Get `Just` the bit in the array at the given index
-}
getBit : Index -> PackedArray -> Maybe Bit
getBit i array =
    array
        |> getPackedInt i
        |> Maybe.map (PackedInt.getBit i)


{-| Get `Just` the `PackedInt` in a `PackedInt` array which holds the bit at
the given index, or `Nothing` if the index is out of range.
-}
getPackedInt : Index -> PackedArray -> Maybe PackedInt
getPackedInt i array =
    array |> Array.get (Index.array i)


{-| Set the bit in a packed array at the given index.
-}
setBit : Index -> Bit -> PackedArray -> PackedArray
setBit i bit array =
    case array |> getPackedInt i of
        Nothing ->
            array

        Just int16 ->
            array
                |> setPackedInt i (int16 |> PackedInt.setBit i bit)


{-| Set the `PackedInt` in the array which holds the bit at the given index
to another `PackedInt`. If the index is out of range, the array is unaltered.
-}
setPackedInt : Index -> PackedInt -> PackedArray -> PackedArray
setPackedInt i int array =
    array |> Array.set (Index.array i) int



--- GETTING INDIVIDUAL PACKED-INTS ---


firstOrZero : PackedArray -> PackedInt
firstOrZero pack =
    pack
        |> Array.get 0
        |> Maybe.withDefault 0x00


lastOrZero : PackedArray -> PackedInt
lastOrZero pack =
    pack
        |> Array.get (Array.length pack - 1)
        |> Maybe.withDefault 0x00



--- TRANSFORMING A PACKED-ARRAY ---


setFirst : PackedInt -> PackedArray -> PackedArray
setFirst =
    Array.set 0


setLast : PackedInt -> PackedArray -> PackedArray
setLast packedInt array =
    array |> Array.set (Array.length array - 1) packedInt


{-| Shift all the bits in the array an `[0-15]` times to the left, filling in
the right side with zeroes.
-}
dropLeft : Size -> Size -> PackedArray -> PackedArray
dropLeft n size pack =
    let
        actualN =
            n |> modBy 16
    in
    pack
        |> dropLeftHelper
            actualN
            Index.origin
            (sizedFor (size - n))


dropLeftHelper : Size -> ArrayIndex -> PackedArray -> PackedArray -> PackedArray
dropLeftHelper n i acc pack =
    case ( pack |> Array.get i, pack |> Array.get (i + 1) ) of
        ( Nothing, _ ) ->
            acc

        ( Just int, Nothing ) ->
            acc |> Array.set i (int |> PackedInt.shiftLeftBy n)

        ( Just int1, Just int2 ) ->
            let
                ( shifted1, shifted2 ) =
                    ( int1 |> PackedInt.shiftLeftBy n
                    , int2 |> PackedInt.shiftRightBy (16 - n)
                    )

                repack =
                    acc |> Array.set i (shifted1 |> PackedInt.combine shifted2)
            in
            dropLeftHelper n (i + 1) repack pack
