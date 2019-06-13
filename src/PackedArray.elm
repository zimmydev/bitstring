module PackedArray exposing (PackedArray, decoder, dropLeft, dropRight, firstOrZero, fold, getBit, index, lastOrZero, map, setBit, setFirst, setLast, sizedFor, slice)

{-| THIS IS AN INTERNAL MODULE.

A `PackedArray` is just an array of `PackedInt`s.

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as BDecode exposing (Decoder, Step(..))
import Index exposing (Index)
import PackedInt exposing (PackedInt)
import Size exposing (Size)



--- TYPES ---


{-| A `PackedArray` is just an array of `PackedInt`s.
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
                                packedInt =
                                    if (n |> modBy 2) == 0 then
                                        x |> Bitwise.shiftLeftBy 8

                                    else
                                        acc
                                            |> Array.get (n // 2)
                                            |> Maybe.withDefault 0x00
                                            |> Bitwise.or x
                            in
                            Loop ( n + 1, acc |> Array.set (n // 2) packedInt )
                        )
        )


{-| Create a packed array filled with `0`'s that is sized large enough to fit
the given number of bits.
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
    array |> Array.get (index i)


{-| Set the bit in a packed array at the given index.
-}
setBit : Index -> Bit -> PackedArray -> PackedArray
setBit i bit array =
    case array |> getPackedInt i of
        Nothing ->
            array

        Just int ->
            let
                newPackedInt =
                    int |> PackedInt.setBit i bit
            in
            array |> setPackedInt i newPackedInt


{-| Set the `PackedInt` in the array which holds the bit at the given index
to another `PackedInt`. If the index is out of range, the array is unaltered.
-}
setPackedInt : Index -> PackedInt -> PackedArray -> PackedArray
setPackedInt i int array =
    array |> Array.set (index i) int



--- GETTING INDIVIDUAL PACKED-INTS ---


firstOrZero : PackedArray -> PackedInt
firstOrZero array =
    array
        |> Array.get 0
        |> Maybe.withDefault 0x00


lastOrZero : PackedArray -> PackedInt
lastOrZero array =
    array
        |> Array.get (Array.length array - 1)
        |> Maybe.withDefault 0x00



--- TRANSFORMING A PACKED-ARRAY BY PACKED-INT ---


setFirst : PackedInt -> PackedArray -> PackedArray
setFirst =
    Array.set 0


setLast : PackedInt -> PackedArray -> PackedArray
setLast int array =
    array |> Array.set (Array.length array - 1) int



--- TRANSFORMING A PACKED-ARRAY BY BITS ---


map : Index -> (Bit -> Bit) -> PackedArray -> PackedArray
map endIndex f array =
    array
        |> fold
            Index.zero
            (\i -> i < endIndex)
            Index.inc
            (\i b acc -> acc |> setBit i (f b))
            (sizedFor endIndex)


{-| Fold over a bitstring using an index, a predicate, and a step function. This
allows for folding in either direction. The given folding function takes an
index as its first parameter for convenience; it can be ignored.
-}
fold :
    Index
    -> (Index -> Bool)
    -> (Index -> Index)
    -> (Index -> Bit -> acc -> acc)
    -> acc
    -> PackedArray
    -> acc
fold i while step f acc array =
    if while i then
        case array |> getBit i of
            Nothing ->
                acc

            Just bit ->
                fold (step i) while step f (f i bit acc) array

    else
        acc


{-| Slice a string of bits in a packed array from a start index up to (but not
including) an end index.
-}
slice : Index -> Index -> PackedArray -> PackedArray
slice start end array =
    let
        sliceSize =
            end - start

        sliced =
            Array.slice
                (start |> index)
                (end |> Index.dec |> index |> Index.inc)
                array

        ( shiftLeftAmount, shiftRightAmount ) =
            ( start |> PackedInt.index, end |> Size.paddingBy 16 )

        maskedLastInt =
            sliced
                |> lastOrZero
                |> PackedInt.shiftRightBy shiftRightAmount
                |> PackedInt.shiftLeftBy shiftRightAmount
    in
    if sliceSize <= 0 then
        Array.empty

    else if shiftLeftAmount == 0 then
        -- We can skip shifting.
        sliced |> setLast maskedLastInt

    else
        -- We'll need to shift all the bits to the left some amount.
        sliced
            |> setLast maskedLastInt
            -- Shift everything over
            |> Array.foldl
                (\int ( i, acc ) ->
                    let
                        shiftedInt =
                            int |> PackedInt.shiftLeftBy shiftLeftAmount
                    in
                    case sliced |> Array.get (i + 1) of
                        Nothing ->
                            ( Index.inc i, acc |> Array.set i shiftedInt )

                        Just nextInt ->
                            let
                                shiftedNextInt =
                                    nextInt |> PackedInt.shiftRightBy (16 - shiftLeftAmount)
                            in
                            ( Index.inc i
                            , acc |> Array.set i (shiftedInt |> PackedInt.combine shiftedNextInt)
                            )
                )
                ( 0, sizedFor sliceSize )
            |> Tuple.second


{-| Drop the leftmost `n` bits, filling in the right side with a minimal amount
of zeroes.
-}
dropLeft : Size -> Size -> PackedArray -> PackedArray
dropLeft n sz array =
    if n <= 0 then
        array

    else
        array |> slice n sz


{-| Drop the rightmost `n` bits, filling in the right side with a minimal amount
of zeroes.
-}
dropRight : Size -> Size -> PackedArray -> PackedArray
dropRight n sz array =
    if n <= 0 then
        array

    else
        array |> slice Index.zero (sz - n)



--- CONVERTING AN INDEX ---


index : Index -> Index
index i =
    i // 16
