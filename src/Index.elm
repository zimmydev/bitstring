module Index exposing (Index, dec, inc, zero)

{-| THIS IS AN INTERNAL MODULE.

An index is just a bit's offset from the beginning of a `Bitstring`. Via this
module, it can be converted into a more useful representation for the purposes
of accessing bits in a `PackedArray`.

-}

--- TYPES ---


{-| An bit's index within a `Bitstring` or a `PackedArray`.
-}
type alias Index =
    Int



--- CONSTANTS ---


zero : Index
zero =
    0



--- TRANSFORMING AN INDEX ---


{-| Increment an index.
-}
inc : Index -> Index
inc i =
    i + 1


{-| Decrement an index.
-}
dec : Index -> Index
dec i =
    i - 1
