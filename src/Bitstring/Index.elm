module Bitstring.Index exposing (Index, absolute, array, bit, fromAbsolute, origin)

{-| THIS IS AN INTERNAL MODULE.

An index is just an index into a bitstring that can be accessed as either its
absolute representation, mod-16 representation, or packed-array-index
representation.

-}

--- TYPES ---


{-| An bit's index within a bitstring.
-}
type alias Index =
    Int



--- INDEX CONSTANTS ---


{-| The origin index.
-}
origin : Index
origin =
    0



--- CREATING AN INDEX ---


{-| Create an index from an absolute index into a bitstring.
-}
fromAbsolute : Index -> Index
fromAbsolute =
    identity



--- CONVERTING AN INDEX ---


{-| Converts an index to its absolute representation, for indexing a bit within
a `PackedArray`.
-}
absolute : Index -> Index
absolute =
    identity


{-| Converts an index to its mod-16 representation, for indexing a bit within an
individual `PackedInt`.
-}
bit : Index -> Index
bit =
    modBy 16


{-| Converts an index to its array-index representation, for indexing a
`PackedInt` within a `PackedArray`.
-}
array : Index -> Index
array i =
    i // 16
