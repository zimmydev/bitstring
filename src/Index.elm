module Index exposing (Index, absolute, array, bit, decrement, increment, zero)

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


zero : Index
zero =
    0



--- CONVERTING AN INDEX ---


absolute : Index -> Index
absolute =
    identity


bit : Index -> Index
bit =
    modBy 16


array : Index -> Index
array i =
    i // 16



--- CHANGING AN INDEX ---


increment : Index -> Index
increment i =
    i + 1


decrement : Index -> Index
decrement i =
    i - 1
