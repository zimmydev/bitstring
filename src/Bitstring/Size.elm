module Bitstring.Size exposing (Division, Size, by, isAlignedBy, paddingBy)

{-| A few utilities for dealing with bitstring sizes.
-}

--- TYPES ---


{-| A size of a bitstring, in bits.
-}
type alias Size =
    Int


{-| A division of bits in a bitstring, which itself is a size in bits.
-}
type alias Division =
    Int



--- UTILITIES ---


{-| Calculates the minimum number of divisions that would be needed to represent
a sequence given its size and the desired division size.
-}
by : Division -> Size -> Size
by divisionSize size =
    if size > 0 then
        (size - 1) // divisionSize + 1

    else
        0


{-| Calculates the size of the minimum padding that would need to be introduced
for a sequence given its size and the desired division size.
-}
paddingBy : Division -> Size -> Size
paddingBy divisionSize size =
    (divisionSize - 1) - (size - 1 |> modBy divisionSize)


{-| Determined if a size is aligned by a given division size.
-}
isAlignedBy : Division -> Size -> Bool
isAlignedBy divisionSize =
    paddingBy divisionSize >> (==) 0
