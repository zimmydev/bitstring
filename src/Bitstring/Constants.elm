module Bitstring.Constants exposing (packedSize, packedSizeInBytes)


packedSizeInBytes : Int
packedSizeInBytes =
    2


packedSize : Int
packedSize =
    packedSizeInBytes * 8
