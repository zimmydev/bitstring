module Bitstring.Tests exposing (suite)

import Bitstring exposing (Bit(..), Bitstring)
import Bytes exposing (Bytes)
import Bytes.Encode as BEncode
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, only, skip, test, todo)



--- SUITE ---


suite : Test
suite =
    describe "module Bitstring"
        [ fuzz bitList "fromList and toList are the inverse of each other" <|
            \l ->
                l
                    |> Bitstring.fromList
                    |> Bitstring.toList
                    |> Expect.equal l
        , test "isEmpty works as expected" <|
            \_ ->
                Bitstring.empty
                    |> Expect.all
                        [ \bs ->
                            bs
                                |> Bitstring.isEmpty
                                |> Expect.true "Expected the bitstring to be empty"
                        , \bs ->
                            bs
                                |> Bitstring.push Zero
                                |> Bitstring.isEmpty
                                |> Expect.false "Didn't expect the bitstring to be empty"
                        ]
        , test "0-bit bistring has the correct size" <|
            \_ ->
                Bitstring.empty
                    |> Expect.all
                        [ \bs ->
                            bs
                                |> Bitstring.size
                                |> Expect.equal 0
                        , \bs ->
                            bs
                                |> Bitstring.sizeInBytes
                                |> Expect.equal 0
                        ]
        , test "8-bit bitstring has the correct size" <|
            \_ ->
                Bitstring.fromList (List.repeat 8 One)
                    |> Expect.all
                        [ \bs ->
                            bs
                                |> Bitstring.size
                                |> Expect.equal 8
                        , \bs ->
                            bs
                                |> Bitstring.sizeInBytes
                                |> Expect.equal 1
                        ]
        , test "9-bit bitstring has the correct size" <|
            \_ ->
                Bitstring.fromList (List.repeat 9 One)
                    |> Expect.all
                        [ \bs ->
                            bs
                                |> Bitstring.size
                                |> Expect.equal 9
                        , \bs ->
                            bs
                                |> Bitstring.sizeInBytes
                                |> Expect.equal 2
                        ]
        , fuzz bitList "pushing works correctly" <|
            \l ->
                l
                    |> List.foldl (\bit acc -> acc |> Bitstring.push bit) Bitstring.empty
                    |> Bitstring.toList
                    |> Expect.equal l
        , fuzz bytes "fromBytes forms a correctly-sized bitstring" <|
            \by ->
                by
                    |> Bitstring.fromBytes
                    |> Bitstring.sizeInBytes
                    |> Expect.equal (Bytes.width by)
        ]



--- HELPER FUNCTIONS ---


bitList : Fuzzer (List Bit)
bitList =
    Fuzz.bool
        |> Fuzz.map
            (\bool ->
                if bool then
                    One

                else
                    Zero
            )
        |> Fuzz.list


bitString : Fuzzer Bitstring
bitString =
    bitList
        |> Fuzz.map Bitstring.fromList


bytes : Fuzzer Bytes
bytes =
    Fuzz.string
        |> Fuzz.map (\s -> BEncode.string s |> BEncode.encode)
