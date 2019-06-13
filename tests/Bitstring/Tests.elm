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
        [ fuzz bitlist "fromList and toList are the inverse of each other" <|
            \l ->
                l
                    |> Bitstring.fromList
                    |> Bitstring.toList
                    |> Expect.equal l
        , fuzz bytes "fromBytes and toBytes are the inverse of each other" <|
            \by ->
                by
                    |> Bitstring.fromBytes
                    |> Bitstring.toBytes
                    |> Expect.equal by
        , fuzz string "fromString and toString are the inverse of each other" <|
            \s ->
                case s |> Bitstring.fromString of
                    Nothing ->
                        Expect.pass

                    Just b ->
                        b
                            |> Bitstring.toString
                            |> Expect.equal s
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
        , fuzz bitlist "bitstrings have the correct size" <|
            \l ->
                l
                    |> Bitstring.fromList
                    |> Bitstring.size
                    |> Expect.equal (List.length l)
        , fuzz bytes "bitstrings have the correct byte-size" <|
            \by ->
                by
                    |> Bitstring.fromBytes
                    |> Bitstring.sizeInBytes
                    |> Expect.equal (Bytes.width by)
        , fuzz bitlist "pushing works correctly" <|
            \l ->
                l
                    |> List.foldl (\bit acc -> acc |> Bitstring.push bit) Bitstring.empty
                    |> Bitstring.toList
                    |> Expect.equal l
        , fuzz (Fuzz.list bitlist) "append/concat works correctly" <|
            \ls ->
                ls
                    |> List.map Bitstring.fromList
                    -- We are implicitly testing `append` here (nice!)
                    |> Bitstring.concat
                    |> Bitstring.toList
                    |> Expect.equal (List.concat ls)
        ]



--- HELPER FUNCTIONS ---


bytes : Fuzzer Bytes
bytes =
    Fuzz.string
        |> Fuzz.map (\s -> BEncode.string s |> BEncode.encode)


bitlist : Fuzzer (List Bit)
bitlist =
    Fuzz.bool
        |> Fuzz.map
            (\bool ->
                if bool then
                    One

                else
                    Zero
            )
        |> Fuzz.list


bitstring : Fuzzer Bitstring
bitstring =
    bytes
        |> Fuzz.map Bitstring.fromBytes


string : Fuzzer String
string =
    Fuzz.oneOf [ Fuzz.constant '1', Fuzz.constant '0' ]
        |> Fuzz.list
        |> Fuzz.map String.fromList
