module FuzzTests exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz as Fuzz exposing (Fuzzer, int, list)
import Test exposing (..)
import Xml.Decode exposing (..)
import Xml.Encode exposing (..)


suite : Test
suite =
    describe "encoding and decoding works"
        [ fuzz Fuzz.string "works for random string values" <|
            \s ->
                let
                    val =
                        string s
                in
                decodeString (encode 0 val) |> Expect.equal (Ok val)
        , fuzz Fuzz.string "works for random string values within tags" <|
            \s ->
                let
                    val =
                        -- string must be non-empty because otherwise
                        -- decoding doesn't know that it is a string
                        object [ ( "tagname", Dict.empty, string ("x" ++ s) ) ]
                in
                decode (encode 0 val) |> Expect.equal (Ok val)
        , fuzz Fuzz.string "works for random strings as attribute values" <|
            \s ->
                let
                    s1 =
                        String.filter
                            (\c ->
                                not <|
                                    -- I'm not sure why some characters are not allowed...
                                    String.contains (String.fromChar c) "=\n\t "
                            )
                            s

                    val =
                        object
                            [ ( "tagname"
                                -- string must be non-empty because otherwise
                                -- decoding doesn't know that it is a string
                              , Dict.fromList [ ( "attrname", string ("x" ++ s1) ) ]
                              , null
                              )
                            ]
                in
                decode (encode 0 val) |> Expect.equal (Ok val)
        ]
