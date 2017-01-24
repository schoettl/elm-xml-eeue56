module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Xml.Encode exposing (..)
import Xml.Decode exposing (..)


example : Value
example =
    object
        [ ( "name", string "noah" )
        , ( "age", int 5 )
        ]


exampleAsString : String
exampleAsString =
    """
<name>noah</name>
<age>5</age>
"""
        |> String.trim


badXml : String
badXml =
    """ f<name>noah</name>"""


badXmlWithNoClose : String
badXmlWithNoClose =
    """ <name>noah</naoh>"""


nestedExample : Value
nestedExample =
    object
        [ ( "person"
          , object
                [ ( "name", string "noah" )
                , ( "age", int 5 )
                ]
          )
        ]


nestedExampleAsString : String
nestedExampleAsString =
    """
<person>
    <name>noah</name>
    <age>5</age>
</person>
"""
        |> String.trim


all : Test
all =
    describe "Encode test"
        [ test "a basic tag is encoded properly" <|
            \_ ->
                Expect.equal exampleAsString (encode 0 example)
        , test "a basic tag is decoded properly" <|
            \_ ->
                Expect.equal (decode exampleAsString) (Ok example)
        , test "a nested tag is encoded properly" <|
            \_ ->
                Expect.equal nestedExampleAsString (encode 4 nestedExample)
        , test "a nested tag is decoded properly" <|
            \_ ->
                Expect.equal (decode nestedExampleAsString) (Ok nestedExample)
        , test "a bad xml is an error" <|
            \_ ->
                Expect.false "xml is not parsed without a closing tag"
                    (case decode badXmlWithNoClose of
                        Err _ ->
                            False

                        _ ->
                            True
                    )
        ]