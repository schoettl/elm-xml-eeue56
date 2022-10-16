module Tests exposing (..)

import Dict
import ExampleStuff
import Expect
import Json.Decode as JD
import Json.Encode as JE
import String
import Test exposing (..)
import Xml exposing (..)
import Xml.Decode exposing (..)
import Xml.Encode exposing (..)
import Xml.Query exposing (contains, tags)


example : Value
example =
    object
        [ ( "name", Dict.empty, string "noah" )
        , ( "age", Dict.empty, int 5 )
        ]


exampleAsString : String
exampleAsString =
    """
<name>noah</name>
<age>5</age>
"""
        |> String.trim


exampleAsStringWithDocType : String
exampleAsStringWithDocType =
    """
<?xml encoding="UTF-8" version="1"?>
<name>noah</name>
<age>5</age>
"""
        |> String.trim


selfClosingExampleAsString : String
selfClosingExampleAsString =
    """
<person>
    <name is="me">kitofr</name>
    <here is="false" />
    <here is="true"/>
    <there/>
    <name is="me">kitofr</name>
</person>
"""


selfClosingExample : Value
selfClosingExample =
    object
        [ ( "person"
          , Dict.empty
          , object
                [ ( "name", Dict.fromList [ ( "is", string "me" ) ], string "kitofr" )
                , ( "here", Dict.fromList [ ( "is", bool False ) ], null )
                , ( "here", Dict.fromList [ ( "is", bool True ) ], null )
                , ( "there", Dict.fromList [], null )
                , ( "name", Dict.fromList [ ( "is", string "me" ) ], string "kitofr" )
                ]
          )
        ]


exampleWithProps : Value
exampleWithProps =
    object
        [ ( "person"
          , Dict.empty
          , object
                [ ( "name", Dict.fromList [ ( "is", string "me" ) ], string "noah" )
                , ( "age", Dict.fromList [ ( "max", int 10 ) ], int 5 )
                , ( "here", Dict.fromList [ ( "is", bool False ) ], null )
                ]
          )
        , ( "person"
          , Dict.empty
          , object
                [ ( "name", Dict.fromList [ ( "is", string "you i guess" ) ], string "dave" )
                , ( "age", Dict.fromList [ ( "max", int 100 ), ( "inc", float 1.5 ) ], int 50 )
                , ( "here", Dict.fromList [ ( "is", bool True ) ], null )
                ]
          )
        ]


exampleWithPropsAsString : String
exampleWithPropsAsString =
    """
<person>
    <name is="me">noah</name>
    <age max="10">5</age>
    <here is="false"></here>
</person>
<person>
    <name is="you i guess">dave</name>
    <age inc="1.5" max="100">50</age>
    <here is="true"></here>
</person>
"""
        |> String.trim


exampleStringWithSpeciaChars =
    -- this is just a bad looking string
    "&'\"<>;&amp;"


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
          , Dict.empty
          , object
                [ ( "name", Dict.empty, string "noah" )
                , ( "age", Dict.empty, int 50 )
                , ( "children"
                  , Dict.empty
                  , object
                        [ ( "person"
                          , Dict.fromList [ ( "gender", string "male" ) ]
                          , object
                                [ ( "name", Dict.empty, string "david" )
                                , ( "age", Dict.empty, int 13 )
                                ]
                          )
                        , ( "person"
                          , Dict.fromList [ ( "gender", string "female" ) ]
                          , object
                                [ ( "name", Dict.empty, string "daisy" )
                                , ( "age", Dict.empty, int 25 )
                                , ( "children"
                                  , Dict.empty
                                  , object
                                        [ ( "person"
                                          , Dict.empty
                                          , object
                                                [ ( "name", Dict.empty, string "daniel" )
                                                ]
                                          )
                                        , ( "person"
                                          , Dict.empty
                                          , object
                                                [ ( "name", Dict.empty, string "duncan" )
                                                ]
                                          )
                                        ]
                                  )
                                ]
                          )
                        ]
                  )
                ]
          )
        ]


nestedExampleAsString : String
nestedExampleAsString =
    """
<person>
    <name>noah</name>
    <age>50</age>
    <children>
        <person gender="male">
            <name>david</name>
            <age>13</age>
        </person>
        <person gender="female">
            <name>daisy</name>
            <age>25</age>
            <children>
                <person>
                    <name>daniel</name>
                </person>
                <person>
                    <name>duncan</name>
                </person>
            </children>
        </person>
    </children>
</person>
"""
        |> String.trim


decodedExampleStuff : Result String Value
decodedExampleStuff =
    decode ExampleStuff.stuff


all : Test
all =
    describe "Encode test"
        [ test "a basic tag is encoded properly" <|
            \_ ->
                Expect.equal exampleAsString (encode 0 example)
        , test "a basic tag is decoded properly" <|
            \_ ->
                Expect.equal (decode exampleAsString) (Ok example)
        , test "a basic tag with doc type is encoded properly" <|
            \_ ->
                Expect.equal (Result.map (encode 0) (decode exampleAsStringWithDocType)) (Ok exampleAsStringWithDocType)
        , test "a tag with props is encoded properly" <|
            \_ ->
                Expect.equal exampleWithPropsAsString (encode 4 exampleWithProps)
        , test "a tag with props is decoded properly" <|
            \_ ->
                Expect.equal (decode exampleWithPropsAsString) (Ok exampleWithProps)
        , test "a nested tag is encoded properly" <|
            \_ ->
                Expect.equal nestedExampleAsString (encode 4 nestedExample)
        , test "a nested tag is decoded properly" <|
            \_ ->
                Expect.equal (decode nestedExampleAsString) (Ok nestedExample)
        , test "a self closing tag is decoded properly" <|
            \_ ->
                Expect.equal (decode selfClosingExampleAsString) (Ok selfClosingExample)
        , test "a bad xml is an error" <|
            \_ ->
                Expect.false "xml is not parsed without a closing tag"
                    (case decode badXmlWithNoClose of
                        Err _ ->
                            False

                        _ ->
                            True
                    )
        , test "a good xml is parsed correctly" <|
            \_ ->
                Expect.true "xml is not parsed without a closing tag"
                    (case decodedExampleStuff of
                        Err m ->
                            False

                        _ ->
                            True
                    )
        , test "the XML contains a node we expect" <|
            \_ ->
                Expect.equal
                    (decodedExampleStuff
                        |> Result.toMaybe
                        |> Maybe.withDefault null
                        |> tags "ListBucketResult"
                        |> List.length
                    )
                    1
        , test "the XML contains a node we expect again" <|
            \_ ->
                Expect.equal
                    (decodedExampleStuff
                        |> Result.toMaybe
                        |> Maybe.withDefault null
                        |> tags "Contents"
                        |> List.length
                    )
                    100
        , test "The query for finding people should find the right tags" <|
            \_ ->
                Expect.equal
                    ExampleStuff.correctPeople
                    ExampleStuff.people
        , test "when parsing complex XML, it finds everything as it should" <|
            \_ ->
                Expect.equal
                    (List.length <| Result.withDefault [] <| ExampleStuff.fromXML <| ExampleStuff.stuff)
                    100
        , test "a string with XML character entities is correctly decoded" <|
            \_ ->
                Expect.equal (decodeString "&amp;&quot;&apos;&lt;&gt;")
                    (Ok <| string "&\"'<>")
        , test "a tag with special characters is correctly encoded and decoded" <|
            \_ ->
                let
                    val =
                        object [ ( "tagname", Dict.empty, string exampleStringWithSpeciaChars ) ]
                in
                Expect.equal (decode <| encode 0 val) (Ok val)
        , test "XML character entities are encoded and decoded, each only once" <|
            \_ ->
                let
                    val =
                        object [ ( "tagname", Dict.fromList [ ( "attr", string "&<>\"&amp;&lt;&gt;&quot;" ) ], string "x" ) ]
                in
                Expect.equal (decode <| encode 0 val) (Ok val)
        , test "Encode tag with NullNode given omitNullTag" <|
            \_ ->
                let
                    val =
                        Tag "tagname" Dict.empty NullNode
                in
                Expect.equal (encode 0 val) ""
        , test "Encode tag with NullNode but also with attributes given omitNullTag" <|
            \_ ->
                let
                    val =
                        Tag "tagname" (Dict.fromList [ ( "attr", NullNode ) ]) NullNode
                in
                Expect.equal (encode 0 val) "<tagname attr=\"\"></tagname>"
        , test "Encode tag with NullNode given not omitNullTag" <|
            \_ ->
                let
                    val =
                        Tag "tagname" Dict.empty NullNode

                    setts =
                        { defaultEncodeSettings | omitNullTag = False }
                in
                Expect.equal (encodeWith setts 0 val) "<tagname></tagname>"
        , describe "Test decode settings"
            [ test "Decode bool True" <|
                \_ ->
                    Expect.equal
                        (decodeBoolWith
                            { defaultDecodeSettings | trueValues = [ "1" ] }
                            "1"
                        )
                        (Ok (bool True))
            , test "Decode bool False" <|
                \_ ->
                    Expect.equal
                        (decodeBoolWith
                            { defaultDecodeSettings | falseValues = [ "0", "False" ] }
                            "False"
                        )
                        (Ok (bool False))
            , test "Decode null" <|
                \_ ->
                    Expect.equal
                        (decodeNull
                            { defaultDecodeSettings | nullValues = [ "nil", "null" ] }
                            "nil"
                        )
                        (Ok NullNode)
            , test "Do not decode integers if not parseNumbers" <|
                \_ ->
                    Expect.equal
                        (decodeIntWith
                            { defaultDecodeSettings | parseNumbers = False }
                            "1"
                        )
                        (Err "number parsing is disabled")
            , test "Do not decode floats if not parseNumbers" <|
                \_ ->
                    Expect.equal
                        (decodeFloatWith
                            { defaultDecodeSettings | parseNumbers = False }
                            "1.0"
                        )
                        (Err "number parsing is disabled")
            , test "Do not decode numbers if not parseNumbers" <|
                \_ ->
                    Expect.equal
                        (decodeWith
                            { defaultDecodeSettings | parseNumbers = False }
                            "<a>1</a>"
                        )
                        (Ok <| Object [ Tag "a" Dict.empty (StrNode "1") ])
            ]
        , describe "Test attributes"
            [ test "Decode tag with single-quoted attribute value" <|
                \_ ->
                    let
                        val =
                            object [ ( "tagname", Dict.fromList [ ( "attr", string "foo" ) ], list [] ) ]
                    in
                    Expect.equal (decode "<tagname attr='foo' ></tagname>")
                        (Ok val)
            , test "Encode attribute value" <|
                \_ ->
                    let
                        val =
                            Tag "tagname" (Dict.fromList [ ( "attr", string "'" ) ]) (StrNode "")
                    in
                    Expect.equal (encode 0 val) "<tagname attr=\"&apos;\"></tagname>"
            , test "Encode attribute value with single quotes" <|
                \_ ->
                    let
                        val =
                            Tag "tagname" (Dict.fromList [ ( "attr", string "\"" ) ]) (StrNode "")

                        setts =
                            { defaultEncodeSettings | attributeSingleQuoteInsteadOfDouble = True }
                    in
                    Expect.equal (encodeWith setts 0 val) "<tagname attr='&quot;'></tagname>"
            , test "Decode empty attribute value" <|
                \_ ->
                    let
                        val =
                            object [ ( "tagname", Dict.fromList [ ( "attr", NullNode ) ], list [] ) ]
                    in
                    Expect.equal (decode "<tagname attr=\"\" ></tagname>") (Ok val)
            , test "Decode attribute value with double-space works" <|
                \_ ->
                    let
                        val =
                            object [ ( "tagname", Dict.fromList [ ( "attr", string "  " ) ], list [] ) ]
                    in
                    Expect.equal (decode "<tagname attr=\"  \" ></tagname>") (Ok val)
            ]
        , test "Decode empty tag" <|
            \_ ->
                let
                    val =
                        object [ ( "tagname", Dict.empty, list [] ) ]
                in
                Expect.equal (decode "<tagname/>") (Ok val)
        , describe "Decode test"
            [ test "decodes emtpy string" <|
                \_ ->
                    Expect.equal
                        (decode "")
                        (Ok <| Object [])
            , test "decodes whitespace-only string" <|
                \_ ->
                    Expect.equal
                        (decode "  ")
                        (Ok <| Object [])
            ]
        , describe "Test xmlDecoder to convert JSON to XML"
            [ test "decodes string" <|
                \_ ->
                    Expect.equal
                        (JD.decodeString xmlDecoder """{"a":"b"}""")
                        (Ok <| Tag "a" Dict.empty (StrNode "b"))
            , test "decodes int" <|
                \_ ->
                    Expect.equal
                        (JD.decodeString xmlDecoder """{"a":1}""")
                        (Ok <| Tag "a" Dict.empty (IntNode 1))
            , test "decodes float" <|
                \_ ->
                    Expect.equal
                        (JD.decodeString xmlDecoder """{"a":1.1}""")
                        (Ok <| Tag "a" Dict.empty (FloatNode 1.1))
            , test "decodes bool" <|
                \_ ->
                    Expect.equal
                        (JD.decodeString xmlDecoder """{"a":true}""")
                        (Ok <| Tag "a" Dict.empty (BoolNode True))
            , test "decodes null" <|
                \_ ->
                    Expect.equal
                        (JD.decodeString xmlDecoder """{"a":null}""")
                        (Ok <| Tag "a" Dict.empty NullNode)
            , test "decodes plain list" <|
                \_ ->
                    Expect.equal
                        (JD.decodeString xmlDecoder """["a", 1]""")
                        (Ok <| Object [ StrNode "a", IntNode 1 ])
            , test "decodes empty list" <|
                \_ ->
                    Expect.equal
                        (JD.decodeString xmlDecoder "[]")
                        (Ok <| Object [])
            , test "decodes list inside a tag" <|
                \_ ->
                    Expect.equal
                        (JD.decodeString xmlDecoder """{"a": [false, 1]}""")
                        (Ok <| Tag "a" Dict.empty <| Object [ BoolNode False, IntNode 1 ])
            , test "decodes list with null" <|
                \_ ->
                    Expect.equal
                        (JD.decodeString xmlDecoder """[null, 1]""")
                        (Ok <| Object [ NullNode, IntNode 1 ])
            , test "decode plain object" <|
                \_ ->
                    Expect.equal
                        (JD.decodeString xmlDecoder """{"a": 1, "b": 2}""")
                        (Ok <|
                            Object
                                [ Tag "a" Dict.empty (IntNode 1)
                                , Tag "b" Dict.empty (IntNode 2)
                                ]
                        )
            , test "decode object with null" <|
                \_ ->
                    Expect.equal
                        (JD.decodeString xmlDecoder """{"a": 1, "b": null}""")
                        (Ok <|
                            Object
                                [ Tag "a" Dict.empty (IntNode 1)
                                , Tag "b" Dict.empty NullNode
                                ]
                        )
            ]
        , describe "Find bug: json object with many fields is encoded as '' if one field is null"
            [ test "object with one empty tag" <|
                \_ ->
                    Expect.equal
                        (decode "<tag1>x</tag1><tag2/>")
                        (Ok <|
                            object
                                [ ( "tag1", Dict.empty, string "x" )
                                , ( "tag2", Dict.empty, null )
                                ]
                        )
            , test "encode object with one empty tag" <|
                \_ ->
                    Expect.equal
                        "<tag1>x</tag1>\n<tag2></tag2>"
                        (encode 0 <|
                            object
                                [ ( "tag1", Dict.empty, string "x" )
                                , ( "tag2", Dict.empty, null )
                                ]
                        )
            , test "encode JSON object with one null field" <|
                \_ ->
                    Expect.equal
                        "<tag1>x</tag1>\n"
                        (encode 0 <|
                            jsonToXml <|
                                JE.object
                                    [ ( "tag1", JE.string "x" )
                                    , ( "tag2", JE.null )
                                    ]
                        )
            , test "xmlDecoder decodes JSON object with exactly one null field" <|
                \_ ->
                    Expect.equal
                        (Ok <| Tag "tag" Dict.empty NullNode)
                        (JD.decodeValue xmlDecoder <|
                            JE.object [ ( "tag", JE.null ) ]
                        )
            , test "xmlDecoder decodes JSON object with one null field" <|
                \_ ->
                    Expect.equal
                        (Ok <|
                            Object
                                [ Tag "tag1" Dict.empty (StrNode "x")
                                , Tag "tag2" Dict.empty NullNode
                                ]
                        )
                        (JD.decodeValue xmlDecoder <|
                            JE.object
                                [ ( "tag1", JE.string "x" )
                                , ( "tag2", JE.null )
                                ]
                        )
            ]
        , describe "XML character entities &xxx;"
            [ test "decode entities, each only once" <|
                \_ -> decodeXmlEntities "&amp;quot;" |> Expect.equal "&quot;"
            , test "decode entity" <|
                \_ -> decodeXmlEntities "&quot;" |> Expect.equal "\""
            , test "encode entity" <|
                \_ -> encodeXmlEntities "&" |> Expect.equal "&amp;"
            , test "encode entities, each only once" <|
                \_ -> encodeXmlEntities "&quot;" |> Expect.equal "&amp;quot;"
            ]
        ]
