module Xml exposing
    ( Value(..)
    , foldl, map
    , xmlToJson2, jsonToXml, xmlDecoder
    , decodeXmlEntities, encodeXmlEntities
    , isValidXmlName
    )

{-| The main data structure along with some trivial helpers.

@docs Value

@docs foldl, map


# XML/JSON conversion

This library can convert to and from `Json.Value`.

@docs xmlToJson2, jsonToXml, xmlDecoder


# XML character entities

This library can encode and decode the five predefined XML character entities.
Numeric character references and other named HTML entities (e.g. `&#x20ac;`
or `&euro;`) are currently not supported.
Please try to use UTF-8 / Unicode instead.

@docs decodeXmlEntities, encodeXmlEntities

@docs isValidXmlName

-}

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as Json
import Regex


{-| Representation of the XML tree
-}
type Value
    = Tag String (Dict String Value) Value
    | StrNode String
    | IntNode Int
    | FloatNode Float
    | BoolNode Bool
    | NullNode
    | Object (List Value)
    | DocType String (Dict String Value)


{-|

    Standard mapping of a value to another value

-}
map : (Value -> Value) -> Value -> Value
map fn value =
    case value of
        Tag name dict nextValue ->
            map fn nextValue
                |> Tag name dict
                |> fn

        Object vals ->
            List.map (map fn) vals
                |> Object
                |> fn

        thing ->
            fn thing


{-| Standard foldl
-}
foldl : (Value -> a -> a) -> a -> Value -> a
foldl fn init value =
    case value of
        Tag name dict nextValue ->
            foldl fn (fn value init) nextValue

        Object values ->
            List.foldl (\elm b -> foldl fn b elm) (fn value init) values

        anything ->
            fn anything init


{-| Encode string with XML entities

    encodeXmlEntities "<hello>"
    --> "&lt;hello&gt;"

-}
encodeXmlEntities : String -> String
encodeXmlEntities s =
    List.foldr (\( x, y ) z -> String.replace (String.fromChar x) ("&" ++ y ++ ";") z) s predefinedEntities


{-| Decode string with XML entities

    decodeXmlEntities "&lt;hello&gt;"
    --> "<hello>"

    Do not decode entities twice!

    decodeXmlEntities "&amp;lt;hello&gt;"
    --> "&lt;hello>"

-}
decodeXmlEntities : String -> String
decodeXmlEntities s =
    List.foldl (\( x, y ) z -> String.replace ("&" ++ y ++ ";") (String.fromChar x) z) s predefinedEntities


predefinedEntities : List ( Char, String )
predefinedEntities =
    -- https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references
    [ ( '"', "quot" )
    , ( '\'', "apos" )
    , ( '<', "lt" )
    , ( '>', "gt" )

    -- & / &amp; must come last!
    , ( '&', "amp" )
    ]


{-| Check if string is a valid XML name for a tag names or an attribute names.
-}
isValidXmlName : String -> Bool
isValidXmlName =
    let
        nameRegex =
            -- O'Reilly: XML in a Nutshell: https://docstore.mik.ua/orelly/xml/xmlnut/ch02_04.htm
            -- Unicode letters are allowed but the JS regex unicode property escape \p{Letter} doesn't seem to work (yet).
            -- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions/Unicode_Property_Escapes
            Maybe.withDefault Regex.never
                (Regex.fromString "^[_a-zA-Z0-9][-_.:a-zA-Z0-9]*$")
    in
    Regex.contains nameRegex


{-| Convert an `Xml.Value` to a `Json.Value`

The conversion of `Tag`s changed in version 2.0.0, so that they can be properly converted back. The attribute names become JSON keys with "@" prepended, and the value becomes a JSON key named "#value": <foo x="x">1</foo> becomes {foo: {#value: 1, @x: "x"}}

Renamed from xmlToJson to force a bump to version 2.0.

    import Dict
    import Json.Encode as Json
    import Xml exposing (jsonToXml, Value(..))

    xmlToJson2 (StrNode "hello")
    --> Json.string "hello"

    xmlToJson2 (IntNode 5)
    --> Json.int 5

    xmlToJson2 (FloatNode 5)
    --> Json.float 5

    xmlToJson2 (BoolNode True)
    --> Json.bool True

    xmlToJson2 (Object [ IntNode 5, BoolNode True ])
    --> Json.list identity [Json.int 5, Json.bool True]

    xmlToJson2 (Tag "foo" (Dict.fromList [("x",StrNode "x")]) (IntNode 1))
    --> Json.object [("foo", Json.object [("#value", Json.int 1), ("@x", Json.string "x")])]

    xmlToJson2 (DocType "" Dict.empty)
    --> Json.null

    xmlToJson2 NullNode
    --> Json.null

-}
xmlToJson2 : Value -> Json.Value
xmlToJson2 xml =
    case xml of
        Tag name attributes nextValue ->
            let
                jsonAttrs =
                    Dict.toList attributes
                        |> List.map (\( nam, value ) -> ( "@" ++ nam, xmlToJson2 value ))
            in
            Json.object
                [ ( name
                  , if List.isEmpty jsonAttrs then
                        xmlToJson2 nextValue

                    else
                        jsonAttrs
                            |> List.append [ ( "#value", xmlToJson2 nextValue ) ]
                            |> Json.object
                  )
                ]

        StrNode str ->
            Json.string str

        IntNode int ->
            Json.int int

        FloatNode float ->
            Json.float float

        BoolNode bool ->
            Json.bool bool

        NullNode ->
            Json.null

        Object values ->
            Json.list xmlToJson2 values

        DocType _ _ ->
            Json.null


{-| A decoder for XML
-}
xmlDecoder : JD.Decoder Value
xmlDecoder =
    JD.oneOf
        [ JD.map StrNode JD.string
        , JD.map IntNode JD.int
        , JD.map FloatNode JD.float
        , JD.map BoolNode JD.bool

        -- This is the most explicit way to store a null value:
        , JD.map (\_ -> NullNode) (JD.null 0)
        , JD.list (JD.lazy (\_ -> xmlDecoder))
            |> JD.andThen
                (\list ->
                    case list of
                        [ x ] ->
                            JD.succeed x

                        _ ->
                            Object list |> JD.succeed
                )
        , JD.map
            (Dict.toList
                >> List.map
                    (\( name, val ) ->
                        if name == "#value" then
                            val

                        else
                            case val of
                                Object list ->
                                    list
                                        -- reverse the list before it
                                        -- is processed with foldl and
                                        -- :: (which reverses the
                                        -- order again)
                                        |> List.reverse
                                        |> List.foldl
                                            (\v ( a_, p_ ) ->
                                                case v of
                                                    Tag str _ b ->
                                                        if String.startsWith "@" str then
                                                            ( ( String.dropLeft 1 str, b ) :: a_, p_ )

                                                        else
                                                            ( a_, v :: p_ )

                                                    _ ->
                                                        ( a_, v :: p_ )
                                            )
                                            ( [], [] )
                                        |> (\( attr, params ) ->
                                                Tag name
                                                    (Dict.fromList attr)
                                                    (case params of
                                                        [ x ] ->
                                                            x

                                                        _ ->
                                                            Object params
                                                    )
                                           )

                                _ ->
                                    Tag name Dict.empty val
                    )
                >> (\list ->
                        case list of
                            [ x ] ->
                                x

                            _ ->
                                Object list
                   )
            )
            (JD.dict (JD.lazy (\_ -> xmlDecoder)))
        ]


{-| Convert a `Json.Value` into an `Xml.Value`

    import Dict
    import Json.Encode as Json
    import Xml exposing (jsonToXml, Value(..))

    jsonToXml (Json.string "hello")
    --> StrNode "hello"

    jsonToXml (Json.int 5)
    --> IntNode 5

    jsonToXml (Json.float 10.5)
    --> FloatNode 10.5

    jsonToXml (Json.bool True)
    --> BoolNode True

    jsonToXml (Json.object [("name", Json.string "hello")])
    --> Tag "name" Dict.empty (StrNode "hello")

    jsonToXml (Json.list identity [Json.string "name", Json.string "hello"])
    --> Object [ StrNode "name", StrNode "hello" ]

    jsonToXml (Json.object [("foo", Json.object [("#value", Json.int 1), ("@x", Json.string "x")])])
    --> Tag "foo" (Dict.fromList [("x",StrNode "x")]) (IntNode 1)

-}
jsonToXml : Json.Value -> Value
jsonToXml json =
    JD.decodeValue xmlDecoder json
        |> Result.withDefault (Object [])
