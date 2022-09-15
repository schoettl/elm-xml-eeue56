module Xml.Encode exposing
    ( encode
    , string, int, float, bool, object, null, list
    , defaultEncodeSettings, encodeWith
    )

{-|

    Use this module for turning your Elm data into an `Xml` representation that can be either
    queried or decoded, or turned into a string.

@docs encode

@docs string, int, float, bool, object, null, list

-}

import Dict exposing (Dict)
import String
import Xml exposing (Value(..), encodeXmlEntities)


type alias EncodeSettings =
    { nullValue : String -- if not omitNullTag, encode NullNode like this
    , trueValue : String -- encode True like this
    , falseValue : String -- encode False like this
    , omitNullTag : Bool -- omit a tag if it has no attributes and its only content is a NullNode
    , attributeSingleQuoteInsteadOfDouble : Bool
    }


defaultEncodeSettings =
    { nullValue = ""
    , trueValue = "true"
    , falseValue = "false"
    , omitNullTag = True
    , attributeSingleQuoteInsteadOfDouble = False
    }


boolToString : EncodeSettings -> Bool -> String
boolToString setts b =
    if b then
        setts.trueValue

    else
        setts.falseValue


propToString : EncodeSettings -> Value -> String
propToString setts value =
    case value of
        StrNode str ->
            encodeXmlEntities str

        IntNode n ->
            String.fromInt n

        BoolNode b ->
            boolToString setts b

        FloatNode f ->
            String.fromFloat f

        _ ->
            ""


propsToString : EncodeSettings -> Dict String Value -> String
propsToString setts props =
    let
        quote =
            if setts.attributeSingleQuoteInsteadOfDouble then
                "'"

            else
                "\""
    in
    Dict.toList props
        |> List.map (\( key, value ) -> key ++ "=" ++ quote ++ propToString setts value ++ quote)
        |> String.join " "
        |> (\x ->
                if String.length x > 0 then
                    " " ++ x

                else
                    ""
           )


needsIndent : Value -> Bool
needsIndent nextValue =
    case nextValue of
        Object [] ->
            False

        Object _ ->
            True

        Tag _ _ _ ->
            True

        _ ->
            False


valueToString : EncodeSettings -> Int -> Int -> Value -> String
valueToString setts level indent value =
    case value of
        Tag name props nextValue ->
            if setts.omitNullTag && Dict.isEmpty props && nextValue == NullNode then
                ""

            else
                let
                    indentString =
                        if needsIndent nextValue then
                            "\n"

                        else
                            ""

                    indentClosing =
                        if needsIndent nextValue then
                            String.repeat (level * indent) " "

                        else
                            ""
                in
                String.repeat (level * indent) " "
                    ++ "<"
                    ++ name
                    ++ propsToString setts props
                    ++ ">"
                    ++ indentString
                    ++ valueToString setts level indent nextValue
                    ++ indentString
                    ++ indentClosing
                    ++ "</"
                    ++ name
                    ++ ">"

        StrNode str ->
            encodeXmlEntities str

        IntNode n ->
            String.fromInt n

        FloatNode n ->
            String.fromFloat n

        BoolNode b ->
            boolToString setts b

        NullNode ->
            setts.nullValue

        Object xs ->
            List.map (valueToString setts (level + 1) indent) xs
                |> String.join "\n"

        DocType name props ->
            "<?"
                ++ name
                ++ propsToString setts props
                ++ "?>"


{-| Take a value, then generate a string from it
-}
encode : Int -> Value -> String
encode =
    encodeWith defaultEncodeSettings


{-| Take a value, then generate a string from it
-}
encodeWith : EncodeSettings -> Int -> Value -> String
encodeWith setts indent value =
    valueToString setts -1 indent value


{-| Encode a string

    string "hello" |> encode 0
    --> "hello"

    string "<hello>" |> encode 0
    --> "&lt;hello&gt;"

-}
string : String -> Value
string =
    StrNode


{-| Encode an int

    int 15 |> encode 0
    --> "15"

-}
int : Int -> Value
int n =
    IntNode n


{-| Encode a float

    float 1.576 |> encode 0
    --> "1.576"

-}
float : Float -> Value
float n =
    FloatNode n


{-| Encode a bool

    bool True |> encode 0
    --> "true"

    bool True |> encode 0
    --> "true"

-}
bool : Bool -> Value
bool b =
    BoolNode b


{-| Encode an "object" (a tag)
-}
object : List ( String, Dict String Value, Value ) -> Value
object values =
    List.map (\( name, props, value ) -> Tag name props value) values
        |> Object


{-| Encode a list of nodes, e.g

    import Dict

    list [ object [ ("Root", Dict.empty, null) ], int 5 ] |> encode 0
    --> "<Root></Root>\n5"

-}
list : List Value -> Value
list values =
    Object values


{-| Empty contents

    null |> encode 0
    --> ""

-}
null : Value
null =
    object []
