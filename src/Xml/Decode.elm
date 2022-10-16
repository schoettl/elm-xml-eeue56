module Xml.Decode exposing
    ( decode, decodeWith, DecodeSettings, defaultDecodeSettings
    , decodeInt, decodeFloat, decodeString, decodeBool
    , decodeChildren
    , decodeBoolWith, decodeFloatWith, decodeIntWith, decodeNull
    )

{-|

@docs decode, decodeWith, DecodeSettings, defaultDecodeSettings

@docs decodeInt, decodeFloat, decodeString, decodeBool

@docs decodeChildren

@docs decodeBoolWith, decodeFloatWith, decodeIntWith, decodeNull

-}

import Dict
import Regex exposing (Regex)
import Xml exposing (Value(..), decodeXmlEntities)
import Xml.Encode as Encode


{-| Settings used by `decodeWith`.

The `*Values` fields define lists of possible values for the
respecitve node types, e.g. `["true", "True"]` for `True`.

`parseNumbers` specifies if numbers are parsed to
`IntNode`/`FloatNode` or just `StrNode`.

-}
type alias DecodeSettings =
    { nullValues : List String
    , trueValues : List String
    , falseValues : List String
    , parseNumbers : Bool
    }


{-| Good default settings for `DecodeSettings`.
-}
defaultDecodeSettings : DecodeSettings
defaultDecodeSettings =
    { nullValues = [ "" ]
    , trueValues = [ "true" ]
    , falseValues = [ "false" ]
    , parseNumbers = True
    }


{-| Try and decode the props from a string
-}
decodeProps : DecodeSettings -> String -> Result String Value
decodeProps setts str =
    List.foldl
        (\decoder val ->
            case val of
                Ok _ ->
                    val

                Err _ ->
                    decoder str
        )
        (Err "")
        [ decodeNull setts, decodeBoolWith setts, decodeIntWith setts, decodeFloatWith setts, decodeString ]


parseProps : DecodeSettings -> List String -> List ( String, Value )
parseProps setts =
    List.filterMap
        (\n ->
            case String.split "=" n of
                [ name, value ] ->
                    let
                        withoutQuotes =
                            value
                                |> String.dropLeft 1
                                |> String.dropRight 1
                    in
                    case decodeProps setts withoutQuotes of
                        Err _ ->
                            Nothing

                        Ok v ->
                            Just ( String.trim name, v )

                _ ->
                    Nothing
        )


propRegex : Maybe Regex.Regex
propRegex =
    Regex.fromString " .+?=(\".*?\"|'.*?')"


findProps : DecodeSettings -> String -> Dict.Dict String Value
findProps setts beforeClose =
    case propRegex of
        Nothing ->
            Dict.empty

        Just regex ->
            beforeClose
                |> Regex.find regex
                |> List.map .match
                |> parseProps setts
                |> Dict.fromList


parseSlice : DecodeSettings -> Int -> Int -> String -> Result String ( Value, Int )
parseSlice setts first firstClose trimmed =
    let
        beforeClose =
            String.slice (first + 1) firstClose trimmed

        words =
            beforeClose
                |> String.words

        tagName =
            words
                |> List.head
                |> Maybe.withDefault ""

        props =
            findProps setts beforeClose

        closeTag =
            "</" ++ tagName ++ ">"
    in
    case String.indexes closeTag trimmed of
        [] ->
            if String.startsWith "?" tagName then
                Ok ( DocType (String.dropLeft 1 tagName) props, firstClose + 1 )

            else if String.endsWith "/" beforeClose then
                let
                    tag =
                        if String.endsWith "/" tagName then
                            String.slice 0 -1 tagName

                        else
                            tagName
                in
                Ok ( Tag tag props (Object []), firstClose + 1 )

            else
                "Failed to find close tag for "
                    ++ tagName
                    |> Err

        firstCloseTag :: otherCloseTags ->
            let
                openTag =
                    "<" ++ tagName

                intermediateNodes =
                    String.indexes openTag trimmed
                        |> List.drop 1

                correctCloseTag =
                    let
                        hasNestedChildrenWithSameName =
                            List.all (\y -> firstCloseTag <= y) intermediateNodes
                                |> not
                    in
                    if hasNestedChildrenWithSameName then
                        (firstCloseTag :: otherCloseTags)
                            |> List.drop (List.length intermediateNodes)
                            |> List.head
                            |> Maybe.withDefault firstCloseTag

                    else
                        firstCloseTag

                contents =
                    String.slice (firstClose + 1) correctCloseTag trimmed
            in
            case decodeChildren setts contents of
                Err s ->
                    Err s

                Ok v ->
                    Ok ( Tag tagName props v, correctCloseTag + String.length closeTag )


actualDecode : DecodeSettings -> String -> Result String (List Value)
actualDecode setts text =
    let
        openIndexes =
            String.indexes "<" text

        closeIndexes =
            String.indexes ">" text
    in
    case ( openIndexes, closeIndexes ) of
        ( first :: restFirst, firstClose :: restFirstClose ) ->
            parseSlice setts first firstClose text
                |> Result.andThen
                    (\( foundValue, firstCloseTag ) ->
                        case actualDecode setts (String.slice firstCloseTag (String.length text + 1) text) of
                            Err err ->
                                if err == "Nothing left" then
                                    Ok [ foundValue ]

                                else
                                    Err ("Parsed to " ++ Encode.encode 0 foundValue ++ ", but then hit " ++ err)

                            Ok thing ->
                                (foundValue :: thing)
                                    |> Ok
                    )

        _ ->
            Err "Nothing left"


{-| Try to decode a string and turn it into an XML value

    import Xml exposing(Value(..))
    import Xml.Encode exposing (null)
    import Dict

    decode "<name></name>"
    --> Ok (Object [Tag "name" Dict.empty null])

-}
decode : String -> Result String Value
decode =
    decodeWith defaultDecodeSettings


{-| Try to decode a string and turn it into an XML value

    import Xml exposing(Value(..))
    import Xml.Encode exposing (null)
    import Dict

    decode "<name></name>"
    --> Ok (Object [Tag "name" Dict.empty null])

-}
decodeWith : DecodeSettings -> String -> Result String Value
decodeWith setts text =
    case String.trim text of
        "" ->
            Ok (Object [])

        trimmed ->
            actualDecode setts trimmed
                |> Result.map Object


{-| Decode a string

    import Xml exposing (Value(..))

    decodeString "hello"
    --> Ok (StrNode "hello")

    decodeString "hello &amp; good bye"
    --> Ok (StrNode "hello & good bye")

-}
decodeString : String -> Result String Value
decodeString str =
    StrNode (decodeXmlEntities str)
        |> Ok


errNumberParsingDisabled : Result String Value
errNumberParsingDisabled =
    Err "number parsing is disabled"


{-| Decode an int

    import Xml exposing (Value(..))

    decodeInt "hello"
    --> Err "could not convert string 'hello' to an Int"

    decodeInt "5"
    --> Ok (IntNode 5)

-}
decodeInt : String -> Result String Value
decodeInt =
    decodeIntWith defaultDecodeSettings


{-| Decode an int with settings
-}
decodeIntWith : DecodeSettings -> String -> Result String Value
decodeIntWith { parseNumbers } str =
    if not parseNumbers then
        errNumberParsingDisabled

    else
        case String.toInt str of
            Nothing ->
                Err <| "could not convert string '" ++ str ++ "' to an Int"

            Just v ->
                IntNode v
                    |> Ok


{-| Decode a float

    import Xml exposing (Value(..))

    decodeFloat "hello"
    --> Err "could not convert string 'hello' to a Float"

    decodeFloat "5"
    --> Ok (FloatNode 5.0)

    decodeFloat "5.5"
    --> Ok (FloatNode 5.5)

-}
decodeFloat : String -> Result String Value
decodeFloat =
    decodeFloatWith defaultDecodeSettings


{-| Decode a float with settings
-}
decodeFloatWith : DecodeSettings -> String -> Result String Value
decodeFloatWith { parseNumbers } str =
    if not parseNumbers then
        errNumberParsingDisabled

    else
        case String.toFloat str of
            Nothing ->
                Err <| "could not convert string '" ++ str ++ "' to a Float"

            Just v ->
                FloatNode v
                    |> Ok


{-| Decode a bool
-}
decodeBool : String -> Result String Value
decodeBool =
    decodeBoolWith defaultDecodeSettings


{-| Decode a bool with settings
-}
decodeBoolWith : DecodeSettings -> String -> Result String Value
decodeBoolWith setts str =
    if List.member str setts.trueValues then
        BoolNode True
            |> Ok

    else if List.member str setts.falseValues then
        BoolNode False
            |> Ok

    else
        Err <|
            "Not a bool. Valid bool values are: "
                ++ String.concat (List.intersperse ", " (List.concat [ setts.falseValues, setts.trueValues ]))


{-| Decode a null
-}
decodeNull : DecodeSettings -> String -> Result String Value
decodeNull setts str =
    if List.member str setts.nullValues then
        Ok NullNode

    else
        Err "Not a null."


{-| Decode children from a string

    import Dict
    import Xml exposing (Value(..))

    decodeChildren defaultDecodeSettings "<name>hello</name>"
    --> Ok (Object [Tag "name" Dict.empty (StrNode "hello")] )

-}
decodeChildren : DecodeSettings -> String -> Result String Value
decodeChildren setts str =
    List.foldl
        (\decoder val ->
            case val of
                Ok _ ->
                    val

                Err _ ->
                    decoder str
        )
        (Err "")
        [ decode, decodeIntWith setts, decodeFloatWith setts, decodeString ]
