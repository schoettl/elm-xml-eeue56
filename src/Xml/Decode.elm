module Xml.Decode exposing
    ( decode
    , decodeInt, decodeFloat, decodeString, decodeBool
    , decodeChildren
    )

{-|

@docs decode

@docs decodeInt, decodeFloat, decodeString, decodeBool

@docs decodeChildren

-}

import Dict
import Regex exposing (Regex)
import Xml exposing (Value(..), decodeXmlEntities)
import Xml.Encode as Encode


type alias DecodeSettings =
    { nullValues : List String
    , trueValues : List String
    , falseValues : List String
    }


defaultDecodeSettings =
    { nullValues = [ "" ], trueValues = [ "true" ], falseValues = [ "false" ] }


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
        [ decodeNull setts, decodeBool setts, decodeInt, decodeFloat, decodeString ]


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
                            Just ( name, v )

                _ ->
                    Nothing
        )


propRegex : Maybe Regex.Regex
propRegex =
    Regex.fromString " .+?=\".+?\""


findProps : DecodeSettings -> List String -> Dict.Dict String Value
findProps setts =
    case propRegex of
        Nothing ->
            \_ -> Dict.empty

        Just regex ->
            List.tail
                >> Maybe.withDefault []
                >> String.join " "
                >> (\s -> " " ++ s)
                >> Regex.find regex
                >> List.map (.match >> String.trim)
                >> parseProps setts
                >> Dict.fromList


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
            findProps setts words

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
            case decodeChildren contents of
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


{-| Decode a int

    import Xml exposing (Value(..))

    decodeInt "hello"
    --> Err "could not convert string 'hello' to an Int"

    decodeInt "5"
    --> Ok (IntNode 5)

-}
decodeInt : String -> Result String Value
decodeInt str =
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
decodeFloat str =
    case String.toFloat str of
        Nothing ->
            Err <| "could not convert string '" ++ str ++ "' to a Float"

        Just v ->
            FloatNode v
                |> Ok


{-| Decode a bool
-}
decodeBool : DecodeSettings -> String -> Result String Value
decodeBool setts str =
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

    decodeChildren "<name>hello</name>"
    --> Ok (Object [Tag "name" Dict.empty (StrNode "hello")] )

-}
decodeChildren : String -> Result String Value
decodeChildren str =
    List.foldl
        (\decoder val ->
            case val of
                Ok _ ->
                    val

                Err _ ->
                    decoder str
        )
        (Err "")
        [ decode, decodeInt, decodeFloat, decodeString ]
