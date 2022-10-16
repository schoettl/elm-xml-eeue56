# elm-xml [![Build Status](https://travis-ci.org/billstclair/elm-xml-eeue56.svg?branch=master)](https://travis-ci.org/billstclair/elm-xml-eeue56)
xml parser for elm

[Note that as of version 2.0.0, `xmlToJson` is renamed to `xmlToJson2`, since it now represents `Tag` objects differently]

First bring XML into Elm as a `Value`. Once imported as a Value, you can then either query the values with `Xml.Query`.

Or you can turn it back to a string using `Xml.Encode.encode`. Or pull it apart using `Xml.Encode.Value`.

In order to turn an `Xml.Value` into a record, you probably want `Xml.Query`, paired with `Result.map`. Or use `xmlToJson` and your existing JSON decoder.

```elm

import Xml exposing (Value)
import Xml.Encode exposing (null)
import Xml.Decode exposing (decode)
import Xml.Query exposing (tags)

decodedXml : Value
decodedXml =
	"""
<person>
	<name>noah</name>
	<age max="100">50</age>
</person>
<person>
	<name>josh</name>
	<age max="100">57</age>
</person>
	"""
		|> decode
		|> Result.toMaybe
		|> Maybe.withDefault null


type alias Person =
	{ name: String
	, age: Int
	}

person : Value -> Result String Person
person value =
    Result.map2
        (\name age ->
            { name = name
            , age = age
            }
        )
        (tag "name" string value)
        (tag "age" int value)


people : List Person
people =
    tags "person" decodedXml
        |> collect person


```

What's *not* supported yet:

- [ ] Single-quoted attribute values (e.g. `<elem attr='val'>`)
- [ ] Empty attributes are discarded (e.g. `<elem attr="">`)
- [ ] Some special characters and consecutive whitespace in attribute values are not allowed (e.g. `<elem attr="=">`)
- [ ] Element and attribute names are not checked for validity in `object` and `jsonToXml` which can lead to invalid XML.
- [ ] …

See skipped tests in [Tests.elm](tests/Tests.elm).
