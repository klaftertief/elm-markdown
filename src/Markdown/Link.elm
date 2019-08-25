module Markdown.Link exposing (..)

import Browser
import Char
import Html exposing (Html)
import Html.Attributes as Attr
import Parser
import Parser.Advanced as Advanced exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type alias Link =
    { description : String, title : Maybe String, destination : String }


parser : Parser Link
parser =
    succeed
        (\description ->
            { description = description
            , title = Nothing
            , destination = "/about"
            }
        )
        |. Advanced.symbol (Advanced.Token "[" (Parser.ExpectingSymbol "["))
        |= getChompedString
            (chompUntil (Advanced.Token "]" (Parser.ExpectingSymbol "]")))


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '*' && char /= '`'