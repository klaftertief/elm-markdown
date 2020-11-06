module Markdown.ListItem exposing (Completion(..), ListItem(..), parser)

import Helpers exposing (endOfLineOrFile)
import Markdown.Block
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra exposing (zeroOrMore)


type ListItem
    = TaskItem Completion String
    | PlainItem String


type alias Info =
    { marker : Marker
    , offset : Int
    , padding : Int
    }


type Marker
    = Minus
    | Plus
    | Asterisk


type Completion
    = Incomplete
    | Complete


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parser : Parser ListItem
parser =
    oneOf
        [ succeed TaskItem
            |= taskItemParser
            |. zeroOrMore Helpers.isSpaceOrTab
        , succeed PlainItem
        ]
        |= Advanced.getChompedString (Advanced.chompUntilEndOr "\n")
        |. endOfLineOrFile


taskItemParser : Parser Completion
taskItemParser =
    oneOf
        [ succeed Complete
            |. Advanced.symbol (Advanced.Token "[x] " (Parser.ExpectingSymbol "[x] "))
        , succeed Complete
            |. Advanced.symbol (Advanced.Token "[X] " (Parser.ExpectingSymbol "[X] "))
        , succeed Incomplete
            |. Advanced.symbol (Advanced.Token "[ ] " (Parser.ExpectingSymbol "[ ] "))
        ]
