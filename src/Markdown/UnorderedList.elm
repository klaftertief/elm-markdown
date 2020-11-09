module Markdown.UnorderedList exposing (Info, Marker, listItemItemParser, parser)

import Helpers
import Markdown.Block
import Markdown.ListItem as ListItem exposing (ListItem)
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra exposing (oneOrMore)
import Parser.Token as Token


type alias Info =
    { marker : Marker

    --, offset : Int
    --, padding : Int
    }


type Marker
    = Minus
    | Plus
    | Asterisk


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parser : Parser (List ListItem)
parser =
    let
        parseSubsequentItems : Token Parser.Problem -> ListItem -> Parser (List ListItem)
        parseSubsequentItems listMarker firstItem =
            loop [] (statementsHelp (singleItemParser listMarker) firstItem)
    in
    succeed parseSubsequentItems
        |= backtrackable listMarkerParser
        |. oneOrMore Helpers.isSpaceOrTab
        |= ListItem.parser
        |> andThen identity


listMarkerParser : Parser (Token Parser.Problem)
listMarkerParser =
    Advanced.oneOf
        [ succeed Token.minus
            |. Helpers.upToThreeSpaces
            |. symbol Token.minus
        , succeed Token.plus
            |. Helpers.upToThreeSpaces
            |. symbol Token.plus
        , succeed Token.asterisk
            |. Helpers.upToThreeSpaces
            |. symbol Token.asterisk
        ]


listItemItemParser : Parser ( Info, ListItem )
listItemItemParser =
    Advanced.oneOf
        [ singleItemParser Token.minus
            |> map (\listItem -> ( { marker = Minus }, listItem ))
        , singleItemParser Token.plus
            |> map (\listItem -> ( { marker = Plus }, listItem ))
        , singleItemParser Token.asterisk
            |> map (\listItem -> ( { marker = Asterisk }, listItem ))
        ]


singleItemParser : Token Parser.Problem -> Parser ListItem
singleItemParser listMarker =
    succeed identity
        |. backtrackable
            (succeed listMarker
                |. Helpers.upToThreeSpaces
                |. symbol listMarker
            )
        |= itemBody


itemBody : Parser ListItem
itemBody =
    oneOf
        [ succeed identity
            |. backtrackable (oneOrMore Helpers.isSpaceOrTab)
            |= ListItem.parser
        , succeed (ListItem.PlainItem "")
            |. Advanced.symbol Token.newline
        ]


statementsHelp :
    Parser ListItem
    -> ListItem
    -> List ListItem
    -> Parser (Step (List ListItem) (List ListItem))
statementsHelp itemParser firstItem revStmts =
    oneOf
        [ itemParser
            |> Advanced.map (\stmt -> Loop (stmt :: revStmts))
        , succeed (Done (firstItem :: List.reverse revStmts))
        ]
