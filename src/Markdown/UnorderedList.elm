module Markdown.UnorderedList exposing (parser)

import Helpers
import Markdown.Block
import Markdown.ListItem as ListItem exposing (ListItem)
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra exposing (oneOrMore)
import Parser.Token as Token


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parser : Parser ( Markdown.Block.Loose, List ListItem )
parser =
    let
        parseSubsequentItems listMarker firstItem =
            loop [] (statementsHelp (singleItemParser listMarker) firstItem)
    in
    succeed parseSubsequentItems
        |= backtrackable listMarkerParser
        |. oneOrMore Helpers.isSpaceOrTab
        |= ListItem.parser
        |> andThen identity
        -- TODO: parse whether is loose
        |> map (\listItems -> ( Markdown.Block.IsTight, listItems ))


listMarkerParser : Parser (Token Parser.Problem)
listMarkerParser =
    Advanced.oneOf
        [ succeed Token.minus
            |. symbol Token.minus
        , succeed Token.plus
            |. symbol Token.plus
        , succeed Token.asterisk
            |. symbol Token.asterisk
        ]


singleItemParser : Token Parser.Problem -> Parser ListItem
singleItemParser listMarker =
    succeed identity
        |. backtrackable (symbol listMarker)
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


statementsHelp : Parser ListItem -> ListItem -> List ListItem -> Parser (Step (List ListItem) (List ListItem))
statementsHelp itemParser firstItem revStmts =
    oneOf
        [ itemParser
            |> Advanced.map (\stmt -> Loop (stmt :: revStmts))
        , succeed (Done (firstItem :: List.reverse revStmts))
        ]
