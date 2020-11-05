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
        parseSubsequentItems : Token Parser.Problem -> ( Markdown.Block.Loose, ListItem ) -> Parser (List ( Markdown.Block.Loose, ListItem ))
        parseSubsequentItems listMarker firstItem =
            loop [] (statementsHelp (singleItemParser listMarker) firstItem)
    in
    succeed parseSubsequentItems
        |= backtrackable listMarkerParser
        |. oneOrMore Helpers.isSpaceOrTab
        |= ListItem.parser
        |> andThen identity
        -- TODO: parse whether is loose
        |> map
            (\listItems ->
                ( if
                    listItems
                        |> List.map Tuple.first
                        |> List.any ((==) Markdown.Block.IsLoose)
                  then
                    Markdown.Block.IsLoose

                  else
                    Markdown.Block.IsTight
                , listItems
                    |> List.map Tuple.second
                )
            )


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


singleItemParser : Token Parser.Problem -> Parser ( Markdown.Block.Loose, ListItem )
singleItemParser listMarker =
    succeed identity
        |. backtrackable
            (succeed listMarker
                |. Helpers.upToThreeSpaces
                |. symbol listMarker
            )
        |= itemBody


itemBody : Parser ( Markdown.Block.Loose, ListItem )
itemBody =
    oneOf
        [ succeed identity
            |. backtrackable (oneOrMore Helpers.isSpaceOrTab)
            |= ListItem.parser
        , succeed ( Markdown.Block.IsTight, ListItem.PlainItem "" )
            |. Advanced.symbol Token.newline
        ]


statementsHelp :
    Parser ( Markdown.Block.Loose, ListItem )
    -> ( Markdown.Block.Loose, ListItem )
    -> List ( Markdown.Block.Loose, ListItem )
    -> Parser (Step (List ( Markdown.Block.Loose, ListItem )) (List ( Markdown.Block.Loose, ListItem )))
statementsHelp itemParser firstItem revStmts =
    oneOf
        [ itemParser
            |> Advanced.map (\stmt -> Loop (stmt :: revStmts))
        , succeed (Done (firstItem :: List.reverse revStmts))
        ]
