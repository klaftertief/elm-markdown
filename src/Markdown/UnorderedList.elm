module Markdown.UnorderedList exposing (Info, Marker(..), listItemItemParser, markerToString, parser)

import Helpers
import Markdown.ListItem as ListItem exposing (ListItem)
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra
import Parser.Token as Token


type alias Info =
    { marker : Marker
    , offset : Int
    , padding : Int
    }


type Marker
    = Minus
    | Plus
    | Asterisk


markerToString : Marker -> String
markerToString marker =
    case marker of
        Minus ->
            "-"

        Plus ->
            "+"

        Asterisk ->
            "*"


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parser : Parser (List ListItem)
parser =
    let
        parseSubsequentItems : ( Token Parser.Problem, Int ) -> ( Int, ListItem ) -> Parser (List ( Int, Int, ListItem ))
        parseSubsequentItems ( listMarker, offset ) ( padding, firstItem ) =
            loop [] (statementsHelp (singleItemParser listMarker) ( offset, padding, firstItem ))
    in
    succeed parseSubsequentItems
        |= backtrackable listMarkerParser
        --|= Parser.Extra.countOneOrMore Helpers.isSpaceOrTab
        |= itemBody
        |> andThen identity
        |> map (List.map (\( _, _, item ) -> item))


listMarkerParser : Parser ( Token Parser.Problem, Int )
listMarkerParser =
    Advanced.oneOf
        [ succeed (Tuple.pair Token.minus)
            |= Helpers.countUpToThreeSpaces
            |. symbol Token.minus
        , succeed (Tuple.pair Token.plus)
            |= Helpers.countUpToThreeSpaces
            |. symbol Token.plus
        , succeed (Tuple.pair Token.asterisk)
            |= Helpers.countUpToThreeSpaces
            |. symbol Token.asterisk
        ]


singleItemParser : Token Parser.Problem -> Parser ( Int, Int, ListItem )
singleItemParser listMarker =
    succeed (\offset ( padding, item ) -> ( offset, padding, item ))
        |= backtrackable
            (succeed identity
                |= Helpers.countUpToThreeSpaces
                |. symbol listMarker
            )
        |= itemBody


itemBody : Parser ( Int, ListItem )
itemBody =
    oneOf
        [ succeed Tuple.pair
            |= backtrackable (Parser.Extra.countOneOrMore Helpers.isSpaceOrTab)
            |= ListItem.parser
        , succeed ( 0, ListItem.PlainItem "" )
            |. Advanced.symbol Token.newline
        ]


statementsHelp :
    Parser ( Int, Int, ListItem )
    -> ( Int, Int, ListItem )
    -> List ( Int, Int, ListItem )
    -> Parser (Step (List ( Int, Int, ListItem )) (List ( Int, Int, ListItem )))
statementsHelp itemParser firstItem revStmts =
    oneOf
        [ itemParser
            |> Advanced.map (\stmt -> Loop (stmt :: revStmts))
        , blankLine
            |> Advanced.map (\_ -> Loop revStmts)
        , succeed (Done (firstItem :: List.reverse revStmts))
        ]


blankLine : Parser ()
blankLine =
    Advanced.backtrackable (chompWhile Helpers.isSpaceOrTab)
        |. symbol Token.newline



--


listItemItemParser : Parser ( Info, ListItem )
listItemItemParser =
    let
        withInfo marker ( offset, padding, listItem ) =
            ( { offset = offset
              , padding = padding
              , marker = marker
              }
            , listItem
            )
    in
    Advanced.oneOf
        [ singleItemParser Token.minus
            |> map (withInfo Minus)
        , singleItemParser Token.plus
            |> map (withInfo Plus)
        , singleItemParser Token.asterisk
            |> map (withInfo Asterisk)
        ]
