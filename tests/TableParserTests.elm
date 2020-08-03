module TableParserTests exposing (delimiterParsingSuite, fullTableSuite, rowParsingSuite)

import Expect exposing (Expectation)
import Markdown.Block exposing (Alignment(..))
import Markdown.Table
import Markdown.TableParser exposing (..)
import Parser
import Parser.Advanced as Advanced exposing (..)
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


expectDelimiterRowOk : String -> List (Maybe Alignment) -> Expectation
expectDelimiterRowOk testString columns =
    testString
        |> Advanced.run delimiterRowParser
        |> Expect.equal
            (Ok (DelimiterRow (String.trim testString) columns))


delimiterParsingSuite : Test
delimiterParsingSuite =
    describe "delimiter row"
        [ test "single column with pipes" <|
            \() ->
                expectDelimiterRowOk "|---|" [ Nothing ]
        , test "two columns" <|
            \() ->
                expectDelimiterRowOk "|--|--|" [ Nothing, Nothing ]
        , test "no leading" <|
            \() ->
                expectDelimiterRowOk "--|--|" [ Nothing, Nothing ]
        , test "no trailing" <|
            \() ->
                expectDelimiterRowOk "|--|--" [ Nothing, Nothing ]
        , test "no leading or trailing" <|
            \() ->
                expectDelimiterRowOk "--|--" [ Nothing, Nothing ]
        , test "only a single hyphen per column" <|
            \() ->
                expectDelimiterRowOk "- | -" [ Nothing, Nothing ]
        , test "delimiter row with no trailing or leading pipes" <|
            \() ->
                "--"
                    |> expectParserFail delimiterRowParser
        , test "delimiter row with space padding" <|
            \() ->
                expectDelimiterRowOk "| -- |-- | --   |" [ Nothing, Nothing, Nothing ]
        , test "delimiter row with space padding and no leading" <|
            \() ->
                expectDelimiterRowOk "-- |-- | --   |" [ Nothing, Nothing, Nothing ]
        , test "delimiter row with space padding and no trailing" <|
            \() ->
                expectDelimiterRowOk "| -- |-- | --   " [ Nothing, Nothing, Nothing ]
        , test "delimiter rows cannot have spaces between the hyphens" <|
            \() ->
                "|---|- -|"
                    |> expectParserFail delimiterRowParser
        , test "delimiter row with alignment in columns" <|
            \() ->
                expectDelimiterRowOk "| :-- |-- | :-: | ---:   " [ Just AlignLeft, Nothing, Just AlignCenter, Just AlignRight ]
        ]


rowParsingSuite : Test
rowParsingSuite =
    describe "row parser"
        [ test "simple row" <|
            \() ->
                "| abc | def |"
                    |> Advanced.run rowParser
                    |> Expect.equal
                        (Ok
                            [ "abc", "def" ]
                        )
        , test "single row" <|
            \() ->
                "| abc |"
                    |> Advanced.run rowParser
                    |> Expect.equal
                        (Ok
                            [ "abc" ]
                        )
        , test "row without trailing or leading pipes" <|
            \() ->
                "cell 1 | cell 2 | cell 3"
                    |> Advanced.run rowParser
                    |> Expect.equal
                        (Ok
                            [ "cell 1", "cell 2", "cell 3" ]
                        )
        , test "row with escaped pipes" <|
            \() ->
                "| abc | a \\| b |"
                    |> Advanced.run rowParser
                    |> Expect.equal
                        (Ok
                            [ "abc", "a | b" ]
                        )
        , test "row with escaped pipes at the end" <|
            \() ->
                "| abc | def\\|"
                    |> Advanced.run rowParser
                    |> Expect.equal
                        (Ok
                            [ "abc", "def|" ]
                        )
        , test "row with escaped pipes at the beginning" <|
            \() ->
                "\\|  abc | def |"
                    |> Advanced.run rowParser
                    |> Expect.equal
                        (Ok
                            [ "|  abc", "def" ]
                        )
        , test "row with empty cell contents" <|
            \() ->
                "| abc |  |"
                    |> Advanced.run rowParser
                    |> Expect.equal
                        (Ok
                            [ "abc", "" ]
                        )
        ]


fullTableSuite : Test
fullTableSuite =
    describe "GFM tables"
        [ test "simple case" <|
            \() ->
                """| abc | def |
|---|---|"""
                    |> Advanced.run parser
                    |> Expect.equal
                        (Ok
                            (Markdown.Table.Table
                                [ { label = "abc", alignment = Nothing }
                                , { label = "def", alignment = Nothing }
                                ]
                                []
                            )
                        )
        , test "simple case with trailing whitespace" <|
            \() ->
                """| abc | def |
|---|---|
"""
                    |> Advanced.run parser
                    |> Expect.equal
                        (Ok
                            (Markdown.Table.Table
                                [ { label = "abc", alignment = Nothing }
                                , { label = "def", alignment = Nothing }
                                ]
                                []
                            )
                        )
        , test "The delimiter row cannot have fewer columns than the header" <|
            \() ->
                """| abc | def |
|---|
"""
                    |> expectFail
        , test "The delimiter row cannot have more columns than the header" <|
            \() ->
                """| abc | def |
|---|--|--|
"""
                    |> expectFail
        , test "tables must have at least one delimiter" <|
            \() ->
                """| abc | def |
|"""
                    |> expectFail
        , test "table must have a delimiter row" <|
            \() ->
                """| abc | def |

Hey, I forgot to finish my table! Whoops!
                           """
                    |> expectFail
        , test "table must have a delimiter row before body rows" <|
            \() ->
                """| abc | def |
| foo | bar |
| bar | baz |
"""
                    |> expectFail
        , test "tables have data rows" <|
            \() ->
                """| abc | def |
| --- | --- |
| foo | bar |
| bar | baz |
"""
                    |> Advanced.run parser
                    |> Expect.equal
                        (Ok
                            (Markdown.Table.Table
                                [ { label = "abc", alignment = Nothing }
                                , { label = "def", alignment = Nothing }
                                ]
                                [ [ "foo", "bar" ]
                                , [ "bar", "baz" ]
                                ]
                            )
                        )
        , test "the data rows can have varying length but the result should be even" <|
            \() ->
                """| abc | def |
| --- | --- |
| bar |
| bar | baz | boo |
"""
                    |> Advanced.run parser
                    |> Expect.equal
                        (Ok
                            (Markdown.Table.Table
                                [ { label = "abc", alignment = Nothing }
                                , { label = "def", alignment = Nothing }
                                ]
                                [ [ "bar", "" ]
                                , [ "bar", "baz" ]
                                ]
                            )
                        )
        , test "tables without surrounding pipes" <|
            \() ->
                """abc | def
--- | ---
foo | bar
bar | baz
"""
                    |> Advanced.run parser
                    |> Expect.equal
                        (Ok
                            (Markdown.Table.Table
                                [ { label = "abc", alignment = Nothing }
                                , { label = "def", alignment = Nothing }
                                ]
                                [ [ "foo", "bar" ]
                                , [ "bar", "baz" ]
                                ]
                            )
                        )
        , test "tables without body and without surrounding pipes" <|
            \() ->
                """abc | def
--- | ---
"""
                    |> Advanced.run parser
                    |> Expect.equal
                        (Ok
                            (Markdown.Table.Table
                                [ { label = "abc", alignment = Nothing }
                                , { label = "def", alignment = Nothing }
                                ]
                                []
                            )
                        )
        , test "tables with only a single column and the delimiter does NOT have surrounding pipes" <|
            \() ->
                """| abc |
---
bar

"""
                    |> expectFail
        , test "tables with only a single column and the delimiter has surrounding pipes" <|
            \() ->
                """| abc |
| --- |
bar
"""
                    |> Advanced.run parser
                    |> Expect.equal
                        (Ok
                            (Markdown.Table.Table
                                [ { label = "abc", alignment = Nothing }
                                ]
                                [ [ "bar" ] ]
                            )
                        )
        , test "tables with only a single column and the delimiter has surrounding pipes but the header does not" <|
            \() ->
                """abc
| --- |
bar
"""
                    |> Advanced.run parser
                    |> Expect.equal
                        (Ok
                            (Markdown.Table.Table
                                [ { label = "abc", alignment = Nothing }
                                ]
                                [ [ "bar" ] ]
                            )
                        )
        ]


expectParserFail someParser input =
    case Advanced.run someParser input of
        Ok _ ->
            Expect.fail "Expected a parser error."

        Err _ ->
            Expect.pass


expectFail input =
    case Advanced.run parser input of
        Ok _ ->
            Expect.fail "Expected a parser error."

        Err _ ->
            Expect.pass
