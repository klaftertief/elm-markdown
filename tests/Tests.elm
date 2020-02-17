module Tests exposing (suite)

import Expect exposing (Expectation)
import Markdown.Block as Block exposing (Block)
import Markdown.Inline as Inline exposing (Inline)
import Markdown.Parser as Markdown exposing (..)
import Parser
import Parser.Advanced as Advanced
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parse : String -> Result (List (Advanced.DeadEnd String Parser.Problem)) (List Block)
parse markdown =
    markdown
        |> Markdown.parse


suite : Test
suite =
    describe "parsing"
        [ describe "headings"
            [ test "Heading 1" <|
                \() ->
                    "# Hello!"
                        |> parse
                        |> Expect.equal (Ok [ Block.Heading 1 (unstyledText "Hello!") ])
            , test "heading can end with trailing #'s'" <|
                \() ->
                    "# Hello! ###"
                        |> parse
                        |> Expect.equal (Ok [ Block.Heading 1 (unstyledText "Hello!") ])
            , test "Heading 2" <|
                \() ->
                    "## Hello!"
                        |> parse
                        |> Expect.equal (Ok [ Block.Heading 2 (unstyledText "Hello!") ])
            , test "Emphasis line is not interpreted as a list" <|
                \() ->
                    "*This is not a list, it's a paragraph with emphasis*\n"
                        |> parse
                        |> Expect.equal (Ok [ Block.Body (emphasisText "This is not a list, it's a paragraph with emphasis") ])
            , test "Line starting with a decimal is not interpreted as a list" <|
                \() ->
                    "3.5 is a number - is not a list\n"
                        |> parse
                        |> Expect.equal (Ok [ Block.Body (unstyledText "3.5 is a number - is not a list") ])
            , test "Heading 7 is invalid" <|
                \() ->
                    "####### Hello!"
                        |> parserError
            ]
        , test "plain text" <|
            \() ->
                "This is just some text"
                    |> parse
                    |> Expect.equal (Ok [ Block.Body (unstyledText "This is just some text") ])
        , test "parse heading then plain text" <|
            \() ->
                """# Heading
This is just some text
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Heading 1 (unstyledText "Heading")
                            , Block.Body (unstyledText "This is just some text")
                            ]
                        )
        , test "doesn't need to end in newline" <|
            \() ->
                """# Heading
This is just some text"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Heading 1 (unstyledText "Heading")
                            , Block.Body (unstyledText "This is just some text")
                            ]
                        )
        , test "long example" <|
            \() ->
                """# Heading

This is just some text.

## Subheading

Body of the subheading.
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Heading 1 (unstyledText "Heading")
                            , Block.Body (unstyledText "This is just some text.")
                            , Block.Heading 2 (unstyledText "Subheading")
                            , Block.Body (unstyledText "Body of the subheading.")
                            ]
                        )
        , test "embedded HTML" <|
            \() ->
                """# Heading
<div>
Hello!
</div>
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Heading 1 (unstyledText "Heading")
                            , Block.Html "div"
                                []
                                [ Block.Body (unstyledText "Hello!")
                                ]
                            ]
                        )
        , test "heading within HTML" <|
            \() ->
                """# Heading
<div>
# Heading in a div!

</div>
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Heading 1 (unstyledText "Heading")
                            , Block.Html "div"
                                []
                                [ Block.Heading 1 (unstyledText "Heading in a div!")
                                ]
                            ]
                        )
        , test "simple list" <|
            \() ->
                """- One
- Two
- Three
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.UnorderedListBlock
                                [ plainListItem "One"
                                , plainListItem "Two"
                                , plainListItem "Three"
                                ]

                            -- TODO why is this extra block here? Fix
                            -- , ListBlock []
                            ]
                        )
        , test "sibling unordered lists with different markers" <|
            \() ->
                """- Item 1
- Item 2
- Item 3
+ Item 4
+ Item 5
+ Item 6
* Item 7
* Item 8
* Item 9
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.UnorderedListBlock
                                [ plainListItem "Item 1"
                                , plainListItem "Item 2"
                                , plainListItem "Item 3"
                                ]
                            , Block.UnorderedListBlock
                                [ plainListItem "Item 4"
                                , plainListItem "Item 5"
                                , plainListItem "Item 6"
                                ]
                            , Block.UnorderedListBlock
                                [ plainListItem "Item 7"
                                , plainListItem "Item 8"
                                , plainListItem "Item 9"
                                ]
                            ]
                        )
        , test "sibling ordered lists with different markers" <|
            \() ->
                """1. foo
2. bar
3) baz
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.OrderedListBlock 1
                                [ unstyledText "foo"
                                , unstyledText "bar"
                                ]
                            , Block.OrderedListBlock 3
                                [ unstyledText "baz"
                                ]
                            ]
                        )
        , test "A paragraph with a numeral that is NOT 1 in the text before a blank line" <|
            \() ->
                """The number of windows in my house is
14.  The number of doors is 6."""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Body (unstyledText """The number of windows in my house is 14.  The number of doors is 6.""")
                            ]
                        )
        , test "A paragraph with a numeral that IS 1 in the text" <|
            \() ->
                """The number of windows in my house is
1.  The number of doors is 6.
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Body (unstyledText "The number of windows in my house is")
                            , Block.OrderedListBlock 1
                                [ unstyledText "The number of doors is 6."
                                ]
                            ]
                        )
        , test "thematic break" <|
            \() ->
                """---"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.ThematicBreak
                            ]
                        )
        , test "thematic break followed by newline" <|
            \() ->
                """---
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.ThematicBreak
                            ]
                        )
        , test "blank lines are ignored" <|
            \() ->
                " \n  \n    \n\t\n"
                    |> parse
                    |> Expect.equal (Ok [])
        , test "mixed content with list" <|
            \() ->
                """# Title

- This is an item
- And so is this

Text after
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Heading 1 (unstyledText "Title")
                            , Block.UnorderedListBlock
                                [ plainListItem "This is an item"
                                , plainListItem "And so is this"
                                ]
                            , Block.Body (unstyledText "Text after")

                            -- TODO why is this extra block here? Fix
                            -- , ListBlock []
                            ]
                        )
        , test "code fence with paragraph and heading below" <|
            \() ->
                """```shell
.
├── content/
├── elm.json
├── images/
├── static/
├── index.js
├── package.json
└── src/
    └── Main.elm
```

This is more stuff

## h2

qwer
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.CodeBlock
                                { body = ".\n├── content/\n├── elm.json\n├── images/\n├── static/\n├── index.js\n├── package.json\n└── src/\n    └── Main.elm"
                                , language = Just "shell"
                                }
                            , Block.Body (unstyledText "This is more stuff")
                            , Block.Heading 2 (unstyledText "h2")
                            , Block.Body (unstyledText "qwer")
                            ]
                        )
        , test "indented code block" <|
            \() ->
                """    foo = 123"""
                    |> parse
                    |> Expect.equal (Ok [ Block.CodeBlock { body = "foo = 123", language = Nothing } ])
        , test "indented code block with tab" <|
            \() ->
                """\tfoo = 123"""
                    |> parse
                    |> Expect.equal (Ok [ Block.CodeBlock { body = "foo = 123", language = Nothing } ])
        , test "image" <|
            \() ->
                """![This is an image](/my/image.jpg)"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Body
                                [ Inline.Image "/my/image.jpg" Nothing []

                                --{ string = "This is an image"
                                -- , style =
                                --       { isBold = False
                                --       , isCode = False
                                --       , isItalic = False
                                --       , link = Just { destination = Block.Image "/my/image.jpg", title = Nothing }
                                --       }
                                --
                                -- }
                                ]
                            ]
                        )
        , describe "blank line"
            [ test "even though paragraphs can start with blank lines, it is not a paragraph if there are only blanks" <|
                \() ->
                    "  \n"
                        |> parse
                        |> Expect.equal (Ok [])
            ]
        , describe "block quotes"
            [ test "Simple block quote" <|
                \() ->
                    ">This is a quote\n"
                        |> parse
                        |> Expect.equal (Ok [ Block.BlockQuote [ Block.Body (unstyledText "This is a quote") ] ])
            , test "block quote with a space after" <|
                \() ->
                    "> This is a quote\n"
                        |> parse
                        |> Expect.equal (Ok [ Block.BlockQuote [ Block.Body (unstyledText "This is a quote") ] ])
            , test "consecutive block quote lines are combined" <|
                \() ->
                    """> # Heading
> Body
"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Block.BlockQuote
                                    [ Block.Heading 1 (unstyledText "Heading")
                                    , Block.Body (unstyledText "Body")
                                    ]
                                ]
                            )
            , test "plain lines immediately after block quote lines are combined" <|
                \() ->
                    """> # Heading
I'm part of the block quote
"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Block.BlockQuote
                                    [ Block.Heading 1 (unstyledText "Heading")
                                    , Block.Body (unstyledText "I'm part of the block quote")
                                    ]
                                ]
                            )
            ]
        , test "indented code" <|
            \() ->
                """    sum a b =
      a + b
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.CodeBlock
                                { body = "sum a b =\n  a + b"
                                , language = Nothing
                                }
                            ]
                        )
        , test "block quotes eat the first space and allow paragraphs to start with 3 spaces" <|
            \() ->
                """>     code

>    not code
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.BlockQuote
                                [ Block.CodeBlock
                                    { body = "code"
                                    , language = Nothing
                                    }
                                ]
                            , Block.BlockQuote
                                [ Block.Body (unstyledText "not code")
                                ]
                            ]
                        )
        , test "blank lines separate paragraphs within block quote" <|
            \() ->
                """> foo
>
> bar
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.BlockQuote
                                [ Block.Body (unstyledText "foo")
                                , Block.Body (unstyledText "bar")
                                ]
                            ]
                        )
        , test "keeps items grouped in a paragraph within block quotes when there are no blank lines separating them" <|
            \() ->
                """> # Foo
> bar
> baz
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.BlockQuote
                                [ Block.Heading 1 (unstyledText "Foo")
                                , Block.Body (unstyledText "bar baz")
                                ]
                            ]
                        )
        ]


plainListItem : String -> { body : List Inline, task : Maybe Bool }
plainListItem body =
    { body = [ Inline.Text body ]
    , task = Nothing
    }


unstyledText : String -> List Inline
unstyledText body =
    [ Inline.Text body ]


emphasisText : String -> List Inline
emphasisText body =
    [ Inline.Emphasis 1 <|
        [ Inline.Text body ]
    ]


parserError : String -> Expect.Expectation
parserError markdown =
    case parse markdown of
        Ok _ ->
            Expect.fail "Expected a parser failure!"

        Err _ ->
            Expect.pass
