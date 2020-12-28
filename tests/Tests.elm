module Tests exposing (suite)

import Expect exposing (Expectation)
import Markdown.Block as Block exposing (..)
import Markdown.Parser as Markdown exposing (..)
import Parser
import Parser.Advanced as Advanced exposing ((|.), (|=))
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
                        |> Expect.equal (Ok [ Block.Heading Block.H1 (unstyledText "Hello!") ])
            , test "heading can end with trailing #'s'" <|
                \() ->
                    "# Hello! ###"
                        |> parse
                        |> Expect.equal (Ok [ Block.Heading Block.H1 (unstyledText "Hello!") ])
            , test "Heading 2" <|
                \() ->
                    "## Hello!"
                        |> parse
                        |> Expect.equal (Ok [ Block.Heading Block.H2 (unstyledText "Hello!") ])
            , test "Setext heading and not an empty list" <|
                \() ->
                    """Hello!
-
"""
                        |> parse
                        |> Expect.equal (Ok [ Block.Heading Block.H2 (unstyledText "Hello!") ])
            , test "Emphasis line is not interpreted as a list" <|
                \() ->
                    "*This is not a list, it's a paragraph with emphasis*\n"
                        |> parse
                        |> Expect.equal (Ok [ Block.Paragraph (emphasisText "This is not a list, it's a paragraph with emphasis") ])
            , test "Line starting with a decimal is not interpreted as a list" <|
                \() ->
                    "3.5 is a number - is not a list\n"
                        |> parse
                        |> Expect.equal (Ok [ Block.Paragraph (unstyledText "3.5 is a number - is not a list") ])
            , test "Heading 7 is parsed using fallback parsing" <|
                \() ->
                    "####### Hello!"
                        |> expectOk [ Block.Paragraph [ Text "####### Hello!" ] ]
            ]
        , test "plain text" <|
            \() ->
                "This is just some text"
                    |> parse
                    |> Expect.equal (Ok [ Block.Paragraph (unstyledText "This is just some text") ])
        , test "parse heading then plain text" <|
            \() ->
                """# Heading
This is just some text
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Heading Block.H1 (unstyledText "Heading")
                            , Block.Paragraph (unstyledText "This is just some text")
                            ]
                        )
        , test "doesn't need to end in newline" <|
            \() ->
                """# Heading
This is just some text"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Heading Block.H1 (unstyledText "Heading")
                            , Block.Paragraph (unstyledText "This is just some text")
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
                            [ Block.Heading Block.H1 (unstyledText "Heading")
                            , Block.Paragraph (unstyledText "This is just some text.")
                            , Block.Heading Block.H2 (unstyledText "Subheading")
                            , Block.Paragraph (unstyledText "Body of the subheading.")
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
                            [ Block.Heading Block.H1 (unstyledText "Heading")
                            , Block.HtmlBlock
                                (Block.HtmlElement "div"
                                    []
                                    [ Block.Paragraph (unstyledText "Hello!")
                                    ]
                                )
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
                            [ Block.Heading Block.H1 (unstyledText "Heading")
                            , Block.HtmlBlock
                                (Block.HtmlElement "div"
                                    []
                                    [ Block.Heading Block.H1 (unstyledText "Heading in a div!")
                                    ]
                                )
                            ]
                        )
        , describe "lists"
            [ test "simple list" <|
                \() ->
                    """- One
- Two
- Three
"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Block.UnorderedList Block.IsTight
                                    [ plainListItem "One"
                                    , plainListItem "Two"
                                    , plainListItem "Three"
                                    ]

                                -- TODO why is this extra block here? Fix
                                -- , ListBlock []
                                ]
                            )
            , test "list with empty item" <|
                \() ->
                    """- One
-
- Three
"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Block.UnorderedList Block.IsTight
                                    [ plainListItem "One"
                                    , emptyListItem
                                    , plainListItem "Three"
                                    ]
                                ]
                            )
            , test "list with different indentations" <|
                \() ->
                    """ - One
- Two
 - Three
"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Block.UnorderedList Block.IsTight
                                    [ plainListItem "One"
                                    , plainListItem "Two"
                                    , plainListItem "Three"
                                    ]
                                ]
                            )
            , test "single list item" <|
                \() ->
                    """- Single
"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Block.UnorderedList Block.IsTight
                                    [ plainListItem "Single"
                                    ]
                                ]
                            )
            , test "single list item followed by new block" <|
                \() ->
                    """- Single

Paragraph
"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Block.UnorderedList Block.IsTight
                                    [ plainListItem "Single"
                                    ]
                                , Block.Paragraph (unstyledText "Paragraph")
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
                                [ Block.UnorderedList Block.IsTight
                                    [ plainListItem "Item 1"
                                    , plainListItem "Item 2"
                                    , plainListItem "Item 3"
                                    ]
                                , Block.UnorderedList Block.IsTight
                                    [ plainListItem "Item 4"
                                    , plainListItem "Item 5"
                                    , plainListItem "Item 6"
                                    ]
                                , Block.UnorderedList Block.IsTight
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
                                [ Block.OrderedList 1
                                    Block.IsTight
                                    [ [ Block.Paragraph (unstyledText "foo") ]
                                    , [ Block.Paragraph (unstyledText "bar") ]
                                    ]
                                , Block.OrderedList 3
                                    Block.IsTight
                                    [ [ Block.Paragraph (unstyledText "baz") ]
                                    ]
                                ]
                            )
            , test "With continuation line" <|
                \() ->
                    """- One A
One B
- Two
"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Block.UnorderedList Block.IsTight
                                    [ plainListItem "One A One B"
                                    , plainListItem "Two"
                                    ]
                                ]
                            )
            , test "loose list by blank line" <|
                \() ->
                    """- One

- Two
- Three
"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Block.UnorderedList Block.IsLoose
                                    [ plainListItem "One"
                                    , plainListItem "Two"
                                    , plainListItem "Three"
                                    ]
                                ]
                            )
            , test "loose list by blank line later" <|
                \() ->
                    """- One
- Two

- Three
"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Block.UnorderedList Block.IsLoose
                                    [ plainListItem "One"
                                    , plainListItem "Two"
                                    , plainListItem "Three"
                                    ]
                                ]
                            )
            , test "loose list by multiple blank lines" <|
                \() ->
                    """- One


- Two



- Three
"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Block.UnorderedList Block.IsLoose
                                    [ plainListItem "One"
                                    , plainListItem "Two"
                                    , plainListItem "Three"
                                    ]
                                ]
                            )
            , test "loose list by two paragraphs" <|
                \() ->
                    """- One A

  One B
- Two
- Three
"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Block.UnorderedList Block.IsLoose
                                    [ Block.ListItem Block.NoTask
                                        [ Block.Paragraph (unstyledText "One A")
                                        , Block.Paragraph (unstyledText "One B")
                                        ]
                                    , plainListItem "Two"
                                    , plainListItem "Three"
                                    ]
                                ]
                            )
            , test "heading in list" <|
                \() ->
                    """- # One
- ## Two
- # Three
"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Block.UnorderedList Block.IsTight
                                    [ Block.ListItem Block.NoTask
                                        [ Block.Heading Block.H1 (unstyledText "One")
                                        ]
                                    , Block.ListItem Block.NoTask
                                        [ Block.Heading Block.H2 (unstyledText "Two")
                                        ]
                                    , Block.ListItem Block.NoTask
                                        [ Block.Heading Block.H1 (unstyledText "Three")
                                        ]
                                    ]
                                ]
                            )
            , test "simple nested list" <|
                \() ->
                    """- One
    - One A
    - One B
- Two
- Three
"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Block.UnorderedList Block.IsTight
                                    [ Block.ListItem Block.NoTask
                                        [ Block.Paragraph (unstyledText "One")
                                        , Block.UnorderedList Block.IsTight
                                            [ plainListItem "One A"
                                            , plainListItem "One B"
                                            ]
                                        ]
                                    , plainListItem "Two"
                                    , plainListItem "Three"
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
                                [ Block.Paragraph (unstyledText "The number of windows in my house is\n14.  The number of doors is 6.")
                                ]
                            )
            ]
        , test "A paragraph with a numeral that IS 1 in the text" <|
            \() ->
                """The number of windows in my house is
1.  The number of doors is 6.
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Paragraph (unstyledText "The number of windows in my house is")
                            , Block.OrderedList 1
                                Block.IsTight
                                [ [ Block.Paragraph (unstyledText "The number of doors is 6.") ]
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
        , test "simple table" <|
            \() ->
                """| abc | def |
|---|---|
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Table
                                [ { label = [ Text "abc" ], alignment = Nothing }
                                , { label = [ Text "def" ], alignment = Nothing }
                                ]
                                []
                            ]
                        )
        , test "simple table with data" <|
            \() ->
                """| abc | def |
|---|---|
| foo | bar |
| bar | baz |
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Table
                                [ { label = [ Text "abc" ], alignment = Nothing }
                                , { label = [ Text "def" ], alignment = Nothing }
                                ]
                                [ [ [ Text "foo" ], [ Text "bar" ] ]
                                , [ [ Text "bar" ], [ Text "baz" ] ]
                                ]
                            ]
                        )
        , test "table with alignment" <|
            \() ->
                """| abc | def | ghi | jkl
|:---|:------:|--:|---|
| foo | bar | baz | boo |
| bar | baz | boo | foo |
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Table
                                [ { label = [ Text "abc" ], alignment = Just AlignLeft }
                                , { label = [ Text "def" ], alignment = Just AlignCenter }
                                , { label = [ Text "ghi" ], alignment = Just AlignRight }
                                , { label = [ Text "jkl" ], alignment = Nothing }
                                ]
                                [ [ [ Text "foo" ], [ Text "bar" ], [ Text "baz" ], [ Text "boo" ] ]
                                , [ [ Text "bar" ], [ Text "baz" ], [ Text "boo" ], [ Text "foo" ] ]
                                ]
                            ]
                        )
        , test "table with a cell that looks like a heading but isn't" <|
            \() ->
                """| abc | def |
| --- | --- |
| bar | baz |
####### asdf
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Table
                                [ { label = [ Text "abc" ], alignment = Nothing }
                                , { label = [ Text "def" ], alignment = Nothing }
                                ]
                                [ [ [ Text "bar" ], [ Text "baz" ] ]
                                , [ [ Text "####### asdf" ], [] ]
                                ]
                            ]
                        )
        , test "table ended by a heading" <|
            \() ->
                """| abc | def |
| --- | --- |
| bar | baz |
###### asdf
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Table
                                [ { label = [ Text "abc" ], alignment = Nothing }
                                , { label = [ Text "def" ], alignment = Nothing }
                                ]
                                [ [ [ Text "bar" ], [ Text "baz" ] ]
                                ]
                            , Block.Heading Block.H6 [ Text "asdf" ]
                            ]
                        )
        , test "tables separated by a blank line should be separate" <|
            \() ->
                """| abc | def |
| --- | --- |
| bar | baz |

| abc | def |
| --- | --- |
| bar | baz |
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Table
                                [ { label = [ Text "abc" ], alignment = Nothing }
                                , { label = [ Text "def" ], alignment = Nothing }
                                ]
                                [ [ [ Text "bar" ], [ Text "baz" ] ]
                                ]
                            , Block.Table
                                [ { label = [ Text "abc" ], alignment = Nothing }
                                , { label = [ Text "def" ], alignment = Nothing }
                                ]
                                [ [ [ Text "bar" ], [ Text "baz" ] ]
                                ]
                            ]
                        )
        , test "multiple thematic breaks" <|
            \() ->
                """***
---
___"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.ThematicBreak
                            , Block.ThematicBreak
                            , Block.ThematicBreak
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
        , test "thematic break after list" <|
            \() ->
                """- List
---
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.UnorderedList Block.IsTight
                                [ plainListItem "List"
                                ]
                            , Block.ThematicBreak
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
                            [ Block.Heading Block.H1 (unstyledText "Title")
                            , Block.UnorderedList Block.IsTight
                                [ plainListItem "This is an item"
                                , plainListItem "And so is this"
                                ]
                            , Block.Paragraph (unstyledText "Text after")

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
                            , Block.Paragraph (unstyledText "This is more stuff")
                            , Block.Heading Block.H2 (unstyledText "h2")
                            , Block.Paragraph (unstyledText "qwer")
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
                            [ Block.Paragraph
                                [ Block.Image "/my/image.jpg" Nothing [ Block.Text "This is an image" ]

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

        --, skip <|
        --    test "autolink" <|
        --        \() ->
        --            "<http://foo.bar.baz>\n"
        --                |> parse
        --                |> Expect.equal
        --                    (Ok
        --                        [ Block.Paragraph
        --                            [ Block.Link "http://foo.bar.baz" Nothing [ Block.Text "http://foo.bar.baz" ] ]
        --                        ]
        --                    )
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
                        |> Expect.equal (Ok [ Block.BlockQuote [ Block.Paragraph (unstyledText "This is a quote") ] ])
            , test "block quote with a space after" <|
                \() ->
                    "> This is a quote\n"
                        |> parse
                        |> Expect.equal (Ok [ Block.BlockQuote [ Block.Paragraph (unstyledText "This is a quote") ] ])
            , test "consecutive block quote lines are combined" <|
                \() ->
                    """> # Heading
> Body
"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Block.BlockQuote
                                    [ Block.Heading Block.H1 (unstyledText "Heading")
                                    , Block.Paragraph (unstyledText "Body")
                                    ]
                                ]
                            )
            , test "lists in block quote" <|
                \() ->
                    """> - One
> - Two
> - Three
"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Block.BlockQuote
                                    [ Block.UnorderedList Block.IsTight
                                        [ plainListItem "One"
                                        , plainListItem "Two"
                                        , plainListItem "Three"
                                        ]
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
                                    [ Block.Heading Block.H1 (unstyledText "Heading")
                                    , Block.Paragraph (unstyledText "I'm part of the block quote")
                                    ]
                                ]
                            )
            , test "list after block quote" <|
                \() ->
                    """> - Quoted list
- List
"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Block.BlockQuote
                                    [ Block.UnorderedList Block.IsTight [ plainListItem "Quoted list" ]
                                    ]
                                , Block.UnorderedList Block.IsTight [ plainListItem "List" ]
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
                                [ Block.Paragraph (unstyledText "not code")
                                ]
                            ]
                        )
        , test "inline HTML" <|
            \() ->
                """This is *italicized inline HTML <bio name="Dillon Kearns" photo="https://avatars2.githubusercontent.com/u/1384166" />*"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Paragraph
                                [ Block.Text "This is "
                                , Block.Emphasis
                                    [ Block.Text "italicized inline HTML "
                                    , Block.HtmlInline
                                        (Block.HtmlElement "bio"
                                            -- NOTE: attribute names are in reverse alphabetical order
                                            [ { name = "photo", value = "https://avatars2.githubusercontent.com/u/1384166" }
                                            , { name = "name", value = "Dillon Kearns" }
                                            ]
                                            []
                                        )
                                    ]
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
                                [ Block.Paragraph (unstyledText "foo")
                                , Block.Paragraph (unstyledText "bar")
                                ]
                            ]
                        )
        , test "hard line break with two spaces" <|
            \() ->
                "foo  \nbaz"
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Paragraph
                                [ Text "foo"
                                , HardLineBreak
                                , Text "baz"
                                ]
                            ]
                        )
        , test "indented code blocks cannot interrupt paragraphs" <|
            \() ->
                """aaa
                        bbb
                                                  ccc"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Paragraph
                                [ Text
                                    """aaa
                        bbb
                                                  ccc"""
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
                                [ Block.Heading Block.H1 (unstyledText "Foo")
                                , Block.Paragraph (unstyledText "bar\nbaz")
                                ]
                            ]
                        )
        , test "backslash line break" <|
            \() ->
                "Before\\\nAfter"
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Paragraph
                                [ Block.Text "Before"
                                , Block.HardLineBreak
                                , Block.Text "After"
                                ]
                            ]
                        )
        , describe "html"
            [ test "html comment" <|
                \() ->
                    "<!-- hello! -->"
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Block.HtmlBlock (Block.HtmlComment " hello! ") ]
                            )
            , test "nested html comment" <|
                \() ->
                    """<Resources>

<Book title="Crime and Punishment">
  <!-- this is the book review -->
  This is my review...
</Book>


</Resources>
"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ HtmlBlock
                                    (HtmlElement "resources"
                                        []
                                        [ HtmlBlock
                                            (HtmlElement "book"
                                                [ { name = "title", value = "Crime and Punishment" } ]
                                                [ HtmlBlock (HtmlComment " this is the book review ")
                                                , Paragraph [ Text "This is my review..." ]
                                                ]
                                            )
                                        ]
                                    )
                                ]
                            )
            ]
        , test "HTML declaration" <|
            \() ->
                """<!DOCTYPE html>"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ HtmlBlock (HtmlDeclaration "DOCTYPE" "html") ]
                        )
        , describe "inline html"
            [ test "cdata sections" <|
                \() ->
                    "foo <![CDATA[>&<]]>"
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Paragraph
                                    [ Text "foo "
                                    , HtmlInline (Cdata ">&<")
                                    ]
                                ]
                            )
            , test "nested HTML" <|
                \() ->
                    """foo <Resources><Resource type="book" title="Notes From Underground" /></Resources>"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Paragraph
                                    [ Text "foo "
                                    , HtmlInline
                                        (HtmlElement "resources"
                                            []
                                            [ HtmlBlock
                                                (HtmlElement "resource"
                                                    [ { name = "type", value = "book" }
                                                    , { name = "title", value = "Notes From Underground" }
                                                    ]
                                                    []
                                                )
                                            ]
                                        )
                                    ]
                                ]
                            )
            , test "nested markdown within nested HTML" <|
                \() ->
                    """foo <Resources><Resource type="book" title="Notes From Underground" />9/10 interesting read!</Resources>"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Paragraph
                                    [ Text "foo "
                                    , HtmlInline
                                        (HtmlElement "resources"
                                            []
                                            [ HtmlBlock
                                                (HtmlElement "resource"
                                                    [ { name = "type", value = "book" }
                                                    , { name = "title", value = "Notes From Underground" }
                                                    ]
                                                    []
                                                )
                                            , Paragraph [ Text "9/10 interesting read!" ]
                                            ]
                                        )
                                    ]
                                ]
                            )
            ]
        , describe "beginning with autolink"
            [ test "simple autolink" <|
                \() ->
                    "<https://elm-lang.org>\n"
                        |> expectOk
                            [ Paragraph
                                [ Link "https://elm-lang.org" Nothing [ Text "https://elm-lang.org" ]
                                ]
                            ]
            , test "email autolink" <|
                \() ->
                    "<foo@bar.example.com>\n"
                        |> expectOk
                            [ Paragraph
                                [ Link "mailto:foo@bar.example.com" Nothing [ Text "foo@bar.example.com" ]
                                ]
                            ]
            ]
        , describe "link reference definitions"
            [ test "basic example" <|
                \() ->
                    """[foo]: /url "title"

[foo]
"""
                        |> parse
                        |> Expect.equal
                            (Ok [ Paragraph [ Link "/url" (Just "title") [ Text "foo" ] ] ])
            , test "invalid reference uses fallback paragraph parsing" <|
                \() ->
                    """[foo]:

[foo]
"""
                        |> parse
                        |> Expect.equal
                            (Ok
                                [ Paragraph [ Text "[foo]:" ]
                                , Paragraph [ Text "[foo]" ]
                                ]
                            )
            ]
        ]


expectOk : List Block -> String -> Expectation
expectOk expected input =
    case input |> parse of
        Ok actual ->
            actual
                |> Expect.equal expected

        Err error ->
            Expect.fail (Debug.toString error)


plainListItem : String -> Block.ListItem Block
plainListItem body =
    Block.ListItem Block.NoTask [ Block.Paragraph (unstyledText body) ]


emptyListItem : Block.ListItem Block
emptyListItem =
    Block.ListItem Block.NoTask []


unstyledText : String -> List Inline
unstyledText body =
    [ Block.Text body ]


emphasisText : String -> List Inline
emphasisText body =
    [ Block.Emphasis <|
        [ Block.Text body ]
    ]


parserError : String -> Expect.Expectation
parserError markdown =
    case parse markdown of
        Ok _ ->
            Expect.fail "Expected a parser failure!"

        Err _ ->
            Expect.pass
