module UnorderedListTests exposing (suite)

import Expect exposing (Expectation)
import Markdown.Block
import Markdown.ListItem as ListItem exposing (ListItem)
import Markdown.Parser
import Markdown.UnorderedList
import Parser
import Parser.Advanced as Advanced
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


suite : Test
suite =
    describe "list parsing"
        [ test "basic list with '-'" <|
            \() ->
                """- Item 1
- Item 2
- Item 3
"""
                    |> Advanced.run Markdown.UnorderedList.parser
                    |> Expect.equal
                        (Ok
                            [ plainItem "Item 1"
                            , plainItem "Item 2"
                            , plainItem "Item 3"
                            ]
                        )
        , test "list that ends without newline" <|
            \() ->
                """- Item 1
- Item 2
- Item 3"""
                    |> Advanced.run Markdown.UnorderedList.parser
                    |> Expect.equal
                        (Ok
                            [ plainItem "Item 1"
                            , plainItem "Item 2"
                            , plainItem "Item 3"
                            ]
                        )
        , test "basic list with '+'" <|
            \() ->
                """+ Item 1
+ Item 2
+ Item 3
"""
                    |> Advanced.run Markdown.UnorderedList.parser
                    |> Expect.equal
                        (Ok
                            [ plainItem "Item 1"
                            , plainItem "Item 2"
                            , plainItem "Item 3"
                            ]
                        )
        , test "basic list with '*'" <|
            \() ->
                """* Item 1
* Item 2
* Item 3
"""
                    |> Advanced.run Markdown.UnorderedList.parser
                    |> Expect.equal
                        (Ok
                            [ plainItem "Item 1"
                            , plainItem "Item 2"
                            , plainItem "Item 3"
                            ]
                        )
        , test "sibling lists with different markers" <|
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
                    |> Advanced.run Markdown.UnorderedList.parser
                    |> Expect.equal
                        (Ok
                            [ plainItem "Item 1"
                            , plainItem "Item 2"
                            , plainItem "Item 3"
                            ]
                        )
        , test "A list item with emphasis in it and starting with '*'" <|
            \() ->
                """* Item 1 is *emphasized*
* Item 2
* Item 3
*emphasized text following the list*
"""
                    |> Advanced.run Markdown.UnorderedList.parser
                    |> Expect.equal
                        (Ok
                            [ plainItem "Item 1 is *emphasized*"
                            , plainItem "Item 2"
                            , plainItem "Item 3"
                            ]
                        )
        , test "When there is an empty item" <|
            \() ->
                """* foo
*
* bar
"""
                    |> Advanced.run Markdown.UnorderedList.parser
                    |> Expect.equal
                        (Ok
                            [ plainItem "foo"
                            , plainItem ""
                            , plainItem "bar"
                            ]
                        )
        , test "Nested list" <|
            \() ->
                """- Item 1
    - Item 1 1
    - Item 1 2
- Item 2
"""
                    |> Advanced.run Markdown.UnorderedList.parser
                    |> Expect.equal
                        (Ok
                            [ plainItem "Item 1"
                            , plainItem "- Item 1 1\n- Item 1 2"
                            , plainItem "Item 2"
                            ]
                        )
        , test "Starting with space before list marker" <|
            \() ->
                """ - Item 1
 - Item 2
 - Item 3
"""
                    |> Advanced.run Markdown.UnorderedList.parser
                    |> Expect.equal
                        (Ok
                            [ plainItem "Item 1"
                            , plainItem "Item 2"
                            , plainItem "Item 3"
                            ]
                        )
        , test "loose list with '-' by containing two block level elements" <|
            \() ->
                """- Item 1 a

  Item 1 b
- Item 2
          """
                    |> Advanced.run Markdown.UnorderedList.parser
                    |> Expect.equal
                        (Ok
                            [ plainItem "Item 1 a\n\nItem 1 b"
                            , plainItem "Item 2"
                            ]
                        )
        , test "basic loose list with '-'" <|
            \() ->
                """- Item 1

- Item 2
- Item 3
          """
                    |> Advanced.run Markdown.UnorderedList.parser
                    |> Expect.equal
                        (Ok
                            [ plainItem "Item 1"
                            , plainItem "Item 2"
                            , plainItem "Item 3"
                            ]
                        )
        ]


plainItem : String -> ListItem
plainItem body =
    ListItem.PlainItem body
