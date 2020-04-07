module Markdown.Block exposing
    ( Block(..)
    , HeadingLevel(..), headingLevelToInt
    , ListItem(..), Task(..), Alignment(..)
    , Html(..)
    , Inline(..)
    , HtmlAttribute
    , extractInlineText
    , walk, walkInlines, validateMapInlines, mapAndAccumulate, foldl
    , Loose(..)
    )

{-|

@docs Block
@docs HeadingLevel, headingLevelToInt


### List Items

@docs ListItem, Task, Alignment


## HTML

@docs Html

See [`Markdown.Html`](Markdown.Html) for more.


## Inlines

@docs Inline
@docs HtmlAttribute
@docs extractInlineText


## Transformations

@docs walk, walkInlines, validateMapInlines, mapAndAccumulate, foldl

-}


{-| This is the AST (abstract syntax tree) that represents your parsed markdown.

In the simplest case, you can pass this directly to a renderer:

    module Main exposing (main)

    import Markdown.Block exposing (Block)
    import Markdown.Parser
    import Markdown.Renderer

    markdown : String
    markdown =
        "# This is a title!\n\nThis is the body."

    astResult : Result (List (Advanced.DeadEnd String Parser.Problem)) (List Block)
    astResult =
        markdown
            |> Markdown.Parser.parse

    main : Html msg
    main =
        case
            astResult
                |> Result.mapError deadEndsToString
                |> Result.andThen (\ast -> Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer ast)
        of
            Ok rendered ->
                div [] rendered

            Err errors ->
                text errors

-}
type Block
    = -- Container Blocks
      HtmlBlock (Html Block)
    | UnorderedList Loose (List (ListItem Block))
    | OrderedList Int Loose (List (List Block))
    | BlockQuote (List Block)
      -- Leaf Blocks With Inlines
    | Heading HeadingLevel (List Inline)
    | Paragraph (List Inline)
    | Table (List { label : List Inline, alignment : Maybe Alignment }) (List (List Inline))
      -- Leaf Blocks Without Inlines
    | CodeBlock { body : String, language : Maybe String }
    | ThematicBreak


{-| Alignment in a header cell in a markdown table. See the `Table` variant in the `Block` type.
-}
type Alignment
    = AlignLeft
    | AlignRight
    | AlignCenter


{-| The value for an unordered list item, which may contain a task.
-}
type ListItem children
    = ListItem Task (List children)


type Loose
    = IsLoose
    | IsTight


{-| A task (or no task), which may be contained in a ListItem.
-}
type Task
    = NoTask
    | IncompleteTask
    | CompletedTask


{-| Markdown limits headings to level 6 or less. HTML does this, too (`<h7>` is not supported by most browsers).
This type represents the possible heading levels that a Markdown heading block may have.

If you do a heading level above 6, the `#` characters will be treated as literal `#`'s.

-}
type HeadingLevel
    = H1
    | H2
    | H3
    | H4
    | H5
    | H6


{-| An Inline block. Note that `HtmlInline`s can contain Blocks, not just nested `Inline`s.
-}
type Inline
    = HtmlInline (Html Block)
    | Link String (Maybe String) (List Inline)
    | Image String (Maybe String) (List Inline)
    | Emphasis (List Inline)
    | Strong (List Inline)
      -- Strikethrough TODO  https://github.github.com/gfm/#strikethrough-extension-
    | CodeSpan String
    | Text String
    | HardLineBreak


{-| A convenience function so that you don't have to write a big case statement if you need a heading level as an Int.
-}
headingLevelToInt : HeadingLevel -> Int
headingLevelToInt headingLevel =
    case headingLevel of
        H1 ->
            1

        H2 ->
            2

        H3 ->
            3

        H4 ->
            4

        H5 ->
            5

        H6 ->
            6


{-| Extract the text from a list of inlines.

    -- Original string: "Heading with *emphasis*"

    import Markdown.Block as Block exposing (..)

    inlines : List (Inline)
    inlines =
        [ Text "Heading with "
        , Emphasis [ Text "emphasis" ]
        ]

    Block.extractInlineText inlines
    --> "Heading with emphasis"

-}
extractInlineText : List Inline -> String
extractInlineText inlines =
    List.foldl extractTextHelp "" inlines


extractTextHelp : Inline -> String -> String
extractTextHelp inline text =
    case inline of
        Text str ->
            text ++ str

        HardLineBreak ->
            text ++ " "

        CodeSpan str ->
            text ++ str

        Link _ _ inlines ->
            text ++ extractInlineText inlines

        Image _ _ inlines ->
            text ++ extractInlineText inlines

        HtmlInline html ->
            case html of
                HtmlElement _ _ blocks ->
                    blocks
                        |> foldl
                            (\block soFar ->
                                soFar ++ extractInlineBlockText block
                            )
                            text

                _ ->
                    text

        Strong inlines ->
            text ++ extractInlineText inlines

        Emphasis inlines ->
            text ++ extractInlineText inlines


extractInlineBlockText : Block -> String
extractInlineBlockText block =
    case block of
        Paragraph inlines ->
            extractInlineText inlines

        HtmlBlock html ->
            case html of
                HtmlElement _ _ blocks ->
                    blocks
                        |> foldl
                            (\nestedBlock soFar ->
                                soFar ++ extractInlineBlockText nestedBlock
                            )
                            ""

                _ ->
                    ""

        UnorderedList _ items ->
            items
                |> List.map
                    (\(ListItem _ blocks) ->
                        blocks
                            |> List.map extractInlineBlockText
                            |> String.join "\n"
                    )
                |> String.join "\n"

        OrderedList _ _ items ->
            items
                |> List.map
                    (List.map extractInlineBlockText >> String.join "\n")
                |> String.join "\n"

        BlockQuote blocks ->
            blocks
                |> List.map extractInlineBlockText
                |> String.join "\n"

        Heading headingLevel inlines ->
            extractInlineText inlines

        Table header rows ->
            [ header
                |> List.map .label
                |> List.map extractInlineText
            , List.map extractInlineText rows
            ]
                |> List.concat
                |> String.join "\n"

        CodeBlock { body } ->
            body

        ThematicBreak ->
            ""



--BlockQuote blocks ->
--
--
--Heading headingLevel inlines ->
--
--
--Table list lists ->
--
--
--CodeBlock record ->
--
--
--ThematicBreak ->


{-| The way HTML is handled is one of the core ideas of this library.

You get the full HTML structure that you can use to process the Blocks before rendering them. Once you render them,
all of the raw text within your HTML is parsed as Markdown.


## `HtmlComment`s and metadata

`HtmlComment`s contain the raw comment text, completely unprocessed. That means
you'll need to trim it if you want to strip the leading or trailing whitespace.

Renderer's do not process `HtmlComment`s. If you want to do any special processing
based on HTML comments, you can inspect the `Markdown.Block.Block`s before rendering
it and perform any special processing based on that. You could even add or remove
`Block`s, for example, based on the presence of certain comment values.

-}
type Html children
    = HtmlElement String (List HtmlAttribute) (List children)
    | HtmlComment String
    | ProcessingInstruction String
    | HtmlDeclaration String String
    | Cdata String


{-| An Html attribute. In <div class="foo">, you would have `{ name = "class", value = "foo" }`.
-}
type alias HtmlAttribute =
    { name : String, value : String }


{-| Apply a function to transform each inline recursively.
If any of the values are `Err`s, the entire value will be an `Err`.

    import Markdown.Block as Block exposing (..)

    lookupLink : String -> Result String String
    lookupLink key =
        case key of
            "elm-lang" ->
                Ok "https://elm-lang.org"
            _ ->
                Err <| "Couldn't find key " ++ key

    resolveLinkInInline : Inline -> Result String Inline
    resolveLinkInInline inline =
        case inline of
            Link destination title inlines ->
                destination
                    |> lookupLink
                    |> Result.map (\resolvedLink -> Link resolvedLink title inlines)
            _ ->
                Ok inline

    [ Paragraph
        [ Link "angular" Nothing [ Text "elm-lang homepage" ]
        ]
    ]
        |> Block.validateMapInlines resolveLinkInInline
    -->  Err [ "Couldn't find key angular" ]

-}
validateMapInlines : (Inline -> Result error Inline) -> List Block -> Result (List error) (List Block)
validateMapInlines function blocks =
    blocks
        |> validateMap (inlineParserValidateWalkBlock function)


{-|

    import Markdown.Block as Block exposing (..)

    [ Paragraph
        [ Link "http://elm-lang.org" Nothing [ Text "elm-lang homepage" ]
        ]
    ]
        |> List.map
            (Block.walkInlines
                (\inline ->
                    case inline of
                        Link destination title inlines ->
                            Link (String.replace "http://" "https://" destination) title inlines
                        _ ->
                            inline
                )
            )
    -->        [ Paragraph
    -->            [ Link "https://elm-lang.org" Nothing [ Text "elm-lang homepage" ]
    -->            ]
    -->        ]

-}
walkInlines : (Inline -> Inline) -> Block -> Block
walkInlines function block =
    walk (walkInlinesHelp function) block


walkInlinesHelp : (Inline -> Inline) -> Block -> Block
walkInlinesHelp function block =
    case block of
        Paragraph inlines ->
            List.map (inlineParserWalk function) inlines
                |> Paragraph

        UnorderedList isLoose listItems ->
            List.map
                (\(ListItem task children) ->
                    ListItem task (List.map (walkInlinesHelp function) children)
                )
                listItems
                |> UnorderedList isLoose

        OrderedList startingIndex isLoose listItems ->
            List.map
                (List.map (walkInlinesHelp function))
                listItems
                |> OrderedList startingIndex isLoose

        BlockQuote children ->
            BlockQuote (List.map (walkInlinesHelp function) children)

        Heading level children ->
            Heading level (List.map function children)

        Table header rows ->
            Table
                (header
                    |> List.map
                        (\{ alignment, label } ->
                            { alignment = alignment
                            , label = List.map function label
                            }
                        )
                )
                (rows |> List.map (List.map function))

        HtmlBlock html ->
            case html of
                HtmlElement string htmlAttributes blocks ->
                    HtmlElement string
                        htmlAttributes
                        (List.map (walkInlinesHelp function) blocks)
                        |> HtmlBlock

                _ ->
                    block

        CodeBlock record ->
            block

        ThematicBreak ->
            block


inlineParserWalk : (Inline -> Inline) -> Inline -> Inline
inlineParserWalk function inline =
    case inline of
        Link url maybeTitle inlines ->
            List.map (inlineParserWalk function) inlines
                |> Link url maybeTitle
                |> function

        Image url maybeTitle inlines ->
            List.map (inlineParserWalk function) inlines
                |> Image url maybeTitle
                |> function

        Emphasis inlines ->
            List.map (inlineParserWalk function) inlines
                |> Emphasis
                |> function

        HtmlInline html ->
            case html of
                HtmlElement string htmlAttributes children ->
                    HtmlElement string htmlAttributes (List.map (walkInlines function) children)
                        |> HtmlInline

                _ ->
                    function inline

        Strong inlines ->
            List.map (inlineParserWalk function) inlines
                |> Strong

        CodeSpan _ ->
            function inline

        Text _ ->
            function inline

        HardLineBreak ->
            function inline


inlineParserValidateWalk : (Inline -> Result error Inline) -> Inline -> Result (List error) Inline
inlineParserValidateWalk function inline =
    case inline of
        Link url maybeTitle inlines ->
            List.map (inlineParserValidateWalk function) inlines
                |> combine
                |> Result.andThen
                    (\nestedInlines ->
                        Link url maybeTitle nestedInlines
                            |> function
                            |> Result.mapError List.singleton
                    )

        Image string maybeString inlines ->
            List.map (inlineParserValidateWalk function) inlines
                |> combine
                |> Result.andThen
                    (\transformedInlines ->
                        Image string maybeString transformedInlines
                            |> function
                            |> Result.mapError List.singleton
                    )

        Emphasis inlines ->
            List.map (inlineParserValidateWalk function) inlines
                |> combine
                |> Result.andThen
                    (\transformedInlines ->
                        Emphasis transformedInlines
                            |> function
                            |> Result.mapError List.singleton
                    )

        Strong inlines ->
            List.map (inlineParserValidateWalk function) inlines
                |> combine
                |> Result.andThen
                    (\transformedInlines ->
                        Strong transformedInlines
                            |> function
                            |> Result.mapError List.singleton
                    )

        CodeSpan string ->
            function inline
                |> Result.mapError List.singleton

        Text string ->
            function inline
                |> Result.mapError List.singleton

        HardLineBreak ->
            function inline
                |> Result.mapError List.singleton

        HtmlInline html ->
            case html of
                HtmlElement tagName htmlAttributes blocks ->
                    blocks
                        |> List.map (inlineParserValidateWalkBlock function)
                        |> combine
                        |> Result.andThen
                            (\transformedBlocks ->
                                HtmlElement tagName htmlAttributes transformedBlocks
                                    |> HtmlInline
                                    |> function
                                    |> Result.mapError List.singleton
                            )

                _ ->
                    function inline
                        |> Result.mapError List.singleton


inlineParserValidateWalkBlock : (Inline -> Result error Inline) -> Block -> Result (List error) Block
inlineParserValidateWalkBlock function block =
    case block of
        ThematicBreak ->
            Ok ThematicBreak

        HtmlBlock html ->
            case html of
                HtmlElement tagName attributes children ->
                    children
                        |> List.map (inlineParserValidateWalkBlock function)
                        |> combine
                        |> Result.map (HtmlElement tagName attributes)
                        |> Result.map HtmlBlock

                _ ->
                    Ok block

        UnorderedList isLoose items ->
            items
                |> List.map
                    (\(ListItem task item) ->
                        item
                            |> List.map (inlineParserValidateWalkBlock function)
                            |> combine
                            |> Result.map (ListItem task)
                    )
                |> combine
                |> Result.map (UnorderedList isLoose)

        OrderedList startingIndex isLoose lists ->
            lists
                |> List.map (List.map (inlineParserValidateWalkBlock function))
                |> List.map combine
                |> combine
                |> Result.map (OrderedList startingIndex isLoose)

        BlockQuote nestedBlocks ->
            nestedBlocks
                |> List.map (inlineParserValidateWalkBlock function)
                |> combine
                |> Result.map BlockQuote

        Heading headingLevel inlines ->
            inlines
                |> List.map (inlineParserValidateWalk function)
                |> combine
                |> Result.map (Heading headingLevel)

        Paragraph inlines ->
            inlines
                |> List.map (inlineParserValidateWalk function)
                |> combine
                |> Result.map Paragraph

        Table header rows ->
            let
                mappedHeader =
                    header
                        |> List.map
                            (\{ label, alignment } ->
                                label
                                    |> List.map (inlineParserValidateWalk function)
                                    |> combine
                                    |> Result.map
                                        (\transformedLabel ->
                                            { alignment = alignment
                                            , label = transformedLabel
                                            }
                                        )
                            )
                        |> combine

                mappedRows =
                    rows
                        |> List.map (List.map (inlineParserValidateWalk function))
                        |> List.map combine
                        |> combine
            in
            Result.map2 Table mappedHeader mappedRows

        CodeBlock record ->
            Ok block


{-| Recursively apply a function to transform each Block.

This example bumps headings down by one level.

    import Markdown.Block as Block exposing (..)

    bumpHeadingLevel : HeadingLevel -> HeadingLevel
    bumpHeadingLevel level =
        case level of
            H1 -> H2
            H2 -> H3
            H3 -> H4
            H4 -> H5
            H5 -> H6
            H6 -> H6

    [ Heading H1 [ Text "First heading" ]
    , Paragraph [ Text "Paragraph" ]
    , BlockQuote
        [ Heading H2 [ Text "Paragraph" ]
        ]
    , Heading H1 [ Text "Second heading" ]
    ]
        |> List.map
            (Block.walk
                (\block ->
                    case block of
                        Heading level children ->
                            Heading (bumpHeadingLevel level) children
                        _ ->
                            block
                )
            )
    --> [ Heading H2 [ Text "First heading" ]
    --> , Paragraph [ Text "Paragraph" ]
    --> , BlockQuote
    --> [ Heading H3 [ Text "Paragraph" ]
    --> ]
    --> , Heading H2 [ Text "Second heading" ]
    --> ]

-}
walk : (Block -> Block) -> Block -> Block
walk function block =
    case block of
        BlockQuote blocks ->
            List.map (walk function) blocks
                |> BlockQuote
                |> function

        HtmlBlock html ->
            case html of
                HtmlElement string htmlAttributes blocks ->
                    HtmlElement string
                        htmlAttributes
                        (List.map (walk function) blocks)
                        |> HtmlBlock
                        |> function

                _ ->
                    function block

        UnorderedList isLoose listItems ->
            List.map
                (\(ListItem task children) ->
                    ListItem task (List.map (walk function) children)
                )
                listItems
                |> UnorderedList isLoose
                |> function

        OrderedList startingIndex isLoose listItems ->
            List.map (List.map (walk function)) listItems
                |> OrderedList startingIndex isLoose
                |> function

        -- These cases don't have nested blocks
        -- So no recursion needed
        Heading _ _ ->
            function block

        Paragraph _ ->
            function block

        Table _ _ ->
            function block

        CodeBlock _ ->
            function block

        ThematicBreak ->
            function block


validateMap : (Block -> Result error value) -> List Block -> Result error (List value)
validateMap mapFn blocks =
    blocks
        |> List.map mapFn
        |> combine


{-| Combine a list of results into a single result (holding a list).
-}
combine : List (Result x a) -> Result x (List a)
combine =
    List.foldr (Result.map2 (::)) (Ok [])


{-| Map values, while also tracking state while traversing every block. Think of it as a helper for `foldl` and `map`
in a single handy function!

In this example, we need to keep track of the number of occurrences of a heading name so that we can
use a unique slug to link to (exactly like Github does for its heading links). We keep the occurences in a Dict,
so this allows us to maintain state rather than just transforming blocks purely based on the current block.

You can see the full end-to-end code for this in `examples/src/Slugs.elm`.

    import Markdown.Block as Block exposing (..)
    import Dict
    gatherHeadingOccurrences : List Block -> ( Dict.Dict String Int, List ( Block, Maybe String ) )
    gatherHeadingOccurrences =
        Block.mapAndAccumulate
            (\soFar block ->
                case block of
                    Heading level inlines ->
                        let
                            inlineText : String
                            inlineText =
                                Block.extractInlineText inlines
                            occurenceModifier : String
                            occurenceModifier =
                                soFar
                                    |> Dict.get inlineText
                                    |> Maybe.map String.fromInt
                                    |> Maybe.withDefault ""
                        in
                        ( soFar |> trackOccurence inlineText
                        , ( Heading level inlines, Just (inlineText ++ occurenceModifier) )
                        )
                    _ ->
                        ( soFar
                        , ( block, Nothing )
                        )
            )
            Dict.empty
    trackOccurence : String -> Dict.Dict String Int -> Dict.Dict String Int
    trackOccurence value occurences =
        occurences
            |> Dict.update value
                (\maybeOccurence ->
                    case maybeOccurence of
                        Just count ->
                            Just <| count + 1
                        Nothing ->
                            Just 1
                )

    [ Heading H1 [ Text "foo" ]
    , Heading H1 [ Text "bar" ]
    , Heading H1 [ Text "foo" ]
    ]
    |> gatherHeadingOccurrences
    --> ( Dict.fromList
    -->        [ ( "bar", 1 )
    -->        , ( "foo", 2 )
    -->        ]
    -->    , [ ( Heading H1 [ Text "foo" ], Just "foo" )
    -->        , ( Heading H1 [ Text "bar" ], Just "bar" )
    -->        , ( Heading H1 [ Text "foo" ], Just "foo1" )
    -->        ]
    -->    )

-}
mapAndAccumulate : (soFar -> Block -> ( soFar, mappedValue )) -> soFar -> List Block -> ( soFar, List mappedValue )
mapAndAccumulate mapFn initialValue blocks =
    let
        ( accFinal, generatedList ) =
            foldl
                (\block ( acc1, ys ) ->
                    let
                        ( acc2, mappedBlock ) =
                            mapFn acc1 block
                    in
                    ( acc2, mappedBlock :: ys )
                )
                ( initialValue, [] )
                blocks
    in
    ( accFinal, List.reverse generatedList )


{-| Fold over all blocks to yield a value.

    import Markdown.Block as Block exposing (..)

    maximumHeadingLevel : List Block -> Maybe HeadingLevel
    maximumHeadingLevel blocks =
        blocks
            |> Block.foldl
                (\block maxSoFar ->
                    case block of
                        Heading level _ ->
                            if Block.headingLevelToInt level > (maxSoFar |> Maybe.map Block.headingLevelToInt |> Maybe.withDefault 0) then
                                Just level
                            else
                                maxSoFar
                        _ ->
                            maxSoFar
                )
                Nothing

    [ Heading H1 [ Text "Document" ]
    , Heading H2 [ Text "Section A" ]
    , Heading H3 [ Text "Subsection" ]
    , Heading H2 [ Text "Section B" ]
    ]
        |> maximumHeadingLevel
    -->  (Just H3)

-}
foldl : (Block -> acc -> acc) -> acc -> List Block -> acc
foldl function acc list =
    case list of
        [] ->
            acc

        block :: remainingBlocks ->
            case block of
                HtmlBlock html ->
                    case html of
                        HtmlElement _ _ children ->
                            foldl function (function block acc) (children ++ remainingBlocks)

                        _ ->
                            foldl function (function block acc) remainingBlocks

                UnorderedList _ listItems ->
                    -- TODO: recurse
                    foldl function (function block acc) remainingBlocks

                OrderedList int _ lists ->
                    -- TODO: recurse
                    foldl function (function block acc) remainingBlocks

                BlockQuote blocks ->
                    foldl function (function block acc) (blocks ++ remainingBlocks)

                -- These cases don't have nested blocks
                -- So no recursion needed
                Heading _ _ ->
                    foldl function (function block acc) remainingBlocks

                Paragraph _ ->
                    foldl function (function block acc) remainingBlocks

                Table _ _ ->
                    foldl function (function block acc) remainingBlocks

                CodeBlock _ ->
                    foldl function (function block acc) remainingBlocks

                ThematicBreak ->
                    foldl function (function block acc) remainingBlocks
