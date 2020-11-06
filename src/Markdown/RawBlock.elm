module Markdown.RawBlock exposing (Attribute, RawBlock(..), SetextLevel(..), UnparsedInlines(..))

import Markdown.Block exposing (Block)
import Markdown.CodeBlock exposing (CodeBlock)
import Markdown.ListItem
import Markdown.Table
import Markdown.TableParser as TableParser


type alias Attribute =
    { name : String, value : String }


type UnparsedInlines
    = UnparsedInlines String


type SetextLevel
    = LevelOne
    | LevelTwo


type RawBlock
    = Heading Int UnparsedInlines
    | OpenBlockOrParagraph UnparsedInlines
    | Html (Markdown.Block.Html Block)
    | UnorderedListBlock
        Markdown.Block.Loose
        (List
            { task : Maybe Bool
            , body : String
            }
        )
    | OrderedListBlock Int Markdown.Block.Loose (List String)
    | CodeBlock CodeBlock
    | IndentedCodeBlock String
    | ThematicBreak
    | Table (Markdown.Table.Table String)
    | TableDelimiter Markdown.Table.TableDelimiterRow
    | BlankLine
    | BlockQuote String
    | SetextLine SetextLevel String
    | UnorderedListItem Markdown.ListItem.ListItem
