module Markdown.RawBlock exposing (Attribute, RawBlock(..), UnparsedInlines(..))

import Markdown.Block exposing (Block)
import Markdown.CodeBlock exposing (CodeBlock)
import Markdown.Table
import Markdown.TableParser as TableParser


type alias Attribute =
    { name : String, value : String }


type UnparsedInlines
    = UnparsedInlines String


type RawBlock
    = Heading Int UnparsedInlines
    | Body UnparsedInlines
    | Html (Markdown.Block.Html Block)
    | UnorderedListBlock
        (List
            { task : Maybe Bool
            , body : String
            }
        )
    | OrderedListBlock Int (List String)
    | CodeBlock CodeBlock
    | IndentedCodeBlock String
    | ThematicBreak
    | Table (Markdown.Table.Table String)
    | BlankLine
    | BlockQuote String
