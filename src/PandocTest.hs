module PandocTest where

import Prelude
import Data.Map(Map)
import qualified Data.Map as Map
import Text.Pandoc
import Text.Pandoc.Writers.Markdown
import Data.Default

md =
  writeMarkdown def pandoc

pandoc ::
  Pandoc
pandoc =
  Pandoc meta blocks

meta ::
  Meta
meta =
  Meta (Map.fromList
    [
      ("title", MetaString "The title")
    , ("author", MetaString "Tony Morris")
    , ("date", MetaString "20170106")
    ]
  )

blocks ::
  [Block]
blocks =
  [Header 1 ("h1-title",[],[]) [Str "H1",Space,Str "title"]
  ,Header 3 ("h3-title",[],[]) [Str "H3",Space,Str "title"]
  ,HorizontalRule
  ,BulletList
   [[Plain [Str "a",Space,Str "bullet",Space,Str "list",Space,Str "item",Space,Str "1"]]
   ,[Plain [Str "a",Space,Str "bullet",Space,Str "list",Space,Str "item",Space,Str "2"]]
   ,[Plain [Str "a",Space,Str "bullet",Space,Str "list",Space,Str "item",Space,Str "3"]]
   ,[Plain [Str "a",Space,Str "bullet",Space,Str "list",Space,Str "item",Space,Str "4"]]]
  ,Para [Image ("",[],[]) [Str "an",Space,Str "image",Space,Str "desc"] ("http://i.imgur.com/0h9dFhl.png","fig:")]
  ,HorizontalRule
  ,OrderedList (1,Decimal,Period)
   [[Plain [Str "a",Space,Str "numbered",Space,Str "list",Space,Str "item",Space,Str "1"]]
   ,[Plain [Str "a",Space,Str "numbered",Space,Str "list",Space,Str "item",Space,Str "2"]]
   ,[Plain [Str "a",Space,Str "numbered",Space,Str "list",Space,Str "item",Space,Str "3"]]
   ,[Plain [Str "a",Space,Str "numbered",Space,Str "list",Space,Str "item",Space,Str "4"]]]
  ,HorizontalRule
  ,CodeBlock ("",[],[]) "some code\nsome more code\nsome more more code"
  ,HorizontalRule
  ,BlockQuote
   [Para [Str "a",Space,Str "quote",SoftBreak,Str "more",Space,Str "quote",SoftBreak,Str "more",Space,Str "more",Space,Str "quote"]]
  ,HorizontalRule
  ,Para [Str "a",Space,Str "paragraph",Space,Str "with",Space,Code ("",[],[]) "inline code",Space,Str "and",Space,Str "some",Space,Strong [Str "bold",Space,Str "text"],Space,Str "and",Space,Str "some",Space,Emph [Str "italic",Space,Str "text"],Str ".",SoftBreak,Str "there",Space,Str "is",Space,Str "also",Space,Link ("",[],[]) [Str "a",Space,Str "link"] ("http://google.com",""),Space,Str "and",Space,Link ("",[],[]) [Str "another",Space,Str "link",Space,Str "with",Space,Str "title"] ("http://blog.tmorris.net/%20the%20blog",""),Str "."]
  ,Para [Str "here",Space,Str "is",Space,Str "a",Space,Strikeout [Str "strikeout"],Str "."]
  ,Para [Str "H",Subscript [Str "2"],Str "O",Space,Str "is",Space,Str "a",Space,Str "liquid.",Space,Str "2",Superscript [Str "10"],Space,Str "is",Space,Str "1024."]
  ,HorizontalRule
  ,CodeBlock ("",[],[]) "a code block"
  ,HorizontalRule
  ,LineBlock
   [[Str "this",Space,Str "is"]
   ,[Str "going",Space,Str "to"]
   ,[Str "be",Space,Str "a"]
   ,[Str "line",Space,Str "block"]]
  ,HorizontalRule
  ,Table [Str "A",Space,Str "simple",Space,Str "table"] [AlignRight,AlignLeft,AlignCenter,AlignDefault] [0.0,0.0,0.0,0.0]
   [[Plain [Str "Right"]]
   ,[Plain [Str "Left"]]
   ,[Plain [Str "Center"]]
   ,[Plain [Str "Default"]]]
   [[[Plain [Str "12"]]
    ,[Plain [Str "12"]]
    ,[Plain [Str "12"]]
    ,[Plain [Str "12"]]]
   ,[[Plain [Str "123"]]
    ,[Plain [Str "123"]]
    ,[Plain [Str "123"]]
    ,[Plain [Str "123"]]]
   ,[[Plain [Str "1"]]
    ,[Plain [Str "1"]]
    ,[Plain [Str "1"]]
    ,[Plain [Str "1"]]]]
  ,HorizontalRule
  ,Table [Str "A",Space,Str "grid",Space,Str "table."] [AlignDefault,AlignDefault,AlignDefault] [0.2222222222222222,0.2222222222222222,0.2916666666666667]
   [[Plain [Str "Fruit"]]
   ,[Plain [Str "Price"]]
   ,[Plain [Str "Advantages"]]]
   [[[Para [Str "Bananas"]]
    ,[Para [Str "$1.34"]]
    ,[BulletList
      [[Plain [Str "built-in",Space,Str "wrapper"]]
      ,[Plain [Str "bright",Space,Str "color"]]]]]
   ,[[Para [Str "Oranges"]]
    ,[Para [Str "$2.10"]]
    ,[BulletList
      [[Plain [Str "cures",Space,Str "scurvy"]]
      ,[Plain [Str "tasty"]]]]]]
  ,HorizontalRule
  ,Table [Str "Demonstration",Space,Str "of",Space,Str "pipe",Space,Str "table",Space,Str "syntax."] [AlignRight,AlignLeft,AlignDefault,AlignCenter] [0.0,0.0,0.0,0.0]
   [[Plain [Str "Right"]]
   ,[Plain [Str "Left"]]
   ,[Plain [Str "Default"]]
   ,[Plain [Str "Center"]]]
   [[[Plain [Str "12"]]
    ,[Plain [Str "12"]]
    ,[Plain [Str "12"]]
    ,[Plain [Str "12"]]]
   ,[[Plain [Str "123"]]
    ,[Plain [Str "123"]]
    ,[Plain [Str "123"]]
    ,[Plain [Str "123"]]]
   ,[[Plain [Str "1"]]
    ,[Plain [Str "1"]]
    ,[Plain [Str "1"]]
    ,[Plain [Str "1"]]]]
  ,HorizontalRule
  ,Para [Str "Here",Space,Str "is",Space,Str "a",Space,Str "footnote",Space,Str "reference,",Note [Para [Str "Here",Space,Str "is",Space,Str "the",Space,Str "footnote."]],Space,Str "and",Space,Str "another.",Note [Para [Str "Here's",Space,Str "one",Space,Str "with",Space,Str "multiple",Space,Str "blocks."],Para [Str "Subsequent",Space,Str "paragraphs",Space,Str "are",Space,Str "indented",Space,Str "to",Space,Str "show",Space,Str "that",Space,Str "they",SoftBreak,Str "belong",Space,Str "to",Space,Str "the",Space,Str "previous",Space,Str "footnote."],CodeBlock ("",[],[]) "{ some.code }",Para [Str "The",Space,Str "whole",Space,Str "paragraph",Space,Str "can",Space,Str "be",Space,Str "indented,",Space,Str "or",Space,Str "just",Space,Str "the",Space,Str "first",SoftBreak,Str "line.",Space,Str "In",Space,Str "this",Space,Str "way,",Space,Str "multi-paragraph",Space,Str "footnotes",Space,Str "work",Space,Str "like",SoftBreak,Str "multi-paragraph",Space,Str "list",Space,Str "items."]]]
  ,Para [Str "This",Space,Str "paragraph",Space,Str "won't",Space,Str "be",Space,Str "part",Space,Str "of",Space,Str "the",Space,Str "note,",Space,Str "because",Space,Str "it",SoftBreak,Str "isn't",Space,Str "indented."]]
