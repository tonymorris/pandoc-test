module PandocTest2 where

import Prelude
import Data.Map(Map)
import qualified Data.Map as Map
import Text.Pandoc
import Text.Pandoc.Writers.Markdown
import Data.Default

mdFile =
  writeFile "/tmp/example-question.md" md

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
    , ("date", MetaString "20170121")
    ]
  )

blocks ::
  [Block]
blocks =
  [Para [Str "What",Space,Str "is",Space,Str "the",Space,Str "colour",Space,Str "of",Space,Str "the",Space,Str "sky?"]
    ,OrderedList (1,Decimal,Period)
     [[Plain [Str "blue"]]
     ,[Plain [Str "green"]]
     ,[Plain [Str "red"]]]
    ,Para [Image ("",[],[]) [Str "data61"] ("http://i.imgur.com/0h9dFhl.png","fig:")]
    ,HorizontalRule
    ,OrderedList (2,Decimal,Period)
     [[Plain [Str "green"]]]]
