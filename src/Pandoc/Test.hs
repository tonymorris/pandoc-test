{-# LANGUAGE GADTs #-}

module Pandoc.Test where
  
import Control.Lens
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Data.Map(Map)
import qualified Data.Map as Map
import System.FilePath
import Text.Pandoc.Definition
import Text.Pandoc

import Prelude

main ::
  IO ()
main =
  let outdir = "/tmp/xyz"
      options = def
  in  sequence_
        [
          writeFile (outdir </> "file.txt") (writeAsciiDoc options pandoc)
        , writeFile (outdir </> "file.xml") (writeDocbook options pandoc)
        , writeDocx options pandoc >>= BSL.writeFile (outdir </> "file.docx")
        , writeFile (outdir </> "file.docuwiki") (writeDokuWiki options pandoc)
        , writeEPUB options pandoc >>= BSL.writeFile (outdir </> "file.epub")
        , writeFB2 options pandoc >>= writeFile (outdir </> "file.fb2")
        , writeFile (outdir </> "file.haddock") (writeHaddock options pandoc)
        , writeFile (outdir </> "file.html") (writeHtmlString options pandoc)
        , writeFile (outdir </> "file.json") (writeJSON options pandoc)
        , writeFile (outdir </> "file.tex") (writeLaTeX options pandoc)
        , writeFile (outdir </> "file.man") (writeMan options pandoc)
        , writeFile (outdir </> "file.md") (writeMarkdown options pandoc)
        , writeFile (outdir </> "file.mediawiki") (writeMediaWiki options pandoc)
        , writeODT options pandoc >>= BSL.writeFile (outdir </> "file.odt")
        , writeFile (outdir </> "file.odt.xml") (writeOpenDocument options pandoc)
        , writeFile (outdir </> "file.org") (writeOrg options pandoc)
        , writeFile (outdir </> "file.plain") (writePlain options pandoc)
        , writeFile (outdir </> "file.rst") (writeRST options pandoc)
        , writeFile (outdir </> "file.rtf") (writeRTF options pandoc)
        , writeFile (outdir </> "file.tei") (writeTEI options pandoc)
        , writeFile (outdir </> "file.texinfo") (writeTexinfo options pandoc)
        , writeFile (outdir </> "file.textile") (writeTextile options pandoc)
        , writeFile (outdir </> "file.zimwiki") (writeZimWiki options pandoc)
        ]

pandoc ::
  Pandoc
pandoc =
  Pandoc metatest blockstest

metatest ::
  (Unwrapped t ~ Map String a, AsMetaString a, Rewrapped t t) =>
  t
metatest =
  _Wrapped # (
    Map.fromList
      [
        ("title", _MetaString # "The title")
      , ("author", _MetaString # "Tony Morris")
      , ("date", _MetaString # "20170121")
      ]
    )

blockstest ::
  (AsPara t, AsOrderedList t, AsHorizontalRule t) =>
  [t]
blockstest =
  let sstr x = intersperse (_Space # ()) . map (_Str #) . words $ x
      hr = _HorizontalRule # ()
      ol x = _OrderedList # ((1,_Decimal # (),_Period # ()), x)
  in  [
        _Para # sstr "What is the colour of the sky"
      , ol
          [
            [_Plain # [_Str # "blue"]]
          , [_Plain # [_Str # "green"]]
          , [_Plain # [_Str # "red"]]
          ]
      , _Para # [_Image # (nullAttr, sstr "data61", ("http://i.imgur.com/0h9dFhl.png","fig:"))]
      , hr
      , ol [[_Plain # sstr "green"]]
      ]

blockstestx ::
  (AsPara t, AsOrderedList t, AsHorizontalRule t) =>
  [t]
blockstestx =
  [
    _Para # [_Str # "What",_Space # (),_Str # "is",_Space # (),_Str # "the",_Space # (),_Str # "colour",_Space # (),_Str # "of",_Space # (),_Str # "the",_Space # (),_Str # "sky?"]
  , _OrderedList # ((1,_Decimal # (),_Period # ())
  , [[_Plain # [_Str # "blue"]]
  , [_Plain # [_Str # "green"]]
  , [_Plain # [_Str # "red"]]])
  , _Para # [_Image # (("",[],[]), [_Str #  "data61"], ("http://i.imgur.com/0h9dFhl.png","fig:"))]
  , _HorizontalRule # ()
  , _OrderedList # ((2,_Decimal # (),_Period # ()), [[_Plain # [_Str # "green"]]])
  ]
  