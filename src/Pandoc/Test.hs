{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Pandoc.Test where
  
import Control.Lens
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Data.Map(Map)
import qualified Data.Map as Map
import System.FilePath
import System.IO
import Text.Pandoc.Definition
import Text.Pandoc
import Text.Pandoc.PDF 

import Prelude

data Test x a =
  Test {
    _question ::
      a
  , _answer ::
      Answer a
  , _testmeta ::
      x
  } deriving (Eq, Ord, Show)

instance Functor (Test x) where
  fmap f (Test a as x) =
    Test (f a) (fmap f as) x

instance Foldable (Test x) where
  foldr f z (Test a as _) =
    f a (foldr f z as)

instance Traversable (Test x) where
  traverse f (Test a as x) =
    Test <$> f a <*> traverse f as <*> pure x

data MultichoiceAnswer a =
  MultichoiceAnswer {
    _multichoicelefts ::
      [a]
  , _multichoicefocus ::
      a
  , _multichoicerights ::
      [a]
  } deriving (Eq, Ord, Show)

instance Functor MultichoiceAnswer where
  fmap f (MultichoiceAnswer l x r) =
    MultichoiceAnswer (fmap f l) (f x) (fmap f r)
    
instance Foldable MultichoiceAnswer where
  foldr f z (MultichoiceAnswer l x r) =
    foldr f (f x (foldr f z r)) l

instance Traversable MultichoiceAnswer where
  traverse f (MultichoiceAnswer l x r) =
    MultichoiceAnswer <$> traverse f l <*> f x <*> traverse f r

data Answer a =
  DirectAnswer a
  | MultiAnswer [a]
  | Multichoice (MultichoiceAnswer a)
  deriving (Eq, Ord, Show)

instance Functor Answer where
  fmap f (DirectAnswer a) =
    DirectAnswer (f a)
  fmap f (MultiAnswer as) =
    MultiAnswer (fmap f as)
  fmap f (Multichoice x) =
    Multichoice (fmap f x)
    
instance Foldable Answer where
  foldr f z (DirectAnswer a) =
    f a z
  foldr f z (MultiAnswer a) =
    foldr f z a
  foldr f z (Multichoice x) =
    foldr f z x

instance Traversable Answer where
  traverse f (DirectAnswer a) =
    DirectAnswer <$> f a
  traverse f (MultiAnswer a) =
    MultiAnswer <$> traverse f a
  traverse f (Multichoice x) =
    Multichoice <$> traverse f x

newtype Tests x a =
  Tests [Test x a]
  deriving (Eq, Ord, Show)

instance Functor (Tests x) where
  fmap f (Tests x) =
    Tests (fmap (fmap f) x)

instance Foldable (Tests x) where
  foldr f z (Tests x) =
    foldr (\q b -> foldr f b q) z x

instance Traversable (Tests x) where
  traverse f (Tests x) =
    Tests <$> traverse (traverse f) x

data Exam x m a =
  Exam {
    _exammeta ::
      m
  , _examtests ::
      Tests x a
  } deriving (Eq, Ord, Show)

instance Functor (Exam x m) where
  fmap f (Exam m x) =
    Exam m (fmap f x)

instance Foldable (Exam x m) where
  foldr f z (Exam _ x) =
    foldr f z x

instance Traversable (Exam x m) where
  traverse f (Exam m x) =
    Exam m <$> traverse f x

newtype Exams x m a =
  Exams [Exam x m a]
  deriving (Eq, Ord, Show)

instance Functor (Exams x m) where
  fmap f (Exams x) =
    Exams (fmap (fmap f) x)

instance Foldable (Exams x m) where
  foldr f z (Exams x) =
    foldr (\q b -> foldr f b q) z x

instance Traversable (Exams x m) where
  traverse f (Exams x) =
    Exams <$> traverse (traverse f) x

makeClassy ''Test
makeClassyPrisms ''Answer
makeClassy ''MultichoiceAnswer
makeWrapped ''Tests
makeWrapped ''Exams

(###) ::
  Test q a
  -> x
  -> Test x a
Test q a _ ### x =
  Test q a x

infixl 4 ###

(~>) ::
  a
  -> MultichoiceAnswer a
  -> Test () a
(~>) q a =
  Test q (Multichoice a) ()

infixl 5 ~>

---- END exam data types

sstr ::
  (AsSpace a, AsStr a) =>
  String
  -> [a]
sstr =
  intersperse (_Space # ()) . map (_Str #) . words

hr ::
  AsHorizontalRule t =>
  t
hr =
  _HorizontalRule # ()

---- END pandoc helpers

data TestMeta =
  RPL
  | PPL
  | Meteorology
  deriving (Eq, Ord, Show)

makeClassyPrisms ''TestMeta

testExam ::
  (AsTestMeta tm, AsHorizontalRule x, AsPlain x, AsPara x, AsMetaValue m) =>
  Exam [tm] [(String, m)] x
testExam =
  Exam
    [
      ("title", _MetaValue . _MetaString # "The title")
    , ("author", _MetaValue . _MetaString # "Tony Morris")
    , ("date", _MetaValue . _MetaString # "20170121")
    ]
    $
    Tests 
      [
        _Para # sstr "What is the colour of the sky" ~>
          MultichoiceAnswer
            [
              _Plain # sstr "green"
            ]
            (_Plain # sstr "blue")
            [
              _Plain # sstr "red"
            ]
          ### [_TestMeta . _PPL # ()]
      , _Para # sstr "No really, what is the colour of the sky" ~>
          MultichoiceAnswer
            [
              _Para # [_Image # (nullAttr, sstr "data61", ("http://i.imgur.com/0h9dFhl.png","fig:"))]
            , hr
            ]
            (_Plain # sstr "blue")
            [
              _Plain # sstr "red"
            ]
          ### [_TestMeta . _Meteorology # ()]
      ]

---- END aviation specific exams

blockstest ::
  (AsPara t, AsOrderedList t, AsHorizontalRule t) =>
  [t]
blockstest =
  let ol x = _OrderedList # ((1,_Decimal # (),_Period # ()), x)
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

---- END test documents

pdfTemplate ::
  String
pdfTemplate =
  "\\documentclass[$if(fontsize)$$fontsize$,$endif$$if(lang)$$babel-lang$,$endif$$if(papersize)$$papersize$paper,$endif$$for(classoption)$$classoption$$sep$,$endfor$]{$documentclass$}\n$if(beamerarticle)$\n\\usepackage{beamerarticle} % needs to be loaded first\n$endif$\n$if(fontfamily)$\n\\usepackage[$for(fontfamilyoptions)$$fontfamilyoptions$$sep$,$endfor$]{$fontfamily$}\n$else$\n\\usepackage{lmodern}\n$endif$\n$if(linestretch)$\n\\usepackage{setspace}\n\\setstretch{$linestretch$}\n$endif$\n\\usepackage{amssymb,amsmath}\n\\usepackage{ifxetex,ifluatex}\n\\usepackage{fixltx2e} % provides \\textsubscript\n\\ifnum 0\\ifxetex 1\\fi\\ifluatex 1\\fi=0 % if pdftex\n  \\usepackage[$if(fontenc)$$fontenc$$else$T1$endif$]{fontenc}\n  \\usepackage[utf8]{inputenc}\n$if(euro)$\n  \\usepackage{eurosym}\n$endif$\n\\else % if luatex or xelatex\n  \\ifxetex\n    \\usepackage{mathspec}\n  \\else\n    \\usepackage{fontspec}\n  \\fi\n  \\defaultfontfeatures{Ligatures=TeX,Scale=MatchLowercase}\n$for(fontfamilies)$\n  \\newfontfamily{$fontfamilies.name$}[$fontfamilies.options$]{$fontfamilies.font$}\n$endfor$\n$if(euro)$\n  \\newcommand{\\euro}{€}\n$endif$\n$if(mainfont)$\n    \\setmainfont[$for(mainfontoptions)$$mainfontoptions$$sep$,$endfor$]{$mainfont$}\n$endif$\n$if(sansfont)$\n    \\setsansfont[$for(sansfontoptions)$$sansfontoptions$$sep$,$endfor$]{$sansfont$}\n$endif$\n$if(monofont)$\n    \\setmonofont[Mapping=tex-ansi$if(monofontoptions)$,$for(monofontoptions)$$monofontoptions$$sep$,$endfor$$endif$]{$monofont$}\n$endif$\n$if(mathfont)$\n    \\setmathfont(Digits,Latin,Greek)[$for(mathfontoptions)$$mathfontoptions$$sep$,$endfor$]{$mathfont$}\n$endif$\n$if(CJKmainfont)$\n    \\usepackage{xeCJK}\n    \\setCJKmainfont[$for(CJKoptions)$$CJKoptions$$sep$,$endfor$]{$CJKmainfont$}\n$endif$\n\\fi\n% use upquote if available, for straight quotes in verbatim environments\n\\IfFileExists{upquote.sty}{\\usepackage{upquote}}{}\n% use microtype if available\n\\IfFileExists{microtype.sty}{%\n\\usepackage{microtype}\n\\UseMicrotypeSet[protrusion]{basicmath} % disable protrusion for tt fonts\n}{}\n$if(geometry)$\n\\usepackage[$for(geometry)$$geometry$$sep$,$endfor$]{geometry}\n$endif$\n\\usepackage[unicode=true]{hyperref}\n$if(colorlinks)$\n\\PassOptionsToPackage{usenames,dvipsnames}{color} % color is loaded by hyperref\n$endif$\n\\hypersetup{\n$if(title-meta)$\n            pdftitle={$title-meta$},\n$endif$\n$if(author-meta)$\n            pdfauthor={$author-meta$},\n$endif$\n$if(keywords)$\n            pdfkeywords={$for(keywords)$$keywords$$sep$, $endfor$},\n$endif$\n$if(colorlinks)$\n            colorlinks=true,\n            linkcolor=$if(linkcolor)$$linkcolor$$else$Maroon$endif$,\n            citecolor=$if(citecolor)$$citecolor$$else$Blue$endif$,\n            urlcolor=$if(urlcolor)$$urlcolor$$else$Blue$endif$,\n$else$\n            pdfborder={0 0 0},\n$endif$\n            breaklinks=true}\n\\urlstyle{same}  % don't use monospace font for urls\n$if(lang)$\n\\ifnum 0\\ifxetex 1\\fi\\ifluatex 1\\fi=0 % if pdftex\n  \\usepackage[shorthands=off,$for(babel-otherlangs)$$babel-otherlangs$,$endfor$main=$babel-lang$]{babel}\n$if(babel-newcommands)$\n  $babel-newcommands$\n$endif$\n\\else\n  \\usepackage{polyglossia}\n  \\setmainlanguage[$polyglossia-lang.options$]{$polyglossia-lang.name$}\n$for(polyglossia-otherlangs)$\n  \\setotherlanguage[$polyglossia-otherlangs.options$]{$polyglossia-otherlangs.name$}\n$endfor$\n\\fi\n$endif$\n$if(natbib)$\n\\usepackage{natbib}\n\\bibliographystyle{$if(biblio-style)$$biblio-style$$else$plainnat$endif$}\n$endif$\n$if(biblatex)$\n\\usepackage[$if(biblio-style)$style=$biblio-style$,$endif$$for(biblatexoptions)$$biblatexoptions$$sep$,$endfor$]{biblatex}\n$for(bibliography)$\n\\addbibresource{$bibliography$}\n$endfor$\n$endif$\n$if(listings)$\n\\usepackage{listings}\n$endif$\n$if(lhs)$\n\\lstnewenvironment{code}{\\lstset{language=Haskell,basicstyle=\\small\\ttfamily}}{}\n$endif$\n$if(highlighting-macros)$\n$highlighting-macros$\n$endif$\n$if(verbatim-in-note)$\n\\usepackage{fancyvrb}\n\\VerbatimFootnotes % allows verbatim text in footnotes\n$endif$\n$if(tables)$\n\\usepackage{longtable,booktabs}\n% Fix footnotes in tables (requires footnote package)\n\\IfFileExists{footnote.sty}{\\usepackage{footnote}\\makesavenoteenv{long table}}{}\n$endif$\n$if(graphics)$\n\\usepackage{graphicx,grffile}\n\\makeatletter\n\\def\\maxwidth{\\ifdim\\Gin@nat@width>\\linewidth\\linewidth\\else\\Gin@nat@width\\fi}\n\\def\\maxheight{\\ifdim\\Gin@nat@height>\\textheight\\textheight\\else\\Gin@nat@height\\fi}\n\\makeatother\n% Scale images if necessary, so that they will not overflow the page\n% margins by default, and it is still possible to overwrite the defaults\n% using explicit options in \\includegraphics[width, height, ...]{}\n\\setkeys{Gin}{width=\\maxwidth,height=\\maxheight,keepaspectratio}\n$endif$\n$if(links-as-notes)$\n% Make links footnotes instead of hotlinks:\n\\renewcommand{\\href}[2]{#2\\footnote{\\url{#1}}}\n$endif$\n$if(strikeout)$\n\\usepackage[normalem]{ulem}\n% avoid problems with \\sout in headers with hyperref:\n\\pdfstringdefDisableCommands{\\renewcommand{\\sout}{}}\n$endif$\n$if(indent)$\n$else$\n\\IfFileExists{parskip.sty}{%\n\\usepackage{parskip}\n}{% else\n\\setlength{\\parindent}{0pt}\n\\setlength{\\parskip}{6pt plus 2pt minus 1pt}\n}\n$endif$\n\\setlength{\\emergencystretch}{3em}  % prevent overfull lines\n\\providecommand{\\tightlist}{%\n  \\setlength{\\itemsep}{0pt}\\setlength{\\parskip}{0pt}}\n$if(numbersections)$\n\\setcounter{secnumdepth}{$if(secnumdepth)$$secnumdepth$$else$5$endif$}\n$else$\n\\setcounter{secnumdepth}{0}\n$endif$\n$if(subparagraph)$\n$else$\n% Redefines (sub)paragraphs to behave more like sections\n\\ifx\\paragraph\\undefined\\else\n\\let\\oldparagraph\\paragraph\n\\renewcommand{\\paragraph}[1]{\\oldparagraph{#1}\\mbox{}}\n\\fi\n\\ifx\\subparagraph\\undefined\\else\n\\let\\oldsubparagraph\\subparagraph\n\\renewcommand{\\subparagraph}[1]{\\oldsubparagraph{#1}\\mbox{}}\n\\fi\n$endif$\n$if(dir)$\n\\ifxetex\n  % load bidi as late as possible as it modifies e.g. graphicx\n  $if(latex-dir-rtl)$\n  \\usepackage[RTLdocument]{bidi}\n  $else$\n  \\usepackage{bidi}\n  $endif$\n\\fi\n\\ifnum 0\\ifxetex 1\\fi\\ifluatex 1\\fi=0 % if pdftex\n  \\TeXXeTstate=1\n  \\newcommand{\\RL}[1]{\\beginR #1\\endR}\n  \\newcommand{\\LR}[1]{\\beginL #1\\endL}\n  \\newenvironment{RTL}{\\beginR}{\\endR}\n  \\newenvironment{LTR}{\\beginL}{\\endL}\n\\fi\n$endif$\n\n% set default figure placement to htbp\n\\makeatletter\n\\def\\fps@figure{htbp}\n\\makeatother\n\n$for(header-includes)$\n$header-includes$\n$endfor$\n\n$if(title)$\n\\title{$title$$if(thanks)$\\thanks{$thanks$}$endif$}\n$endif$\n$if(subtitle)$\n\\providecommand{\\subtitle}[1]{}\n\\subtitle{$subtitle$}\n$endif$\n$if(author)$\n\\author{$for(author)$$author$$sep$ \\and $endfor$}\n$endif$\n$if(institute)$\n\\providecommand{\\institute}[1]{}\n\\institute{$for(institute)$$institute$$sep$ \\and $endfor$}\n$endif$\n\\date{$date$}\n\n\\begin{document}\n$if(title)$\n\\maketitle\n$endif$\n$if(abstract)$\n\\begin{abstract}\n$abstract$\n\\end{abstract}\n$endif$\n\n$for(include-before)$\n$include-before$\n\n$endfor$\n$if(toc)$\n{\n$if(colorlinks)$\n\\hypersetup{linkcolor=$if(toccolor)$$toccolor$$else$black$endif$}\n$endif$\n\\setcounter{tocdepth}{$toc-depth$}\n\\tableofcontents\n}\n$endif$\n$if(lot)$\n\\listoftables\n$endif$\n$if(lof)$\n\\listoffigures\n$endif$\n$body$\n\n$if(natbib)$\n$if(bibliography)$\n$if(biblio-title)$\n$if(book-class)$\n\\renewcommand\\bibname{$biblio-title$}\n$else$\n\\renewcommand\\refname{$biblio-title$}\n$endif$\n$endif$\n\\bibliography{$for(bibliography)$$bibliography$$sep$,$endfor$}\n\n$endif$\n$endif$\n$if(biblatex)$\n\\printbibliography$if(biblio-title)$[title=$biblio-title$]$endif$\n\n$endif$\n$for(include-after)$\n$include-after$\n\n$endfor$\n\\end{document}"  

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
        , makePDF "pdflatex" writeLaTeX (options { writerTemplate = Just pdfTemplate }) pandoc >>= either (hPutStrLn stderr . BSLC8.unpack) (BSL.writeFile (outdir </> "file.pdf"))
        ]

pandoc ::
  Pandoc
pandoc =
  Pandoc metatest blockstest

metatest ::
  (Unwrapped t ~ Map String a, AsMetaValue a, Rewrapped t t) =>
  t
metatest =
  _Wrapped # (
    Map.fromList
      [
        ("title", _MetaValue . _MetaString # "The title")
      , ("author", _MetaValue . _MetaString # "Tony Morris")
      , ("date", _MetaValue . _MetaString # "20170121")
      ]
    )
