--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module LoremMarkdownum.Web.Views
    ( index
    , markdownHtml
    ) where


--------------------------------------------------------------------------------
import           Data.Foldable                (for_)
import           Data.Maybe                   (fromMaybe)
import           Data.Text                    (Text)
import qualified Text.Blaze.Html5             as H
import           Text.Blaze.Html5             (Html, (!))
import qualified Text.Blaze.Html5.Attributes  as A


--------------------------------------------------------------------------------
import           LoremMarkdownum.Gen.Markdown
import           LoremMarkdownum.Options
import           LoremMarkdownum.Print


--------------------------------------------------------------------------------
index :: Options -> Markdown -> Html
index opts markdown = H.docTypeHtml $ do
    H.head $ do
        H.meta H.! A.charset "UTF-8"
        H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1"
        H.title "Lorem Markdownum"
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "lorem-markdownum.css?v=1"
        H.script ! A.type_ "text/javascript" ! A.src "lorem-markdownum.js?v=2"  $ ""

    H.body $ do
        H.header "Lorem Markdownum"
        H.hr
        H.div ! A.id "info" $ do
            H.p $ do
                "Inspired by the many excellent "
                H.a ! A.href loremIpsumUrl $ "lorem ipsum generators"
                ", this simple webapp generates structured placeholder text in "
                H.a ! A.href markdownUrl $ "markdown format"
                ". In order to do so, it uses "
                H.a ! A.href markovChainUrl $ "Markov Chains"
                " and many heuristics."

            H.p $ do
                "Written in "
                H.a ! A.href haskellUrl $ "Haskell"
                " by "
                H.a ! A.href jaspervdjUrl $ "Jasper Van der Jeugt"
                ". The source code is "
                H.a ! A.href githubUrl $ "on GitHub"
                " and an "
                H.a ! A.href httpApiUrl $ "HTTP API"
                " is also available!"

        H.hr
        H.div H.! A.id "controls" $ do
            H.form ! A.id "form-generate" ! A.method "GET" $ do
                H.div ! A.id "advanced" ! A.class_ "slider" ! A.style "height: 0px" $
                    H.div ! A.class_ "form-columns" ! A.style "display: flex;" $ do
                        H.div ! A.class_ "labelgrid" $ do
                            H.div ! A.class_ "labelgroup" $ "Blocks"
                            choice (fromMaybe HeaderHash         $ oHeaders opts)        "headers"         "Headers"
                            choice (fromMaybe CodeBlockIndent    $ oCodeBlocks opts)     "code-blocks"     "Code blocks"
                            choice (fromMaybe OrderedListDecimal $ oOrderedLists opts)   "ordered-lists"   "Ordered Lists"
                            choice (fromMaybe UnorderedListDash  $ oUnorderedLists opts) "unordered-lists" "Unordered Lists"
                            checkbox (oNoQuotes opts)         "no-quotes"         "No blockquotes"
                            H.input ! A.type_ "text" ! A.size "2"
                                ! A.name "num-blocks" ! A.id "num-blocks"
                                ! A.class_ "small"
                            H.label ! A.for "num-blocks" ! A.class_ "input-label"
                                $ "Number of blocks"

                        H.div ! A.class_ "labelgrid" $ do
                            H.div ! A.class_ "labelgroup" $ "Inline"
                            checkbox (oReferenceLinks opts)   "reference-links"   "Reference-style links"
                            checkbox (oNoInlineCode opts)     "no-inline-code"    "No inline code"
                            choice (fromMaybe EmphasisAsterisk $ oEmphasis opts) "emphasis" "Emphasis"
                            choice (fromMaybe StrongAsterisk   $ oStrong opts)   "strong"   "Strong"
                            H.div ! A.class_ "labelgroup" $ "Global"
                            checkbox (oNoWrapping opts)       "no-wrapping"       "No wrapping"
                            H.input ! A.type_ "text" ! A.size "2"
                                ! A.name "seed" ! A.id "seed"
                                ! A.class_ "small"
                            H.label ! A.for "seed" ! A.class_ "input-label"
                                $ "Seed"

                H.div ! A.class_ "labelgrid" $ do
                    checkbox False "show-advanced" "Advanced settings"
                    checkbox False "preview-html" "Preview as HTML"

                H.div ! A.class_ "buttons form-columns" $ do
                    H.input ! A.type_ "button" ! A.id "copy" !
                        A.value "Copy to clipboard"
                    H.input ! A.type_ "submit" ! A.id "generate" !
                        A.value "Generate some markdown!"

        H.hr
        H.div ! A.id "results" $ do
            H.div ! A.id "loading" ! A.style "display: none;" $ "⌛"
            H.div ! A.id "markdown-html" ! A.class_ "slider" $
                markdownHtml opts markdown
  where
    loremIpsumUrl  = "http://www.lipsum.com/"
    markdownUrl    = "http://daringfireball.net/projects/markdown/"
    markovChainUrl = "http://en.wikipedia.org/wiki/Markov_chain"
    jaspervdjUrl   = "http://jaspervdj.be/"
    haskellUrl     = "http://www.haskell.org/"
    githubUrl      = "http://github.com/jaspervdj/lorem-markdownum"
    httpApiUrl     = "http://github.com/jaspervdj/lorem-markdownum#http-api"


--------------------------------------------------------------------------------
checkbox :: Bool -> Text -> Html -> Html
checkbox checked id' label = do
    (if checked then (! A.checked "checked") else id) $
        H.input ! A.type_ "checkbox" ! A.id (H.toValue id')
            ! A.name (H.toValue id') ! A.class_ "small"
    H.label ! A.for (H.toValue id') $ label


--------------------------------------------------------------------------------
choice :: Choice c => c -> Text -> Html -> Html
choice value id' label = do
    H.select ! A.type_ "checkbox" ! A.id (H.toValue id')
            ! A.name (H.toValue id') ! A.class_ "small" $
        for_ [minBound .. maxBound] $ \c ->
            (if c == value then (! A.selected "selected") else id) $
            H.option ! A.value (H.toValue $ choiceValue c) $
                H.toHtml $ choiceLabel c
    H.label ! A.for (H.toValue id') $ label


--------------------------------------------------------------------------------
markdownHtml :: Options -> Markdown -> Html
markdownHtml opts md = do
    H.pre ! A.class_ "markdown" $
        H.toHtml $ runPrintWith (toPrintOptions opts) $ printMarkdown mopts md
    H.div ! A.class_ "html" ! A.style "display: none;" $
        previewMarkdown mopts md
  where
    mopts = toMarkdownOptions opts
