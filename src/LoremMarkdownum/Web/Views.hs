--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module LoremMarkdownum.Web.Views
    ( index
    , markdownHtml
    ) where


--------------------------------------------------------------------------------
import           Data.Maybe                   (isNothing)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import           Text.Blaze.Html5             (Html, (!))
import qualified Text.Blaze.Html5             as H
import qualified Text.Blaze.Html5.Attributes  as A


--------------------------------------------------------------------------------
import           LoremMarkdownum.Gen.Markdown
import           LoremMarkdownum.Print


--------------------------------------------------------------------------------
index :: PrintConfig -> MarkdownConfig -> Markdown -> Html
index pc mc markdown = H.docTypeHtml $ do
    H.head $ do
        H.meta H.! A.charset "UTF-8"
        H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1"
        H.title "Lorem Markdownum"
        H.style $ do
            "html {"
            "    text-align: center;"
            "}"

            "body {"
            "    background-color: #fafafa;"
            "    color: #111;"
            "    font-family: serif;"
            "    font-size: 16px;"
            "    margin: 0px auto;"
            "    padding: 12px;"
            "    text-align: left;"
            "    max-width: 640px;"
            "}"

            "div#info h1 {"
            "    color: #777;"
            "    font-size: 24px;"
            "    font-style: italic;"
            "    font-variant: small-caps;"
            "    margin: 0px;"
            "    text-align: center;"
            "}"

            "div#info p {"
            "    line-height: 140%;"
            "    margin: 12px 0px;"
            "    text-align: justify;"
            "}"

            "div#info a {"
            "    color: #222;"
            "}"

            "div#controls div#advanced {"
            "    margin-bottom: 24px;"
            "}"

            "div#controls div.columns {"
            "    display: block;"
            "}"

            "@media (min-width: 480px) {"
            "    div#controls div.columns {"
            "        display: flex;"
            "        justify-content: space-between;"
            "    }"
            "}"

            "div#controls div.column {"
            "    display: inline;"
            "    flex-grow: 1;"
            "    flex-basis: 0;"
            "}"

            "div.buttons {"
            "    margin: 12px 0px 6px 0px;"
            "}"

            "div.buttons input {"
            "    width: 100%;"
            "    margin: 0px 0px 6px 0px;"
            "}"

            "div.buttons div.column {"
            "    margin: 0px 3px 0px 3px;"
            "}"

            "input.small {"
            "    margin: 0px 6px 0px 0px;"
            "    padding: none;"
            "    text-align: center;"
            "    width: 25px;"
            "}"

            "div#output {"
            "    margin: 0px 6px 0px 0px;"
            "    padding: none;"
            "    text-align: center;"
            "    width: 25px;"
            "}"

            "pre, code {"
            "    color: #333;"
            "    font-family: 'Inconsolata', 'Courier New', monospace;"
            "    font-size: 14px;"
            "}"

            "div#loading {"
            "    text-align: center;"
            "    max-width: 640px;"
            "}"

            -- Blockquotes are rendered awfully by default so we need to fix
            -- that a bit.
            "blockquote {"
            "    border-left: solid #777 2px;"
            "    margin-left: 12px;"
            "    padding-left: 12px;"
            "}"

        H.script ! A.type_ "text/javascript" ! A.src "jquery-1.10.2.min.js" $ ""
        H.script ! A.type_ "text/javascript" ! A.src "lorem-markdownum.js"  $ ""

    H.body $ do
        H.div ! A.id "info" $ do

            H.h1 "Lorem Markdownum"
            H.hr
            H.p $ do
                "Inspired by the many excellent "
                H.a ! A.href loremIpsumUrl $ "lorem ipsum generators"
                ", this simple webapp generates structured placeholder text in "
                H.a ! A.href markdownUrl $ "markdown format"
                ". In order to do so, it uses "
                H.a ! A.href markovChainUrl $ "Markov Chains"
                " and many heuristics. It was written in "
                H.a ! A.href haskellUrl $ "Haskell"
                " and the source code is available "
                H.a ! A.href githubUrl $ "on GitHub"
                ". Created by "
                H.a ! A.href jaspervdjUrl $ "Jasper Van der Jeugt"
                ". An "
                H.a ! A.href httpApiUrl $ "HTTP API"
                " is also available!"

        H.hr
        H.div H.! A.id "controls" $ do
            H.form ! A.id "form-generate" ! A.method "GET" $ do

                H.div ! A.id "advanced" ! A.class_ "columns" ! A.style "display: none;" $ do
                    H.div ! A.class_ "column" $ do
                        checkbox (mcNoHeaders mc)        "no-headers"        "No headers"
                        checkbox (mcNoCode mc)           "no-code"           "No code snippets"
                        checkbox (mcNoInlineMarkup mc)   "no-inline-markup"  "No inline markup"
                        checkbox (mcNoQuotes mc)         "no-quotes"         "No blockquotes"
                        checkbox (mcNoLists mc)          "no-lists"          "No lists"
                        checkbox (mcNoExternalLinks mc)  "no-external-links" "No external links"
                        checkbox (isNothing $ pcWrapCol pc) "no-wrapping"    "No wrapping"

                    H.div ! A.class_ "column" $ do
                        checkbox (mcUnderlineHeaders mc) "underline-headers" "Underlined headers"
                        checkbox (mcReferenceLinks mc)   "reference-links"   "Reference-style links"

                        checkbox (mcUnderscoreEm mc)     "underscore-em"     $ (H.code "_" <> "-style em")
                        checkbox (mcUnderscoreStrong mc) "underscore-strong" $ (H.code "__" <> "-style strong text")
                        checkbox (mcFencedCodeBlocks mc) "fenced-code-blocks" $ (H.code "```" <> "-style code blocks")
                        H.input ! A.type_ "text" ! A.size "2"
                            ! A.name "num-blocks" ! A.id "num-blocks"
                            ! A.class_ "small"
                        H.label ! A.for "num-blocks" ! A.class_ "input-label"
                            $ "Number of blocks"

                checkbox False "show-advanced" "Advanced settings"
                " "
                checkbox False "preview-html" "Preview as HTML"

                H.div ! A.class_ "buttons columns" $ do
                    H.div ! A.class_ "column" $
                        H.input ! A.type_ "button" ! A.id "copy" !
                            A.value "Copy to clipboard"
                    H.div ! A.class_ "column" $
                        H.input ! A.type_ "submit" ! A.id "generate" !
                            A.value "Generate some markdown!"
            H.hr

        H.div ! A.id "results" $ do
            H.div ! A.id "loading" ! A.style "display: none;" $
                H.img ! A.src "loading.gif" ! A.alt "Loading..."
            H.div ! A.id "markdown-html" $ markdownHtml pc mc markdown
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
    H.br


--------------------------------------------------------------------------------
markdownHtml :: PrintConfig -> MarkdownConfig -> Markdown -> Html
markdownHtml pc mc md = do
    H.pre ! A.class_ "markdown" $
        H.toHtml $ runPrintWith pc $ printMarkdown mc md
    H.div ! A.class_ "html" ! A.style "display: none;" $ previewMarkdown md
