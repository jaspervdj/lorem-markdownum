--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module LoremMarkdownum.Web.Views
    ( index
    , markdownHtml
    ) where


--------------------------------------------------------------------------------
import           Data.Maybe                   (isNothing)
import           Data.Text                    (Text)
import           Text.Blaze.Html5             (Html, (!))
import qualified Text.Blaze.Html5             as H
import qualified Text.Blaze.Html5.Attributes  as A


--------------------------------------------------------------------------------
import           LoremMarkdownum.Gen.Markdown
import           LoremMarkdownum.Print


--------------------------------------------------------------------------------
index :: PrintConfig -> MarkdownEnv -> Markdown -> Html
index pc me markdown = H.docTypeHtml $ do
    H.head $ do
        H.meta H.! A.charset "UTF-8"
        H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1"
        H.title "Lorem Markdownum"
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "lorem-markdownum.css"
        H.script ! A.type_ "text/javascript" ! A.src "lorem-markdownum.js"  $ ""

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
                    H.div ! A.class_ "columns" ! A.style "display: flex;" $ do
                        H.div ! A.class_ "column" $ do
                            checkbox (moNoHeaders mo)        "no-headers"        "No headers"
                            checkbox (moNoCode mo)           "no-code"           "No code snippets"
                            checkbox (moNoInlineMarkup mo)   "no-inline-markup"  "No inline markup"
                            checkbox (moNoQuotes mo)         "no-quotes"         "No blockquotes"
                            checkbox (moNoLists mo)          "no-lists"          "No lists"
                            checkbox (isNothing $ pcWrapCol pc) "no-wrapping"    "No wrapping"

                        H.div ! A.class_ "column" $ do
                            checkbox (moUnderlineHeaders mo) "underline-headers" "Underlined headers"
                            checkbox (moReferenceLinks mo)   "reference-links"   "Reference-style links"

                            checkbox (moUnderscoreEm mo)     "underscore-em"     $ (H.code "_" <> "-style em")
                            checkbox (moUnderscoreStrong mo) "underscore-strong" $ (H.code "__" <> "-style strong text")
                            checkbox (moFencedCodeBlocks mo) "fenced-code-blocks" $ (H.code "```" <> "-style code blocks")
                            H.input ! A.type_ "text" ! A.size "2"
                                ! A.name "num-blocks" ! A.id "num-blocks"
                                ! A.class_ "small"
                            H.label ! A.for "num-blocks" ! A.class_ "input-label"
                                $ "Number of blocks"
                            H.br
                            H.input ! A.type_ "text" ! A.size "2"
                                ! A.name "seed" ! A.id "seed"
                                ! A.class_ "small"
                            H.label ! A.for "seed" ! A.class_ "input-label"
                                $ "Seed"

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
            H.div ! A.id "loading" ! A.style "display: none;" $ "⌛"
            H.div ! A.id "markdown-html" ! A.class_ "slider" $
                markdownHtml pc me markdown
  where
    loremIpsumUrl  = "http://www.lipsum.com/"
    markdownUrl    = "http://daringfireball.net/projects/markdown/"
    markovChainUrl = "http://en.wikipedia.org/wiki/Markov_chain"
    jaspervdjUrl   = "http://jaspervdj.be/"
    haskellUrl     = "http://www.haskell.org/"
    githubUrl      = "http://github.com/jaspervdj/lorem-markdownum"
    httpApiUrl     = "http://github.com/jaspervdj/lorem-markdownum#http-api"

    mo = meOptions me


--------------------------------------------------------------------------------
checkbox :: Bool -> Text -> Html -> Html
checkbox checked id' label = do
    (if checked then (! A.checked "checked") else id) $
        H.input ! A.type_ "checkbox" ! A.id (H.toValue id')
            ! A.name (H.toValue id') ! A.class_ "small"
    H.label ! A.for (H.toValue id') $ label
    H.br


--------------------------------------------------------------------------------
markdownHtml :: PrintConfig -> MarkdownEnv -> Markdown -> Html
markdownHtml pc mc md = do
    H.pre ! A.class_ "markdown" $
        H.toHtml $ runPrintWith pc $ printMarkdown mc md
    H.div ! A.class_ "html" ! A.style "display: none;" $ previewMarkdown md
