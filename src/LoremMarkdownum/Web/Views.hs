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
        H.title "Lorem Markdownum"
        H.style $ do
            "html {"
            "    text-align: center;"
            "}"

            "body {"
            "    background-color: #fff;"
            "    color: #222;"
            "    font-family: sans-serif;"
            "    font-size: 16px;"
            "    margin: 36px auto 36px auto;"
            "    text-align: left;"
            "    width: 640px;"
            "}"

            "div#info {"
            "    margin-bottom: 36px;"
            "}"

            "div#info h1 {"
            "    color: #777;"
            "    font-family: serif;"
            "    font-size: 28px;"
            "    font-style: italic;"
            "    font-variant: small-caps;"
            "    margin: 0px;"
            "}"

            "div#info form input#generate {"
            "    float: right;"
            "}"

            "div#info div#advanced {"
            "    margin-bottom: 24px;"
            "}"

            "div#info div.column {"
            "    display: inline;"
            "    margin-right: 10%;"
            "    width: 40%;"
            "}"

            "div#info p {"
            "    border-bottom: solid #777 2px;"
            "    border-top: solid #777 2px;"
            "    line-height: 140%;"
            "    margin: 12px 0px 12px 0px;"
            "    padding: 12px 0px 12px 0px;"
            "    text-align: justify;"
            "}"

            "div#info a {"
            "    color: #222;"
            "}"

            "input.small {"
            "    border: none;"
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
            "    width: 640px;"
            "}"

            "div#info a.donate {"
            "    background-color: #409e48;"
            "    border-radius: 3px;"
            "    color: #ffffff;"
            "    float: right;"
            "    font-weight: bold;"
            "    margin: 3px;"
            "    padding: 3px 6px;"
            "    text-decoration: none;"
            "}"

            -- Blockquotes are rendered awfully by default so we need to fix
            -- that a bit.
            "blockquote {"
            "    border-left: solid #777 2px;"
            "    margin-left: 12px;"
            "    padding-left: 12px;"
            "}"

        -- Google analytics
        H.script ! A.type_ "text/javascript" $
            "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){    \n\
            \(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),  \n\
            \m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m) \n\
            \})(window,document,'script','//www.google-analytics.com/analytics.js','ga');    \n\
            \ga('create', 'UA-11993001-1', 'jaspervdj.be');                                  \n\
            \ga('send', 'pageview');"

        H.script ! A.type_ "text/javascript" ! A.src "jquery-1.10.2.min.js" $ ""
        H.script ! A.type_ "text/javascript" ! A.src "lorem-markdownum.js"  $ ""

    H.body $ do
        H.div ! A.id "info" $ do

            -- Flattr
            H.a ! A.href "https://jaspervdj.be/contact.html#donate"
                ! A.target "_blank"
                ! A.class_ "donate" $ "Donate"

            H.h1 "Lorem Markdownum"
            H.p $ do
                "Inspired by the many excellent "
                H.a ! A.href loremIpsumUrl $ "lorem ipsum generators"
                ", this simple webapp generates placeholder text. However, "
                "instead of generating plain text, this generator gives you "
                "structured text in the form of "
                H.a ! A.href markdownUrl $ "markdown"
                ". In order to do so, it uses "
                H.a ! A.href markovChainUrl $ "Markov Chains"
                " and many heuristics. It was written in "
                H.a ! A.href haskellUrl $ "Haskell"
                " and the source code is available "
                H.a ! A.href githubUrl $ "on GitHub"
                ". Crafted with a lot of love for the markdown format by "
                H.a ! A.href jaspervdjUrl $ "Jasper Van der Jeugt"
                ". An "
                H.a ! A.href httpApiUrl $ "HTTP API"
                " is also available!"
            H.form ! A.id "form-generate" ! A.method "GET" ! A.action "?" $ do

                H.div ! A.id "advanced" ! A.style "display: none;" $ do
                    H.div ! A.class_ "column" ! A.style "float: right;" $ do
                        checkbox (mcNoQuotes mc)         "no-quotes"         "No blockquotes"
                        checkbox (mcNoLists mc)          "no-lists"          "No lists"
                        checkbox (mcUnderlineHeaders mc) "underline-headers" "Underlined headers"
                        checkbox (mcReferenceLinks mc)   "reference-links"   "Reference-style links"
                        H.input ! A.type_ "text" ! A.size "2"
                            ! A.name "num-blocks" ! A.id "num-blocks"
                            ! A.class_ "small"
                        H.label ! A.for "num-blocks" ! A.class_ "input-label"
                            $ "Number of blocks"

                    H.div ! A.class_ "column" $ do
                        checkbox (mcNoHeaders mc)        "no-headers"        "No headers"
                        checkbox (mcNoCode mc)           "no-code"           "No code snippets"
                        checkbox (mcNoInlineMarkup mc)   "no-inline-markup"  "No inline markup"
                        checkbox (mcUnderscoreEm mc)     "underscore-em"     $ (H.code "_" <> "-style em")
                        checkbox (mcUnderscoreStrong mc) "underscore-strong" $ (H.code "__" <> "-style strong text")
                        checkbox (isNothing $ pcWrapCol pc) "no-wrapping"    "No wrapping"

                H.input ! A.type_ "submit" ! A.id "generate" !
                    A.value "Generate some markdown!"

                checkbox False "show-advanced" "Advanced settings"
                " "
                checkbox False "preview-html" "Preview as HTML"

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
