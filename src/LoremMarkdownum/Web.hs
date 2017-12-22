--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}


--------------------------------------------------------------------------------
import           Control.Monad.Reader          (ReaderT, ask, runReaderT)
import           Control.Monad.Trans           (liftIO)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Char8         as BC
import qualified Data.Text                     as T
import qualified Snap.Blaze                    as Snap
import           Snap.Core                     (Snap)
import qualified Snap.Core                     as Snap
import qualified Snap.Http.Server              as Snap
import qualified Snap.Util.FileServe           as Snap
import           System.Environment            (getArgs, getProgName)
import           System.Exit                   (exitFailure)


--------------------------------------------------------------------------------
import qualified LoremMarkdownum.FrequencyTree as FT
import           LoremMarkdownum.Gen
import           LoremMarkdownum.Gen.Code
import           LoremMarkdownum.Gen.Markdown
import qualified LoremMarkdownum.Markov        as Markov
import           LoremMarkdownum.Print
import           LoremMarkdownum.Token
import           LoremMarkdownum.Token.Parse
import qualified LoremMarkdownum.Web.Views     as Views


--------------------------------------------------------------------------------
type AppEnv = (MarkdownConfig, MarkdownState)


--------------------------------------------------------------------------------
type AppM a = ReaderT AppEnv Snap a


--------------------------------------------------------------------------------
readDataFiles :: IO AppEnv
readDataFiles = do
    techspeak     <- concat <$> streamsFromDir "data/techspeak"
    metamorphoses <- streamsFromDir "data/metamorphoses"
    let codeFreqTree  = FT.fromList $ map T.toLower $ streamElements techspeak
        wordFrequency = wordFrequencyTreePerLength $ concat metamorphoses
        tokenQueue    = [Element "lorem", Element "markdownum"]
        lengthMarkov  =
            Markov.optimize $ foldr (Markov.feed 2) Markov.empty $
            fmap (fmap (fmap T.length)) metamorphoses

        codeConfig     = CodeConfig codeFreqTree
        markdownState  = MarkdownState tokenQueue
        markdownConfig =
                mkDefaultMarkdownConfig lengthMarkov wordFrequency codeConfig
    return (markdownConfig, markdownState)


--------------------------------------------------------------------------------
main :: IO ()
main = do
    args     <- getArgs
    progName <- getProgName
    case args of
        [address, port] -> do
            let config = Snap.setBind (BC.pack address) $
                    Snap.setPort (read port) (Snap.defaultConfig)
            appEnv <- readDataFiles
            Snap.httpServe config $ runReaderT app appEnv
        _               -> do
            putStrLn $ "Usage: " ++ progName ++ " <address> <port>"
            exitFailure


--------------------------------------------------------------------------------
app :: AppM ()
app = Snap.route
    [ ("",                     Snap.ifTop index)
    , ("markdown.txt",         markdown)
    , ("markdown-html.html",   markdownHtml)
    , ("loading.gif",          Snap.serveFile "static/loading.gif")
    , ("lorem-markdownum.js",  Snap.serveFile "static/lorem-markdownum.js")
    , ("jquery-1.10.2.min.js", Snap.serveFile "static/jquery-1.10.2.min.js")
    ]


--------------------------------------------------------------------------------
index :: AppM ()
index = do
    (mc, markdownState) <- ask
    pc <- getPrintConfig
    m  <- liftIO $ runGenIO $ runMarkdownGen genMarkdown mc markdownState
    Snap.blaze $ Views.index pc mc m


--------------------------------------------------------------------------------
markdown :: AppM ()
markdown = do
    (_, markdownState) <- ask
    mc                 <- getMarkdownConfig
    pc                 <- getPrintConfig
    m <- liftIO $ runGenIO $ runMarkdownGen genMarkdown mc markdownState
    Snap.modifyResponse $ Snap.setContentType "text/plain"

    -- This allows the resource to be fetched using the Fetch API.
    Snap.modifyResponse $ Snap.setHeader "Access-Control-Allow-Origin" "*"

    Snap.writeLazyText $ runPrintWith pc $ printMarkdown mc m


--------------------------------------------------------------------------------
markdownHtml :: AppM ()
markdownHtml = do
    (_, markdownState) <- ask
    mc                 <- getMarkdownConfig
    pc                 <- getPrintConfig
    m <- liftIO $ runGenIO $ runMarkdownGen genMarkdown mc markdownState
    Snap.blaze $ Views.markdownHtml pc mc m


--------------------------------------------------------------------------------
getBoolParam :: ByteString -> AppM Bool
getBoolParam name = do
    param <- Snap.getParam name
    case param of
        -- Browers usually send nothing when the checkbox is turned off.
        Just "true"  -> return True
        Just "false" -> return False
        Just "on"    -> return True
        Just "off"   -> return False
        _            -> return False


--------------------------------------------------------------------------------
getIntParam :: ByteString -> AppM (Maybe Int)
getIntParam name = do
    param <- Snap.getParam name
    case fmap (reads . BC.unpack) param of
        Just [(x, "")] -> return $ Just x
        _              -> return Nothing


--------------------------------------------------------------------------------
getMarkdownConfig :: AppM MarkdownConfig
getMarkdownConfig = do
    (mc, _)          <- ask
    noHeaders        <- getBoolParam "no-headers"
    noCode           <- getBoolParam "no-code"
    noQuotes         <- getBoolParam "no-quotes"
    noLists          <- getBoolParam "no-lists"
    noInlineMarkup   <- getBoolParam "no-inline-markup"
    referenceLinks   <- getBoolParam "reference-links"
    noExternalLinks  <- getBoolParam "no-external-links"
    underlineHeaders <- getBoolParam "underline-headers"
    underscoreEm     <- getBoolParam "underscore-em"
    underscoreStrong <- getBoolParam "underscore-strong"
    numBlocks        <- getIntParam  "num-blocks"
    return mc
        { mcNoHeaders        = noHeaders
        , mcNoCode           = noCode
        , mcNoQuotes         = noQuotes
        , mcNoLists          = noLists
        , mcNoInlineMarkup   = noInlineMarkup
        , mcReferenceLinks   = referenceLinks
        , mcNoExternalLinks  = noExternalLinks
        , mcUnderlineHeaders = underlineHeaders
        , mcUnderscoreEm     = underscoreEm
        , mcUnderscoreStrong = underscoreStrong
        , mcNumBlocks        = fmap (max 1 . min 15) numBlocks
        }


--------------------------------------------------------------------------------
getPrintConfig :: AppM PrintConfig
getPrintConfig = do
    noWrapping <- getBoolParam "no-wrapping"
    case noWrapping of
        False -> return defaultPrintConfig
        True  -> return defaultPrintConfig {pcWrapCol = Nothing}
