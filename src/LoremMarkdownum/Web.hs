--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}


--------------------------------------------------------------------------------
import           Control.Monad.Reader          (ReaderT, ask, runReaderT)
import           Control.Monad.Trans           (liftIO)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Char8         as BC
import           Data.Maybe                    (fromMaybe)
import qualified Data.Text                     as T
import qualified Snap.Blaze                    as Snap
import qualified Snap.Core                     as Snap
import           Snap.Core                     (Snap)
import qualified Snap.Http.Server              as Snap
import qualified Snap.Util.FileServe           as Snap
import           System.Environment            (lookupEnv)
import           System.FilePath               ((</>))
import qualified System.IO                     as IO


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
data Config = Config
    { cBindAddress :: String
    , cBindPort    :: Int
    , cDataDir     :: FilePath
    , cStaticDir   :: FilePath
    } deriving (Show)


--------------------------------------------------------------------------------
configFromEnv :: IO Config
configFromEnv = Config
    <$> (fromMaybe "127.0.0.1" <$> lookupEnv "LOREM_MARKDOWNUM_BIND_ADDRESS")
    <*> (maybe 8000 read       <$> lookupEnv "LOREM_MARKDOWNUM_BIND_PORT")
    <*> (fromMaybe "data"      <$> lookupEnv "LOREM_MARKDOWNUM_DATA_DIR")
    <*> (fromMaybe "static"    <$> lookupEnv "LOREM_MARKDOWNUM_STATIC_DIR")


--------------------------------------------------------------------------------
type AppEnv = (MarkdownConfig, MarkdownState)


--------------------------------------------------------------------------------
type AppM a = ReaderT AppEnv Snap a


--------------------------------------------------------------------------------
readDataFiles :: Config -> IO AppEnv
readDataFiles config = do
    techspeak     <- concat <$> streamsFromDir (cDataDir config </> "techspeak")
    metamorphoses <- streamsFromDir (cDataDir config </> "metamorphoses")
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
    config <- configFromEnv
    let snapConfig =
            Snap.setBind (BC.pack $ cBindAddress config) $
            Snap.setPort (cBindPort config) $
            Snap.setAccessLog (Snap.ConfigIoLog $ BC.hPutStrLn IO.stderr) $
            Snap.setErrorLog (Snap.ConfigIoLog $ BC.hPutStrLn IO.stderr) $
            Snap.defaultConfig
    appEnv <- readDataFiles config
    Snap.httpServe snapConfig $ runReaderT (app config) appEnv


--------------------------------------------------------------------------------
app :: Config -> AppM ()
app config = Snap.route
    [ ("",                      Snap.ifTop index)
    , ("markdown.txt",          markdown)
    , ("markdown-html.html",    markdownHtml)
    , ("loading.gif",           Snap.serveFile $ cStaticDir config </> "loading.gif")
    , ("lorem-markdownum.css",  Snap.serveFile $ cStaticDir config </> "lorem-markdownum.css")
    , ("lorem-markdownum.js",   Snap.serveFile $ cStaticDir config </> "lorem-markdownum.js")
    , ("jquery-1.10.2.min.js",  Snap.serveFile $ cStaticDir config </> "jquery-1.10.2.min.js")
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
    fencedCodeBlocks <- getBoolParam "fenced-code-blocks"
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
        , mcNumBlocks        = fmap (max 1 . min 50) numBlocks
        , mcFencedCodeBlocks = fencedCodeBlocks
        }


--------------------------------------------------------------------------------
getPrintConfig :: AppM PrintConfig
getPrintConfig = do
    noWrapping <- getBoolParam "no-wrapping"
    case noWrapping of
        False -> return defaultPrintConfig
        True  -> return defaultPrintConfig {pcWrapCol = Nothing}
