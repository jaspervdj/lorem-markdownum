--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


--------------------------------------------------------------------------------
import           Control.Monad.Reader         (ReaderT, ask, runReaderT)
import qualified Data.ByteString.Char8        as BC
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Snap.Blaze                   as Snap
import qualified Snap.Core                    as Snap
import           Snap.Core                    (Snap)
import qualified Snap.Http.Server             as Snap
import qualified Snap.Util.FileServe          as Snap
import           System.Environment           (lookupEnv)
import           System.FilePath              ((</>))
import qualified System.IO                    as IO


--------------------------------------------------------------------------------
import           LoremMarkdownum.App
import           LoremMarkdownum.Gen.Markdown
import           LoremMarkdownum.Print
import qualified LoremMarkdownum.Web.Views    as Views


--------------------------------------------------------------------------------
data Config = Config
    { cBindAddress :: String
    , cBindPort    :: Int
    , cDataDir     :: FilePath
    , cStaticDir   :: FilePath
    , cRev         :: String
    } deriving (Show)


--------------------------------------------------------------------------------
configFromEnv :: IO Config
configFromEnv = Config
    <$> (fromMaybe "127.0.0.1" <$> lookupEnv "LOREM_MARKDOWNUM_BIND_ADDRESS")
    <*> (maybe 8000 read       <$> lookupEnv "LOREM_MARKDOWNUM_BIND_PORT")
    <*> (fromMaybe "data"      <$> lookupEnv "LOREM_MARKDOWNUM_DATA_DIR")
    <*> (fromMaybe "static"    <$> lookupEnv "LOREM_MARKDOWNUM_STATIC_DIR")
    <*> (fromMaybe "dirty"     <$> lookupEnv "LOREM_MARKDOWNUM_REV")


--------------------------------------------------------------------------------
type AppM a = ReaderT MarkdownModel Snap a


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
    model <- loadMarkdownModel (cDataDir config)
    Snap.httpServe snapConfig $ runReaderT (app config) model


--------------------------------------------------------------------------------
app :: Config -> AppM ()
app config = do
    Snap.modifyResponse $
        Snap.setHeader "X-Revision" $ T.encodeUtf8 $ T.pack (cRev config)
    Snap.route
        [ ("",                      Snap.ifTop index)
        , ("markdown.txt",          markdown)
        , ("markdown-html.html",    markdownHtml)
        , ("lorem-markdownum.css",  Snap.serveFile $ cStaticDir config </> "lorem-markdownum.css")
        , ("lorem-markdownum.js",   Snap.serveFile $ cStaticDir config </> "lorem-markdownum.js")
        ]


--------------------------------------------------------------------------------
index :: AppM ()
index = do
    model <- ask
    mo    <- unAppOptionsParser parseMarkdownOptions
    po    <- unAppOptionsParser parsePrintOptions
    m     <- generateMarkdown model mo
    Snap.blaze $ Views.index po mo m


--------------------------------------------------------------------------------
markdown :: AppM ()
markdown = do
    model <- ask
    mo    <- unAppOptionsParser parseMarkdownOptions
    po    <- unAppOptionsParser parsePrintOptions
    m     <- generateMarkdown model mo
    Snap.modifyResponse $ Snap.setContentType "text/plain"

    -- This allows the resource to be fetched using the Fetch API.
    Snap.modifyResponse $ Snap.setHeader "Access-Control-Allow-Origin" "*"

    Snap.writeLazyText $ runPrintWith po $ printMarkdown mo m


--------------------------------------------------------------------------------
markdownHtml :: AppM ()
markdownHtml = do
    model <- ask
    mo    <- unAppOptionsParser parseMarkdownOptions
    po    <- unAppOptionsParser parsePrintOptions
    m     <- generateMarkdown model mo
    Snap.blaze $ Views.markdownHtml po mo m


--------------------------------------------------------------------------------
getBoolParam :: T.Text -> AppM Bool
getBoolParam name = do
    param <- Snap.getParam $ T.encodeUtf8 name
    case param of
        -- Browers usually send nothing when the checkbox is turned off.
        Just "true"  -> return True
        Just "false" -> return False
        Just "on"    -> return True
        Just "off"   -> return False
        _            -> return False


--------------------------------------------------------------------------------
getIntParam :: T.Text -> AppM (Maybe Int)
getIntParam name = do
    param <- Snap.getParam $ T.encodeUtf8 name
    case fmap (reads . BC.unpack) param of
        Just [(x, "")] -> return $ Just x
        _              -> return Nothing


--------------------------------------------------------------------------------
newtype AppOptionsParser a = AppOptionsParser {unAppOptionsParser :: AppM a}
    deriving (Applicative, Functor)


--------------------------------------------------------------------------------
instance OptionsParser AppOptionsParser where
    getBoolOption = AppOptionsParser . getBoolParam
    getIntOption  = AppOptionsParser . getIntParam
