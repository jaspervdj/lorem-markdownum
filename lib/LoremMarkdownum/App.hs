--------------------------------------------------------------------------------
-- | This module is shared between the CLI and the web server.  It can use
-- some refactoring and renaming.
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module LoremMarkdownum.App
    ( OptionsParser (..)
    , parseMarkdownOptions
    , parsePrintOptions

    , AppEnv
    , readDataFiles
    , appGenMarkdown
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Reader          (MonadReader, ask)
import           Control.Monad.Trans           (MonadIO, liftIO)
import qualified Data.Text                     as T
import           System.FilePath               ((</>))


--------------------------------------------------------------------------------
import qualified LoremMarkdownum.FrequencyTree as FT
import           LoremMarkdownum.Gen
import           LoremMarkdownum.Print
import           LoremMarkdownum.Gen.Code
import           LoremMarkdownum.Gen.Markdown
import qualified LoremMarkdownum.Markov        as Markov
import           LoremMarkdownum.Token
import           LoremMarkdownum.Token.Parse


--------------------------------------------------------------------------------
class Applicative m => OptionsParser m where
    getBoolOption :: T.Text -> m Bool
    getIntOption  :: T.Text -> m (Maybe Int)


--------------------------------------------------------------------------------
parseMarkdownOptions :: OptionsParser m => m MarkdownOptions
parseMarkdownOptions = MarkdownOptions
    <$> getBoolOption                           "no-headers"
    <*> getBoolOption                           "no-code"
    <*> getBoolOption                           "no-quotes"
    <*> getBoolOption                           "no-lists"
    <*> getBoolOption                           "no-inline-markup"
    <*> getBoolOption                           "reference-links"
    <*> getBoolOption                           "underline-headers"
    <*> getBoolOption                           "underscore-em"
    <*> getBoolOption                           "underscore-strong"
    <*> (fmap (max 1 . min 50) <$> getIntOption "num-blocks")
    <*> getBoolOption                           "fenced-code-blocks"
    <*> getIntOption                            "seed"


--------------------------------------------------------------------------------
parsePrintOptions :: OptionsParser m => m PrintConfig
parsePrintOptions =
    (\noWrapping -> case noWrapping of
        False -> defaultPrintConfig
        True  -> defaultPrintConfig {pcWrapCol = Nothing}) <$>
    getBoolOption "no-wrapping"


--------------------------------------------------------------------------------
type AppEnv = (MarkdownEnv, MarkdownState)


--------------------------------------------------------------------------------
readDataFiles :: FilePath -> IO AppEnv
readDataFiles dataDir = do
    techspeak     <- concat <$> streamsFromDir (dataDir </> "techspeak")
    metamorphoses <- streamsFromDir (dataDir </> "metamorphoses")
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
appGenMarkdown :: (MonadIO m, MonadReader AppEnv m) => m Markdown
appGenMarkdown = do
    (me, markdownState) <- ask
    case moSeed (meOptions me) of
        Just seed -> pure $ runGenPure
            (runMarkdownGen genMarkdown me markdownState) seed
        Nothing -> liftIO $ runGenIO $
            runMarkdownGen genMarkdown me markdownState
