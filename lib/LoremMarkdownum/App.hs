--------------------------------------------------------------------------------
-- | This module is shared between the CLI and the web server.  It can use
-- some refactoring and renaming.
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module LoremMarkdownum.App
    ( AppEnv
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
import           LoremMarkdownum.Gen.Code
import           LoremMarkdownum.Gen.Markdown
import qualified LoremMarkdownum.Markov        as Markov
import           LoremMarkdownum.Token
import           LoremMarkdownum.Token.Parse


--------------------------------------------------------------------------------
type AppEnv = (MarkdownConfig, MarkdownState)


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
    (mc, markdownState) <- ask
    case mcSeed mc of
        Just seed -> pure $ runGenPure
            (runMarkdownGen genMarkdown mc markdownState) seed
        Nothing -> liftIO $ runGenIO $
            runMarkdownGen genMarkdown mc markdownState
