--------------------------------------------------------------------------------
-- | This module is shared between the CLI and the web server.  It can use
-- some refactoring and renaming.
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module LoremMarkdownum.App
    ( loadMarkdownModel
    , generateMarkdown
    ) where


--------------------------------------------------------------------------------
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
loadMarkdownModel :: FilePath -> IO MarkdownModel
loadMarkdownModel dataDir = do
    code          <- concat <$> streamsFromDir (dataDir </> "code")
    metamorphoses <- streamsFromDir (dataDir </> "metamorphoses")
    let codeFreqTree  = FT.fromList $ map T.toLower $ streamElements code
        wordFrequency = wordFrequencyTreePerLength $ concat metamorphoses
        lengthMarkov  =
            Markov.optimize $ foldr (Markov.feed 2) Markov.empty $
            fmap (fmap (fmap T.length)) metamorphoses

        codeConfig    = CodeConfig codeFreqTree
        markdownModel = MarkdownModel
            { mmCodeConfig = codeConfig
            , mmLengthMarkov = lengthMarkov
            , mmWordFrequency = wordFrequency
            , mmStart         = [Element "lorem", Element "markdownum"]
            }
    return markdownModel


--------------------------------------------------------------------------------
generateMarkdown :: MonadIO m => MarkdownModel -> MarkdownOptions -> m Markdown
generateMarkdown model opts = case moSeed opts of
    Just seed -> pure $ runGenPure
        (runMarkdownGen genMarkdown model opts) seed
    Nothing -> liftIO $ runGenIO $
        runMarkdownGen genMarkdown model opts
