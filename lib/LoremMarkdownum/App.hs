--------------------------------------------------------------------------------
-- | This module is shared between the CLI and the web server.  It can use
-- some refactoring and renaming.
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module LoremMarkdownum.App
    ( OptionsParser (..)
    , parseMarkdownOptions
    , parsePrintOptions

    , loadMarkdownModel
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
import           LoremMarkdownum.Print
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
parsePrintOptions :: OptionsParser m => m PrintOptions
parsePrintOptions =
    (\noWrapping -> case noWrapping of
        False -> defaultPrintOptions
        True  -> defaultPrintOptions {pcWrapCol = Nothing}) <$>
    getBoolOption "no-wrapping"


--------------------------------------------------------------------------------
loadMarkdownModel :: FilePath -> IO MarkdownModel
loadMarkdownModel dataDir = do
    techspeak     <- concat <$> streamsFromDir (dataDir </> "techspeak")
    metamorphoses <- streamsFromDir (dataDir </> "metamorphoses")
    let codeFreqTree  = FT.fromList $ map T.toLower $ streamElements techspeak
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
