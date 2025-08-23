--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module LoremMarkdownum.Options
    ( Options (..)
    , OptionsParser (..)
    , parseOptions
    , toMarkdownOptions
    , toPrintOptions
    ) where


--------------------------------------------------------------------------------
import qualified Data.Text                    as T


--------------------------------------------------------------------------------
import           LoremMarkdownum.Gen.Markdown
import           LoremMarkdownum.Print


--------------------------------------------------------------------------------
data Options = Options
    { oNoHeaders        :: Bool
    , oNoCode           :: Bool
    , oNoQuotes         :: Bool
    , oNoLists          :: Bool
    , oNoInlineMarkup   :: Bool
    , oNoInlineCode     :: Bool
    , oReferenceLinks   :: Bool
    , oUnderlineHeaders :: Bool
    , oUnderscoreEm     :: Bool
    , oUnderscoreStrong :: Bool
    , oNumBlocks        :: Maybe Int
    , oFencedCodeBlocks :: Bool
    , oSeed             :: Maybe Int
    , oNoWrapping       :: Bool
    } deriving (Show)


--------------------------------------------------------------------------------
class Applicative m => OptionsParser m where
    getBoolOption :: T.Text -> m Bool
    getIntOption  :: T.Text -> m (Maybe Int)


--------------------------------------------------------------------------------
parseOptions :: OptionsParser m => m Options
parseOptions = Options
    <$> getBoolOption                           "no-headers"
    <*> getBoolOption                           "no-code"
    <*> getBoolOption                           "no-quotes"
    <*> getBoolOption                           "no-lists"
    <*> getBoolOption                           "no-inline-markup"
    <*> getBoolOption                           "no-inline-code"
    <*> getBoolOption                           "reference-links"
    <*> getBoolOption                           "underline-headers"
    <*> getBoolOption                           "underscore-em"
    <*> getBoolOption                           "underscore-strong"
    <*> (fmap (max 1 . min 50) <$> getIntOption "num-blocks")
    <*> getBoolOption                           "fenced-code-blocks"
    <*> getIntOption                            "seed"
    <*> getBoolOption                           "no-wrapping"


--------------------------------------------------------------------------------
toMarkdownOptions :: Options -> MarkdownOptions
toMarkdownOptions Options {..} = MarkdownOptions
    { moNoHeaders        = oNoHeaders
    , moNoCode           = oNoCode
    , moNoQuotes         = oNoQuotes
    , moNoLists          = oNoLists
    , moNoInlineMarkup   = oNoInlineMarkup
    , moNoInlineCode     = oNoInlineCode
    , moReferenceLinks   = oReferenceLinks
    , moUnderlineHeaders = oUnderlineHeaders
    , moUnderscoreEm     = oUnderscoreEm
    , moUnderscoreStrong = oUnderscoreStrong
    , moNumBlocks        = oNumBlocks
    , moFencedCodeBlocks = oFencedCodeBlocks
    , moSeed             = oSeed
    }


--------------------------------------------------------------------------------
toPrintOptions :: Options -> PrintOptions
toPrintOptions Options {..} = PrintOptions
    { pcWrapCol = case oNoWrapping of
        False -> pcWrapCol $ defaultPrintOptions
        True  -> Nothing
    }
