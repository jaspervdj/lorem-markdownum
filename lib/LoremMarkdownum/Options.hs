--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module LoremMarkdownum.Options
    ( Options (..)
    , OptionsParser (..)
    , Choice (..)
    , parseOptions
    , toMarkdownOptions
    , toPrintOptions
    ) where


--------------------------------------------------------------------------------
import qualified Data.Text                    as T
import           Text.Read                    (readMaybe)


--------------------------------------------------------------------------------
import           LoremMarkdownum.Gen.Markdown
import           LoremMarkdownum.Print


--------------------------------------------------------------------------------
data Options = Options
    { oHeaders                    :: Maybe HeaderOption
    , oNoQuotes                   :: Bool
    , oNoLists                    :: Bool
    , oEmphasis                   :: Maybe EmphasisOption
    , oStrong                     :: Maybe StrongOption
    , oNoInlineCode               :: Bool
    , oReferenceLinks             :: Bool
    , oNumBlocks                  :: Maybe Int
    , oSeed                       :: Maybe Int
    , oNoWrapping                 :: Bool
    , oCodeBlocks                 :: Maybe CodeBlockOption
    , oDeprecatedNoHeaders        :: Bool
    , oDeprecatedUnderlineHeaders :: Bool
    , oDeprecatedNoInlineMarkup   :: Bool
    , oDeprecatedUnderscoreEm     :: Bool
    , oDeprecatedUnderscoreStrong :: Bool
    , oDeprecatedNoCode           :: Bool
    , oDeprecatedFencedCodeBlocks :: Bool
    } deriving (Show)


--------------------------------------------------------------------------------
class Applicative m => OptionsParser m where
    getFlag   :: T.Text -> m Bool
    getOption :: T.Text -> m (Maybe T.Text)


--------------------------------------------------------------------------------
getIntOption :: OptionsParser m => T.Text -> m (Maybe Int)
getIntOption k = fmap (>>= readMaybe . T.unpack) $ getOption k


--------------------------------------------------------------------------------
class (Bounded c, Enum c, Eq c) => Choice c where
    choiceLabel :: c -> T.Text
    choiceValue :: c -> T.Text


--------------------------------------------------------------------------------
instance Choice HeaderOption where
    choiceLabel HeaderHash      = "#"
    choiceLabel HeaderUnderline = "=="
    choiceLabel HeaderOff       = "off"
    choiceValue HeaderHash      = "hash"
    choiceValue HeaderUnderline = "underline"
    choiceValue HeaderOff       = "off"


--------------------------------------------------------------------------------
instance Choice EmphasisOption where
    choiceLabel EmphasisAsterisk   = "*"
    choiceLabel EmphasisUnderscore = "_"
    choiceLabel EmphasisOff        = "off"
    choiceValue EmphasisAsterisk   = "asterisk"
    choiceValue EmphasisUnderscore = "underscore"
    choiceValue EmphasisOff        = "off"


--------------------------------------------------------------------------------
instance Choice StrongOption where
    choiceLabel StrongAsterisk   = "**"
    choiceLabel StrongUnderscore = "__"
    choiceLabel StrongOff        = "off"
    choiceValue StrongAsterisk   = "asterisk"
    choiceValue StrongUnderscore = "underscore"
    choiceValue StrongOff        = "off"


--------------------------------------------------------------------------------
instance Choice CodeBlockOption where
    choiceLabel CodeBlockIndent = "indent"
    choiceLabel CodeBlockFenced = "```"
    choiceLabel CodeBlockOff    = "off"
    choiceValue CodeBlockIndent = "indent"
    choiceValue CodeBlockFenced = "fenced"
    choiceValue CodeBlockOff    = "off"


--------------------------------------------------------------------------------
getChoiceOption :: (Choice c, OptionsParser m) => T.Text -> m (Maybe c)
getChoiceOption key =
    (>>= (`lookup` [(choiceValue x, x) | x <- [minBound .. maxBound]])) <$>
    getOption key


--------------------------------------------------------------------------------
parseOptions :: OptionsParser m => m Options
parseOptions = Options
    <$> getChoiceOption                         "headers"
    <*> getFlag                                 "no-quotes"
    <*> getFlag                                 "no-lists"
    <*> getChoiceOption                         "emphasis"
    <*> getChoiceOption                         "strong"
    <*> getFlag                                 "no-inline-code"
    <*> getFlag                                 "reference-links"
    <*> (fmap (max 1 . min 50) <$> getIntOption "num-blocks")
    <*> getIntOption                            "seed"
    <*> getFlag                                 "no-wrapping"
    <*> getChoiceOption                         "code-blocks"
    <*> getFlag                                 "no-headers"
    <*> getFlag                                 "underline-headers"
    <*> getFlag                                 "no-inline-markup"
    <*> getFlag                                 "underscore-em"
    <*> getFlag                                 "underscore-strong"
    <*> getFlag                                 "no-code"
    <*> getFlag                                 "fenced-code-blocks"


--------------------------------------------------------------------------------
toMarkdownOptions :: Options -> MarkdownOptions
toMarkdownOptions Options {..} = MarkdownOptions
    { moHeaders          = case oHeaders of
        Just headers                          -> headers
        Nothing | oDeprecatedNoHeaders        -> HeaderOff
        Nothing | oDeprecatedUnderlineHeaders -> HeaderUnderline
        Nothing                               -> HeaderHash
    , moCodeBlocks       = case oCodeBlocks of
        Just codeBlocks                       -> codeBlocks
        Nothing | oDeprecatedNoCode           -> CodeBlockOff
        Nothing | oDeprecatedFencedCodeBlocks -> CodeBlockFenced
        Nothing                               -> CodeBlockIndent
    , moNoQuotes         = oNoQuotes
    , moNoLists          = oNoLists
    , moEmphasis         = case oEmphasis of
        Just emphasis           -> emphasis
        Nothing | oDeprecatedUnderscoreEm -> EmphasisUnderscore
        Nothing                           -> EmphasisAsterisk
    , moStrong           = case oStrong of
        Just strong                           -> strong
        Nothing | oDeprecatedUnderscoreStrong -> StrongUnderscore
        Nothing                               -> StrongAsterisk
    , moNoInlineCode     = oNoInlineCode
    , moReferenceLinks   = oReferenceLinks
    , moNumBlocks        = oNumBlocks
    , moSeed             = oSeed
    }


--------------------------------------------------------------------------------
toPrintOptions :: Options -> PrintOptions
toPrintOptions Options {..} = PrintOptions
    { pcWrapCol = case oNoWrapping of
        False -> pcWrapCol $ defaultPrintOptions
        True  -> Nothing
    }
