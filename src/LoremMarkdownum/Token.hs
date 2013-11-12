--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module LoremMarkdownum.Token
    ( Stream
    , streamElements
    , mapStream
    , printStream
    , previewStream

    , Token (..)
    , tokenIsElement
    , tokenIsDelimiter
    , tokenEndsSentence
    , castToken
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative   (pure, (<$>))
import           Data.Foldable         (Foldable (..))
import           Data.Monoid           (mappend, mempty)
import           Data.Traversable      (Traversable (..))
import           Text.Blaze.Html       (Html)


--------------------------------------------------------------------------------
import           LoremMarkdownum.Print


--------------------------------------------------------------------------------
type Stream a = [Token a]


--------------------------------------------------------------------------------
streamElements :: [Token a] -> [a]
streamElements tokens = [x | Element x <- tokens]


--------------------------------------------------------------------------------
mapStream :: (a -> b) -> Stream a -> Stream b
mapStream = map . fmap


--------------------------------------------------------------------------------
printStream :: (a -> Print ()) -> Stream a -> Print ()
printStream printElement = go
  where
    go []                       = return ()
    go [t]                      = printToken t
    go (t : xs@(Element _ : _)) =
        printToken t >> printBrkSp >> go xs
    go (t : xs@(_ : _))         =
        printToken t >> go xs

    printToken (Element x) = printElement x
    printToken Comma       = printText ","
    printToken Colon       = printText ":"
    printToken Semicolon   = printText ";"
    printToken FullStop    = printText "."
    printToken Question    = printText "?"
    printToken Exclamation = printText "!"


--------------------------------------------------------------------------------
previewStream :: (a -> Html) -> Stream a -> Html
previewStream previewElement = go
  where
    go []                       = mempty
    go [t]                      = printToken t
    go (t : xs@(Element _ : _)) =
        printToken t `mappend` " " `mappend` go xs
    go (t : xs@(_ : _))         =
        printToken t `mappend` go xs

    printToken (Element x) = previewElement x
    printToken Comma       = ","
    printToken Colon       = ":"
    printToken Semicolon   = ";"
    printToken FullStop    = "."
    printToken Question    = "?"
    printToken Exclamation = "!"


--------------------------------------------------------------------------------
data Token a
    = Element !a
    | Comma
    | Colon
    | Semicolon
    | FullStop
    | Question
    | Exclamation
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
instance Functor Token where
    fmap f (Element x)  = Element (f x)
    fmap _ Comma        = Comma
    fmap _ Colon        = Colon
    fmap _ Semicolon    = Semicolon
    fmap _ FullStop     = FullStop
    fmap _ Question     = Question
    fmap _ Exclamation  = Exclamation


--------------------------------------------------------------------------------
instance Foldable Token where
    foldr f z (Element x) = f x z
    foldr _ z _           = z


--------------------------------------------------------------------------------
instance Traversable Token where
    traverse f (Element x) = Element <$> f x
    traverse _ Comma       = pure Comma
    traverse _ Colon       = pure Colon
    traverse _ Semicolon   = pure Semicolon
    traverse _ FullStop    = pure FullStop
    traverse _ Question    = pure Question
    traverse _ Exclamation = pure Exclamation


--------------------------------------------------------------------------------
tokenIsElement :: Token a -> Bool
tokenIsElement = not . tokenIsDelimiter


--------------------------------------------------------------------------------
tokenIsDelimiter :: Token a -> Bool
tokenIsDelimiter (Element _) = False
tokenIsDelimiter _           = True


--------------------------------------------------------------------------------
tokenEndsSentence :: Token a -> Bool
tokenEndsSentence FullStop    = True
tokenEndsSentence Question    = True
tokenEndsSentence Exclamation = True
tokenEndsSentence _           = False


--------------------------------------------------------------------------------
castToken :: Token a -> Maybe (Token b)
castToken (Element _) = Nothing
castToken Comma       = Just Comma
castToken Colon       = Just Colon
castToken Semicolon   = Just Semicolon
castToken FullStop    = Just FullStop
castToken Question    = Just Question
castToken Exclamation = Just Exclamation
