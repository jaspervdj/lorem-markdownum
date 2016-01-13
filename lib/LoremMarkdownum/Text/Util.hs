--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module LoremMarkdownum.Text.Util
    ( capitalize
    , camelCase
    , snakeCase
    ) where


--------------------------------------------------------------------------------
import           Data.Char (toUpper)
import           Data.Text (Text)
import qualified Data.Text as T


--------------------------------------------------------------------------------
capitalize :: Text -> Text
capitalize text = case T.uncons text of
    Nothing      -> text
    Just (x, xs) -> T.cons (toUpper x) xs



--------------------------------------------------------------------------------
camelCase :: [Text] -> Text
camelCase []       = T.empty
camelCase (x : xs) = T.concat $ x : map capitalize xs


--------------------------------------------------------------------------------
snakeCase :: [Text] -> Text
snakeCase = T.intercalate "_"
