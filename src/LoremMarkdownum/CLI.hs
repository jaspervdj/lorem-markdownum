--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
import           Control.Monad.Reader         (Reader, ask, runReader)
import qualified Data.Text                    as T
import qualified Data.Text.Lazy.IO            as TL
import           System.Environment           (getArgs)


--------------------------------------------------------------------------------
import           LoremMarkdownum.App
import           LoremMarkdownum.Gen.Markdown
import           LoremMarkdownum.Options
import           LoremMarkdownum.Print        (runPrintWith)


--------------------------------------------------------------------------------
newtype CLIOptionsParser a
    = CLIOptionsParser {unCLIOptionsParser :: Reader [T.Text] a}
    deriving (Applicative, Functor)


--------------------------------------------------------------------------------
instance OptionsParser CLIOptionsParser where
    getFlag name = CLIOptionsParser $ do
        args <- ask
        pure $ any (== ("--" <> name)) args

    getOption name = CLIOptionsParser $ do
        args <- ask
        pure $ case break (== ("--" <> name)) args of
            (_, _ : val : _) -> Just val
            _                -> Nothing


--------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- map T.pack <$> getArgs
    let options = runReader (unCLIOptionsParser parseOptions) args
        mopts   = toMarkdownOptions options
        popts   = toPrintOptions options
    model <- loadMarkdownModel "data"
    markdown <- generateMarkdown model mopts
    TL.putStr $ runPrintWith popts (printMarkdown mopts markdown)
