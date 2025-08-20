--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
import           Control.Monad.Reader         (Reader, ask, runReader)
import qualified Data.Text                    as T
import qualified Data.Text.Lazy.IO            as TL
import           System.Environment           (getArgs)
import           Text.Read                    (readMaybe)


--------------------------------------------------------------------------------
import           LoremMarkdownum.App
import           LoremMarkdownum.Gen.Markdown
import           LoremMarkdownum.Print        (runPrintWith)


--------------------------------------------------------------------------------
newtype CLIOptionsParser a
    = CLIOptionsParser {unCLIOptionsParser :: Reader [T.Text] a}
    deriving (Applicative, Functor)


--------------------------------------------------------------------------------
instance OptionsParser CLIOptionsParser where
    getBoolOption name = CLIOptionsParser $ do
        args <- ask
        pure $ any (== ("--" <> name)) args

    getIntOption name = CLIOptionsParser $ do
        args <- ask
        pure $ case break (== ("--" <> name)) args of
            (_, _ : val : _) -> readMaybe $ T.unpack val
            _                -> Nothing


--------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- map T.pack <$> getArgs
    let options = runReader (unCLIOptionsParser parseMarkdownOptions) args
        pc      = runReader (unCLIOptionsParser parsePrintOptions) args
    model <- loadMarkdownModel "data"
    markdown <- generateMarkdown model options
    TL.putStr $ runPrintWith pc (printMarkdown options markdown)
