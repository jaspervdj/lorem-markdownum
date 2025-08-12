--------------------------------------------------------------------------------
import           Control.Monad.Reader         (runReaderT)
import qualified Data.Text.Lazy.IO            as TL


--------------------------------------------------------------------------------
import           LoremMarkdownum.App
import           LoremMarkdownum.Gen.Markdown (printMarkdown)
import           LoremMarkdownum.Print        (runPrint)


--------------------------------------------------------------------------------
main :: IO ()
main = do
    appEnv <- readDataFiles "data"
    markdown <- runReaderT appGenMarkdown appEnv
    TL.putStr $ runPrint (printMarkdown (fst appEnv) markdown)
