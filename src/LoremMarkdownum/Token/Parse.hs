--------------------------------------------------------------------------------
module LoremMarkdownum.Token.Parse
    ( streamsFromDir
    , wordFrequencyTreePerLength
    ) where


--------------------------------------------------------------------------------
import           Data.Char                     (isAlpha, isSpace)
import           Data.List                     (foldl')
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as M
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.IO             as TL
import           System.Directory              (getDirectoryContents)
import           System.FilePath               (takeExtension, (</>))


--------------------------------------------------------------------------------
import           LoremMarkdownum.FrequencyTree (FrequencyTree)
import qualified LoremMarkdownum.FrequencyTree as FT
import           LoremMarkdownum.Token


--------------------------------------------------------------------------------
toStream :: TL.Text -> Stream Text
toStream = go True
  where
    go firstWordInSentence text = case TL.uncons cleaned of
        Nothing         -> []
        Just (c, cs) -> case c of
            ',' -> Comma       : go False cs
            ':' -> Colon       : go False cs
            ';' -> Semicolon   : go False cs
            '.' -> FullStop    : go True  cs
            '?' -> Question    : go True  cs
            '!' -> Exclamation : go True  cs
            _   ->
                let (w, ws) = TL.break (not . isAlpha) cleaned
                    w'      = if firstWordInSentence then TL.toLower w else w
                in if TL.null w'
                    then go False cs
                    else Element (TL.toStrict w') : go False ws
      where
        cleaned = TL.dropWhile isSpace text


--------------------------------------------------------------------------------
streamFromFile :: FilePath -> IO (Stream Text)
streamFromFile = fmap toStream . TL.readFile


--------------------------------------------------------------------------------
streamsFromDir :: FilePath -> IO [Stream Text]
streamsFromDir dir = do
    contents <- getDirectoryContents dir
    mapM streamFromFile
        [dir </> fp | fp <- contents, takeExtension fp == ".txt"]


--------------------------------------------------------------------------------
wordFrequencyTreePerLength :: Stream Text -> Map Int (FrequencyTree Text)
wordFrequencyTreePerLength =
    M.map FT.optimize .
    foldl'
        (\m t -> M.insertWith FT.append (T.length t) (FT.singleton t) m)
        M.empty .
    streamElements
