--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module LoremMarkdownum.Gen.Markdown
    ( MarkdownConfig (..)
    , MarkdownState (..)
    , MarkdownGen
    , runMarkdownGen

    , Markdown
    , Block (..)
    , Paragraph
    , Sentence

    , genMarkdown
    , genSection
    , genParagraph
    , genOrderedList
    , genUnorderedList
    , genSentence
    , genPhrase

    , printMarkdown
    , printBlock
    , printParagraph
    , printOrderedList
    , printUnorderedList
    , printSentence
    , printPhrase
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative           ((<$>))
import           Control.Monad                 (forM, forM_, replicateM)
import           Control.Monad                 (unless)
import           Control.Monad.Reader          (ReaderT, ask, runReaderT)
import           Control.Monad.State           (StateT, evalStateT, get, modify)
import           Data.List                     (intersperse)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (maybeToList)
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Traversable              (traverse)


--------------------------------------------------------------------------------
import           LoremMarkdownum.FrequencyTree            (FrequencyTree)
import           LoremMarkdownum.Markov                   (Markov)
import qualified LoremMarkdownum.Markov                   as Markov
import           LoremMarkdownum.Text.Util
import           LoremMarkdownum.Gen
import           LoremMarkdownum.Gen.Code
import           LoremMarkdownum.Print
import           LoremMarkdownum.Token


--------------------------------------------------------------------------------
data MarkdownConfig = MarkdownConfig
    { mcLengthMarkov  :: Markov (Token Int)
    , mcWordFrequency :: Map Int (FrequencyTree Text)
    , mcCodeConfig    :: CodeConfig
    , mcAllowHeaders  :: Bool
    , mcAllowCode     :: Bool
    , mcAllowQuotes   :: Bool
    , mcAllowLists    :: Bool
    , mcInlineLinks   :: Bool
    , mcHashHeaders   :: Bool
    , mcAsteriskEm    :: Bool
    , mcAsteriskStrong :: Bool
    } deriving (Show)


--------------------------------------------------------------------------------
data MarkdownState = MarkdownState
    { msTokenQueue :: [Token Text]
    } deriving (Show)


--------------------------------------------------------------------------------
type MarkdownGen m a = ReaderT MarkdownConfig (StateT MarkdownState m) a


--------------------------------------------------------------------------------
runMarkdownGen :: MonadGen m
               => MarkdownGen m a -> MarkdownConfig -> MarkdownState -> m a
runMarkdownGen mg mc ms = evalStateT (runReaderT mg mc) ms


--------------------------------------------------------------------------------
type Markdown = [Block]


--------------------------------------------------------------------------------
data Block
    = HeaderB Header
    | ParagraphB Paragraph
    | OrderedListB [Phrase]
    | UnorderedListB [Phrase]
    | CodeB Code
    | QuoteB Paragraph
    deriving (Show)


--------------------------------------------------------------------------------
data Header = Header Int PlainPhrase deriving (Show)


--------------------------------------------------------------------------------
type Paragraph = [Sentence]


--------------------------------------------------------------------------------
-- | Starts with a capital, ends with a full stop/exclamation/...
type Sentence = Stream Markup


--------------------------------------------------------------------------------
-- | Starts with a capital, contains no commas/semicolons/...
type PlainPhrase = Stream Text


--------------------------------------------------------------------------------
type Phrase = Stream Markup


--------------------------------------------------------------------------------
data Markup
    = PlainM  Text
    | ItalicM (Stream Markup)
    | BoldM   (Stream Markup)
    | LinkM   (Stream Markup) Text
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
markdownLinks :: Markdown -> [(Stream Markup, Text)]
markdownLinks = M.toList . M.fromList . concatMap blockLinks
  where
    blockLinks (ParagraphB p) = paragraphLinks p
    blockLinks (QuoteB p)     = paragraphLinks p
    blockLinks _              = []
    paragraphLinks            = concatMap sentenceLinks
    sentenceLinks str         = [(s, t) | LinkM s t <- streamElements str]


--------------------------------------------------------------------------------
genMarkdown :: MonadGen m => MarkdownGen m Markdown
genMarkdown = do
    allowHeaders  <- mcAllowHeaders <$> ask
    numParagraphs <- randomInt (7, 9)
    numSections   <- randomInt (2, 4)
    partitioning  <- partitionNicely numSections numParagraphs
    blocks        <- forM partitioning $ \numParsInSection -> do
        section <- genSection numParsInSection
        h2      <- HeaderB <$> genHeader 2
        return $ h2 : section

    h1      <- HeaderB    <$> genHeader 1
    lastPar <- ParagraphB <$> genParagraph
    return $ (if allowHeaders then id else removeHeaders) $
        [h1] ++ concat blocks ++ [lastPar]
  where
    removeHeaders = filter (\b -> case b of HeaderB _ -> False; _ -> True)


--------------------------------------------------------------------------------
genSection :: MonadGen m => Int -> MarkdownGen m [Block]
genSection numBlocks
    | numBlocks <= 0 = return []
    | otherwise      = do
        par0    <- ParagraphB <$> genParagraph
        special <- (&& numBlocks > 1) <$> randomBool 3 1
        pars    <- if special then return <$> genSpecialBlock else return []
        ((par0 : pars) ++) <$> genSection (numBlocks - 1 - length pars)


--------------------------------------------------------------------------------
-- TODO (jaspervdj: Not sure about the name
genSpecialBlock :: MonadGen m => MarkdownGen m Block
genSpecialBlock = do
    allowCode   <- mcAllowCode   <$> ask
    allowQuotes <- mcAllowQuotes <$> ask
    allowLists  <- mcAllowLists  <$> ask
    let freqs =
            [ (if allowLists  then 1 else 0, OrderedListB   <$> genOrderedList)
            , (if allowLists  then 1 else 0, UnorderedListB <$> genUnorderedList)
            , (if allowCode   then 2 else 0, CodeB          <$> genCodeBlock)
            , (if allowQuotes then 1 else 0, QuoteB         <$> genParagraph)
            ]
    if sum (map fst freqs) <= 0
        then ParagraphB <$> genParagraph
        else oneOfFrequencies freqs
  where
    genCodeBlock = do
        codeConfig <- mcCodeConfig <$> ask
        depth0 (runCodeGen genCode codeConfig)


--------------------------------------------------------------------------------
genHeader :: MonadGen m => Int -> MarkdownGen m Header
genHeader n = Header n <$> genPlainPhrase


--------------------------------------------------------------------------------
genParagraph :: MonadGen m => MarkdownGen m Paragraph
genParagraph = do
    -- TODO (jaspervdj): Use some gaussian distribution here
    numSentences <- randomInt (2, 5)
    replicateM numSentences genRegularSentence


--------------------------------------------------------------------------------
genOrderedList :: MonadGen m => MarkdownGen m [Phrase]
genOrderedList = do
    numElements <- randomInt (3, 6)
    replicateM numElements genPhrase


--------------------------------------------------------------------------------
genUnorderedList :: MonadGen m => MarkdownGen m [Phrase]
genUnorderedList = genOrderedList


--------------------------------------------------------------------------------
genToken :: MonadGen m => MarkdownGen m (Token Text)
genToken = do
    lengthMarkov <- mcLengthMarkov <$> ask
    wordsQueue   <- msTokenQueue   <$> get
    let keys = map (fmap T.length) wordsQueue
    case Markov.lookup keys lengthMarkov of
        Just ft -> do
            nextLen   <- sampleFromFrequencyTree ft
            nextToken <- traverse genWord nextLen
            modify $ \s -> s {msTokenQueue = tail wordsQueue ++ [nextToken]}
            return (head wordsQueue)

        Nothing -> error $
            "Text.LoremMarkdownum.Gen.Markdown.genToken: no keys for: " ++
            show keys


--------------------------------------------------------------------------------
genElementToken :: MonadGen m => MarkdownGen m (Token Text)
genElementToken = do
    token <- genToken
    if tokenIsElement token then return token else genElementToken


--------------------------------------------------------------------------------
genWord :: MonadGen m => Int -> MarkdownGen m Text
genWord len = do
    wordSamples <- mcWordFrequency <$> ask
    case M.lookup len wordSamples of
        Just ft -> sampleFromFrequencyTree ft
        Nothing -> error $
            "Text.LoremMarkdownum.Gen.Markdown.genWord: " ++ show len ++
            " no words with this length"


--------------------------------------------------------------------------------
genRegularSentence :: MonadGen m => MarkdownGen m Sentence
genRegularSentence = randomInt (10, 20) >>= genSentence


--------------------------------------------------------------------------------
genSentence :: MonadGen m => Int -> MarkdownGen m Sentence
genSentence n = genPlainSentence n >>= genMarkup


--------------------------------------------------------------------------------
genPlainSentence :: MonadGen m => Int -> MarkdownGen m (Stream Text)
genPlainSentence maxLen = do
    -- A sentence starts with at least two words.
    token0 <- genElementToken
    token1 <- genElementToken
    tokens <- go token1 2
    return $ fmap capitalize token0 : token1 : tokens
  where
    go prev len
        | len >= maxLen && tokenIsElement prev = return <$> genSentenceEnd
        | otherwise                            = do
            token <- genToken
            if tokenEndsSentence token
                then return [token]
                else (token :) <$> go token (len + 1)


--------------------------------------------------------------------------------
genSentenceEnd :: MonadGen m => MarkdownGen m (Token a)
genSentenceEnd = sampleFromFrequencies
    [(FullStop, 10), (Question, 1), (Exclamation, 1)]


--------------------------------------------------------------------------------
genPhrase :: MonadGen m => MarkdownGen m Phrase
genPhrase = mapStream PlainM <$> genPlainPhrase


--------------------------------------------------------------------------------
genPlainPhrase :: MonadGen m => MarkdownGen m PlainPhrase
genPlainPhrase = takeWhile tokenIsElement <$> genPlainSentence 7


--------------------------------------------------------------------------------
genMarkup :: MonadGen m => Stream Text -> MarkdownGen m (Stream Markup)
genMarkup = go True
  where
    go _     []               = return []
    go allow (Element x : xs) = do
        applyMarkup <- (allow &&) <$> randomBool 1 20
        if not applyMarkup
            then (Element (PlainM x) :) <$> go True xs
            else do
                len    <- randomInt (0, 2)
                let (inc, xs') = takeWhileMax tokenIsElement len xs
                    elements   = map (Element . PlainM) $ x : streamElements inc
                markup <- genMarkupConstructor elements
                (Element markup :) <$> go False xs'
    go allow (x         : xs) =
        (maybeToList (castToken x) ++) <$> go allow xs


--------------------------------------------------------------------------------
takeWhileMax :: (a -> Bool) -> Int -> [a] -> ([a], [a])
takeWhileMax _ 0 xs = ([], xs)
takeWhileMax _ _ [] = ([], [])
takeWhileMax p i (x : xs)
    | p x           = let (y, z) = takeWhileMax p (i - 1) xs in (x : y, z)
    | otherwise     = ([], x : xs)


--------------------------------------------------------------------------------
genMarkupConstructor :: MonadGen m => Stream Markup -> MarkdownGen m Markup
genMarkupConstructor m = oneOf
    [ return $ ItalicM m
    , return $ BoldM m
    , genLink >>= \link -> return $ LinkM m link
    ]


--------------------------------------------------------------------------------
genLink :: MonadGen m => m Text
genLink = sampleFromList
    [ "http://eelslap.com/"
    , "http://en.wikipedia.org/wiki/Sterling_Archer"
    , "http://example.com/"
    , "http://gifctrl.com/"
    , "http://haskell.org/"
    , "http://heeeeeeeey.com/"
    , "http://hipstermerkel.tumblr.com/"
    , "http://html9responsiveboilerstrapjs.com/"
    , "http://imgur.com/"
    , "http://jaspervdj.be/"
    , "http://kimjongunlookingatthings.tumblr.com/"
    , "http://landyachtz.com/"
    , "http://news.ycombinator.com/"
    , "http://omfgdogs.com/"
    , "http://omgcatsinspace.tumblr.com/"
    , "http://reddit.com/r/thathappened"
    , "http://seenly.com/"
    , "http://stoneship.org/"
    , "http://textfromdog.tumblr.com/"
    , "http://tumblr.com/"
    , "http://twitter.com/search?q=haskell"
    , "http://www.billmays.net/"
    , "http://www.lipsum.com/"
    , "http://www.metafilter.com/"
    , "http://www.mozilla.org/"
    , "http://www.raynelongboards.com/"
    , "http://www.reddit.com/r/haskell"
    , "http://www.thesecretofinvisibility.com/"
    , "http://www.uselessaccount.com/"
    , "http://www.wedrinkwater.com/"
    , "http://www.wtfpl.net/"
    , "http://www.youtube.com/watch?v=MghiBW3r65M"
    , "http://zeus.ugent.be/"
    , "http://zombo.com/"
    ]


--------------------------------------------------------------------------------
printMarkdown :: MarkdownConfig -> Markdown -> Print ()
printMarkdown mc blocks = do
    sequence_ $ intersperse printNl $ map (printBlock mc) blocks
    let links = markdownLinks blocks
    unless (mcInlineLinks mc || null links) $ do
        printNl
        mapM_ (uncurry printLink) links
  where
    printLink str link =
        printText "[" >> printSentence mc str >> printText "]: " >>
        printText link >> printNl


--------------------------------------------------------------------------------
printBlock :: MarkdownConfig -> Block -> Print ()
printBlock mc (HeaderB h)        = printHeader mc h
printBlock mc (ParagraphB p)     = printParagraph mc p
printBlock mc (OrderedListB l)   = printOrderedList mc l
printBlock mc (UnorderedListB l) = printUnorderedList mc l
printBlock _  (CodeB c)          = printIndent4 (printCode c)
printBlock mc (QuoteB p)         = printWrapIndent "> " $
    printText "> " >> printParagraph mc p


--------------------------------------------------------------------------------
printHeader :: MarkdownConfig -> Header -> Print ()
printHeader mc (Header i p)
    | mcHashHeaders mc && i <= 2 =
        printText (T.replicate i "#" <> " ") >> printPlainPhrase p >> printNl
    | otherwise                  = do
        let len  = runPrintLength (printPlainPhrase p)
            char = if i <= 1 then "=" else "-"
        printPlainPhrase p >> printNl
        printText (T.replicate len char) >> printNl


--------------------------------------------------------------------------------
printParagraph :: MarkdownConfig -> Paragraph -> Print ()
printParagraph mc paragraph = do
    sequence_ $ intersperse printBrkSp (map (printSentence mc) paragraph)
    printNl


--------------------------------------------------------------------------------
printOrderedList :: MarkdownConfig -> [Phrase] -> Print ()
printOrderedList mc phrases = forM_ (zip [1 :: Int .. ] phrases) $ \(i, p) ->
    printShow i >> printText ". " >> printPhrase mc p >> printNl


--------------------------------------------------------------------------------
printUnorderedList :: MarkdownConfig -> [Phrase] -> Print ()
printUnorderedList mc phrases = forM_ phrases $ \p ->
    printText "- " >> printPhrase mc p >> printNl


--------------------------------------------------------------------------------
printSentence :: MarkdownConfig -> Sentence -> Print ()
printSentence mc = printStream printMarkup
  where
    printMarkup (PlainM t) = printText t
    printMarkup (ItalicM m)
        | mcAsteriskEm mc =
            printText "*" >> printStream printMarkup m >> printText "*"
        | otherwise       =
            printText "_" >> printStream printMarkup m >> printText "_"
    printMarkup (BoldM  m)
        | mcAsteriskStrong mc =
            printText "**" >> printStream printMarkup m >> printText "**"
        | otherwise           =
            printText "__" >> printStream printMarkup m >> printText "__"
    printMarkup (LinkM m l)
        | mcInlineLinks mc =
            printText "[" >> printStream printMarkup m >> printText "](" >>
            printText l >> printText ")"
        | otherwise        =
            printText "[" >> printStream printMarkup m >> printText "]"


--------------------------------------------------------------------------------
printPhrase :: MarkdownConfig -> Phrase -> Print ()
printPhrase = printSentence


--------------------------------------------------------------------------------
printPlainPhrase :: PlainPhrase -> Print ()
printPlainPhrase = printStream printText
