--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module LoremMarkdownum.Gen.Markdown
    ( MarkdownConfig (..)
    , mkDefaultMarkdownConfig
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

    , previewMarkdown
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative           ((<$>))
import           Control.Monad                 (forM, forM_, replicateM, when)
import           Control.Monad.Reader          (ReaderT, ask, runReaderT)
import           Control.Monad.State           (StateT, evalStateT, get, modify)
import           Data.List                     (intersperse)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (maybeToList)
import           Data.Monoid                   (mconcat, (<>))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Traversable              (traverse)
import           Text.Blaze.Html5              (Html, (!))
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
import           LoremMarkdownum.FrequencyTree (FrequencyTree)
import           LoremMarkdownum.Gen
import           LoremMarkdownum.Gen.Code
import           LoremMarkdownum.Markov        (Markov)
import qualified LoremMarkdownum.Markov        as Markov
import           LoremMarkdownum.Print
import           LoremMarkdownum.Text.Util
import           LoremMarkdownum.Token


--------------------------------------------------------------------------------
data MarkdownConfig = MarkdownConfig
    { mcLengthMarkov     :: Markov (Token Int)
    , mcWordFrequency    :: Map Int (FrequencyTree Text)
    , mcCodeConfig       :: CodeConfig
    , mcNoHeaders        :: Bool
    , mcNoCode           :: Bool
    , mcNoQuotes         :: Bool
    , mcNoLists          :: Bool
    , mcNoInlineMarkup   :: Bool
    , mcReferenceLinks   :: Bool
    , mcUnderlineHeaders :: Bool
    , mcUnderscoreEm     :: Bool
    , mcUnderscoreStrong :: Bool
    , mcNumBlocks        :: Maybe Int
    , mcHeaderDepth      :: Int
    } deriving (Show)


--------------------------------------------------------------------------------
mkDefaultMarkdownConfig :: Markov (Token Int)
                        -> Map Int (FrequencyTree Text)
                        -> CodeConfig
                        -> MarkdownConfig
mkDefaultMarkdownConfig mrkv ft cc = MarkdownConfig mrkv ft cc
    False False False False False False False False False Nothing 2


--------------------------------------------------------------------------------
data MarkdownState = MarkdownState
    { msTokenQueue :: [Token Text]
    } deriving (Show)


--------------------------------------------------------------------------------
type MarkdownGen m a = ReaderT MarkdownConfig (StateT MarkdownState m) a


--------------------------------------------------------------------------------
runMarkdownGen :: MonadGen m
               => MarkdownGen m a -> MarkdownConfig -> MarkdownState -> m a
runMarkdownGen mg mc = evalStateT (runReaderT mg mc)


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
    noHeaders     <- mcNoHeaders <$> ask
    maxHDepth     <- mcHeaderDepth <$> ask
    numBlocks     <- maybe (randomInt (7, 9)) return . mcNumBlocks =<< ask
    numSections   <- randomInt (2, max 3 (numBlocks `div` 2 + 1))
    partitioning  <- partitionNicely numSections (numBlocks - 1)
    blocks        <- forM partitioning $ \numBlocksInSection -> do
        section <- genSection numBlocksInSection 3 maxHDepth
        h2      <- HeaderB <$> genHeader 2
        return $ h2 : section

    h1      <- HeaderB    <$> genHeader 1
    lastPar <- ParagraphB <$> genParagraph
    return $ (if noHeaders then removeHeaders else id) $
        [h1] ++ concat blocks ++ [lastPar]
  where
    removeHeaders = filter (\b -> case b of HeaderB _ -> False; _ -> True)


--------------------------------------------------------------------------------
genSection :: MonadGen m => Int -> Int -> Int -> MarkdownGen m [Block]
genSection numBlocks headerDepth maxHDepth
    | numBlocks <= 0 = return []
    | otherwise      = do
        par0    <- ParagraphB <$> genParagraph
        subHead <- (&& numBlocks > 2) . (&& headerDepth <= maxHDepth) <$> randomBool maxHDepth headerDepth 
        special <- (&& numBlocks > 1) <$> randomBool 3 1
        pars    <- case (subHead, special) of
                        (True, _) -> ((:) . HeaderB <$> genHeader headerDepth) <*>
                                     (return . ParagraphB <$> genParagraph)
                        (_, True) -> return <$> genSpecialBlock
                        _         -> return []
        let hDepth = if subHead then headerDepth + 1 else headerDepth
        ((par0 : pars) ++) <$> genSection (numBlocks - 1 - length pars) hDepth maxHDepth


--------------------------------------------------------------------------------
-- TODO (jaspervdj: Not sure about the name
genSpecialBlock :: MonadGen m => MarkdownGen m Block
genSpecialBlock = do
    noCode   <- mcNoCode   <$> ask
    noQuotes <- mcNoQuotes <$> ask
    noLists  <- mcNoLists  <$> ask
    let freqs =
            [ (if noLists  then 0 else 1, OrderedListB   <$> genOrderedList)
            , (if noLists  then 0 else 1, UnorderedListB <$> genUnorderedList)
            , (if noCode   then 0 else 2, CodeB          <$> genCodeBlock)
            , (if noQuotes then 0 else 1, QuoteB         <$> genParagraph)
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
genSentence n = do
    noInlineMarkup <- mcNoInlineMarkup <$> ask
    sentence       <- genPlainSentence n
    if noInlineMarkup
        then return (map (fmap PlainM) sentence)
        else genMarkup sentence


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
genLink :: MonadGen m => MarkdownGen m Text
genLink = do
    n0          <- randomInt (1, 2)
    domainParts <- replicateM n0 genLinkPart
    n1          <- randomInt (0, 2)
    pathParts   <- replicateM n1 genLinkPart
    domainPart  <- sampleFromList
        [T.concat domainParts, T.intercalate "-" domainParts]
    pathPart    <- sampleFromList
        [T.concat pathParts, T.intercalate "-" pathParts]
    www         <- sampleFromFrequencies [("", 3), ("www.", 1)]
    tld         <- sampleFromList [".org", ".net", ".com", ".io"]
    ext         <- sampleFromFrequencies
        [("", 5), (".php", 1), (".html", 2), (".aspx", 1)]

    return $ T.concat ["http://", www, domainPart, tld, "/", pathPart, ext]
  where
    genLinkPart :: MonadGen m => MarkdownGen m Text
    genLinkPart = do
        token <- genToken
        case token of
            Element x -> return (T.toLower x)
            _         -> genLinkPart


--------------------------------------------------------------------------------
printMarkdown :: MarkdownConfig -> Markdown -> Print ()
printMarkdown mc blocks = do
    sequence_ $ intersperse printNl $ map (printBlock mc) blocks
    let links = markdownLinks blocks
    when (mcReferenceLinks mc && not (null links)) $ do
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
    | mcUnderlineHeaders mc && i <= 2 = do
        let len  = runPrintLength (printPlainPhrase p)
            char = if i <= 1 then "=" else "-"
        printPlainPhrase p >> printNl
        printText (T.replicate len char) >> printNl
    | otherwise                       =
        printText (T.replicate i "#" <> " ") >> printPlainPhrase p >> printNl


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
        | mcUnderscoreEm mc =
            printText "_" >> printStream printMarkup m >> printText "_"
        | otherwise         =
            printText "*" >> printStream printMarkup m >> printText "*"
    printMarkup (BoldM  m)
        | mcUnderscoreStrong mc =
            printText "__" >> printStream printMarkup m >> printText "__"
        | otherwise             =
            printText "**" >> printStream printMarkup m >> printText "**"
    printMarkup (LinkM m l)
        | mcReferenceLinks mc =
            printText "[" >> printStream printMarkup m >> printText "]"
        | otherwise        =
            printText "[" >> printStream printMarkup m >> printText "](" >>
            printText l >> printText ")"


--------------------------------------------------------------------------------
printPhrase :: MarkdownConfig -> Phrase -> Print ()
printPhrase = printSentence


--------------------------------------------------------------------------------
printPlainPhrase :: PlainPhrase -> Print ()
printPlainPhrase = printStream printText


--------------------------------------------------------------------------------
previewMarkdown :: Markdown -> Html
previewMarkdown = mconcat . map previewBlock


--------------------------------------------------------------------------------
previewBlock :: Block -> Html
previewBlock (HeaderB h)        = previewHeader h
previewBlock (ParagraphB p)     = previewParagraph p
previewBlock (OrderedListB l)   = previewOrderedList l
previewBlock (UnorderedListB l) = previewUnorderedList l
previewBlock (CodeB c)          = H.pre $ H.toHtml $ runPrint $ printCode c
previewBlock (QuoteB q)         = H.blockquote $ previewParagraph q


--------------------------------------------------------------------------------
previewHeader :: Header -> Html
previewHeader (Header 1 pf) = H.h1 $ previewPlainPhrase pf
previewHeader (Header 2 pf) = H.h2 $ previewPlainPhrase pf
previewHeader (Header 3 pf) = H.h3 $ previewPlainPhrase pf
previewHeader (Header 4 pf) = H.h4 $ previewPlainPhrase pf
previewHeader (Header 5 pf) = H.h5 $ previewPlainPhrase pf
previewHeader (Header _ pf) = H.h6 $ previewPlainPhrase pf


--------------------------------------------------------------------------------
previewParagraph :: Paragraph -> Html
previewParagraph = H.p . mconcat . intersperse " " . map previewSentence


--------------------------------------------------------------------------------
previewOrderedList :: [Phrase] -> Html
previewOrderedList = H.ol . mconcat . map (H.li . previewPhrase)


--------------------------------------------------------------------------------
previewUnorderedList :: [Phrase] -> Html
previewUnorderedList = H.ul . mconcat . map (H.li . previewPhrase)


--------------------------------------------------------------------------------
previewSentence :: Sentence -> Html
previewSentence = previewStream previewMarkup


--------------------------------------------------------------------------------
previewPlainPhrase :: PlainPhrase -> Html
previewPlainPhrase = previewStream H.toHtml


--------------------------------------------------------------------------------
previewPhrase :: Phrase -> Html
previewPhrase = previewSentence


--------------------------------------------------------------------------------
previewMarkup :: Markup -> Html
previewMarkup (PlainM t)  = H.toHtml t
previewMarkup (ItalicM s) = H.em $ previewSentence s
previewMarkup (BoldM s)   = H.strong $ previewSentence s
previewMarkup (LinkM s h) = H.a ! A.href (H.toValue h) $ previewSentence s
