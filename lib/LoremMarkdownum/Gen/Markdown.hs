--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LoremMarkdownum.Gen.Markdown
    ( MarkdownOptions (..)
    , MarkdownModel (..)
    , defaultMarkdownOptions

    , MarkdownGen
    , runMarkdownGen

    , Markdown
    , Block (..)
    , Paragraph
    , Sentence

    , genMarkdown
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
import           Control.Monad                 (forM, forM_, replicateM, when)
import           Control.Monad.Reader          (ReaderT, ask, asks, local,
                                                runReaderT)
import           Control.Monad.State           (StateT, evalStateT, get, modify)
import           Data.Bifoldable               (Bifoldable (..), biconcatMap)
import           Data.Bifunctor                (Bifunctor (..))
import           Data.Bitraversable            (bitraverse)
import           Data.List                     (intersperse)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (maybeToList)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Traversable              (mapAccumM)
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5              (Html, (!))
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
-- | MarkdownOptions can be tweaked for each invokation.
data MarkdownOptions = MarkdownOptions
    { moNoHeaders        :: Bool
    , moNoCode           :: Bool
    , moNoQuotes         :: Bool
    , moNoLists          :: Bool
    , moNoInlineMarkup   :: Bool
    , moReferenceLinks   :: Bool
    , moUnderlineHeaders :: Bool
    , moUnderscoreEm     :: Bool
    , moUnderscoreStrong :: Bool
    , moNumBlocks        :: Maybe Int
    , moFencedCodeBlocks :: Bool
    , moSeed             :: Maybe Int
    } deriving (Show)


--------------------------------------------------------------------------------
-- | MarkdownModel has all the bits that are static accross invokations.
data MarkdownModel = MarkdownModel
    { mmLengthMarkov  :: Markov (Token Int)
    , mmWordFrequency :: Map Int (FrequencyTree Text)
    , mmCodeConfig    :: CodeConfig
    , mmStart         :: [Token Text]
    } deriving (Show)

--------------------------------------------------------------------------------
defaultMarkdownOptions :: MarkdownOptions
defaultMarkdownOptions = MarkdownOptions
    False False False False False False False False False Nothing False Nothing


--------------------------------------------------------------------------------
data MarkdownEnv = MarkdownEnv
    { meOptions       :: MarkdownOptions
    , meModel         :: MarkdownModel
    , meInternalLinks :: [T.Text]
    } deriving (Show)


--------------------------------------------------------------------------------
data MarkdownState = MarkdownState
    { msTokenQueue :: [Token Text]
    } deriving (Show)


--------------------------------------------------------------------------------
type MarkdownGen m a = ReaderT MarkdownEnv (StateT MarkdownState m) a


--------------------------------------------------------------------------------
runMarkdownGen
    :: MonadGen m
    => MarkdownGen m a -> MarkdownModel -> MarkdownOptions -> m a
runMarkdownGen mg mm mo = evalStateT (runReaderT mg env) state0
  where
    env    = MarkdownEnv mo mm []
    state0 = MarkdownState (mmStart mm)


--------------------------------------------------------------------------------
data Skeleton title block =
    Skeleton title (Either [block] [Skeleton title block])
    deriving (Show)


--------------------------------------------------------------------------------
instance Functor (Skeleton title) where
    fmap f (Skeleton t b) = Skeleton t $ bimap (map f) (map (fmap f)) b


--------------------------------------------------------------------------------
instance Foldable (Skeleton title) where
    foldMap f (Skeleton _ b) = either (foldMap f) (foldMap (foldMap f)) b


--------------------------------------------------------------------------------
instance Traversable (Skeleton title) where
    traverse f (Skeleton t b) =
        Skeleton t <$> bitraverse (traverse f) (traverse (traverse f)) b


--------------------------------------------------------------------------------
instance Bifunctor Skeleton where
    bimap f g (Skeleton t b) =
        Skeleton (f t) $ bimap (map g) (map (bimap f g)) b


--------------------------------------------------------------------------------
instance Bifoldable Skeleton where
    bifoldMap f g (Skeleton t b) =
        f t <> either (foldMap g) (foldMap (bifoldMap f g)) b


--------------------------------------------------------------------------------
-- | Generates a rough plan for what the document should look like.
genSkeleton
    :: forall m title block. MonadGen m
    => (MarkdownGen m title)
    -> (Int -> MarkdownGen m [block])
    -> Int
    -> MarkdownGen m (Skeleton title block)
genSkeleton genTitle genBlocks = go 1
  where
    go :: Int -> Int -> MarkdownGen m (Skeleton title block)
    go lvl numBlocks = do
        title <- genTitle
        numSections <- randomInt (1, max 3 (numBlocks `div` 2 + 1))
        if numSections == 1 || numBlocks < 4 || lvl >= 6
            then do
                blocks <- genBlocks numBlocks
                pure $ Skeleton title $ Left blocks
            else do
                partitioning <- partitionNicely numSections numBlocks
                children <- forM partitioning (go (lvl + 1))
                pure $ Skeleton title $ Right children


--------------------------------------------------------------------------------
type Markdown = [Block]


--------------------------------------------------------------------------------
data Block
    = HeaderB Int Header
    | ParagraphB Paragraph
    | OrderedListB [Phrase]
    | UnorderedListB [Phrase]
    | CodeB Code
    | QuoteB Paragraph
    deriving (Show)


--------------------------------------------------------------------------------
data Header = Header PlainPhrase deriving (Show)


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
skeletonToMarkdown :: Skeleton Header Block -> Markdown
skeletonToMarkdown = go 1
  where
    go lvl (Skeleton title body) =
        [HeaderB lvl title] ++
        case body of
            Left blocks    -> blocks
            Right children -> concatMap (go (lvl + 1)) children


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
    -- So we can start with the right words, lorem lipsum.
    p1 <- ParagraphB <$> genParagraph

    numBlocks <- maybe (randomInt (7, 9)) return . moNumBlocks . meOptions =<< ask
    skeleton <- genSkeleton genHeader genSpecialBlocksPlan numBlocks

    let internalLinks = map ("#" <>) $ map headerID $
            biconcatMap pure (const []) skeleton
    (_, hollow) <- local (\me -> me {meInternalLinks = internalLinks }) $
        mapAccumM
            (\isFirst special -> fmap ((,) False) $ case (isFirst, special) of
                (True, _)  -> pure p1
                (_, True)  -> genSpecialBlock
                (_, False) -> ParagraphB <$> genParagraph)
            True
            skeleton

    noHeaders <- asks $ moNoHeaders . meOptions
    return $ (if noHeaders then removeHeaders else id) $
        skeletonToMarkdown hollow
  where
    removeHeaders = filter (\b -> case b of HeaderB _ _ -> False; _ -> True)

    -- Sprinkle special blocks in between normal blocks.
    genSpecialBlocksPlan n
        | n <= 0    = pure []
        | n == 1    = pure [False]
        | otherwise = do
            special <- randomBool 3 1
            ([False, special] ++) <$> genSpecialBlocksPlan (n - 2)


--------------------------------------------------------------------------------
-- TODO (jaspervdj: Not sure about the name
genSpecialBlock :: MonadGen m => MarkdownGen m Block
genSpecialBlock = do
    noCode   <- asks $ moNoCode   . meOptions
    noQuotes <- asks $ moNoQuotes . meOptions
    noLists  <- asks $ moNoLists  . meOptions
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
        codeConfig <- mmCodeConfig . meModel <$> ask
        depth0 (runCodeGen genCode codeConfig)


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
    lengthMarkov <- mmLengthMarkov . meModel <$> ask
    wordsQueue   <- msTokenQueue <$> get
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
    wordSamples <- asks $ mmWordFrequency . meModel
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
    noInlineMarkup <- moNoInlineMarkup . meOptions <$> ask
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
genHeader :: MonadGen m => MarkdownGen m Header
genHeader = Header . takeWhile tokenIsElement <$> genPlainSentence 4


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
genMarkupConstructor m = do
    linker <- genLink
    oneOf $
        [ return $ ItalicM m
        , return $ BoldM m
        ] ++
        case linker of
            Nothing -> []
            Just l -> pure $ do
                link <- l
                pure $ LinkM m link


--------------------------------------------------------------------------------
headerID :: Header -> T.Text
headerID (Header plain) = T.intercalate "-" $ do
    token <- plain
    case token of
        Element txt -> pure $ T.toLower txt
        _           -> []


--------------------------------------------------------------------------------
genLink :: MonadGen m => MarkdownGen m (Maybe (MarkdownGen m Text))
genLink = do
    internalLinks <- asks meInternalLinks
    pure $ case internalLinks of
        []    -> Nothing
        links -> Just $ sampleFromList links


--------------------------------------------------------------------------------
printMarkdown :: MarkdownOptions -> Markdown -> Print ()
printMarkdown mo blocks = do
    sequence_ $ intersperse printNl $ map (printBlock mo) blocks
    let links = markdownLinks blocks
    when (moReferenceLinks mo && not (null links)) $ do
        printNl
        mapM_ (uncurry printLink) links
  where
    printLink str link =
        printText "[" >> printSentence mo str >> printText "]: " >>
        printText link >> printNl


--------------------------------------------------------------------------------
printBlock :: MarkdownOptions -> Block -> Print ()
printBlock mo (HeaderB lvl h)    = printHeader mo lvl h
printBlock mo (ParagraphB p)     = printParagraph mo p
printBlock mo (OrderedListB l)   = printOrderedList mo l
printBlock mo (UnorderedListB l) = printUnorderedList mo l
printBlock mo  (CodeB c)
    | moFencedCodeBlocks mo = do
        printText "```" >> printNl
        printCode c
        printText "```" >> printNl
    | otherwise                  = printIndent4 (printCode c)
printBlock mo (QuoteB p)         = printWrapIndent "> " $
    printText "> " >> printParagraph mo p


--------------------------------------------------------------------------------
printHeader :: MarkdownOptions -> Int -> Header -> Print ()
printHeader mo lvl (Header p)
    | moUnderlineHeaders mo && lvl <= 2 = do
        let len  = runPrintLength (printPlainPhrase p)
            char = if lvl <= 1 then "=" else "-"
        printPlainPhrase p >> printNl
        printText (T.replicate len char) >> printNl
    | otherwise                       =
        printText (T.replicate lvl "#" <> " ") >> printPlainPhrase p >> printNl


--------------------------------------------------------------------------------
printParagraph :: MarkdownOptions -> Paragraph -> Print ()
printParagraph mo paragraph = do
    sequence_ $ intersperse printBrkSp (map (printSentence mo) paragraph)
    printNl


--------------------------------------------------------------------------------
printOrderedList :: MarkdownOptions -> [Phrase] -> Print ()
printOrderedList mo phrases = forM_ (zip [1 :: Int .. ] phrases) $ \(i, p) ->
    printShow i >> printText ". " >> printPhrase mo p >> printNl


--------------------------------------------------------------------------------
printUnorderedList :: MarkdownOptions -> [Phrase] -> Print ()
printUnorderedList mo phrases = forM_ phrases $ \p ->
    printText "- " >> printPhrase mo p >> printNl


--------------------------------------------------------------------------------
printSentence :: MarkdownOptions -> Sentence -> Print ()
printSentence mo = printStream printMarkup
  where
    MarkdownOptions {..} = mo

    printMarkup (PlainM t) = printText t
    printMarkup (ItalicM m)
        | moUnderscoreEm =
            printText "_" >> printStream printMarkup m >> printText "_"
        | otherwise         =
            printText "*" >> printStream printMarkup m >> printText "*"
    printMarkup (BoldM  m)
        | moUnderscoreStrong =
            printText "__" >> printStream printMarkup m >> printText "__"
        | otherwise             =
            printText "**" >> printStream printMarkup m >> printText "**"
    printMarkup (LinkM m l)
        | moReferenceLinks =
            printText "[" >> printStream printMarkup m >> printText "]"
        | otherwise        =
            printText "[" >> printStream printMarkup m >> printText "](" >>
            printText l >> printText ")"


--------------------------------------------------------------------------------
printPhrase :: MarkdownOptions -> Phrase -> Print ()
printPhrase = printSentence


--------------------------------------------------------------------------------
printPlainPhrase :: PlainPhrase -> Print ()
printPlainPhrase = printStream printText


--------------------------------------------------------------------------------
previewMarkdown :: Markdown -> Html
previewMarkdown = mconcat . map previewBlock


--------------------------------------------------------------------------------
previewBlock :: Block -> Html
previewBlock (HeaderB lvl h@(Header pf))  =
    el H.! A.id (H.toValue $ headerID h) $ previewPlainPhrase pf
  where
    el = case lvl of
        1 -> H.h1
        2 -> H.h2
        3 -> H.h3
        4 -> H.h4
        5 -> H.h5
        _ -> H.h6
previewBlock (ParagraphB p)     = previewParagraph p
previewBlock (OrderedListB l)   = previewOrderedList l
previewBlock (UnorderedListB l) = previewUnorderedList l
previewBlock (CodeB c)          = H.pre $ H.toHtml $ runPrint $ printCode c
previewBlock (QuoteB q)         = H.blockquote $ previewParagraph q


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
