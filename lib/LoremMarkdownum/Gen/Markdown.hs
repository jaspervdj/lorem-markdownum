--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LoremMarkdownum.Gen.Markdown
    ( HeaderOption (..)
    , OrderedListOption (..)
    , UnorderedListOption (..)
    , EmphasisOption (..)
    , StrongOption (..)
    , CodeBlockOption (..)
    , MarkdownOptions (..)
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
data HeaderOption
    = HeaderHash
    | HeaderUnderline
    | HeaderOff
    deriving (Bounded, Enum, Eq, Show)


--------------------------------------------------------------------------------
data OrderedListOption
    = OrderedListDecimal
    | OrderedListAlpha
    | OrderedListRoman
    | OrderedListOff
    deriving (Bounded, Enum, Eq, Show)


--------------------------------------------------------------------------------
data UnorderedListOption
    = UnorderedListDash
    | UnorderedListAsterisk
    | UnorderedListOff
    deriving (Bounded, Enum, Eq, Show)


--------------------------------------------------------------------------------
data EmphasisOption
    = EmphasisAsterisk
    | EmphasisUnderscore
    | EmphasisOff
    deriving (Bounded, Enum, Eq, Show)


--------------------------------------------------------------------------------
data StrongOption
    = StrongAsterisk
    | StrongUnderscore
    | StrongOff
    deriving (Bounded, Enum, Eq, Show)


--------------------------------------------------------------------------------
data CodeBlockOption
    = CodeBlockIndent
    | CodeBlockFenced
    | CodeBlockOff
    deriving (Bounded, Enum, Eq, Show)


--------------------------------------------------------------------------------
-- | MarkdownOptions can be tweaked for each invokation.
data MarkdownOptions = MarkdownOptions
    { moHeaders          :: HeaderOption
    , moCodeBlocks       :: CodeBlockOption
    , moOrderedLists     :: OrderedListOption
    , moUnorderedLists   :: UnorderedListOption
    , moNoQuotes         :: Bool
    , moEmphasis         :: EmphasisOption
    , moStrong           :: StrongOption
    , moNoInlineCode     :: Bool
    , moReferenceLinks   :: Bool
    , moNumBlocks        :: Maybe Int
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
    HeaderHash CodeBlockIndent OrderedListDecimal UnorderedListDash False
    EmphasisAsterisk StrongAsterisk False False Nothing Nothing


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
    = PlainM    Text
    | EmphasisM (Stream Markup)
    | StrongM   (Stream Markup)
    | LinkM     (Stream Markup) Text
    | CodeM     Text
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

    noHeaders <- asks $ (== HeaderOff) . moHeaders . meOptions
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
    opts <- asks meOptions
    let code           = moCodeBlocks opts /= CodeBlockOff
        quotes         = not $ moNoQuotes opts
        orderedLists   = moOrderedLists opts /= OrderedListOff
        unorderedLists = moUnorderedLists opts /= UnorderedListOff
    let freqs =
            [ (1, OrderedListB   <$> genOrderedList)   | orderedLists   ] ++
            [ (1, UnorderedListB <$> genUnorderedList) | unorderedLists ] ++
            [ (2, CodeB          <$> genCodeBlock)     | code           ] ++
            [ (1, QuoteB         <$> genParagraph)     | quotes         ]
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
genHeader :: MonadGen m => MarkdownGen m Header
genHeader = Header . takeWhile tokenIsElement <$> genPlainSentence 4


--------------------------------------------------------------------------------
genPlainPhrase :: MonadGen m => MarkdownGen m PlainPhrase
genPlainPhrase = takeWhile tokenIsElement <$> genPlainSentence 7


--------------------------------------------------------------------------------
genMarkup :: MonadGen m => Stream Text -> MarkdownGen m (Stream Markup)
genMarkup = go True
  where
    go _allowMarkup [] = return []
    go False (Element x : xs) = (Element (PlainM x) :) <$> go True xs
    go True (Element x : xs) = do
        linker <- genLink
        mo <- meOptions <$> ask
        oneOfFrequencies $
            [ (,) 60 $ (Element (PlainM x) :) <$> go True xs
            ] ++
            [ (,) 1 $ do
                (elements, xs') <- aFew x xs
                (Element (EmphasisM elements) :) <$> go False xs'
            | moEmphasis mo /= EmphasisOff
            ] ++
            [ (,) 1 $ do
                (elements, xs') <- aFew x xs
                (Element (StrongM elements) :) <$> go False xs'
            | moStrong mo /= StrongOff
            ] ++
            [ (,) 1 $ do
                codeConfig <- mmCodeConfig . meModel <$> ask
                Identifier ident <- runCodeGen genIdentifier codeConfig
                ([Element (PlainM x), Element (CodeM ident)] ++) <$> go False xs
            | not (moNoInlineCode mo)
            ] ++
            case linker of
                Nothing -> []
                Just l -> pure $ (,) 1 $ do
                    link <- l
                    (elements, xs') <- aFew x xs
                    (Element (LinkM elements link) :) <$> go False xs'
    go allowMarkup (x : xs) =
        (maybeToList (castToken x) ++) <$> go allowMarkup xs

    aFew x xs = do
        len <- randomInt (0, 2)
        let (inc, xs') = takeWhileMax tokenIsElement len xs
            elements   = map (Element . PlainM) $ x : streamElements inc
        pure (elements, xs')


--------------------------------------------------------------------------------
takeWhileMax :: (a -> Bool) -> Int -> [a] -> ([a], [a])
takeWhileMax _ 0 xs = ([], xs)
takeWhileMax _ _ [] = ([], [])
takeWhileMax p i (x : xs)
    | p x           = let (y, z) = takeWhileMax p (i - 1) xs in (x : y, z)
    | otherwise     = ([], x : xs)


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
printBlock mo  (CodeB c) = case moCodeBlocks mo of
    CodeBlockIndent -> printIndent4 (printCode c)
    CodeBlockFenced -> do
        printText "```" >> printNl
        printCode c
        printText "```" >> printNl
    CodeBlockOff -> pure ()
printBlock mo (QuoteB p)         = printWrapIndent "> " $
    printText "> " >> printParagraph mo p


--------------------------------------------------------------------------------
printHeader :: MarkdownOptions -> Int -> Header -> Print ()
printHeader mo lvl (Header p) = case moHeaders mo of
    HeaderOff -> pure ()
    HeaderUnderline | lvl <= 2 -> do
        let len  = runPrintLength (printPlainPhrase p)
            char = if lvl <= 1 then "=" else "-"
        printPlainPhrase p >> printNl
        printText (T.replicate len char) >> printNl
    _ ->
        printText (T.replicate lvl "#" <> " ") >> printPlainPhrase p >> printNl


--------------------------------------------------------------------------------
printParagraph :: MarkdownOptions -> Paragraph -> Print ()
printParagraph mo paragraph = do
    sequence_ $ intersperse printBrkSp (map (printSentence mo) paragraph)
    printNl


--------------------------------------------------------------------------------
printOrderedList :: MarkdownOptions -> [Phrase] -> Print ()
printOrderedList _  []      = pure ()
printOrderedList mo phrases = forM_ (zip indices phrases) $ \(i, p) -> do
    printText (T.justifyLeft longest ' ' i) >> printText " "
    printPhrase mo p >> printNl
  where
    longest = maximum $ map T.length $ take (length phrases) indices
    indices = map (\i -> T.pack i <> ".") $ case moOrderedLists mo of
        OrderedListOff     -> []
        OrderedListDecimal -> map show [1 :: Int ..]
        OrderedListAlpha   -> map pure ['a' .. 'z']
        OrderedListRoman   ->
            let ten = ["i", "ii", "iii", "iv", "v", "vi", "vii", "vii", "x"]
            in ten ++ map ('x' :) ten


--------------------------------------------------------------------------------
printUnorderedList :: MarkdownOptions -> [Phrase] -> Print ()
printUnorderedList mo phrases = forM_ phrases $ \p ->
    printText prefix >> printPhrase mo p >> printNl
  where
    prefix = case moUnorderedLists mo of
        UnorderedListDash     -> "- "
        UnorderedListAsterisk -> "* "
        UnorderedListOff      -> mempty


--------------------------------------------------------------------------------
printSentence :: MarkdownOptions -> Sentence -> Print ()
printSentence mo = printStream printMarkup
  where
    MarkdownOptions {..} = mo

    printMarkup (PlainM t) = printText t
    printMarkup (EmphasisM m) = case moEmphasis of
        EmphasisOff -> pure ()
        EmphasisUnderscore ->
            printText "_" >> printStream printMarkup m >> printText "_"
        EmphasisAsterisk ->
            printText "*" >> printStream printMarkup m >> printText "*"
    printMarkup (StrongM  m) = case moStrong of
        StrongOff -> pure ()
        StrongUnderscore ->
            printText "__" >> printStream printMarkup m >> printText "__"
        StrongAsterisk ->
            printText "**" >> printStream printMarkup m >> printText "**"
    printMarkup (LinkM m l)
        | moReferenceLinks =
            printText "[" >> printStream printMarkup m >> printText "]"
        | otherwise        =
            printText "[" >> printStream printMarkup m >> printText "](" >>
            printText l >> printText ")"
    printMarkup (CodeM t) = printText "`" >> printText t >> printText "`"


--------------------------------------------------------------------------------
printPhrase :: MarkdownOptions -> Phrase -> Print ()
printPhrase = printSentence


--------------------------------------------------------------------------------
printPlainPhrase :: PlainPhrase -> Print ()
printPlainPhrase = printStream printText


--------------------------------------------------------------------------------
previewMarkdown :: MarkdownOptions -> Markdown -> Html
previewMarkdown mopts = mconcat . map (previewBlock mopts)


--------------------------------------------------------------------------------
previewBlock :: MarkdownOptions -> Block -> Html
previewBlock _ (HeaderB lvl h@(Header pf)) =
    el H.! A.id (H.toValue $ headerID h) $ previewPlainPhrase pf
  where
    el = case lvl of
        1 -> H.h1
        2 -> H.h2
        3 -> H.h3
        4 -> H.h4
        5 -> H.h5
        _ -> H.h6
previewBlock _ (ParagraphB p) = previewParagraph p
previewBlock mopts (OrderedListB l) = previewOrderedList mopts l
previewBlock _ (UnorderedListB l) = previewUnorderedList l
previewBlock _ (CodeB c) = H.pre $ H.toHtml $ runPrint $ printCode c
previewBlock _ (QuoteB q) = H.blockquote $ previewParagraph q


--------------------------------------------------------------------------------
previewParagraph :: Paragraph -> Html
previewParagraph = H.p . mconcat . intersperse " " . map previewSentence


--------------------------------------------------------------------------------
previewOrderedList :: MarkdownOptions -> [Phrase] -> Html
previewOrderedList mopts = style . H.ol . mconcat . map (H.li . previewPhrase)
  where
    style = case moOrderedLists mopts of
        OrderedListOff     -> id
        OrderedListAlpha   -> (! A.style "list-style-type: lower-alpha")
        OrderedListDecimal -> (! A.style "list-style-type: decimal")
        OrderedListRoman   -> (! A.style "list-style-type: lower-roman")


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
previewMarkup (PlainM t)    = H.toHtml t
previewMarkup (EmphasisM s) = H.em $ previewSentence s
previewMarkup (StrongM s)   = H.strong $ previewSentence s
previewMarkup (LinkM s h)   = H.a ! A.href (H.toValue h) $ previewSentence s
previewMarkup (CodeM t)     = H.code $ H.toHtml t
