--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LoremMarkdownum.Gen
    ( MonadGen (..)

    , GenIO
    , runGenIO

    , randomBool

    , oneOf
    , oneOfFrequencies

    , sampleFromList
    , sampleFromFrequencies
    , sampleFromFrequencyTree

    , shuffle
    , partitionNicely

    , depth0
    , deeper
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative  (Applicative, (<$>))
import           Control.Monad        (join)
import           Control.Monad.Reader (ReaderT, ask, local, mapReaderT,
                                       runReaderT)
import           Control.Monad.State  (StateT, get, put, runStateT)
import           Control.Monad.Trans  (lift)
import           Data.Tuple           (swap)
import           System.Random        (randomRIO)


--------------------------------------------------------------------------------
import           LoremMarkdownum.FrequencyTree   (FrequencyTree)
import qualified LoremMarkdownum.FrequencyTree   as FT


--------------------------------------------------------------------------------
class (Applicative m, Functor m, Monad m) => MonadGen m where
    randomInt :: (Int, Int) -> m Int

    depth     :: m Int
    withDepth :: Int -> m a -> m a


--------------------------------------------------------------------------------
instance MonadGen m => MonadGen (ReaderT r m) where
    randomInt   = lift . randomInt
    depth       = lift depth
    withDepth d = mapReaderT (withDepth d)


--------------------------------------------------------------------------------
instance MonadGen m => MonadGen (StateT r m) where
    randomInt   = lift . randomInt
    depth       = lift depth
    withDepth d = \ms -> do
        s       <- get
        (x, s') <- lift $ withDepth d $ runStateT ms s
        put s'
        return x


--------------------------------------------------------------------------------
newtype GenIO a = GenIO {unGenIO :: ReaderT Int IO a}
    deriving (Applicative, Functor, Monad)


--------------------------------------------------------------------------------
instance MonadGen GenIO where
    randomInt   = GenIO . lift . randomRIO
    depth       = GenIO ask
    withDepth d = GenIO . local (const d) . unGenIO


--------------------------------------------------------------------------------
runGenIO :: GenIO a -> IO a
runGenIO gio = runReaderT (unGenIO gio) 0


--------------------------------------------------------------------------------
randomBool :: MonadGen m => Int -> Int -> m Bool
randomBool t f = (<= t) <$> randomInt (1, t + f)


--------------------------------------------------------------------------------
oneOf :: MonadGen m => [m a] -> m a
oneOf [] = error "Text.LoremMarkdownum.Gen.oneOf: empty list"
oneOf xs = do
    idx <- randomInt (0, length xs - 1)
    xs !! idx


--------------------------------------------------------------------------------
oneOfFrequencies :: MonadGen m => [(Int, m a)] -> m a
oneOfFrequencies = join . sampleFromFrequencies . map swap


--------------------------------------------------------------------------------
sampleFromList :: MonadGen m => [a] -> m a
sampleFromList = oneOf . map return


--------------------------------------------------------------------------------
sampleFromFrequencies :: MonadGen m => [(a, Int)] -> m a
sampleFromFrequencies freqs = do
    -- We could also use 'sampleFromFrequencyTree' but this way we don't have
    -- the 'Eq/Ord' constraint, which is nice (we can sample functions etc.).
    idx <- randomInt (0, sum (map snd freqs) - 1)
    return $ go idx freqs
  where
    go i ((x, f) : xs)
        | i < f     = x
        | otherwise = go (i - f) xs
    go _ []         = error
        "Text.LoremMarkdownum.Gen.sampleFromFrequencies: empty list"


--------------------------------------------------------------------------------
sampleFromFrequencyTree :: (Ord a, MonadGen m)
                        => FrequencyTree a -> m a
sampleFromFrequencyTree ft = do
    idx <- randomInt (0, FT.sum ft - 1)
    return $ FT.sample idx ft


--------------------------------------------------------------------------------
-- | Super-slow.
shuffle :: MonadGen m => [a] -> m [a]
shuffle list
    | len < 2   = return list
    | otherwise = do
        i <- randomInt (0, len - 1)
        let (xs, ys) = splitAt i list
        (take 1 ys ++) <$> shuffle (xs ++ drop 1 ys)
  where
    len = length list


--------------------------------------------------------------------------------
partitionNicely :: MonadGen m => Int -> Int -> m [Int]
partitionNicely nGroups total = shuffle $ filter (> 0) $
    let (x, remainder) = total `divMod` nGroups
    in replicate remainder (x + 1) ++ replicate (nGroups - remainder) x


--------------------------------------------------------------------------------
depth0 :: MonadGen m => m a -> m a
depth0 = withDepth 0


--------------------------------------------------------------------------------
deeper :: MonadGen m => m a -> m a
deeper ma = depth >>= \d -> withDepth (d + 1) ma
