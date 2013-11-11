--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
module LoremMarkdownum.FrequencyTree
    ( FrequencyTree
    , singleton
    , fromList
    , fromFrequencies
    , append
    , optimize
    , sum
    , sample
    , sampleIO
    ) where


--------------------------------------------------------------------------------
import           Data.List       (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Prelude         hiding (sum)
import           System.Random   (randomRIO)


--------------------------------------------------------------------------------
data FrequencyTree a
    = FrequencyTreeLeaf {-# UNPACK #-} !Int !a
    | FrequencyTreeNode {-# UNPACK #-} !Int (FrequencyTree a) (FrequencyTree a)
    deriving (Read)


--------------------------------------------------------------------------------
instance Show a => Show (FrequencyTree a) where
    show = unlines . show'
      where
        indent             = map ("    " ++)
        mapLast _ []       = []
        mapLast f [x]      = [f x]
        mapLast f (x : xs) = x : mapLast f xs

        show' (FrequencyTreeLeaf f i) =
            ["(FrequencyTreeLeaf " ++ show f ++ " " ++ show i ++ ")"]
        show' (FrequencyTreeNode f l r) =
            ["(FrequencyTreeNode " ++ show f] ++
            indent (show' l) ++
            indent (mapLast (++ ")") $ show' r)


--------------------------------------------------------------------------------
singleton :: Ord a => a -> FrequencyTree a
singleton = FrequencyTreeLeaf 1


--------------------------------------------------------------------------------
fromList :: Ord a => [a] -> FrequencyTree a
fromList = frequencyMapToTree . listToFrequencyMap


--------------------------------------------------------------------------------
fromFrequencies :: Ord a => [(a, Int)] -> FrequencyTree a
fromFrequencies = frequencyMapToTree . M.fromListWith (+)


--------------------------------------------------------------------------------
append :: Ord a
       => FrequencyTree a -> FrequencyTree a -> FrequencyTree a
append x y = FrequencyTreeNode (sum x + sum y) x y


--------------------------------------------------------------------------------
optimize :: Ord a => FrequencyTree a -> FrequencyTree a
optimize = frequencyMapToTree . frequencyTreeToMap


--------------------------------------------------------------------------------
sum :: Ord a => FrequencyTree a -> Int
sum (FrequencyTreeLeaf f _)   = f
sum (FrequencyTreeNode f _ _) = f


--------------------------------------------------------------------------------
sample :: Ord a => Int -> FrequencyTree a -> a
sample seed initial =
    go (seed `mod` sum initial) initial
  where
    go _   (FrequencyTreeLeaf _ i)   = i
    go idx (FrequencyTreeNode _ l r)
        | idx < lsum                 = go idx          l
        | otherwise                  = go (idx - lsum) r
      where
        lsum = sum l


--------------------------------------------------------------------------------
sampleIO :: Ord a => FrequencyTree a -> IO a
sampleIO ft = do
    idx <- randomRIO (0, sum ft - 1)
    return $ sample idx ft


--------------------------------------------------------------------------------
type FrequencyMap a = Map a Int


--------------------------------------------------------------------------------
frequencyTreeToMap :: Ord a => FrequencyTree a -> FrequencyMap a
frequencyTreeToMap (FrequencyTreeLeaf f i)   = M.singleton i f
frequencyTreeToMap (FrequencyTreeNode _ l r) =
    M.unionWith (+) (frequencyTreeToMap l) (frequencyTreeToMap r)


--------------------------------------------------------------------------------
-- | This is the only function building an optimized 'FrequencyTree'.
frequencyMapToTree :: Ord a => FrequencyMap a -> FrequencyTree a
frequencyMapToTree hm = case M.toList hm' of
    -- Here, we take advantage of the fact that 'M.toList' produces a list
    -- lazily. Otherwise this would be kind of slow.
    []          -> error "frequencyMapToTree: Empty FrequencyMap"
    [(i, f)]    -> FrequencyTreeLeaf f i
    (_ : _ : _) -> FrequencyTreeNode fsum
        (frequencyMapToTree lhm) (frequencyMapToTree rhm)
  where
    -- Two big slow folds here!
    hm'           = M.filter (>= 0) hm
    fsum          = M.foldl' (+) 0 hm'
    half          = fsum `div` 2
    (lhm, rhm, _) = M.foldlWithKey' partition (M.empty, M.empty, 0) hm'

    partition (l, r, n) i f
        -- The first guard ensures we *always* add the last item to the right
        -- tree. Otherwise we might end up with an empty right tree in some
        -- cases.
        | n + f >= fsum = (l, M.insertWith (+) i f r, n + f)
        | n < half      = (M.insertWith (+) i f l, r, n + f)
        | otherwise     = (l, M.insertWith (+) i f r, n + f)


--------------------------------------------------------------------------------
listToFrequencyMap :: Ord a => [a] -> FrequencyMap a
listToFrequencyMap = foldl' (\hm i -> M.insertWith (+) i 1 hm) M.empty
