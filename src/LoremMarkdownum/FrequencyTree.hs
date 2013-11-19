--------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Data.List       (foldl', sortBy)
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
-- | This function builds a 'FrequencyTree' that has good performance
-- characteristics in most cases.
frequencyMapToTree :: forall a. Ord a => FrequencyMap a -> FrequencyTree a
frequencyMapToTree =
    listToTree . sortBy (\(_, x) (_, y) -> compare y x) .
    filter ((> 0) . snd) . M.toList
  where
    listToTree :: [(a, Int)] -> FrequencyTree a
    listToTree []       = error "frequencyMapToTree: Empty FrequencyMap"
    listToTree [(x, f)] = FrequencyTreeLeaf f x
    listToTree xs       =
        let (l, lSum, r, rSum) = partition xs
        in FrequencyTreeNode (lSum + rSum)
            (listToTree l) (listToTree r)

    partition :: [(a, Int)] -> ([(a, Int)], Int, [(a, Int)], Int)
    partition = go [] 0 [] 0
      where
        go l lSum r rSum ls = case ls of
            []                 -> (reverse l, lSum, reverse r, rSum)
            ((x, f) : xs)
                | lSum <= rSum -> go ((x, f) : l) (f + lSum) r rSum xs
                | otherwise    -> go l lSum ((x, f) : r) (f + rSum) xs


--------------------------------------------------------------------------------
listToFrequencyMap :: Ord a => [a] -> FrequencyMap a
listToFrequencyMap = foldl' (\hm i -> M.insertWith (+) i 1 hm) M.empty
