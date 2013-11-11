--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
module LoremMarkdownum.Markov
    ( Markov
    , empty
    , feed
    , lookup
    , optimize
    ) where


--------------------------------------------------------------------------------
import           Prelude                       hiding (lookup)


--------------------------------------------------------------------------------
import           LoremMarkdownum.FrequencyTree (FrequencyTree)
import qualified LoremMarkdownum.FrequencyTree as FT
import           LoremMarkdownum.Trie          (Trie)
import qualified LoremMarkdownum.Trie          as Trie


--------------------------------------------------------------------------------
type Markov a = Trie a (FrequencyTree a)


--------------------------------------------------------------------------------
empty :: Ord a => Markov a
empty = Trie.empty


--------------------------------------------------------------------------------
feed :: Ord a => Int -> [a] -> Markov a -> Markov a
feed k
    | k < 1     = error "Data.Markov.feed: k < 1"
    | otherwise = go
  where
    go ls mk = case splitAt k ls of
        (_,    [])         -> mk
        (keys, (next : _)) ->
            let !mk' = Trie.insertWith FT.append keys (FT.singleton next) mk
            in go (drop 1 ls) mk'


--------------------------------------------------------------------------------
lookup :: Ord a => [a] -> Markov a -> Maybe (FrequencyTree a)
lookup = Trie.lookup


--------------------------------------------------------------------------------
optimize :: Ord a => Markov a -> Markov a
optimize = fmap FT.optimize
