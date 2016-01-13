--------------------------------------------------------------------------------
-- | Super-simple trie datastructure
{-# LANGUAGE BangPatterns #-}
module LoremMarkdownum.Trie
    ( Trie
    , empty
    , insert
    , insertWith
    , lookup
    , fromList
    , toList
    ) where


--------------------------------------------------------------------------------
import           Data.List (foldl')
import           Data.Map  (Map)
import qualified Data.Map  as M
import           Prelude   hiding (lookup, map)


--------------------------------------------------------------------------------
data Trie k v
    = Trie !(Maybe v) !(Map k (Trie k v))
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
instance Ord k => Functor (Trie k) where
    fmap f (Trie mv children) = Trie (fmap f mv) (M.map (fmap f) children)


--------------------------------------------------------------------------------
empty :: Ord k => Trie k v
empty = Trie Nothing M.empty


--------------------------------------------------------------------------------
insert :: Ord k => [k] -> v -> Trie k v -> Trie k v
insert = insertWith const


--------------------------------------------------------------------------------
insertWith :: Ord k => (v -> v -> v) -> [k] -> v -> Trie k v -> Trie k v
insertWith f keys !v = go keys
  where
    go []       (Trie Nothing   children) = Trie (Just v) children
    go []       (Trie (Just v') children) =
        let !nv = f v v' in Trie (Just nv) children
    go (k : ks) (Trie mv        children) = case M.lookup k children of
        Nothing -> Trie mv (M.insert k (go ks empty) children)
        Just t  -> Trie mv (M.insert k (go ks t)     children)


--------------------------------------------------------------------------------
lookup :: Ord k => [k] -> Trie k v -> Maybe v
lookup []       (Trie mv _)        = mv
lookup (k : ks) (Trie _  children) = case M.lookup k children of
    Nothing -> Nothing
    Just c  -> lookup ks c


--------------------------------------------------------------------------------
fromList :: Ord k => [([k], v)] -> Trie k v
fromList = foldl' (\t (k, v) -> insert k v t) empty


--------------------------------------------------------------------------------
toList :: Ord k => Trie k v -> [([k], v)]
toList = go id
  where
    go path (Trie mv children) = maybe id (\v -> ((path [], v) :)) mv $
        [ l
        | (k, child) <- M.toList children
        , l          <- go ((++ [k]) . path) child
        ]
