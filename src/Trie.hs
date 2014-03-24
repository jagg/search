{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Trie
( createTrie
, empty
, union
, unionWith
, intersection
, ingest
, insert
, insertWith
, lookupByPrefix
, Trie.lookup
, Trie
) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad
import           Data.Char
import qualified Data.IntMap.Strict  as M
import qualified Data.List           as L
import qualified Data.Text           as T

data Trie a  = Leaf !a
             | Node !(Maybe a) !(M.IntMap (Trie a))
               deriving (Show, Eq)

createTrie :: T.Text -> a -> Trie a
createTrie t v = case T.uncons t of
                    Nothing        -> Leaf v
                    Just (c, rest) -> Node Nothing (M.insert (ord c) (createTrie rest v) M.empty)

empty :: Trie a
empty = Node Nothing M.empty

union :: Trie a -> Trie a -> Trie a
union (Leaf x) (Node _ m) = Node (Just x) m
union (Node _ m) (Leaf x) = Node (Just x) m
union l@(Leaf _) (Leaf _) = l
union (Node x m) (Node y m') = Node (mplus x y) (M.unionWith union m m')

unionWith :: (a -> a -> a) -> Trie a -> Trie a -> Trie a
unionWith merge (Leaf x) (Node my m) = Node (merge <$> (return x) <*> my) m
unionWith merge (Node mx m) (Leaf y) = Node (merge <$> mx <*> (return y)) m
unionWith merge (Leaf x) (Leaf y) = Leaf (merge x y)
unionWith merge (Node x m) (Node y m') = Node (merge <$> x <*> y)
                                              (M.unionWith (unionWith merge) m m')

intersection :: Trie a -> Trie a -> Trie a
intersection (Leaf x) (Node _ _) = Leaf x
intersection (Node _ _) (Leaf x) = Leaf x
intersection l@(Leaf _) (Leaf _) = l
intersection (Node x m) (Node y m') = Node (mplus x y) (M.intersectionWith intersection m m')



ingest :: Trie a -> [(T.Text, a)] -> Trie a
ingest = L.foldl' (flip (union . subTrie'))
         where subTrie' (t,v) = createTrie t v

insert :: T.Text -> a -> Trie a -> Trie a
insert k v = union (createTrie k v)

insertWith ::  (a -> a -> a) -> T.Text -> a -> Trie a -> Trie a
insertWith merge k v t = unionWith merge (createTrie k v) t

lookup :: T.Text -> Trie a -> Maybe a
lookup t trie = join (fmap topValue (subTrie t trie))

subTrie :: T.Text -> Trie t -> Maybe (Trie t)
subTrie t trie = case T.uncons t of
  Nothing -> Just trie
  Just (c, rest) -> case trie of
                       Leaf _ -> Nothing
                       Node _ m -> case M.lookup (ord c) m of
                         Nothing -> Nothing
                         Just ntrie -> Trie.subTrie rest ntrie


-- FIXME: very slow
terms :: T.Text -> Trie a -> [T.Text]
terms prefix t = case t of
  Leaf _    -> [prefix]
  Node _ m  -> join . M.elems $ M.mapWithKey f m
               where f k = terms (T.append prefix (T.pack [chr k]))


lookupByPrefix :: T.Text -> Trie a -> Maybe [T.Text]
lookupByPrefix t trie = fmap (terms t) (subTrie t trie)

topValue :: Trie a -> Maybe a
topValue (Leaf x) = Just x
topValue (Node mx _) = mx

