module Index
( InMemoryIndex
, empty
, addTerm
, wildcard
, Index.lookup
) where

import           Control.Monad (forM)
import           Data.List     (foldl1')

import qualified Data.Text     as T
import qualified Postings      as PS
import qualified Trie          as TR


newtype InMemoryIndex = IMI { getIMI :: TR.Trie PS.Postings } deriving (Show)

empty :: InMemoryIndex
empty = IMI TR.empty

addTerm :: T.Text -> PS.DocId -> [Int] -> InMemoryIndex -> InMemoryIndex
addTerm t dId ps (IMI trie) = IMI $ TR.insertWith PS.union t (PS.singleton dId ps) trie

lookup :: T.Text -> InMemoryIndex -> Maybe PS.Postings
lookup w (IMI trie) = TR.lookup w trie

wildcard :: T.Text -> InMemoryIndex -> Maybe PS.Postings
wildcard t imi@(IMI trie) = TR.lookupByPrefix t trie >>= flip Index.or imi

or :: [T.Text] -> InMemoryIndex -> Maybe PS.Postings
or ts imi = foldl1' union' postings
            where postings = forM ts Index.lookup imi
                  union' Nothing mps2 = mps2
                  union' mps1 Nothing = mps1
                  union' (Just ps1) (Just ps2) = Just $ PS.union ps1 ps2

