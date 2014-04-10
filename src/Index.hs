module Index
( InMemoryIndex
, empty
, addTerm
, wildcard
, Index.lookup
) where

import           Control.Monad   (forM)
import           Data.List       (foldl1')

import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Postings        as PS
import qualified Trie            as TR

type FieldName = T.Text

newtype InMemoryIndex = IMI { getIMI :: M.Map FieldName (TR.Trie PS.Postings) } deriving (Show)

empty :: InMemoryIndex
empty = IMI M.empty

addTerm :: FieldName -> T.Text -> PS.DocId -> [Int] -> InMemoryIndex -> InMemoryIndex
addTerm fname t dId ps (IMI tmap) = IMI $ nmap
                                    where mtrie = M.lookup fname tmap
                                          nmap = case mtrie of
                                                  Nothing   -> insertTrie TR.empty
                                                  Just trie -> insertTrie trie
                                          insertTrie trie = M.insert fname ntrie tmap
                                                            where ntrie = TR.insertWith PS.union t
                                                                             (PS.singleton dId ps)
                                                                             trie

lookup :: FieldName -> T.Text -> InMemoryIndex -> Maybe PS.Postings
lookup fname w (IMI tmap) = case mtrie of
                              Just trie -> TR.lookup w trie
                              Nothing   -> Nothing
                            where mtrie = M.lookup fname tmap


wildcard :: FieldName -> T.Text -> InMemoryIndex -> Maybe PS.Postings
wildcard fname t imi@(IMI tmap) = case mtrie of
                                   Just trie -> TR.lookupByPrefix t trie >>= (\ts -> Index.or fname ts imi)
                                   Nothing   -> Nothing
                                 where mtrie = M.lookup fname tmap

or :: FieldName -> [T.Text] -> InMemoryIndex -> Maybe PS.Postings
or fname ts imi = foldl1' union' postings
                  where postings = forM ts (Index.lookup fname) imi
                        union' Nothing mps2 = mps2
                        union' mps1 Nothing = mps1
                        union' (Just ps1) (Just ps2) = Just $ PS.union ps1 ps2

