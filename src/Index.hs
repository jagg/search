module Index where


import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T
import qualified Postings              as PS
import qualified Trie                  as TR

import           Control.Monad         (forM)
import           Control.Monad.Free
import           Data.List             (foldl1')
import           Data.Maybe            (fromMaybe)
import           Data.Text.Encoding    (decodeUtf8)
import           Data.Traversable      (traverse)
import           Query


newtype InMemoryIndex = IMI { getIMI :: TR.Trie PS.Postings } deriving (Show)

empty :: InMemoryIndex
empty = IMI TR.empty

addTerm :: T.Text -> PS.DocId -> [Int] -> InMemoryIndex -> InMemoryIndex
addTerm t dId ps (IMI trie) = IMI $ TR.insertWith PS.union t (PS.singleton dId ps) trie

lookup :: T.Text -> InMemoryIndex -> Maybe PS.Postings
lookup w (IMI trie) = TR.lookup w trie

-- TODO separatedBy leaves empty lists when there is no matches,
-- replace that with Nothing and shortcut the calculation with the
-- Maybe Monad when that happens
distanceQ :: Int -> [T.Text] -> InMemoryIndex -> Maybe PS.Postings
distanceQ d ts imi = fmap (foldl1' (PS.separatedBy d)) postings
                     where postings = traverse (flip Index.lookup imi) ts

and :: [T.Text] -> InMemoryIndex -> Maybe PS.Postings
and ts imi = fmap (foldl1' PS.intersection) postings
              where postings = traverse (flip Index.lookup imi) ts

or :: [T.Text] -> InMemoryIndex -> Maybe PS.Postings
or ts imi = foldl1' union' postings
            where postings = forM ts Index.lookup imi
                  union' Nothing mps2 = mps2
                  union' mps1 Nothing = mps1
                  union' (Just ps1) (Just ps2) = Just $ PS.union ps1 ps2

andD :: [T.Text] -> InMemoryIndex -> Maybe PS.Postings
andD = distanceQ 1


wildcard :: T.Text -> InMemoryIndex -> Maybe PS.Postings
wildcard t imi@(IMI trie) = TR.lookupByPrefix t trie >>= flip Index.or imi

testIndex :: InMemoryIndex
testIndex = addTerm (T.pack "teresa") 1 [0,2]
                    (addTerm (T.pack "tony") 1 [1]
                             (addTerm (T.pack "casa") 1 [3] empty))

runQuery :: Query QTerm -> InMemoryIndex -> PS.Postings
runQuery (Free (And e1 e2)) imi = PS.intersection (runQuery e1 imi) (runQuery e2 imi)
runQuery (Free (Or e1 e2)) imi = PS.union (runQuery e1 imi) (runQuery e2 imi)
runQuery (Free (Exp (QT _ x))) imi = fromMaybe PS.empty (Index.lookup (toText x) imi)
runQuery (Free (Wild (QT _ x))) imi = fromMaybe PS.empty (wildcard (toText x) imi)
runQuery (Free (Not e1)) imi = runQuery e1 imi
runQuery (Pure  ()) _ = PS.empty

runProximityQuery :: Int -> Query QTerm -> InMemoryIndex -> PS.Postings
runProximityQuery n (Free (And e1 e2)) imi = PS.separatedBy n (runProximityQuery n e1 imi)
                                                              (runProximityQuery n e2 imi)
runProximityQuery n (Free (Or e1 e2)) imi = PS.union (runProximityQuery n e1 imi)
                                                     (runProximityQuery n e2 imi)
runProximityQuery _ (Free (Exp (QT _ x))) imi = fromMaybe PS.empty (Index.lookup (toText x) imi)
runProximityQuery _ (Free (Wild (QT _ x))) imi = fromMaybe PS.empty (wildcard (toText x) imi)
runProximityQuery n (Free (Not e1)) imi = runProximityQuery n e1 imi
runProximityQuery _ (Pure  ()) _ = PS.empty


search :: InMemoryIndex -> B.ByteString -> Either String PS.Postings
search imi bs = runSimpleQParser bs >>= (\q -> return $ runQuery q imi)

searchComplex :: InMemoryIndex -> B.ByteString -> Either String PS.Postings
searchComplex imi bs = runComplexQParser bs >>= (\q -> return $ runQuery q imi)

searchProxim :: Int -> InMemoryIndex -> B.ByteString -> Either String PS.Postings
searchProxim d imi bs = runSimpleQParser bs >>= (\q -> return $ runProximityQuery d q imi)

toText :: B.ByteString -> T.Text
toText = decodeUtf8

