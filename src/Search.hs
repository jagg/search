module Search
( search
, searchComplex
, searchProxim
) where


import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T
import qualified Postings              as PS

import           Control.Monad.Free
import           Data.Maybe            (fromMaybe)
import           Data.Text.Encoding    (decodeUtf8)

import           Index
import           Query

testIndex :: InMemoryIndex
testIndex = addTerm (T.pack "Name")
                    (T.pack "teresa") 1 [0,2]
                    (addTerm (T.pack "Name")
                             (T.pack "tony") 1 [1]
                             (addTerm (T.pack "Name")
                                      (T.pack "casa") 1 [3] empty))

toText :: B.ByteString -> T.Text
toText = decodeUtf8


--------------------------------------------------------------------------------
-- Query interpreters
--------------------------------------------------------------------------------

runQuery :: Query QTerm -> InMemoryIndex -> PS.Postings
runQuery (Free (And e1 e2)) imi = PS.intersection (runQuery e1 imi) (runQuery e2 imi)
runQuery (Free (Or e1 e2)) imi = PS.union (runQuery e1 imi) (runQuery e2 imi)
runQuery (Free (Exp (QT fname x))) imi = fromMaybe PS.empty (Index.lookup (toText fname) (toText x) imi)
runQuery (Free (Wild (QT fname x))) imi = fromMaybe PS.empty (wildcard (toText fname) (toText x) imi)
runQuery (Free (Not e1)) imi = runQuery e1 imi
runQuery (Pure  ()) _ = PS.empty

runProximityQuery :: Int -> Query QTerm -> InMemoryIndex -> PS.Postings
runProximityQuery n (Free (And e1 e2)) imi = PS.separatedBy n (runProximityQuery n e1 imi)
                                                              (runProximityQuery n e2 imi)
runProximityQuery n (Free (Or e1 e2)) imi = PS.union (runProximityQuery n e1 imi)
                                                     (runProximityQuery n e2 imi)
runProximityQuery _ (Free (Exp (QT fname x))) imi = fromMaybe PS.empty (Index.lookup (toText fname) (toText x) imi)
runProximityQuery _ (Free (Wild (QT fname x))) imi = fromMaybe PS.empty (wildcard (toText fname) (toText x) imi)
runProximityQuery n (Free (Not e1)) imi = runProximityQuery n e1 imi
runProximityQuery _ (Pure  ()) _ = PS.empty

--------------------------------------------------------------------------------
-- Search API
--------------------------------------------------------------------------------

search :: InMemoryIndex -> B.ByteString -> B.ByteString -> Either String PS.Postings
search imi defField bs = runSimpleQParser defField bs >>= (\q -> return $ runQuery q imi)

searchComplex :: InMemoryIndex -> B.ByteString -> B.ByteString -> Either String PS.Postings
searchComplex imi defField bs = runComplexQParser defField bs >>= (\q -> return $ runQuery q imi)

searchProxim :: Int -> InMemoryIndex -> B.ByteString -> B.ByteString -> Either String PS.Postings
searchProxim d imi defField bs = runSimpleQParser defField bs >>= (\q -> return $ runProximityQuery d q imi)


