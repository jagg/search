module Main where

import Test.QuickCheck
import Trie
import qualified Data.Text as T


import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen


instance Arbitrary T.Text where
  arbitrary = arbitrary >>= return . T.pack
              
instance Arbitrary a => Arbitrary (Trie a) where
  arbitrary = do { wvs <- arbitrary
                 ; return $ ingest empty wvs
                 }  



-- It holds only if the word have the same value
prop_conmutative w1 w2 = both w1 w2 == both w2 w1
                         where both a b = union (createTrie (T.pack a) 1) (createTrie (T.pack b) 1)

-- It doesn't hold
prop_conmutative_union t1 t2 = union t1 t2 == union t2 t1

prop_empty_union t1 = (union t1 empty == t1) && (union empty t1 == t1)
                         
prop_idempotent_union trie1 trie2 = union (union trie1 trie2) trie2 == union trie1 trie2


prop_idempotent_insert w1 trie = insert t1 1 trie == insert t1 1 (insert t1 1 trie)
                                 where t1 = T.pack w1

prop_container w1 v1 trie = Trie.lookup t1 ntrie == Just v1
                            where t1 = T.pack w1
                                  ntrie = (insert t1 v1 trie)

main :: IO ()
main = do
  putStrLn "Testing simple conditions"
  quickCheck (prop_idempotent_union :: Trie Int -> Trie Int -> Bool)
  quickCheck (prop_empty_union :: Trie Int -> Bool)
  quickCheck (prop_idempotent_insert :: String -> Trie Int -> Bool)
  quickCheck (prop_container :: String -> Int -> Trie Int -> Bool)

