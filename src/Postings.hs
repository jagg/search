{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Postings
( empty,
  toList,
  fromList,
  insert,
  union,
  intersection,
  separatedBy,
  singleton,
  Postings,
  DocId,
  Occurrence
  ) where

import qualified Data.IntMap.Strict as IM
import qualified Data.List.Ordered  as OL
import           Data.Monoid


data Occurrence = SingleO Int
                | LinkO Int Int
                deriving (Show, Eq)

instance Ord Occurrence where
    compare o1 o2 = compare (infLimit o1) (infLimit o2)


occFromList :: [Int] -> OcList
occFromList xs = OCL $ map SingleO xs

supLimit :: Occurrence -> Int
supLimit o = case o of
                SingleO x -> x
                LinkO _ y -> y

infLimit :: Occurrence -> Int
infLimit o = case o of
                SingleO x -> x
                LinkO x _ -> x

data OcList = OCL [Occurrence]
              deriving (Show, Eq)

instance Monoid OcList where
    mempty = emptyOL
    mappend = keepAdjacentOL 1

emptyOL :: OcList
emptyOL = OCL []

-- Generates the intersection of both OccurenceLists, considering two terms are a LinkO if
-- they are at least 'limit' spaces apart. The intersection of two occurence lists only
-- keeps Links.
keepAdjacentOL :: Int -> OcList -> OcList -> OcList
keepAdjacentOL limit (OCL oxs) (OCL oys) = OCL $ reverse $ goRec [] oxs oys
                           where goRec acc [] _ = acc
                                 goRec acc _ [] = acc
                                 goRec acc xx@(x:xs) yy@(y:ys)
                                            | infLimit y - infLimit x <= limit = goRec (LinkO (infLimit x) (supLimit y) : acc) xs ys
                                            | infLimit y < infLimit x = goRec acc xx ys
                                            | infLimit x < infLimit y = goRec acc xs yy

-- This is not checking distances so it should just join the Ocurrence lists keeping the order
unionOL :: OcList -> OcList -> OcList
unionOL (OCL oxs) (OCL oys) = OCL $ reverse $ goRec [] oxs oys
                           where goRec acc [] ys = ys ++ acc
                                 goRec acc xs [] = xs ++ acc
                                 goRec acc xx@(x:xs) yy@(y:ys)
                                            | infLimit y <= infLimit x = goRec (y : acc) xx ys
                                            | infLimit x <  infLimit y = goRec (x : acc) xs yy


type DocId = Int

-- Represents the documents in which a token (or tokens if there is more than one list
-- of postings) appears and the positions in which it appears
data Postings = PS (IM.IntMap OcList)
                deriving (Show)

empty :: Postings
empty = PS IM.empty

singleton :: DocId -> [Int] -> Postings
singleton dId ps = PS $ IM.singleton dId $ occFromList (OL.sort ps)

toList :: Postings -> [(DocId, OcList)]
toList (PS im) = IM.toList im

fromList :: [(DocId, OcList)] -> Postings
fromList vs = PS $ IM.fromList vs

insert :: DocId -> OcList -> Postings -> Postings
insert dId ocl (PS im) = PS $ IM.insert dId ocl im

union :: Postings -> Postings -> Postings
union (PS im1) (PS im2) = PS $ IM.unionWith unionOL im1 im2

intersection :: Postings -> Postings -> Postings
intersection (PS im1) (PS im2) = PS $ IM.intersectionWith unionOL im1 im2

separatedBy :: Int -> Postings -> Postings -> Postings
separatedBy limit (PS im1) (PS im2) = PS $ IM.intersectionWith (keepAdjacentOL limit) im1 im2

