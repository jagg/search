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

import qualified Data.List.Ordered as OL
import qualified Data.IntMap.Strict as IM
import Data.Monoid


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
    mappend = intersectOL 1

emptyOL :: OcList
emptyOL = OCL []

intersectOL :: Int -> OcList -> OcList -> OcList
intersectOL limit (OCL oxs) (OCL oys) = OCL $ reverse $ goRec [] oxs oys 
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
separatedBy limit (PS im1) (PS im2) = PS $ IM.intersectionWith (intersectOL limit) im1 im2 

{--


-- 1. Instead of using (++) we can take the consecutive elements between both list so we don't need to take them all!
-- then a Posting would be (IM.IntMap [Occurrence]) and it will be combination of several terms, we would have to keep
-- the number of terms represented by a Posting as an attribute, since we need to know how many elements we have
-- to match to make a sequennce for the AND queries
-- 2. or Should we do:
-- newtype Posting = (IM.IntMap [Occurrence])
-- newtype Result = (IM.IntMap [[Occurrence]])
-- Result -> Result -> Result ??

intersection :: Postings -> Postings -> Postings
intersection (PS im1) (PS im2) = PS $ IM.intersectionWith (++) im1 im2 

-- for 1
-- This example will create problems:
-- keepConsec 1 [] [1,3,7,8] [2,4,5]
-- >> [3,4,1,2]
-- since we will have a long group we won't know that we only matching 2 terms, we will have to reduce it later
keepConsec :: Int -> [Occurrence] -> [Occurrence] -> [Occurrence] -> [Occurrence]
keepConsec _ acc [] _ = acc
keepConsec _ acc _ [] = acc
keepConsec d acc lx@(x:_) ly@(y:_) = if (x < y) then keepC lx ly else keepC ly lx 
                                       where keepC (x':xs') l@(y':ys')  
                                                  | (y' - x') <= d  = keepConsec d (x':y':acc) xs' ys'
                                                  | otherwise     = keepConsec d acc xs' l

-- Returns the DocIds of the documents whose tokens aren't farther than d
separatedBy :: Int -> Postings -> Postings
separatedBy d ps = fromList $ filter check (toList ps)
                   where check (dId, pList) = case pList of
                                                (p1:p2:pps) -> if closeEnough d True p1 p2 then check (dId, (p2:pps)) else False
                                                [_]        -> True
                                                []          -> True

closeEnough :: Int -> Bool -> [Occurrence] -> [Occurrence] -> Bool
closeEnough _ _ [] _ = False
closeEnough _ _ _ [] = False
closeEnough d ordMatters (x:xs) (y:ys) 
    | check x y        = True
    | x < y            = closeEnough d ordMatters xs (y:ys)
    | otherwise        = closeEnough d ordMatters (x:xs) ys
    where check e1 e2 = if ordMatters then  (e1 - e2) <= d && e1 < e2 else abs (e1 - e2) <= d
--}
