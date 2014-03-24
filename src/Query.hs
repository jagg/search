{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Query
( showParsedResult
, runComplexQParser
, runSimpleQParser
, Query
, qAnd
, qOr
, qNot
, qExp
, showQuery
, Query'(..)
, QTerm (..)
)
where

import           Control.Applicative
import           Control.Monad.Free
import           Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as B


data Query' t next =
    Exp t
  | Wild t
  | Not next
  | And next next
  | Or  next next
  deriving (Show)

instance Functor (Query' t) where
    fmap _ (Exp t) = Exp t
    fmap f (And next next') = And (f next) (f next')
    fmap f (Or next next') = Or (f next) (f next')

type Query a = Free (Query' a) ()

-- Toy implementation of the Free Monad
{--
data Free f r = Free (f (Free f r)) | Pure r

instance (Functor f) => Monad (Free f) where
    return = Pure
    (Free x) >>= f = Free (fmap (>>= f) x)
    (Pure r) >>= f = f r

liftF :: (Functor f) => f r -> Free f r
liftF command = Free (fmap Pure command)

--}
qAnd :: Query a -> Query a -> Query a
qAnd e1 e2 = Free $ And e1 e2


qOr :: Query a -> Query a -> Query a
qOr e1 e2 = Free $ Or e1 e2


qExp :: a -> Query a
qExp x = Free (Exp x)

qWild :: a -> Query a
qWild x = Free (Wild x)


qNot :: Query a -> Query a
qNot x = Free (Not x)

showQuery :: (Show a) => Query a -> String
showQuery (Free (And e1 e2)) = "(" ++ showQuery e1 ++ " AND " ++ showQuery e2 ++ ")"
showQuery (Free (Or e1 e2)) = "(" ++ showQuery e1 ++ " OR " ++ showQuery e2 ++ ")"
showQuery (Free (Exp x)) = show x
showQuery (Free (Wild x)) = show x ++ "*"
showQuery (Free (Not e1)) = "(NOT " ++ showQuery e1 ++ ")"
showQuery (Pure ()) = ""


type Field = B.ByteString
type Value = B.ByteString

data QTerm = QT Field Value

instance Show QTerm where
    show (QT f v) = "[" ++ B.unpack f ++ ":" ++ B.unpack v ++ "]"

-- Query parser

spaces :: Parser ()
spaces = skipWhile (== ' ')


-- Should we use ANDs instead?
simpleParser :: Parser (Query QTerm)
simpleParser = orify <$> many1 parseTerm
               where orify = foldl1 qOr

parseQuery :: Parser (Query QTerm)
parseQuery = try parseAnd <|> try parseOr <|> parseNot <|> parseTerm

parseTerm :: Parser (Query QTerm)
parseTerm = try parseSpecificTerm <|> parseGenericTerm

parseGenericTerm :: Parser (Query QTerm)
parseGenericTerm = do
                    spaces
                    value <- takeWhile1 isNotDelimiter
                    return $ if not (B.elem '*' value) then qExp $ QT "default" value
                             else qWild $ QT "default" (B.take ((B.length value) - 1) value)
                   where isNotDelimiter x = x `notElem`  " :)("

parseSpecificTerm :: Parser (Query QTerm)
parseSpecificTerm = do
                      spaces
                      field <- takeWhile1 isNotDelimiter
                      spaces
                      char ':'
                      spaces
                      value <- takeWhile1 isNotDelimiter
                      return $ if not (B.elem '*' value) then qExp $ QT field value
                               else qWild $ QT field (B.take ((B.length value) - 1) value)
                    where isNotDelimiter x = x `notElem` " :)("

parseBlock :: Parser (Query QTerm)
parseBlock = do
               char '('
               spaces
               x <- parseQuery
               spaces
               char ')'
               spaces
               return x

parseNot :: Parser (Query QTerm)
parseNot = do
             string "NOT"
             skipWhile (== ' ')
             term <- parseBlock <|> parseTerm
             return $ qNot term



parseAnd :: Parser (Query QTerm)
parseAnd = do
             term1 <- parseBlock <|> parseTerm
             skipWhile (== ' ')
             string "AND"
             skipWhile (== ' ')
             term2 <- parseBlock <|> parseTerm
             return $ qAnd term1 term2


parseOr :: Parser (Query QTerm)
parseOr = do
             term1 <- parseBlock <|> parseTerm
             skipWhile (== ' ')
             string "OR"
             skipWhile (== ' ')
             term2 <- parseBlock <|> parseTerm
             return $ qOr term1 term2

runComplexQParser :: B.ByteString -> Either String (Query QTerm)
runComplexQParser = parseOnly parseQuery

runSimpleQParser :: B.ByteString -> Either String (Query QTerm)
runSimpleQParser = parseOnly simpleParser

type QueryParser = B.ByteString -> Either String (Query QTerm)

showParsedResult :: QueryParser -> B.ByteString -> String
showParsedResult pf bs = case result of
                           Right q  -> showQuery q
                           Left err -> err
                          where result = pf bs


