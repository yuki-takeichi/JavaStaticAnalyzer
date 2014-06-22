module Language.Java.StaticAnalyzer.ExpressionParser (
classDef
) where

import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Maybe

data JavaAST = ClassDefinition AccessLevel [MemberVar] [MemberMethod]
             | Expression
               deriving (Show)

data MemberVar = MemberVar deriving (Show)
data MemberMethod = MemberMethod deriving (Show)
data AccessLevel = Public | Protected | Private deriving (Show)

classDef :: Parser JavaAST
classDef = do classAcccessLevel <- (liftM (fromMaybe Private)) accessLevel
              optional space
              string "class"
              optional space
              char '{'
              --exprs <- between (char '{') (char '}') $ (many1 digit) `sepEndBy` (char ';')
              char '}'
              return $ ClassDefinition classAcccessLevel [] []

accessLevel :: Parser (Maybe AccessLevel)
accessLevel = liftM (liftM makeAccessLevel) $ optionMaybe $ choice [string "public", string "protected", string "private"]
  where
    makeAccessLevel "public" = Public
    makeAccessLevel "protected" = Protected
    makeAccessLevel "private" = Private
    makeAccessLevel _ = error "never reach here"
