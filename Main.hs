import Text.ParserCombinators.Parsec
import Language.Java.StaticAnalyzer.ExpressionParser

main :: IO ()
main = do line <- getLine
          putStrLn $ parseClassDef line

parseClassDef :: String -> String
parseClassDef line = case parse classDef "java" line of
  Left e     -> show e
  Right tree -> show tree
