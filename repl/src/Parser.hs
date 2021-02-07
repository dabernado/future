module Parser where

import Types

import Data.List (nub)
import Data.Ratio ((%))
import qualified Data.Vector as Vector
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "*+!/:-_?=&<>|"

spaces :: Parser ()
spaces = skipMany (char ',') >> skipMany1 space

parseComment :: Parser FutureVal
parseComment = do
  char ';'
  return $ List AnyT []

parseList :: Parser FutureVal
parseList = liftM (List AnyT) $ sepBy parseExpr spaces

parseDottedList :: Parser FutureVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList (AnyT, AnyT) head tail

parseAnyList :: Parser FutureVal
parseAnyList = do
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x

parseVector :: Parser FutureVal
parseVector = do
    char '['
    xs <- sepBy parseExpr spaces
    char ']'
    return $ Vector AnyT $ Vector.fromList xs

parseMap :: Parser FutureVal
parseMap = do
    char '{'
    xs <- sepBy parseKV spaces
    char '}'
    return $ List AnyT [Atom ":Map", List AnyT (concat xs)]
  where
    parseKV = do
        key <- parseExpr
        spaces
        value <- parseExpr
        return $ [key, value]

parsePound :: Parser FutureVal
parsePound = do
    char '#'
    x <- try parseSet <|> parseAnonFunc
    return x

parseSet :: Parser FutureVal
parseSet = do
    char '{'
    xs <- sepBy parseExpr spaces
    char '}'
    return $ List AnyT [Atom ":Set", List AnyT xs]

parseAnonFunc :: Parser FutureVal
parseAnonFunc = do
    char '('
    xs <- sepBy (parseExpr <|> parseAnonArg) spaces
    char ')'
    let args = sortArgs $ nub (filter isAnonArg xs)
    return $ if last args == (Atom "%&")
                then List AnyT (Atom "fn"
                               : DottedList (AnyT, AnyT) (init args) (last args)
                               : [List AnyT xs])
                else List AnyT (Atom "fn" : List AnyT args : [List AnyT xs])
  where
    parseAnonArg = do
      char '%'
      num <- many1 digit <|> (char '&' >>= singleton)
      return $ Atom ('%':num)
    singleton x = return [x]
    sortArgs [] = []
    sortArgs (x:xs) = sortArgs [y | y <- xs, ordArgs y x]
                      ++ [x]
                      ++ sortArgs [y | y <- xs, not (ordArgs y x)]
    ordArgs (Atom ('%':a)) (Atom ('%':b)) =
        if a == "&"
        then False
        else if b == "&" then True else a < b
    isAnonArg (Atom ('%':_)) = True
    isAnonArg _ = False
    isVarArg (Atom "%&") = True
    isVarArg _ = False

parseQuoted :: Parser FutureVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List AnyT [Atom "quote", x]

parseQuasiQuoted :: Parser FutureVal
parseQuasiQuoted = do
    char '`'
    x <- parseQQExpr
    return $ List AnyT [Atom "quasiquote", x]
  where
    parseTilde = do
        char '~'
        next <- optionMaybe parseAtSymbol
        case next of
          Just xs -> return xs
          Nothing -> do
            x <- parseQQExpr
            return $ List AnyT [Atom "unquote", x]
    parseAtSymbol = do
        char '@'
        x <- parseAnyQQList
        return $ List AnyT [Atom "unquote-splicing", x]
    parseQQList = liftM (List AnyT) $ sepBy parseQQExpr spaces
    parseQQDottedList = do
        head <- endBy parseQQExpr spaces
        tail <- char '.' >> spaces >> parseQQExpr
        return $ DottedList (AnyT, AnyT) head tail
    parseAnyQQList = do
        char '('
        x <- try parseQQList <|> parseQQDottedList
        char ')'
        return x
    parseQQExpr = parseAtom
               <|> parseChar
               <|> parseString
               <|> parseNumber
               <|> parseTilde
               <|> parseQuoted
               <|> parseQuasiQuoted
               <|> parseVector
               <|> parseSet
               <|> parseMap
               <|> parseAnyQQList
               <|> parsePound
               <|> parseComment

parseChar :: Parser FutureVal
parseChar = do
    char '\\'
    x <- letter
    return $ Char x

parseString :: Parser FutureVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseNumber :: Parser FutureVal
parseNumber = do
    whole <- many1 digit
    symbol <- optionMaybe $ oneOf "./"
    case symbol of
      Just '.' -> do
          part <- many1 digit
          return $ (Float . read) $ whole ++ "." ++ part
      Just '/' -> do
          den <- many1 digit
          return $ Ratio $ (read whole) % (read den)
      Nothing -> return $ (Integer . read) whole

parseAtom :: Parser FutureVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
               "true" -> Bool True
               "false" -> Bool False
               _ -> Atom atom

parseExpr :: Parser FutureVal
parseExpr = parseAtom
        <|> parseChar
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> parseQuasiQuoted
        <|> parseVector
        <|> parseSet
        <|> parseMap
        <|> parseAnyList
        <|> parsePound
        <|> parseComment

readExpr :: String -> Result FutureVal
readExpr input = case parse parseExpr "future" input of
    Left err -> throwError $ Parser err
    Right val -> return val
