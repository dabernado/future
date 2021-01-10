module Parser where

import Types

import Data.Ratio ((%))
import qualified Data.Vector as Vector
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "*+!/:-_?=<>&|"

spaces :: Parser ()
spaces = skipMany (char ',') >> skipMany1 space

parseList :: Parser FutureVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser FutureVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

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
    return $ Vector $ Vector.fromList xs

parseSet :: Parser FutureVal
parseSet = do
    char '#'
    char '{'
    xs <- sepBy parseExpr spaces
    char '}'
    return $ List [Atom ":Set", List xs]

parseMap :: Parser FutureVal
parseMap = do
    char '{'
    xs <- sepBy parseKV spaces
    char '}'
    return $ List [Atom ":Map", List (concat xs)]
  where
    parseKV = do
        key <- parseExpr
        spaces
        value <- parseExpr
        return $ [key, value]

parseQuoted :: Parser FutureVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser FutureVal
parseQuasiQuoted = do
    char '`'
    x <- parseQQExpr
    return $ List [Atom "quasiquote", x]
  where
    parseTilde = do
        char '~'
        next <- optionMaybe parseAtSymbol
        case next of
          Just xs -> return xs
          Nothing -> do
            x <- parseQQExpr
            return $ List [Atom "unquote", x]
    parseAtSymbol = do
        char '@'
        x <- parseAnyQQList
        return $ List [Atom "unquote-splicing", x]
    parseQQList = liftM List $ sepBy parseQQExpr spaces
    parseQQDottedList = do
        head <- endBy parseQQExpr spaces
        tail <- char '.' >> spaces >> parseQQExpr
        return $ DottedList head tail
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

readExpr :: String -> Result FutureVal
readExpr input = case parse parseExpr "future" input of
    Left err -> throwError $ Parser err
    Right val -> return val
