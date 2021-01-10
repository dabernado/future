module Main where

import Parser (readExpr)
import Evaluator (eval)
import Types (trapError, extractValue)

import Control.Monad
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
