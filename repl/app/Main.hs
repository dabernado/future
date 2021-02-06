module Main where

import Parser (readExpr)
import Evaluator (eval)
import Primitives (primOps, primTypes)
import Types (
    Env
  , FutureVal(..)
  , bindVars
  , trapError
  , extractValue
  , liftResult
  , nullEnv
  , runIOResult
  )

import Control.Monad
import System.Environment
import System.IO

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOResult $ liftM show $ (liftResult $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action

primitiveBindings :: IO Env
primitiveBindings = nullEnv
                >>= (flip bindVars $ map makePrimitive primOps)
                >>= (flip bindVars primTypes)
    where makePrimitive (var, func) = (var, Primitive func)

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Future>>> ") . evalAndPrint

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne $ args !! 0
    _ -> putStrLn "Program takes only 0 or 1 arguments"
