module Evaluator where

import Types

import Control.Monad.Except

eval :: FutureVal -> Result FutureVal
eval val@(Char _) = return val
eval val@(String _) = return val
eval val@(Bool _) = return val
eval val@(Integer _) = return val
eval val@(Float _) = return val
eval val@(Ratio _) = return val
eval (List [val]) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [FutureVal] -> Result FutureVal
apply func args = maybe
  (throwError $ NotFunction "Unrecognized primitive function args" func)
  ($ args) (lookup func primitives)

primitives :: [(String, [FutureVal] -> Result FutureVal)]
primitives = [ ("+", numBinop (+))
             , ("-", numBinop (-))
             , ("*", numBinop (*))
             , ("/", numBinop (/))
             , ("mod", numBinop mod)
             , ("quot", numBinop quot)
             , ("rem", numBinop rem)
             ]

numBinop :: (FutureVal -> FutureVal -> FutureVal) -> [FutureVal] -> Result FutureVal
numBinop op [] = throwError $ NumArgs 2 []
numBinop op val@[_] = throwError $ NumArgs 2 val
numBinop op params@(x:xs) = case foldM numBinopTypeCheck x xs of
                           err@(Left _) -> err
                           Right _ -> (return . foldl1 op) params

numBinopTypeCheck :: FutureVal -> FutureVal -> Result FutureVal
numBinopTypeCheck (Integer _) succ@(Integer _) = return succ
numBinopTypeCheck (Float _) succ@(Float _) = return succ
numBinopTypeCheck (Ratio _) succ@(Ratio _) = return succ
numBinopTypeCheck a@(Integer _) b@(_) = throwError $ TypeError a b
numBinopTypeCheck a@(Float _) b@(_) = throwError $ TypeError a b
numBinopTypeCheck a@(Ratio _) b@(_) = throwError $ TypeError a b
numBinopTypeCheck a@_ _ = throwError $ TypeError (Integer 0) a
