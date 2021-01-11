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
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    Bool True -> eval conseq
    _ -> throwError $ TypeError (Bool True) result
eval form@(List (Atom "cond" : clauses)) =
  if null clauses
  then throwError $ BadSpecialForm "no clause in cond expression: " form
  else case head clauses of
         List [Atom "else", val] -> eval val
         List [test, expr] -> eval $ List [Atom "if"
                                       , test
                                       , expr
                                       , List (Atom "cond" : tail clauses)]
         _ -> throwError $ BadSpecialForm "ill-formed cond expression: " form
eval form@(List (Atom "case" : key : clauses)) =
  if null clauses
  then throwError $ BadSpecialForm "no clause in case expression: " form
  else case head clauses of
         List (Atom "else" : exprs) -> mapM eval exprs >>= return . last
         List (List datums : exprs) -> do
           result <- eval key
           equality <- mapM (\x -> eval (List [Atom "=", x, result])) datums
           if Bool True `elem` equality
              then mapM eval exprs >>= return . last
              else eval $ List (Atom "case" : key : tail clauses)
         _ -> throwError $ BadSpecialForm "ill-formed case expression: " form
eval (List [Atom "quote", val]) = return val
eval (List [Atom "quasiquote", List val]) = mapM evalQQ val >>= (return . List)
eval (List [Atom "quasiquote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalQQ :: FutureVal -> Result FutureVal
evalQQ val@(Char _) = return val
evalQQ val@(String _) = return val
evalQQ val@(Bool _) = return val
evalQQ val@(Integer _) = return val
evalQQ val@(Float _) = return val
evalQQ val@(Ratio _) = return val
evalQQ (List [Atom "unquote", val]) = eval val
--evalQQ (List [Atom "unquote-splicing", List val]) = eval val
evalQQ val@(List _) = return val

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
             , ("=", genBinop eq)
             , ("/=", genBinop notEq)
             , ("<", genBinop lt)
             , (">", genBinop gt)
             , ("<=", genBinop ltEq)
             , (">=", genBinop gtEq)
             , ("&&", boolBinop andF)
             , ("||", boolBinop orF)
             , ("++", strBinop concatF)
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             , ("symbol?", isSymbol)
             , ("symbol->string", symToString)
             , ("string->symbol", strToSymbol)
             , ("string", makeString)
             , ("string-length", strLength)
             , ("string-ref", indexString)
             ]
  where
    numBinop = binop numBinopTypeCheck
    genBinop = binop genBinopTypeCheck
    boolBinop = binop boolBinopTypeCheck
    strBinop = binop strBinopTypeCheck
    eq a b = Bool $ a == b
    notEq a b = Bool $ a /= b
    lt a b = Bool $ a < b
    gt a b = Bool $ a > b
    ltEq a b = Bool $ a <= b
    gtEq a b = Bool $ a >= b
    andF (Bool a) (Bool b) = Bool $ a && b
    orF (Bool a) (Bool b) = Bool $ a || b
    concatF (String a) (String b) = String $ a ++ b

car :: [FutureVal] -> Result FutureVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [arg] = throwError $ TypeError (List []) arg
car args = throwError $ NumArgs 1 args

cdr :: [FutureVal] -> Result FutureVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [arg] = throwError $ TypeError (List []) arg 
cdr args = throwError $ NumArgs 1 args

cons :: [FutureVal] -> Result FutureVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x, y] = return $ DottedList [x] y
cons args = throwError $ NumArgs 2 args

isSymbol :: [FutureVal] -> Result FutureVal
isSymbol [] = throwError $ NumArgs 1 []
isSymbol [Atom _] = return $ Bool True
isSymbol [_] = return $ Bool False
isSymbol n = throwError $ NumArgs 1 n

symToString :: [FutureVal] -> Result FutureVal
symToString [] = throwError $ NumArgs 1 []
symToString [Atom a] = return $ String a
symToString [t] = throwError $ TypeError (Atom "") t
symToString n = throwError $ NumArgs 1 n

strToSymbol :: [FutureVal] -> Result FutureVal
strToSymbol [] = throwError $ NumArgs 1 []
strToSymbol [String s] = return $ Atom s
strToSymbol [t] = throwError $ TypeError (String "") t
strToSymbol n = throwError $ NumArgs 1 n

makeString :: [FutureVal] -> Result FutureVal
makeString cs = case charTypeCheck cs of
                  err@(Left _) -> err
                  Right _ -> return $ String (map unpackChar cs)
  where
    unpackChar (Char c) = c
    charTypeCheck :: [FutureVal] -> Result FutureVal
    charTypeCheck (Char _ : cs) = charTypeCheck cs
    charTypeCheck [] = return $ Bool True
    charTypeCheck (x:xs) = throwError $ TypeError (Char ' ') x

strLength :: [FutureVal] -> Result FutureVal
strLength [] = throwError $ NumArgs 1 []
strLength [String s] = return $ Integer (length s)
strLength [t] = throwError $ TypeError (String "") t
strLength n = throwError $ NumArgs 1 n

indexString :: [FutureVal] -> Result FutureVal
indexString [] = throwError $ NumArgs 1 []
indexString [Integer n, String s] = return $ Char (s !! n)
indexString [a@(Integer _), b] = throwError $ TypeError a b
indexString [a, _] = throwError $ TypeError (Integer 0) a
indexString n = throwError $ NumArgs 1 n

binop :: (FutureVal -> FutureVal -> Result FutureVal) -> (FutureVal -> FutureVal -> FutureVal) -> [FutureVal] -> Result FutureVal
binop typeCheck op [] = throwError $ NumArgs 2 []
binop typeCheck op val@[_] = throwError $ NumArgs 2 val
binop typeCheck op params@(x:xs) = case foldM typeCheck x xs of
                           err@(Left _) -> err
                           Right _ -> (return . foldl1 op) params

genBinopTypeCheck :: FutureVal -> FutureVal -> Result FutureVal
genBinopTypeCheck a b = if (showType a) == (showType b)
                        then return b
                        else throwError $ TypeError a b

numBinopTypeCheck :: FutureVal -> FutureVal -> Result FutureVal
numBinopTypeCheck (Integer _) succ@(Integer _) = return succ
numBinopTypeCheck (Float _) succ@(Float _) = return succ
numBinopTypeCheck (Ratio _) succ@(Ratio _) = return succ
numBinopTypeCheck a@(Integer _) b@_ = throwError $ TypeError a b
numBinopTypeCheck a@(Float _) b@_ = throwError $ TypeError a b
numBinopTypeCheck a@(Ratio _) b@_ = throwError $ TypeError a b
numBinopTypeCheck a@_ _ = throwError $ TypeError (Integer 0) a

boolBinopTypeCheck :: FutureVal -> FutureVal -> Result FutureVal
boolBinopTypeCheck (Bool _) succ@(Bool _) = return succ
boolBinopTypeCheck a@(Bool _) b@_ = throwError $ TypeError a b
boolBinopTypeCheck a@_ _ = throwError $ TypeError (Bool False) a

strBinopTypeCheck :: FutureVal -> FutureVal -> Result FutureVal
strBinopTypeCheck (String _) succ@(String _) = return succ
strBinopTypeCheck a@(String _) b@_ = throwError $ TypeError a b
strBinopTypeCheck a@_ _ = throwError $ TypeError (String "") a
