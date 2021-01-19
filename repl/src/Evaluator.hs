module Evaluator where

import Types

import Control.Monad.Except

eval :: Env -> FutureVal -> IOResult FutureVal
eval env val@(Char _) = return val
eval env val@(String _) = return val
eval env val@(Bool _) = return val
eval env val@(Integer _) = return val
eval env val@(Float _) = return val
eval env val@(Ratio _) = return val
eval env (Atom id) = getVar env id
eval env (List [val]) = return val
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "quasiquote", List val]) = mapM (evalQQ env) val >>= (return . List)
eval env (List [Atom "quasiquote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    Bool True -> eval env conseq
    _ -> throwError $ TypeError BoolT (getType result)
eval env form@(List (Atom "cond" : clauses)) =
  if null clauses
  then throwError $ BadSpecialForm "no clause in cond expression: " form
  else case head clauses of
         List [Atom "else", val] -> eval env val
         List [test, expr] -> eval env $ List [Atom "if"
                                       , test
                                       , expr
                                       , List (Atom "cond" : tail clauses)]
         _ -> throwError $ BadSpecialForm "ill-formed cond expression: " form
eval env form@(List (Atom "case" : key : clauses)) =
  if null clauses
  then throwError $ BadSpecialForm "no clause in case expression: " form
  else case head clauses of
         List (Atom "else" : exprs) -> mapM (eval env) exprs >>= return . last
         List (List datums : exprs) -> do
           result <- eval env key
           equality <- mapM (\x -> eval env (List [Atom "=", x, result])) datums
           if Bool True `elem` equality
              then mapM (eval env) exprs >>= return . last
              else eval env $ List (Atom "case" : key : tail clauses)
         _ -> throwError $ BadSpecialForm "ill-formed case expression: " form
eval env (List [Atom "def", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "defn" : Atom var : List params : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "defn" : Atom var : DottedList params varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "fn" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "fn" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "fn" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List (func : args)) = do
  f <- eval env func
  argVals <- mapM (eval env) args
  apply f argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

makeFunc varargs env params body = do
  p <- expandParams env params
  return $ Function p varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

expandParams :: Env -> [FutureVal] -> IOResult [(String, FutureType)]
expandParams env (Atom const@(':':_) : Atom id : params) = do
  TypeConst _ t <- getVar env const
  xs <- expandParams env params
  return $ (id, t) : xs
expandParams env (Atom id : params) = do
  xs <- expandParams env params
  return $ (id, AnyT) : xs
expandParams env [] = return []

evalQQ :: Env -> FutureVal -> IOResult FutureVal
evalQQ env val@(Char _) = return val
evalQQ env val@(String _) = return val
evalQQ env val@(Bool _) = return val
evalQQ env val@(Integer _) = return val
evalQQ env val@(Float _) = return val
evalQQ env val@(Ratio _) = return val
evalQQ env (List [Atom "unquote", val]) = eval env val
--evalQQ (List [Atom "unquote-splicing", List val]) = eval val
evalQQ env val@(List _) = return val

apply :: FutureVal -> [FutureVal] -> IOResult FutureVal
apply (Primitive func) args = liftResult $ func args
apply (Function params varargs body env) args =
      if length params /= length args && varargs == Nothing
         then throwError $ NumArgs (length params) args
         else do
           argVals <- zipTypes params args
           (liftIO $ bindVars env argVals) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            evalBody env = liftM last $ mapM (eval env) body
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env

zipTypes :: [(String, FutureType)] -> [FutureVal] -> IOResult [(String, FutureVal)]
zipTypes [] [] = return []
zipTypes [] args = throwError $ NumArgs 0 args
zipTypes params [] = throwError $ NumArgs (length params) []
zipTypes ((var,t):params) (val:args) = case t of
  AnyT -> (zipTypes params args) >>= (return . ((:) (var,val)))
  _   -> if (getType val) == t
           then do
             xs <- zipTypes params args
             return $ (var,val):xs
           else throwError $ TypeError t (getType val)

primOps :: [(String, [FutureVal] -> Result FutureVal)]
primOps = [ ("+", numBinop (+))
          , ("-", numBinop (-))
          , ("*", numBinop (*))
          , ("/", numBinop (/))
          , ("mod", intBinop mod)
          , ("quot", intBinop quot)
          , ("rem", intBinop rem)
          , ("=", valBinop eq)
          , ("/=", valBinop notEq)
          , ("<", valBinop lt)
          , (">", valBinop gt)
          , ("<=", valBinop ltEq)
          , (">=", valBinop gtEq)
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
    intBinop = binop intBinopTypeCheck
    valBinop = binop valBinopTypeCheck
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
car [arg] = throwError $ TypeError (ListT AnyT) (getType arg)
car args = throwError $ NumArgs 1 args

cdr :: [FutureVal] -> Result FutureVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [arg] = throwError $ TypeError (ListT AnyT) (getType arg) 
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
symToString [t] = throwError $ TypeError SymbolT (getType t)
symToString n = throwError $ NumArgs 1 n

strToSymbol :: [FutureVal] -> Result FutureVal
strToSymbol [] = throwError $ NumArgs 1 []
strToSymbol [String s] = return $ Atom s
strToSymbol [t] = throwError $ TypeError StringT (getType t)
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
    charTypeCheck (x:xs) = throwError $ TypeError CharT (getType x)

strLength :: [FutureVal] -> Result FutureVal
strLength [] = throwError $ NumArgs 1 []
strLength [String s] = return $ Integer (length s)
strLength [t] = throwError $ TypeError StringT (getType t)
strLength n = throwError $ NumArgs 1 n

indexString :: [FutureVal] -> Result FutureVal
indexString [] = throwError $ NumArgs 1 []
indexString [Integer n, String s] = return $ Char (s !! n)
indexString [Integer _, b] = throwError $ TypeError IntegerT (getType b)
indexString [a, _] = throwError $ TypeError IntegerT (getType a)
indexString n = throwError $ NumArgs 1 n

binop :: (FutureVal -> FutureVal -> Result FutureVal) -> (FutureVal -> FutureVal -> FutureVal) -> [FutureVal] -> Result FutureVal
binop typeCheck op [] = throwError $ NumArgs 2 []
binop typeCheck op val@[_] = throwError $ NumArgs 2 val
binop typeCheck op params@(x:xs) = case foldM typeCheck x xs of
                           err@(Left _) -> err
                           Right _ -> (return . foldl1 op) params

valBinopTypeCheck :: FutureVal -> FutureVal -> Result FutureVal
valBinopTypeCheck (Primitive _) _ = throwError $ TypeError IntegerT PrimitiveFuncT
valBinopTypeCheck a@(Function _ _ _ _) _ = throwError $ TypeError IntegerT (getType a)
valBinopTypeCheck a b = if (showType a) == (showType b)
                        then return b
                        else throwError $ TypeError (getType a) (getType b)

numBinopTypeCheck :: FutureVal -> FutureVal -> Result FutureVal
numBinopTypeCheck (Integer _) succ@(Integer _) = return succ
numBinopTypeCheck (Float _) succ@(Float _) = return succ
numBinopTypeCheck (Ratio _) succ@(Ratio _) = return succ
numBinopTypeCheck (Integer _) b = throwError $ TypeError IntegerT (getType b)
numBinopTypeCheck (Float _) b = throwError $ TypeError FloatT (getType b)
numBinopTypeCheck (Ratio _) b = throwError $ TypeError RatioT (getType b)
numBinopTypeCheck a _ = throwError $ TypeError IntegerT (getType a)

intBinopTypeCheck :: FutureVal -> FutureVal -> Result FutureVal
intBinopTypeCheck (Integer _) succ@(Integer _) = return succ
intBinopTypeCheck (Integer _) b = throwError $ TypeError IntegerT (getType b)
intBinopTypeCheck a _ = throwError $ TypeError IntegerT (getType a)

boolBinopTypeCheck :: FutureVal -> FutureVal -> Result FutureVal
boolBinopTypeCheck (Bool _) succ@(Bool _) = return succ
boolBinopTypeCheck (Bool _) b = throwError $ TypeError BoolT (getType b)
boolBinopTypeCheck a _ = throwError $ TypeError BoolT (getType a)

strBinopTypeCheck :: FutureVal -> FutureVal -> Result FutureVal
strBinopTypeCheck (String _) succ@(String _) = return succ
strBinopTypeCheck a@(String _) b = throwError $ TypeError StringT (getType b)
strBinopTypeCheck a _ = throwError $ TypeError StringT (getType a)


-- Primitive Types --

primTypes :: [(String, FutureVal)]
primTypes = [ (":Symbol", basicType SymbolT)
            , (":Bool", basicType BoolT)
            , (":Char", basicType CharT)
            , (":String", basicType StringT)
            , (":Int", basicType IntegerT)
            , (":Float", basicType FloatT)
            , (":Ratio", basicType RatioT)
            , (":Any", basicType AnyT)
            , (":Type", basicType TypeT)
            -- TODO: Implement nested types
            , (":List", TypeConst [] (ListT AnyT))
            , (":DottedList", TypeConst [] (DottedListT AnyT AnyT))
            , (":Vector", TypeConst [] (VectorT AnyT))
            , (":Function", TypeConst [] (FuncT { paramTypes = []
                                                , varargType = Nothing 
                                                , result = AnyT
                                                }))
            ]
