module Core.Primitives where

import Core.Types

import Control.Monad.Except

primVals :: [(String, FutureVal)]
primVals = [ ("true", makeBool True)
           , ("false", makeBool False)
           ]

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
          , ("type", getTypePrim)
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
    eq a b = makeBool $ a == b
    notEq a b = makeBool $ a /= b
    lt a b = makeBool $ a < b
    gt a b = makeBool $ a > b
    ltEq a b = makeBool $ a <= b
    gtEq a b = makeBool $ a >= b
    andF (Custom _ (a,_) _) (Custom _ (b,_) _) = makeBool $ (a == 0) && (b == 0)
    orF (Custom _ (a,_) _) (Custom _ (b,_) _) = makeBool $ (a == 0) || (b == 0)
    concatF (String a) (String b) = String $ a ++ b

getTypePrim :: [FutureVal] -> Result FutureVal
getTypePrim [v] = return $ Type (getType v)
getTypePrim n = throwError $ NumArgs 1 n

car :: [FutureVal] -> Result FutureVal
car [List _ (x:xs)] = return x
car [DottedList _ (x:xs) _] = return x
car [arg] = throwError $ TypeError (ListT AnyT) (getType arg)
car args = throwError $ NumArgs 1 args

cdr :: [FutureVal] -> Result FutureVal
cdr [List t (x : xs)] = return $ List t xs
cdr [DottedList _ [_] x] = return x
cdr [DottedList t (_ : xs) x] = return $ DottedList t xs x
cdr [arg] = throwError $ TypeError (ListT AnyT) (getType arg) 
cdr args = throwError $ NumArgs 1 args

cons :: [FutureVal] -> Result FutureVal
cons [x, List t []] = if checkType x t
                         then return $ List t [x]
                         else throwError $ TypeError t (getType x)
cons [x, List t xs] = if checkType x t
                         then return $ List t (x:xs)
                         else throwError $ TypeError t (getType x)
cons [x, DottedList t@(t1,_) xs xlast] = if checkType x t1
                                            then return $ DottedList t (x:xs) xlast
                                            else throwError $ TypeError t1 (getType x)
cons [x, y] = return $ DottedList (AnyT, AnyT) [x] y
cons args = throwError $ NumArgs 2 args

isSymbol :: [FutureVal] -> Result FutureVal
isSymbol [Atom _] = return $ makeBool True
isSymbol [_] = return $ makeBool False
isSymbol n = throwError $ NumArgs 1 n

symToString :: [FutureVal] -> Result FutureVal
symToString [Atom a] = return $ String a
symToString [t] = throwError $ TypeError SymbolT (getType t)
symToString n = throwError $ NumArgs 1 n

strToSymbol :: [FutureVal] -> Result FutureVal
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
    charTypeCheck [] = return $ makeBool True
    charTypeCheck (x:xs) = throwError $ TypeError CharT (getType x)

strLength :: [FutureVal] -> Result FutureVal
strLength [String s] = return $ Integer (length s)
strLength [t] = throwError $ TypeError StringT (getType t)
strLength n = throwError $ NumArgs 1 n

indexString :: [FutureVal] -> Result FutureVal
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

strBinopTypeCheck :: FutureVal -> FutureVal -> Result FutureVal
strBinopTypeCheck (String _) succ@(String _) = return succ
strBinopTypeCheck a@(String _) b = throwError $ TypeError StringT (getType b)
strBinopTypeCheck a _ = throwError $ TypeError StringT (getType a)

boolBinopTypeCheck :: FutureVal -> FutureVal -> Result FutureVal
boolBinopTypeCheck a b = if isBool a
  then if isBool b then return b else throwError $ TypeError boolT (getType b)
  else throwError $ TypeError boolT (getType a)
  where boolT = CustomT ":Bool" []
        isBool (Custom t _ _) = if t /= boolT then False else True
        isBool _ = False


-- Primitive Types --

primTypes :: [(String, FutureVal)]
primTypes = [ (":Symbol", Type SymbolT)
            , (":Bool", Type boolT)
            , (":Char", Type CharT)
            , (":String", Type StringT)
            , (":Int", Type IntegerT)
            , (":Float", Type FloatT)
            , (":Ratio", Type RatioT)
            , (":?", Type AnyT)
            , (":Type", Type TypeT)
            , (":List", Type PartialT { args = 1, returnType = ListT AnyT })
            , (":Pair", Type PartialT { args = 2
                                            , returnType = DottedListT AnyT AnyT
                                            })
            , (":Vector", Type PartialT { args = 1, returnType = VectorT AnyT })
            , (":Func", Type PartialT { args = 2
                                      , returnType = FuncT { paramsType = List AnyT []
                                                           , result = Nothing
                                                           }
                                      })
            , (":λ", Type PartialT { args = 2
                                      , returnType = FuncT { paramsType = List AnyT []
                                                           , result = Nothing
                                                           }
                                      })
            ]
