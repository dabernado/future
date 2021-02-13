module Evaluator where

import Core.Types

import Control.Monad.Except
import Data.List (elemIndex)
import Data.Foldable (toList)
import Data.Sequence (update, fromList)

eval :: Env -> FutureVal -> IOResult FutureVal
eval env val@(Char _) = return val
eval env val@(String _) = return val
eval env val@(Integer _) = return val
eval env val@(Float _) = return val
eval env val@(Ratio _) = return val
eval env val@(List _ []) = return val
eval env val@(DottedList _ _ _) = return val
eval env (Atom id) = getVar env id
eval env (List _ [Atom "quote", val]) = return val
eval env (List _ [Atom "quasiquote", List _ val]) = mapM (evalQQ env) val >>= (return . List AnyT)
eval env (List _ [Atom "quasiquote", val]) = return val
eval env (List _ [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Custom (CustomT ":Bool" []) (1,"false") [] -> eval env alt
    Custom (CustomT ":Bool" []) (0,"true") [] -> eval env conseq
    _ -> throwError $ TypeError boolT (getType result)
eval env form@(List _ (Atom "cond" : clauses)) =
  if null clauses
  then throwError $ BadSpecialForm "no clause in cond expression: " form
  else case head clauses of
         List _ [Atom "else", val] -> eval env val
         List _ [test, expr] -> eval env $ List AnyT [Atom "if"
                                                     , test
                                                     , expr
                                                     , List AnyT (Atom "cond" : tail clauses)]
         _ -> throwError $ BadSpecialForm "ill-formed cond expression: " form
eval env form@(List _ (Atom "case" : key : clauses)) =
  if null clauses
  then throwError $ BadSpecialForm "no clause in case expression: " form
  else case head clauses of
         List _ (Atom "else" : exprs) -> mapM (eval env) exprs >>= return . last
         List _ (List _ datums : exprs) -> do
           result <- eval env key
           equality <- mapM (\x -> eval env (List AnyT [Atom "=", x, result])) datums
           if makeBool True `elem` equality
              then mapM (eval env) exprs >>= return . last
              else eval env $ List AnyT (Atom "case" : key : tail clauses)
         _ -> throwError $ BadSpecialForm "ill-formed case expression: " form
eval env (List _ [Atom "def", Atom var, form]) = eval env form >>= defineVar env var
eval env (List _ (Atom "deftype" : List _ (Atom var : targs) : consts)) =
  makeType env var targs consts >>= defineVar env var
eval env (List _ (Atom "defn" : Atom var : List _ params : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List _ (Atom "defn" : Atom var : DottedList _ params varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List _ (Atom "fn" : List _ params : body)) =
  makeNormalFunc env params body
eval env (List _ (Atom "fn" : DottedList _ params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List _ (Atom "fn" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List _ (func@(Atom _) : args)) = do
  f <- eval env func
  argVals <- mapM (eval env) args
  case f of
    Type t@(PartialT 2 (FuncT _ _)) -> constructFunc env t argVals
    _ -> apply f argVals
eval env (List _ [val]) = return val
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

makeFunc varargs env params body = do
  p <- expandParams env params
  return $ Function p varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal
makeType env name typeargs consts = do
  defineConsts env newType (map showVal typeargs) consts
  return $ Type newType
    where dynList = replicate (length typeargs) AnyT
          newType = if typeargs /= []
                       then PartialT (length typeargs) (CustomT name dynList)
                       else CustomT name dynList

expandParams :: Env -> [FutureVal] -> IOResult [(String, FutureType)]
expandParams env (Atom const@(':':_) : Atom id : params) = do
  Type t <- getVar env const
  case t of
    PartialT _ rt -> do
      xs <- expandParams env params
      return $ (id, rt) : xs
    _ -> do
      xs <- expandParams env params
      return $ (id, t) : xs
expandParams env (const@(List _ (Atom (':':_) : _)) : Atom id : params) = do
  result  <- eval env const
  case result of
    Type (PartialT _ t) -> do
      xs <- expandParams env params
      return $ (id, t) : xs
    Type t -> do
      xs <- expandParams env params
      return $ (id, t) : xs
    _ -> throwError $ TypeError TypeT (getType result)
expandParams env (Atom id : params) = do
  xs <- expandParams env params
  return $ (id, AnyT) : xs
expandParams env [] = return []

evalQQ :: Env -> FutureVal -> IOResult FutureVal
evalQQ env val@(Char _) = return val
evalQQ env val@(String _) = return val
evalQQ env val@(Integer _) = return val
evalQQ env val@(Float _) = return val
evalQQ env val@(Ratio _) = return val
evalQQ env (List _ [Atom "unquote", val]) = eval env val
--evalQQ (List _ [Atom "unquote-splicing", List _ val]) = eval val
evalQQ env val@(List _ _) = return val

defineConsts :: Env -> FutureType -> [String] -> [FutureVal] -> IOResult [FutureVal]
defineConsts env newType targs consts = do
  constVals <- mapM toConstVal (zip consts [0..])
  mapM (\(var,val) -> defineVar env var val) constVals
  where toConstVal ((List _ (Atom const : args)), i) = do
          argMaps <- checkArgs args
          return $ (const, buildConst i const argMaps)
        checkArgs :: [FutureVal] -> IOResult [(FutureType, Int)]
        checkArgs [] = return []
        checkArgs (Atom x : xs) = case elemIndex x targs of
          Just n -> checkArgs xs >>= (return . (:) (AnyT, n))
          Nothing -> if x /= typeName then do
              Type argType <- getVar env x
              checkArgs xs >>= (return . (:) (argType, -1))
            else checkArgs xs >>= (return . (:) (newType, -1))
        typeName = case unwrap newType of CustomT n _ -> n
        buildConst i c [] =
          Custom { valType = unwrap newType
                 , variant = (i, c)
                 , inner = []
                 }
        buildConst i c args =
          TypeConst { input = [t | (t,_) <- args]
                    , output = unwrap newType
                    , enum = (i, c)
                    , typeIndices = [n | (_,n) <- args]
                    }

apply :: FutureVal -> [FutureVal] -> IOResult FutureVal
apply (Primitive func) args = liftResult $ func args
apply (Type (PartialT argNum rt)) (Type t : args) = case t of
  PartialT _ def -> apply (constructType argNum rt def) args
  _              -> apply (constructType argNum rt t) args
apply (Type t) [] = return $ Type t
apply (Type t) [arg] = constructVal t arg
apply (Type t) args = throwError $ NumArgs 1 args
apply (TypeConst inputTypes out enum indices) args = do
  constChecked <- checkTypeList inputTypes args
  typeChecked <- checkTypeList (indexTypes indices $ typesList out) constChecked
  return $ Custom out enum (zip typeChecked indices)
    where typesList (CustomT _ ts) = ts
          typesList (PartialT _ (CustomT _ ts)) = ts
apply (Function params varargs body env) args =
  if length params /= length args && varargs == Nothing
     then throwError $ NumArgs (length params) args
     else do
       argVals <- zipTypes params args
       (liftIO $ bindVars env argVals) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List AnyT $ remainingArgs)]
            Nothing      -> return env

zipTypes :: [(String, FutureType)] -> [FutureVal] -> IOResult [(String, FutureVal)]
zipTypes [] [] = return []
zipTypes [] args = throwError $ NumArgs 0 args
zipTypes params [] = throwError $ NumArgs (length params) []
zipTypes ((var,func@(FuncT _ _)):params) (val:args) = case val of
  Primitive _ -> (zipTypes params args) >>= (return . ((:) (var,val)))
  _           -> if (getType val) == func
                    then do
                      xs <- zipTypes params args
                      return $ (var,val):xs
                    else throwError $ TypeError func (getType val)
zipTypes ((var,t):params) (val:args) = case t of
  AnyT           -> (zipTypes params args) >>= (return . ((:) (var,val)))
  PartialT _ def -> if (getType val) == def
                       then do
                         xs <- zipTypes params args
                         return $ (var,val):xs
                       else throwError $ TypeError def (getType val)
  _              -> if (getType val) == t
                       then do
                         xs <- zipTypes params args
                         return $ (var,val):xs
                       else throwError $ TypeError t (getType val)

constructVal :: FutureType -> FutureVal -> IOResult FutureVal
constructVal (ListT t) (List _ v) = do
  vals <- mapM (constructVal t) v
  return $ List t vals
constructVal (DottedListT t1 t2) (DottedList _ vals var) = do
  first <- mapM (constructVal t1) vals
  last <- constructVal t2 var
  return $ DottedList (t1,t2) first last
constructVal (VectorT t) (Vector _ v) = do
  vals <- mapM (constructVal t) v
  return $ Vector t vals
constructVal (PartialT _ t) v = constructVal t v
-- TODO: Implement recursive type checking
constructVal t@(CustomT n1 ts) v@(Custom vt variant vs) = if checkType v t
  then do
    _ <- checkTypeList (indexTypes [i | (_,i) <- vs] ts) [v | (v,_) <- vs]
    return $ Custom t variant vs
  else throwError $ TypeError t vt
-- TODO: Evaluate return type via function body
constructVal t@(FuncT (List TypeT args) _rt) func@(Function params Nothing b c) =
  if length args /= length params
     then throwError $ TypeError t (getType func)
     else do
       newParams <- checkParams args params
       return $ Function newParams Nothing b c
constructVal t@(FuncT (DottedList (TypeT, TypeT) args vt) rt) func@(Function params (Just vararg) b c) =
  if length args /= length params
     then throwError $ TypeError t (getType func)
     else do
       newParams <- checkParams args params
       return $ Function newParams (Just vararg) b c
constructVal AnyT v = return v
constructVal t v = if checkType v t
                      then return v
                      else throwError $ TypeError t (getType v)

checkParams :: [FutureVal] -> [(String, FutureType)] -> IOResult [(String, FutureType)]
checkParams [] [] = return []
checkParams (Type AnyT : xs) ((n, _) : ys) = do
  rest <- checkParams xs ys
  return $ (n, AnyT) : rest
checkParams (Type t : xs) ((n, AnyT) : ys) = do
  rest <- checkParams xs ys
  return $ (n, t) : rest
checkParams (Type t1 : xs) ((n, t2) : ys) = if t1 /= t2
  then throwError $ TypeError t1 t2
  else do
    rest <- checkParams xs ys
    return $ (n, t1) : rest

constructType :: Int -> FutureType -> FutureType -> FutureVal
constructType 1 (ListT _) t = Type (ListT t)
constructType 1 (VectorT _) t = Type (VectorT t)
constructType 1 (DottedListT t1 _) t2 = Type (DottedListT t1 t2)
constructType 1 (FuncT p _) t = Type (FuncT p (Just t))
constructType 1 (CustomT name ts) t = Type (CustomT name (init ts ++ [t]))
constructType 2 (DottedListT _ _) t = Type (PartialT 1 (DottedListT t AnyT))
constructType n (CustomT name ts) t = Type (PartialT (n-1) (CustomT name updatedTypes))
  where updatedTypes = toList $ update ((length ts) - n) t $ fromList ts

constructFunc :: Env -> FutureType -> [FutureVal] -> IOResult FutureVal
constructFunc env (PartialT 2 (FuncT _ _)) (List _ params : args) = do
  evaled <- mapM (eval env) params
  types <- mapM (constructVal TypeT) evaled
  apply (Type (PartialT 1 (FuncT (List TypeT types) Nothing))) args
constructFunc env (PartialT 2 (FuncT _ _)) (DottedList _ params vararg : args) = do
  evaled <- mapM (eval env) params
  types <- mapM (constructVal TypeT) evaled
  vt <- constructVal TypeT vararg
  apply (Type (PartialT 1 (FuncT (DottedList (TypeT, TypeT) types vt) Nothing))) args
