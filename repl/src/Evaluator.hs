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
  Type t <- getVar env const
  case t of
    TypeConst _ rt -> do
      xs <- expandParams env params
      return $ (id, rt) : xs
    _ -> do
      xs <- expandParams env params
      return $ (id, t) : xs
expandParams env (const@(List (Atom (':':_) : _)) : Atom id : params) = do
  result  <- eval env const
  case result of
    Type (TypeConst _ t) -> do
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
evalQQ env val@(Bool _) = return val
evalQQ env val@(Integer _) = return val
evalQQ env val@(Float _) = return val
evalQQ env val@(Ratio _) = return val
evalQQ env (List [Atom "unquote", val]) = eval env val
--evalQQ (List [Atom "unquote-splicing", List val]) = eval val
evalQQ env val@(List _) = return val

-- TODO: add clause for function type constructors
apply :: FutureVal -> [FutureVal] -> IOResult FutureVal
apply (Primitive func) args = liftResult $ func args
apply (Type (TypeConst argNum rt)) (Type t : args) = case t of
      TypeConst _ def -> apply (constructType argNum rt def) args
      _               -> apply (constructType argNum rt t) args
apply (Type t) [] = return $ Type t
apply (Type t) [arg] = constructVal t arg
apply (Type t) args = throwError $ NumArgs 1 args
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
  AnyT            -> (zipTypes params args) >>= (return . ((:) (var,val)))
  TypeConst _ def -> if (getType val) == def
                        then do
                          xs <- zipTypes params args
                          return $ (var,val):xs
                        else throwError $ TypeError def (getType val)
  _               -> if (getType val) == t
                        then do
                          xs <- zipTypes params args
                          return $ (var,val):xs
                        else throwError $ TypeError t (getType val)

-- TODO: add clauses for custom types and functions
-- TODO: refactor for updated collection vals
constructVal :: FutureType -> FutureVal -> IOResult FutureVal
constructVal (ListT t) (List v) = do
  vals <- mapM (constructVal t) v
  return $ List vals
constructVal (DottedListT t1 t2) (DottedList vals var) = do
  first <- mapM (constructVal t1) vals
  last <- constructVal t2 var
  return $ DottedList first last
constructVal (VectorT t) (Vector v) = do
  vals <- mapM (constructVal t) v
  return $ Vector vals
constructVal (TypeConst _ t) v = constructVal t v
constructVal AnyT v = return v
constructVal t v = if t /= (getType v)
                      then throwError $ TypeError t (getType v)
                      else return $ v

-- TODO: add clause for custom types
constructType :: Int -> FutureType -> FutureType -> FutureVal
constructType 1 (ListT _) t = Type (ListT t)
constructType 1 (VectorT _) t = Type (VectorT t)
constructType 1 (DottedListT t1 _) t2 = Type (DottedListT t1 t2)
constructType 2 (DottedListT _ _) t = Type (TypeConst 1 (DottedListT t AnyT))
