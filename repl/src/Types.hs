module Types where

import Data.IORef
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Ratio (numerator, denominator, (%))
import Control.Monad.Except
import Text.ParserCombinators.Parsec

type Env = IORef [(String, FutureVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound env var = readIORef env >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOResult FutureVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (return)
        (lookup var env)

defineVar :: Env -> String -> FutureVal -> IOResult FutureVal
defineVar envRef var val = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
     then throwError $ Immutable "Variable is already defined" var
     else liftIO $ do
          env <- readIORef envRef
          writeIORef envRef ((var, val) : env)
          return val

bindVars :: Env -> [(String, FutureVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (return bindings)

data FutureVal = Atom String
               | List [FutureVal]
               | DottedList [FutureVal] FutureVal
               | Vector (Vector FutureVal)
               | Integer Int
               | Float Double
               | Ratio Rational
               | String String
               | Char Char
               | Bool Bool
               deriving (Eq, Ord)

instance Show FutureVal where
  show v@(Atom a) = showType v ++ " " ++ a 
  show v@(String s) = showType v ++ " " ++ "\"" ++ s ++ "\""
  show v@(Char c) = showType v ++ " " ++ '\\':(c:"")
  show v@(Bool True) = showType v ++ " " ++ "true"
  show v@(Bool False) = showType v ++ " " ++ "false"
  show v@(Integer n) = showType v ++ " " ++ show n
  show v@(Float f) = showType v ++ " " ++ show f
  show v@(Ratio r) = showType v ++ " " ++
                     show (numerator r) ++ "/" ++ show (denominator r)
  show v@(List xs) = showType v ++ " " ++ "(" ++ unwordsList xs ++ ")"
  show v@(DottedList x xs) = showType v ++ " " ++
                             "(" ++ unwordsList x ++ " . " ++ show xs ++ ")"
  show val@(Vector v) = showType val ++ " " ++
                        "(" ++ (unwordsList . Vector.toList) v ++ ")"

showType :: FutureVal -> String
showType (Atom _) = ":Atom"
showType (String _) = ":String"
showType (Char _) = ":Char"
showType (Bool _) = ":Bool"
showType (Integer _) = ":Integer"
showType (Float _) = ":Float"
showType (Ratio _) = ":Ratio"
showType (List _) = ":List"
showType (DottedList _ _) = ":DottedList"
showType (Vector _) = ":Vector"

instance Num FutureVal where
  (+) (Integer a) (Integer b) = Integer $ a + b
  (+) (Float a) (Float b) = Float $ a + b
  (+) (Ratio a) (Ratio b) = Ratio $ a + b
  (-) (Integer a) (Integer b) = Integer $ a - b
  (-) (Float a) (Float b) = Float $ a - b
  (-) (Ratio a) (Ratio b) = Ratio $ a - b
  (*) (Integer a) (Integer b) = Integer $ a * b
  (*) (Float a) (Float b) = Float $ a * b
  (*) (Ratio a) (Ratio b) = Ratio $ a * b
  abs (Integer n) = Integer $ abs n
  abs (Float f) = Float $ abs f
  abs (Ratio r) = Ratio $ abs r
  signum (Integer n) = Integer $ signum n
  signum (Float f) = Float $ signum f
  signum (Ratio r) = Ratio $ signum r
  fromInteger n = Integer $ fromInteger n

instance Fractional FutureVal where
  fromRational r = Ratio r
  (/) (Integer a) (Integer b) = Integer $ div a b
  (/) (Float a) (Float b) = Float $ a / b
  (/) (Ratio a) (Ratio b) = Ratio $ a / b

instance Real FutureVal where
  toRational (Integer n) = toRational n
  toRational (Float f) = toRational f
  toRational (Ratio r) = r

instance Enum FutureVal where
  toEnum n = Integer n
  fromEnum (Integer n) = n
  fromEnum (Float f) = fromEnum f
  fromEnum (Ratio r) = fromEnum r

instance Integral FutureVal where
  quotRem (Integer a) (Integer b) = ((Integer $ quot a b), (Integer $ rem a b))
  toInteger (Integer n) = toInteger n

unwordsList :: [FutureVal] -> String
unwordsList = unwords . map show



data FutureError = NumArgs Int [FutureVal]
                 | TypeError FutureVal FutureVal
                 | Parser ParseError
                 | BadSpecialForm String FutureVal
                 | NotFunction String String
                 | UnboundVar String String
                 | Immutable String String
                 | Default String

type Result = Either FutureError
type IOResult = ExceptT FutureError IO

liftResult :: Result a -> IOResult a
liftResult (Left err) = throwError err
liftResult (Right val) = return val

runIOResult :: IOResult String -> IO String
runIOResult action = runExceptT (trapError action) >>= return . extractValue

instance Show FutureError where
  show (UnboundVar msg var) = msg ++ " - " ++ var
  show (Immutable msg var) = msg ++ " - " ++ var
  show (BadSpecialForm msg form) = msg ++ " - " ++ show form
  show (NotFunction msg func) = msg ++ " - " ++ func
  show (NumArgs exp found) = "Expected " ++ show exp ++
                             " args; found " ++ unwordsList found
  show (TypeError exp found) = "Type error - expected " ++ showType exp ++
                               ", found " ++ showType found
  show (Parser err) = "Parse error at " ++ show err

trapError action = catchError action (return . show)

extractValue :: Result a -> a
extractValue (Right val) = val
