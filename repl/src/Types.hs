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
               | TypeConst [FutureType] FutureType
               | Primitive ([FutureVal] -> Result FutureVal)
               | Function { params :: [(String, FutureType)]
                          , vararg :: Maybe (String, FutureType)
                          , body :: [FutureVal]
                          , closure :: Env
                          }

instance Eq FutureVal where
  (==) (Atom a) (Atom b) = a == b
  (==) (String a) (String b) = a == b
  (==) (Char a) (Char b) = a == b
  (==) (Bool a) (Bool b) = a == b
  (==) (Integer a) (Integer b) = a == b
  (==) (Float a) (Float b) = a == b
  (==) (Ratio a) (Ratio b) = a == b
  (==) (List a) (List b) = a == b
  (==) (DottedList as a) (DottedList bs b) = (a == b) && (as == bs)
  (==) (Vector a) (Vector b) = a == b
  (==) (TypeConst _ a) (TypeConst _ b) = a == b

instance Ord FutureVal where
  (<=) (Atom a) (Atom b) = a <= b
  (<=) (String a) (String b) = a <= b
  (<=) (Char a) (Char b) = a <= b
  (<=) (Bool a) (Bool b) = a <= b
  (<=) (Integer a) (Integer b) = a <= b
  (<=) (Float a) (Float b) = a <= b
  (<=) (Ratio a) (Ratio b) = a <= b
  (<=) (List a) (List b) = a <= b
  (<=) (DottedList as a) (DottedList bs b) = (a <= b) && (as <= bs)
  (<=) (Vector a) (Vector b) = a <= b
  (<=) (TypeConst _ a) (TypeConst _ b) = a <= b

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
  show v@(Primitive _) = showType v
  show v@(TypeConst _ t) = "(" ++ showType v ++ " " ++ show t ++ ")"
  show v@(Function { params = args
                   , vararg = varargs
                   , body = body
                   , closure = env
                   }) = showType v ++ " (fn (" ++ unwords (map show args) ++
                        (case varargs of
                           Nothing -> ""
                           Just arg -> " . " ++ arg) ++ ") ...)"

showVal :: FutureVal -> String
showVal v@(Atom a) = a 
showVal v@(String s) = "\"" ++ s ++ "\""
showVal v@(Char c) = '\\':(c:"")
showVal v@(Bool True) = "true"
showVal v@(Bool False) = "false"
showVal v@(Integer n) = show n
showVal v@(Float f) = show f
showVal v@(Ratio r) = show (numerator r) ++ "/" ++ show (denominator r)
showVal v@(List xs) = "(" ++ unwordsList xs ++ ")"
showVal v@(DottedList x xs) = "(" ++ unwordsList x ++ " . " ++ show xs ++ ")"
showVal val@(Vector v) = "(" ++ (unwordsList . Vector.toList) v ++ ")"
showVal v@(Primitive _) = "<primitive>"
showVal (TypeConst _ t) = show t
showVal v@(Function { params = args
                   , vararg = varargs
                   , body = _
                   , closure = env
                   }) = " (fn (" ++ unwords (map show args) ++
                        (case varargs of
                           Nothing -> ""
                           Just arg -> " . " ++ arg) ++ ") ...)"

getType :: FutureVal -> FutureType
getType (Atom _) = Symbol
getType (String _) = String
getType (Char _) = Char
getType (Bool _) = Bool
getType (Integer _) = Integer
getType (Float _) = Float
getType (Ratio _) = Ratio
getType (List _) = List Any
getType (DottedList _ x) = DottedList Any (getType x)
getType (Vector _) = Vector Any
getType (Primitive _) = PrimitiveFunc
getType (TypeConst _ _) = Type
getType (Function p Nothing _ _) =
  Func { params = [t | (_,t) <- p, t]
       , vararg = Nothing
       , result = Any
       }
getType (Function p (Just (_,v)) _ _) =
  Func { params = [t | (_,t) <- p, t]
       , vararg = Just v
       , result = Any
       }

showType :: FutureVal -> String
showType = show $ getType

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

instance Enum FutureVal where
  toEnum n = Integer n
  fromEnum (Integer n) = n
  fromEnum (Float f) = fromEnum f
  fromEnum (Ratio r) = fromEnum r

instance Real FutureVal where
  toRational (Integer x) = toRational x

instance Integral FutureVal where
  quotRem (Integer a) (Integer b) = (Integer $ quot a b, Integer $ rem a b)
  toInteger (Integer x) = toInteger x

unwordsList :: [FutureVal] -> String
unwordsList = unwords . map show

data FutureType = Symbol
                | Bool
                | Char
                | String
                | Integer
                | Float
                | Ratio
                | List FutureType
                | DottedList FutureType FutureType
                | Vector FutureType
                | Custom String [FutureType]
                | Any
                | Type
                | PrimitiveFunc
                | Func { params = [FutureType]
                       , vararg = Maybe FutureType
                       , result = FutureType
                       }
                deriving (Eq)

instance Show FutureType where
  show (Symbol) = ":Symbol"
  show (String) = ":String"
  show (Char) = ":Char"
  show (Bool) = ":Bool"
  show (Integer) = ":Integer"
  show (Float) = ":Float"
  show (Ratio) = ":Ratio"
  show (Any) = ":Any"
  show (Type) = ":Type"
  show (List t) = "(:List " ++ show t ++ ")"
  show (DottedList a b) = "(:DottedList " ++ show a ++ " " ++ show b ++ ")"
  show (Vector t) = "(:Vector " ++ show t ++ ")"
  show (PrimitiveFunc) = "(:Function <primitive>)"
  show (Func args Nothing return) = "(:Function (" ++ unwords (map show args) ++
                                    ") " ++ show return ++ ")"
  show (Func args (Just vararg) return) = "(:Function (" ++ unwords (map show args) ++
                                          " . " ++ show vararg ++
                                          ") " ++ show return ++ ")"
  show (Custom t []) = ":" ++ t
  show (Custom t args) = "(:" ++ t ++ " " ++ unwords (map show args) ++ ")"

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
