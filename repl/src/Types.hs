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
               | Integer Int
               | Float Double
               | Ratio Rational
               | String String
               | Char Char
               | Bool Bool
               | Type FutureType
             -- TODO: Add type info to collections
               | List [FutureVal]
               | DottedList [FutureVal] FutureVal
               | Vector (Vector FutureVal)
               | Primitive ([FutureVal] -> Result FutureVal)
               | Function { params :: [(String, FutureType)]
                          , vararg :: Maybe String
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
  (==) (Type a ) (Type b ) = a == b

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
  show v@(Type t ) = show t
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
showVal (Type t) = show t
showVal v@(Function { params = args
                   , vararg = varargs
                   , body = _
                   , closure = env
                   }) = " (fn (" ++ unwords (map show args) ++
                        (case varargs of
                           Nothing -> ""
                           Just arg -> " . " ++ arg) ++ ") ...)"

getType :: FutureVal -> FutureType
getType (Atom _) = SymbolT
getType (String _) = StringT
getType (Char _) = CharT
getType (Bool _) = BoolT
getType (Integer _) = IntegerT
getType (Float _) = FloatT
getType (Ratio _) = RatioT
getType (List _) = ListT AnyT
getType (DottedList _ x) = DottedListT AnyT (getType x)
getType (Vector _) = VectorT AnyT
getType (Primitive _) = PrimitiveFuncT
getType (Type _ ) = TypeT
getType (Function p Nothing _ _) =
  FuncT { paramsType = List [Type t | (_,t) <- p]
        , result = Just AnyT
        }
getType (Function p (Just t) _ _) =
  FuncT { paramsType = DottedList [Type t | (_,t) <- p] (Type AnyT)
        , result = Just AnyT
        }

showType :: FutureVal -> String
showType = show . getType

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

data FutureType = SymbolT
                | BoolT
                | CharT
                | StringT
                | IntegerT
                | FloatT
                | RatioT
                | ListT FutureType
                | DottedListT FutureType FutureType
                | VectorT FutureType
                | CustomT String [FutureType]
                | AnyT
                | TypeT
                | PrimitiveFuncT
                | FuncT { paramsType :: FutureVal
                        , result :: Maybe FutureType
                        }
                | TypeConst { args :: Int
                            , returnType :: FutureType
                            }
                deriving (Eq)

instance Show FutureType where
  show (SymbolT) = ":Symbol"
  show (StringT) = ":String"
  show (CharT) = ":Char"
  show (BoolT) = ":Bool"
  show (IntegerT) = ":Int"
  show (FloatT) = ":Float"
  show (RatioT) = ":Ratio"
  show (AnyT) = ":Any"
  show (TypeT) = ":Type"
  show (ListT t) = "(:List " ++ show t ++ ")"
  show (DottedListT a b) = "(:DottedList " ++ show a ++ " " ++ show b ++ ")"
  show (VectorT t) = "(:Vector " ++ show t ++ ")"
  show (PrimitiveFuncT) = "(:Function <primitive>)"
  show (FuncT (List args) return) = "(:Function (" ++ unwords (map show args) ++
                                    ") " ++ show return ++ ")"
  show (FuncT (DottedList args vararg) return) = "(:Function (" ++ unwords (map show args)
                                                 ++ " . " ++ show vararg ++
                                                 ") " ++ show return ++ ")"
  show (CustomT t []) = ":" ++ t
  show (CustomT t args) = "(:" ++ t ++ " " ++ unwords (map show args) ++ ")"
  show (TypeConst _ t) = show t

data FutureError = NumArgs Int [FutureVal]
                 | TypeError FutureType FutureType
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
  show (TypeError exp found) = "Type error - expected " ++ show exp ++
                               ", found " ++ show found
  show (Parser err) = "Parse error at " ++ show err

trapError action = catchError action (return . show)

extractValue :: Result a -> a
extractValue (Right val) = val
