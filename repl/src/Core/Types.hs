module Core.Types where

import Data.IORef
import Data.List (elemIndex)
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import Data.Sequence (update, fromList)
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

data TypeIndex = Base Int | Recursive [(Int, TypeIndex)]
  deriving (Eq)

-- TODO: Add maps
data FutureVal = Atom String
               | Integer Int
               | Float Double
               | Ratio Rational
               | String String
               | Char Char
               | Type FutureType
               | List FutureType [FutureVal]
               | Vector FutureType (Vector FutureVal)
               | DottedList (FutureType, FutureType) [FutureVal] FutureVal
               | Custom { valType :: FutureType
                        , variant :: (Int, String)
                        , inner :: [(FutureVal, TypeIndex)]
                        }
               | Primitive ([FutureVal] -> Result FutureVal)
               | TypeConst { input :: [FutureType]
                           , output :: FutureType
                           , enum :: (Int, String)
                           , typeIndices :: [TypeIndex]
                           }
               | Function { params :: [(String, FutureType)]
                          -- TODO: Add type info to vararg
                          , vararg :: Maybe String
                          , body :: [FutureVal]
                          , closure :: Env
                          }

makeBool :: Bool -> FutureVal
makeBool True =  Custom { valType = CustomT ":Bool" []
                        , variant = (0, "true")
                        , inner = []
                        }
makeBool False =  Custom { valType = CustomT ":Bool" []
                         , variant = (1, "false")
                         , inner = []
                         }

instance Eq FutureVal where
  (==) (Atom a) (Atom b) = a == b
  (==) (String a) (String b) = a == b
  (==) (Char a) (Char b) = a == b
  (==) (Integer a) (Integer b) = a == b
  (==) (Float a) (Float b) = a == b
  (==) (Ratio a) (Ratio b) = a == b
  (==) (List _ a) (List _ b) = a == b
  (==) (DottedList _ as a) (DottedList _ bs b) = (a == b) && (as == bs)
  (==) (Vector _ a) (Vector _ b) = a == b
  (==) (Type a ) (Type b ) = a == b
  (==) (Custom t1 v1 xs) (Custom t2 v2 ys) =
    (t1 == t2) && (v1 == v2) && ([v | (v,_) <- xs] == [v | (v,_) <- ys])
  (==) (TypeConst i1 o1 v1 _) (TypeConst i2 o2 v2 _) =
    (i1 == i2) && (v1 == v2) && (o1 == o2)

instance Ord FutureVal where
  (<=) (Atom a) (Atom b) = a <= b
  (<=) (String a) (String b) = a <= b
  (<=) (Char a) (Char b) = a <= b
  (<=) (Integer a) (Integer b) = a <= b
  (<=) (Float a) (Float b) = a <= b
  (<=) (Ratio a) (Ratio b) = a <= b
  (<=) (List _ a) (List _ b) = a <= b
  (<=) (DottedList _ as a) (DottedList _ bs b) = (a <= b) && (as <= bs)
  (<=) (Vector _ a) (Vector _ b) = a <= b
  (<=) (Custom _ (a,_) _) (Custom _ (b,_) _) = a <= b
  (<=) (TypeConst _ _ (a,_) _) (TypeConst _ _ (b,_) _) = a <= b

instance Show FutureVal where
  show v = showType v ++ " " ++ showVal v

showVal :: FutureVal -> String
showVal v@(Atom a) = a 
showVal v@(String s) = "\"" ++ s ++ "\""
showVal v@(Char c) = '\\':(c:"")
showVal v@(Integer n) = show n
showVal v@(Float f) = show f
showVal v@(Ratio r) = show (numerator r) ++ "/" ++ show (denominator r)
showVal v@(Custom _ (_,con) xs) = con ++ " " ++ unwords (map showVal [v | (v,_) <- xs])
showVal v@(List _ xs) = "(" ++ unwordsList xs ++ ")"
showVal v@(DottedList _ x xs) = "(" ++ unwordsList x ++ " . " ++ show xs ++ ")"
showVal val@(Vector _ v) = "(" ++ (unwordsList . Vector.toList) v ++ ")"
showVal (Type t) = show t
showVal (Primitive _) = "(fn ...)"
showVal (TypeConst _ _ _ _) = "<constructor>"
showVal v@(Function { params = args
                   , vararg = varargs
                   , body = _
                   , closure = env
                   }) = "(fn (" ++ unwords [n | (n,_) <- args] ++
                        (case varargs of
                           Nothing -> ""
                           Just arg -> " . " ++ arg) ++ ") ...)"

getType :: FutureVal -> FutureType
getType (Atom _) = SymbolT
getType (String _) = StringT
getType (Char _) = CharT
getType (Integer _) = IntegerT
getType (Float _) = FloatT
getType (Ratio _) = RatioT
getType (Custom t _ _) = t
getType (List t _) = ListT t
getType (DottedList (t1,t2) _ _) = DottedListT t1 t2
getType (Vector t _) = VectorT t
getType (Type _ ) = TypeT
getType (Primitive _) = FuncT (Type AnyT) (Just AnyT)
getType (TypeConst input output _ _) =
  FuncT { paramsType = List TypeT (map (Type) input)
        , result = Just output
        }
getType (Function p Nothing _ _) =
  FuncT { paramsType = List TypeT [Type t | (_,t) <- p]
        , result = Just AnyT
        }
getType (Function p (Just _) _ _) =
  FuncT { paramsType = DottedList (TypeT, TypeT) [Type t | (_,t) <- p] (Type AnyT)
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

-- Errors
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

-- TODO: Add type for maps
data FutureType = SymbolT
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
                | FuncT { paramsType :: FutureVal
                        , result :: Maybe FutureType
                        }
                | PartialT { args :: Int
                           , returnType :: FutureType
                           }

boolT = CustomT ":Bool" []

instance Eq FutureType where
  (==) CharT CharT = True
  (==) IntegerT IntegerT = True
  (==) FloatT FloatT = True
  (==) RatioT RatioT = True
  (==) TypeT TypeT = True
  (==) (ListT a) (ListT b) = a == b
  (==) (VectorT a) (VectorT b) = a == b
  (==) (DottedListT a1 a2) (DottedListT b1 b2) = (a1 == b1) && (a2 == b2)
  (==) (CustomT an as) (CustomT bn bs) = (an == bn) && (as == bs)
  (==) (FuncT (Type AnyT) r1) (FuncT _ r2) = r1 == r2
  (==) (FuncT _ r1) (FuncT (Type AnyT) r2) = r1 == r2
  (==) (FuncT p1 r1) (FuncT p2 r2) = (p1 == p2) && (r1 == r2)
  (==) (PartialT _ a) (PartialT _ b) = a == b
  (==) (PartialT _ a) b = a == b
  (==) AnyT _ = True
  (==) _ AnyT = True
  (==) _ _ = False

instance Show FutureType where
  show (SymbolT) = ":Symbol"
  show (StringT) = ":String"
  show (CharT) = ":Char"
  show (IntegerT) = ":Int"
  show (FloatT) = ":Float"
  show (RatioT) = ":Ratio"
  show (AnyT) = ":?"
  show (TypeT) = ":Type"
  show (CustomT n []) = n
  show (CustomT n ts) = "(" ++ n ++ " " ++ unwords (map show ts) ++ ")"
  show (ListT t) = "(:List " ++ show t ++ ")"
  show (DottedListT a b) = "(:Pair " ++ show a ++ " " ++ show b ++ ")"
  show (VectorT t) = "(:Vector " ++ show t ++ ")"
  show (PartialT _ t) = show t
  show (FuncT (Type AnyT) _) = "(:Fn :? :?)"
  show (FuncT (List _ args) Nothing) = "(:Fn '(" ++ unwords (map showVal args) ++ "))"
  show (FuncT (List _ args) (Just return)) =
    "(:Fn '(" ++ unwords (map showVal args) ++
    ") " ++ show return ++ ")"
  show (FuncT (DottedList _ args vararg) Nothing) =
    "(:Fn (" ++ unwords (map showVal args)
    ++ " . " ++ showVal vararg ++ "))"
  show (FuncT (DottedList _ args vararg) (Just return)) =
    "(:Fn (" ++ unwords (map showVal args)
    ++ " . " ++ showVal vararg ++
    ") " ++ show return ++ ")"

unwrap :: FutureType -> FutureType
unwrap (PartialT _ t) = t
unwrap t = t

checkType :: FutureVal -> FutureType -> Bool
checkType = (==) . getType

checkTypeList :: [FutureType] -> [FutureVal] -> IOResult ()
checkTypeList [] [] = return ()
checkTypeList (t:ts) (v:vs) =
  if checkType v t
     then checkTypeList ts vs
     else throwError $ TypeError t (getType v)

checkTypeIndices :: [FutureVal] -> [TypeIndex] -> [FutureType] -> IOResult ()
checkTypeIndices [] [] _ = return ()
checkTypeIndices (_:vs) (Base (-1) : is) ts = checkTypeIndices vs is ts
checkTypeIndices (v:vs) (Base i : is) ts = if getType v /= (ts !! i)
  then throwError $ TypeError (ts !! i) (getType v)
  else checkTypeIndices vs is ts
checkTypeIndices (v:vs) (Recursive xs : is) ts = do
  checkTypeArgs v xs
  checkTypeIndices vs is ts
    where checkTypeArgs :: FutureType -> [(Int, TypeIndex)] -> IOResult ()
          checkTypeArgs _ [] = return ()
          checkTypeArgs v ((n, Base i) : ns) =
            let inner = getInner v n
             in do
               checkTypeList (replicate (length inner) (ts !! i)) inner
               checkTypeArgs v ns
          checkTypeArgs v ((n, Recursive l) : ns) =
            let inner = getInner v n
             in do
               checkTypeArgs (List AnyT inner) l
               checkTypeArgs v ns

indexTypes :: [FutureType] -> [TypeIndex] -> [FutureType] -> [FutureType]
indexTypes as is ts = wrapped is
  where wrapped [] = []
        wrapped (Base (-1) : rest) = AnyT : wrapped rest
        wrapped (Base i : rest) = (ts !! i) : wrapped rest
        wrapped (i@(Recursive l) : rest) =
          (recursive l $ as !! (fromJust $ elemIndex i is)) : wrapped rest
        recursive [] t = t
        recursive ((n, Base i) : xs) t = recursive xs $ fillType t n (ts !! i)

-- TODO: Add clauses for function types
fillType :: FutureType -> Int -> FutureType -> FutureType
fillType (ListT _) 0 t = ListT t
fillType (DottedListT _ x) 0 t = DottedListT t x
fillType (DottedListT x _) 1 t = DottedListT x t
fillType (VectorT _) 0 t = VectorT t
fillType (CustomT name l) n t = CustomT name $ toList $ update n t $ fromList l
fillType ft n t = fillType (unwrap ft) n t

getInner :: FutureVal -> Int -> [FutureVal]
getInner (List _ x) 0 = x
getInner (DottedList _ x _) 0 = x
getInner (DottedList _ _ x) 1 = [x]
getInner (Vector _ x) 0 = toList x
-- Look through l and find the val with a TypeIndex that matches Base n
getInner (Custom _ _ l) n =
