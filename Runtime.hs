module Runtime where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Data.Map as M

type LispM a = StateT Environment IO a

type Bindings = M.Map Value Value
data Environment = Environment { currentNs :: Maybe Value, parent :: Maybe Environment, binding :: Bindings } deriving (Ord, Show, Eq)

instance Show Value where
  show (Vint v)            = show v
  show (Vfloat v)          = show v
  show (Vlist v)           = show v
  show (Vbool v)           = show v
  show (Vstr v)            = show "\"" ++ v ++ "\""
  show (Vsym (Just va) vb) = show va ++ "/" ++ show vb
  show (Vsym Nothing v)    = show v
  show (Vmap v)            = show v
  show (Vlambda args _ _)  = "lambda (" ++ show args ++ ")"
  show (Vbuiltinfn name _) = "lambda (builtin: " ++ name ++ ")"

instance Eq Value where
  (Vbuiltinfn va _) == (Vbuiltinfn vb _) = va == vb
  va == vb                               = va == vb
  a /= b                                 = not(a == b)

instance Ord Value where
  compare (Vbuiltinfn va _) (Vbuiltinfn vb _) = compare va vb
  compare (Vbuiltinfn va _) _                 = LT
  compare _ (Vbuiltinfn vb _)                 = GT
  compare va vb                               = compare va vb

data Value = Vint Int
           | Vfloat Float
           | Vlist [Value]
           | Vbool Bool
           | Vstr String
           | Vsym (Maybe Value) Value
           | Vmap Bindings
           | Vlambda [Value] Environment [Expression]
           | Vbuiltinfn String ([Value] -> LispM Value)

data Expression = Eint Int
                | Efloat Float
                | Ebool Bool
                | Estr String
                | Esym (Maybe Value) Value
                | EMap Bindings
                | Elist [Expression] deriving (Ord, Show, Eq)

lispEval :: Expression -> LispM Value
lispEval (Eint e) = return (Vint e)
lispEval (Efloat e) = return (Vfloat e)
lispEval (Ebool e) = return (Vbool e)
lispEval (Estr e) = return (Vstr e)
lispEval (Esym ns str) = lookupSym ns str
lispEval (Elist exprs) = evalList exprs

lookupSym :: Maybe Value -> Value -> LispM Value
lookupSym ns symbol@(Vstr symName) = do
  env <- get
  case ns of
    Just ns'@(Vstr nsName) -> case M.lookup ns' (binding env) of
                                Just (Vmap nsBindings) -> case M.lookup symbol nsBindings of
                                                            Just result -> return result
                                                            Nothing -> error $ "Symbol " ++ symName ++ " not found in namespace " ++ nsName
                                Nothing -> case (parent env) of
                                             Just env' -> do
                                               put env'
                                               result <- lookupSym ns symbol
                                               put env
                                               return result
                                             Nothing -> error $ "Referenced namespace " ++ nsName ++ " but it did not exist"
    Just ns' -> error $ "Value " ++ (show ns') ++ " given as namespace for symbol"
    Nothing -> case M.lookup symbol (binding env) of
                 Just result -> return result
                 Nothing -> case (parent env) of
                              Just env' -> do
                                put env'
                                result <- lookupSym ns symbol
                                put env
                                return result
                              Nothing -> return (Vsym Nothing symbol)
lookupSym _ symbol = error $ "Value " ++ show symbol ++ " given as a symbol (expecting a string)"

evalList :: [Expression] -> LispM Value
evalList [] = return (Vlist [])
-- TODO: Implement Macros
evalList (macro:args) | macro == (Esym Nothing (Vstr "ns")) = do
                                                              env <- get
                                                              ns' <- (lispEval . head) args
                                                              let ns = currentNs env
                                                              modify $ \env -> env{currentNs = Just ns'}
                                                              mapM evalList [(tail args)]
                                                              modify $ \env -> env{currentNs = ns}
                                                              return ns'
evalList (fnv:args) = do
  fn <- lispEval fnv
  args <- mapM lispEval args
  case fn of
    (Vlambda params env exprs) -> do
      let env' = bindInEnv env params args
      if length(args) == length(params)
      then do
        original <- get
        put env'
        results <- mapM lispEval exprs
        put original
        return (last results)
      else error $ "Expected " ++ (show . length) args ++ " but received " ++ (show . length) params ++ " arguments"
    (Vbuiltinfn _ f) -> f args
    sym@(Vsym _ _) -> error $ "could not find function " ++ show sym
    v -> error $ "tried to invoke value " ++ show v ++ " as a function"

bindInEnv :: Environment -> [Value] -> [Value] -> Environment
bindInEnv env [] [] = env
bindInEnv env (param:params) (arg:args) = let b' = M.insert param arg (binding env)
                                          in bindInEnv env{binding = b'} params args

globalEnvironment :: Environment
globalEnvironment = (Environment Nothing Nothing M.empty)

run :: [Expression] -> IO Value
run expressions = do
  let action = sequence (map lispEval expressions)
  (result, _) <- runStateT action globalEnvironment
  return (last result)

