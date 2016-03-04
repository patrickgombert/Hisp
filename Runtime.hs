module Runtime where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Data.Map as M

type LispM a = StateT Environment IO a

type Bindings = M.Map Value Value
data Environment = Environment { currentNs :: Maybe Value, parent :: Maybe Environment, binding :: Bindings } deriving (Show, Eq)

instance Show Value where
  show (Vint v)            = show v
  show (Vfloat v)          = show v
  show (Vlist v)           = show v
  show (Vbool v)           = show v
  show (Vstr v)            = show v
  show (Vsym (Just va) vb) = show "sym@" ++ show va ++ "/" ++ show vb
  show (Vsym Nothing v)    = show "sym@" ++ show v
  show (Vmap v)            = show v
  show (Vlambda args _ _)  = "lambda " ++ show args
  show (Vbuiltinfn name _) = "kernel/" ++ name

instance Eq Value where
  (Vint va)             == (Vint vb)             = va == vb
  (Vfloat va)           == (Vfloat vb)           = va == vb
  (Vlist va)            == (Vlist vb)            = va == vb
  (Vbool va)            == (Vbool vb)            = va == vb
  (Vstr va)             == (Vstr vb)             = va == vb
  (Vsym (Just va1) va2) == (Vsym (Just vb1) vb2) = (va1 == vb1) && (va2 == vb2)
  (Vsym Nothing va)     == (Vsym Nothing vb)     = va == vb
  (Vmap va)             == (Vmap vb)             = va == vb
  (Vlambda va1 va2 va3) == (Vlambda vb1 vb2 vb3) = (va1 == vb1) && (va2 == vb2) && (va3 == vb3)
  (Vbuiltinfn va _)     == (Vbuiltinfn vb _)     = va == vb
  _                     == _                     = False
  a                     /= b                     = not(a == b)

instance Ord Value where
  compare va vb = compare (show va) (show vb)

data Value = Vint Int
           | Vfloat Float
           | Vlist [Value]
           | Vbool Bool
           | Vstr String
           | Vsym (Maybe Value) Value
           | Vmap Bindings
           | Vlambda [Value] Environment Expression
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
    Just ns'@(Vstr nsName) -> case M.lookup (Vsym Nothing ns') (binding env) of
                                Just (Vmap nsBindings) -> case M.lookup (Vsym Nothing symbol) nsBindings of
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
    Nothing -> case M.lookup (Vsym Nothing symbol) (binding env) of
                 Just result -> return result
                 Nothing -> case (parent env) of
                              Just env' -> do
                                put env'
                                result <- lookupSym ns symbol
                                put env
                                return result
                              Nothing -> return (Vsym Nothing symbol)
lookupSym _ symbol = error $ "Value " ++ show symbol ++ " given as a symbol (expecting a string)"

bindingFromValue :: Value -> Bindings
bindingFromValue (Vmap b) = b
bindingFromValue _ = M.empty

evalList :: [Expression] -> LispM Value
evalList [] = return (Vlist [])
-- TODO: Implement macros
evalList (macro:args) | macro == (Esym Nothing (Vstr "ns")) = do env <- get
                                                                 let ns = currentNs env
                                                                     body = tail args
                                                                 ns' <- (lispEval . head) args
                                                                 modify $ \env -> env{currentNs = Just ns'}
                                                                 mapM lispEval (quote (head body))
                                                                 modify $ \env -> env{currentNs = ns}
                                                                 return ns'
                      | macro == (Esym Nothing (Vstr "def")) = let name = args !! 0
                                                                   body = args !! 1
                                                               in do dname <- (lispEval name)
                                                                     dbody <- (lispEval body)
                                                                     env <- get
                                                                     modify $ (\env -> let cNs = case (currentNs env) of
                                                                                                   Just ns -> ns
                                                                                                   Nothing -> error $ "Can not invoke def outside of namespace declaration"
                                                                                           currentNsBinding = case M.lookup cNs (binding env) of
                                                                                                                Just b -> bindingFromValue b
                                                                                                                Nothing -> M.empty
                                                                                           updatedBinding = M.insert dname dbody currentNsBinding
                                                                                       in env{binding = M.insert cNs (Vmap updatedBinding) (binding env)})
                                                                     return dbody
                    | macro == (Esym Nothing (Vstr "if")) = let conditional = args !! 0
                                                                truthy      = args !! 1
                                                                falsy       = if length(args) >= 3
                                                                                then Just (args !! 2)
                                                                                else Nothing
                                                            in do cond <- lispEval conditional
                                                                  if cond == (Vbool True)
                                                                    then do truthyResult <- lispEval truthy
                                                                            return truthyResult
                                                                    else case falsy of
                                                                           Just falsy' -> do falsyResult <- lispEval falsy'
                                                                                             return falsyResult
                                                                           Nothing -> return (Vsym Nothing (Vstr "null"))
                    | macro == (Esym Nothing (Vstr "lambda")) = let arguments = args !! 0
                                                                    body      = args !! 1
                                                                in case arguments of
                                                                     (Elist argExpressionSyms) -> do
                                                                       let argValueSyms = map (\(Esym Nothing s) -> (Vsym Nothing s)) argExpressionSyms
                                                                       env <- get
                                                                       return (Vlambda argValueSyms env body)
                                                                     _ -> error $ "Lambda arguments must be in list form"
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
        result <- lispEval exprs
        put original
        return result
      else error $ "Expected " ++ (show . length) args ++ " but received " ++ (show . length) params ++ " arguments"
    (Vbuiltinfn _ f) -> f args
    sym@(Vsym _ _) -> error $ "could not find function " ++ show sym
    v -> error $ "tried to invoke value " ++ show v ++ " as a function"

-- TODO: Real quoting
quote :: Expression -> [Expression]
quote (Elist xs) = xs
quote _ = error $ "Tried to quote something other than a list"

bindInEnv :: Environment -> [Value] -> [Value] -> Environment
bindInEnv env [] [] = env
bindInEnv env (param:params) (arg:args) = let b' = M.insert param arg (binding env)
                                          in bindInEnv env{binding = b'} params args
bindInEnv _ _ _ = error $ "Mismatch of arguments/parameters"

