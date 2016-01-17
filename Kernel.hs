module Kernel where

import Runtime
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Data.Map as M

globalEnvironment :: Environment
globalEnvironment = Environment Nothing Nothing $ M.fromList [
                                                              ((Vsym Nothing (Vstr "print-line")), printLine),
                                                              ((Vsym Nothing (Vstr "=")), equals),
                                                              ((Vsym Nothing (Vstr "!=")), notEquals),
                                                              ((Vsym Nothing (Vstr "+")), addition),
                                                              ((Vsym Nothing (Vstr "-")), subtraction),
                                                              ((Vsym Nothing (Vstr "*")), multiplication),
                                                              ((Vsym Nothing (Vstr "/")), division)
                                                             ]

printLine :: Value
printLine = Vbuiltinfn "print-line" (\vals -> do lift $ mapM_ (putStrLn . show) vals
                                                 return (Vsym Nothing (Vstr "null")))
equalsReduce :: Bool -> [Value] -> Bool
equalsReduce state (v:vs) = if state
                              then if length vs == 0
                                   then state
                                   else equalsReduce (and [state, (v == (head vs))]) vs
                              else False

equals :: Value
equals = Vbuiltinfn "=" (\vals -> return (Vbool (equalsReduce True vals)))

notEquals :: Value
notEquals = Vbuiltinfn "!=" (\vals -> return (Vbool (not (equalsReduce True vals))))

class Add a where
  add :: a -> a -> a

instance Add Value where
  add (Vint x) (Vint y) = (Vint $ x + y)
  add (Vint x) (Vfloat y) = (Vfloat $ fromIntegral x + y)
  add (Vfloat x) (Vint y) = (Vfloat $ x + fromIntegral y)
  add (Vfloat x) (Vfloat y) = (Vfloat $ x + y)
  add _ _ = error $ "+ can only be applied to numerical arguments"

addition :: Value
addition = Vbuiltinfn "+" (\vals -> return $ foldl add (Vint 0) vals)

class Subtract a where
  sub :: a -> a -> a

instance Subtract Value where
  sub (Vint x) (Vint y) = (Vint $ x - y)
  sub (Vint x) (Vfloat y) = (Vfloat $ fromIntegral x - y)
  sub (Vfloat x) (Vint y) = (Vfloat $ x - fromIntegral y)
  sub (Vfloat x) (Vfloat y) = (Vfloat $ x - y)
  sub _ _ = error $ "- can only be applied to numerical arguments"

subtraction :: Value
subtraction = Vbuiltinfn "-" (\vals -> return $ foldl sub (head vals) (tail vals))

class Multiply a where
  multiply :: a -> a -> a

instance Multiply Value where
  multiply (Vint x) (Vint y) = (Vint $ x * y)
  multiply (Vint x) (Vfloat y) = (Vfloat $ fromIntegral x * y)
  multiply (Vfloat x) (Vint y) = (Vfloat $ x * fromIntegral y)
  multiply (Vfloat x) (Vfloat y) = (Vfloat $ x * y)
  multiply _ _ = error $ "* can only be applied to numerical arguments"

multiplication :: Value
multiplication = Vbuiltinfn "*" (\vals -> return $ foldl multiply (Vint 1) vals)

class Divide a where
  divide :: a -> a -> a

instance Divide Value where
  divide (Vint x) (Vint y) = do
                               let res = x `div` y
                               if res == 0
                                 then (Vfloat $ fromIntegral x / fromIntegral y)
                                 else (Vint res)
  divide (Vint x) (Vfloat y) = (Vfloat $ fromIntegral x / y)
  divide (Vfloat x) (Vint y) = (Vfloat $ x / fromIntegral y)
  divide (Vfloat x) (Vfloat y) = (Vfloat $ x / y)
  divide _ _ = error $ "/ can only be applied to numerical arguments"

division :: Value
division = Vbuiltinfn "/" (\vals -> return $ foldl divide (head vals) (tail vals))

