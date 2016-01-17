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
                                                              ((Vsym Nothing (Vstr "!=")), notEquals)
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

