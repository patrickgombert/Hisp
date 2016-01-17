module Main where

import Kernel
import Runtime
import Parser
import Control.Monad.Trans.State

run :: [Expression] -> IO Value
run expressions = do
  let action = sequence (map lispEval expressions)
  (result, _) <- runStateT action globalEnvironment
  return (last result)

eval :: String -> IO Value
eval input = run . parseProgram $ input

