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

fromStr :: String -> IO Value
fromStr input = run . parseProgram $ input

