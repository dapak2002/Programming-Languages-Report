
module Main where

import LexNumbers
import ParNumbers
import AbsNumbers
import Interpreter

import ErrM
import PrintNumbers

main = do
  interact calc
  putStrLn ""

calc s = 
  let Ok e = pExp (myLexer s) 
  in printTree (eval e)

-- ghc Calculator.hs
-- echo "1+2*3" | ./Calculator