module Main ( main ) where

import LexLambdaNat
import ParLambdaNat
import PrintLambdaNat
import AbsLambdaNat
import ErrM

import Interpreter

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad ( when )


type ParseFun a = [Token] -> Err a

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s


runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun Program -> String -> IO ()
run v p s = let ts = myLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do 
                          putStrLn "\n Input:"
                          putStrLn $ printTree tree
                          -- putStrLn "\nParse Successful!"
                          showTree v tree
                          putStrLn "\n Output:"
                          putStrLn $ printTree $ execCBN tree
                          -- putStrLn "\n Result computed using call by value:"
                          -- putStrLn $ printTree $ execCBV tree
                          exitSuccess

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      -- putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree


usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2 pProgram
    "-s":fs -> mapM_ (runFile 0 pProgram) fs
    fs -> mapM_ (runFile 2 pProgram) fs
