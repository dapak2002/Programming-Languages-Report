{-# LANGUAGE ScopedTypeVariables, TypeApplications, RecordWildCards, 
  KindSignatures, DataKinds, GADTs, AllowAmbiguousTypes, TypeFamilies, FlexibleContexts #-}

module Main ( main ) where

import System.Console.Repline hiding (dontCrash)
import Control.Monad.IO.Class(MonadIO, liftIO)
import Control.Monad.Trans.State.Strict(StateT(..), evalStateT, get, modify)
import Control.Monad.Trans.Class(lift)
-- import System.Console.Haskeline.MonadException(catch, SomeException(..), handle,IOException)
import Control.Exception(SomeException(..), handle,IOException)
import Control.Monad.Catch(catch)
import Data.Char(isSpace)
import Data.List(isPrefixOf, intercalate, dropWhileEnd)
import Data.List.Extra(takeEnd)
import Data.List.Split(splitOn)
import System.Exit (exitSuccess)
import Data.String.Conv
import Control.Monad(void)
import Data.Map(empty, lookup)
import Text.Pretty.Simple(pPrint)
import Data.Singletons(SingI)
import Text.Earley(Report(..))
import System.IO.Silently(capture)

-- import Environment
import LamFunInterpreter
import LamInterpreter()
import LamNatInterpreter()
import LamRecInterpreter()
import LamMemInterpreter()
import LamArrayInterpreter()
import LamFunParser
import LamFunSyntax
import LamFunLexer
import Utils
import GHC.IO.Encoding
import System.IO (openFile, IOMode(..), hSetNewlineMode, universalNewlineMode, hGetContents)


-- converts line endings to \n to ensure consistent decoding...
-- should fix issues with parsing?
readFile' :: FilePath -> IO String
readFile' name = do
    h <- openFile name ReadMode
    hSetNewlineMode h universalNewlineMode 
    hGetContents h


type Repl (v :: Version) a = HaskelineT (StateT (Settings, (GlobEnv v, RawEnv v)) IO) a

data Settings = Settings {
  currentFile :: Maybe FilePath
, verbose :: Bool
, buffer :: [String]
}

getEnv :: Repl v (GlobEnv v, RawEnv v)
getEnv = do
  (_,env) <- lift get
  return env

putEnv :: forall v. (GlobEnv v, RawEnv v) -> Repl v ()
putEnv env = lift $ modify (\(s,_) -> (s,env))


getCurrentFile :: Repl v (Maybe FilePath)
getCurrentFile =  do
  (Settings{..},_) <- lift get
  return currentFile


putCurrentFile :: Maybe FilePath -> Repl v ()
putCurrentFile f = lift $ modify (\(s,env) -> (s{currentFile = f},env))


getVerbose :: Repl v Bool
getVerbose =  do
  (Settings{..},_) <- lift get
  return verbose


putVerbose :: Bool -> Repl v ()
putVerbose v = lift $ modify (\(s,env) -> (s{verbose = v},env))


getBuffer :: Repl v [String]
getBuffer =  do
  (Settings{..},_) <- lift get
  return buffer


appendBuffer :: [String] -> Repl v ()
appendBuffer xs = lift $ modify (\(s@Settings{..},env) -> (s{buffer = buffer ++ xs},env))

clearBuffer :: Repl v ()
clearBuffer = lift $ modify (\(s,env) -> (s{buffer = []},env))


printReport :: (MonadIO m, StringConv a String) =>
                     Report a i -> String -> m ()
printReport Report{..} input = do
  let toks = tokenize defaultPosition $ toS input
      linesInput = lines input
      (Token _ (Row row) _ (Col pos) _) = toks !! ((\a -> if a < 0 then 0 else a) $ position - 1)
      highlight' s = "\ESC[7m" ++ s ++ normal ++ fgcol red

  liftIO $ putStrLn $ (color red $ "\nParsing error in" ++ (if length linesInput > 1 then " line " ++ show (row+1) else "") ++ ":\n") 
    ++ (intercalate "\n" $ takeEnd 3 $ take (row+1) linesInput)
  liftIO $ putStrLn $ color red $ (replicate pos ' ') ++ "^" ++ "\nExpected one of the following here: " ++ intercalate ", " (map (highlight' . toS) expected)

-- Evaluation : handle each line user inputs
cmd :: forall v. (SingI v, Interpreter v) => String -> Repl v Bool
cmd input = do
  b <- getBuffer
  case b of
    [] -> case parse (program_Grammar @v) (map (fmap unTok)) (toS input) of
      Left r -> do
        printReport r $ input -- dropWhile (== '\n') $ dropWhileEnd (== '\n') input
        return False
      Right progs -> do
        env <- getEnv
        (output, env') <- liftIO $ capture $ runProg progs env
        liftIO $ putStr $ color blue output
        putEnv env'
        return True
    _ -> do
      appendBuffer [input ++ "\n"]
      return True

-- Prefix tab completeter
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":l"       , fileCompleter)
  , (":load"    , fileCompleter)
  , (":setLang" , listCompleter ["LamCBN", "LamCBV", "LamNat", "LamRec", "LamMem", "LamArray"])
  , (":verbose" , listCompleter ["true", "false"])
  ]

-- Default tab completer
byWord :: Monad m => WordCompleter m
byWord n = do
  let names = [":load", ":reload", ":quit", ":setLang", ":tree", ":tokens", ":info", ":env", ":verbose"]
  return $ filter (isPrefixOf n) names

-- Functions of the REPL
help :: String -> Repl v ()
help _ = liftIO $ putStrLn $ 
  "Commands available from the prompt:\n" ++
  "   <statement>                                            evaluate/run <statement>\n" ++
  "   :load <file>                                           load a file to interpret\n" ++
  "   :reload                                                reload a file\n" ++
  "   :setLang LamCBN/LamCBV/LamNat/LamRec/LamMem/LamArray   switch between different interpreters available\n" ++
  "   :verbose true/false                                    switch between the verbosity of error messages\n" ++
  "   :tree <statement>                                      print the parse tree of a given expression\n" ++
  "   :info <idemt>                                          print the parse tree of a definition in the environment\n" ++
  "   :tokens <statement>                                    print the tokenizer output of a given expression\n" ++
  "   :env                                                   print the current state of the global environment\n" ++
  "   :{                                                     start a multi-line block\n" ++
  "   :}                                                     end a multi-line block and evaluate\n" ++
  "   :quit                                                  exit λfun"


loadFile :: forall v. (SingI v, Interpreter v) => String -> Repl v ()
loadFile fileName' = do
  oldEnv <- getEnv
  putEnv (init_env @v, empty)
  let fileName = dropWhileEnd isSpace $ dropWhile isSpace fileName'
  contents <- liftIO $ readFile' $ fileName
  loaded <- cmd contents
  case loaded of 
    True -> do
      putCurrentFile $ Just fileName
      liftIO $ putStrLn $ "Succesfully parsed " ++ fileName ++ " ..."
    False -> do
      putEnv oldEnv
      return ()


reloadFile :: forall v. (SingI v, Interpreter v) => String -> Repl v ()
reloadFile _ = do
  f <- getCurrentFile
  case f of
    Just fileName -> do
      oldEnv <- getEnv
      putEnv (init_env @v, empty)
      contents <- liftIO $ readFile' fileName
      loaded <- cmd contents
      case loaded of 
        True -> do
          liftIO $ putStrLn $ "Succesfully parsed " ++ fileName ++ " ..."
        False -> do
          putCurrentFile $ Nothing
          putEnv oldEnv
          return ()
    Nothing -> return ()

quit :: String -> Repl v ()
quit _ = liftIO $ do
  putStrLn $ "Leaving LamFun ... goodbye."
  exitSuccess

parseTree :: forall v. SingI v => String -> Repl v ()
parseTree args = case parse (program_Grammar @v) (map (fmap unTok)) (toS args) of
  Left r -> printReport r args
  Right progs -> mapM_ pPrint progs



info :: String -> Repl v ()
info args = do
  (_, rawEnv) <- getEnv
  case Data.Map.lookup (toS $ args) rawEnv of
    Just t -> pPrint t
    Nothing -> liftIO $ putStrLn $ color red $ args ++ " has not been defined or is a built-in function ..."


setLang :: forall (v :: Version). Interpreter v => String -> Repl v ()
setLang args = do
  verbose <- getVerbose
  liftIO $ setLang_ args verbose (putStrLn $ "Setting language to " ++ (highlight args) ++ " ...")


setLang_ :: String -> Bool -> IO () -> IO ()
setLang_ version verbErrors greeting = case version of
  "LamCBN"   -> set @'LamCBN "LamCBN"
  "LamCBV"   -> set @'LamCBV "LamCBV"
  "LamNat"   -> set @'LamNat "LamNat"
  "LamRec"   -> set @'LamRec "LamRec"
  "LamMem"   -> set @'LamMem "LamMem"
  "LamArray" -> set @'LamArray "LamArray"
  _ -> putStrLn $ color red $ "\nChoose one of the following options: LamCBN, LamCBV LamNat, LamRec, LamMem, LamArray"
  where
    set :: forall (v :: Version). (SingI v, Interpreter v) => String -> IO ()
    set str = do
      writeFile ".lamfun" str
      flip evalStateT (Settings Nothing verbErrors [], (init_env @v, empty)) $
        evalRepl 
          (\_ -> prompt)
          (dontCrashVerbose . void . (cmd @v)) 
          (opts @v) 
          (Just ':')
          Nothing
          (Prefix (wordCompleter byWord) defaultMatcher) 
          (liftIO greeting)
          (liftIO (putStrLn "Goodbye!") >> return Exit)


  


tokens :: String -> Repl v ()
tokens args = pPrint $ tokenize defaultPosition (toS args)


dumpEnv :: forall v. Interpreter v => String -> Repl v ()
dumpEnv _ = do
  (e,_) <- getEnv
  liftIO $ putStrLn $ show_env e


setVerbose :: String -> Repl v ()
setVerbose args = case args of
  "false" -> putVerbose False
  _ -> putVerbose True


startBlock :: String -> Repl v ()
startBlock args = appendBuffer [args ++ "\n"]

endBlock :: (SingI v, Interpreter v) => String -> Repl v ()
endBlock _ = do
  buf <- getBuffer
  clearBuffer
  _ <- cmd $ concat buf
  return ()


opts :: forall v. (SingI v, Interpreter v) => [(String, String -> Repl v ())]
opts = [
    ("load", dontCrashVerbose . loadFile)    -- :say
  , ("reload", dontCrashVerbose . reloadFile)
  , ("quit", quit)
  , ("help", help)
  , ("tree", parseTree)
  , ("tokens", tokens)
  , ("info", info)
  , ("env", dumpEnv)
  , ("setLang", setLang)
  , ("verbose", setVerbose)
  , ("{", startBlock)
  , ("}", endBlock)
  ]

ini :: IO ()
ini = putStrLn "\n  $$$$\\        $$$$$$$$\\                     \n    $$ \\       $$  _____|                    \n     $$ \\      $$ |    $$\\   $$\\ $$$$$$$\\  \n     $$$ \\     $$$$$\\  $$ |  $$ |$$  __$$\\ \n    $$ $$ \\    $$  __| $$ |  $$ |$$ |  $$ |\n   $$ / $$ \\   $$ |    $$ |  $$ |$$ |  $$ |\n $$$ /   $$$\\  $$ |    \\$$$$$$  |$$ |  $$ |\n \\___|   \\___| \\__|     \\______/ \\__|  \\__|\n\nWelcome to λFun v3.14.1 ...\n"


prompt :: Repl v String
prompt = do
  buf <- getBuffer
  case buf of 
    [] -> return $ color blue $ "λλλ "
    _ ->  return $ " | "


dontCrashVerbose :: Repl v () -> Repl v ()
dontCrashVerbose m = do
   v <- getVerbose
   catch m ( \ e@SomeException{} -> 
      liftIO ( putStrLn ( color red $ "\n" ++ showVerbose v e ) ) 
    )
  where
    -- We cut of the CallStack ... bit of the error message for cleaner error
    -- ... a bit hacky ...
    showVerbose vrb e = 
      if vrb then show e 
      else head $ splitOn "\nCallStack" $ show e

main :: IO ()
main = do
  setLocaleEncoding utf8
  version <- handle (\(_::IOException) -> do
    putStrLn $ "Settings file not found ... defaulting to " ++ (highlight "LamRec")
    return "LamRec") $ do
    ver <- readFile' ".lamfun"
    putStrLn $ "Found settings file ... setting language to " ++ (highlight ver)
    return ver
  setLang_ version False ini