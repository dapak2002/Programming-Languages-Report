{-
 * This file has been copied and adapted from the materials written by 
 * Mike Spivey (https://spivey.oriel.ox.ac.uk/corner/Welcome_to_Spivey%27s_Corner)
 * for the POPL course at the Oxford University CS department.
-}

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DataKinds, TypeFamilies, BangPatterns #-}

module LamArrayInterpreter() where

import Control.Monad.IO.Class(MonadIO, liftIO)
import LamFunSyntax
import LamFunInterpreter
import Environment
import Memory
import Data.Text(Text)
import Data.List(intersect, intercalate)
import Data.Map(toList, insert)
import Data.String.Conv(toS)


-- MONAD

type M a = Mem -> (String, a, Mem)

result :: a -> M a
result x mem = ("", x, mem)

($>) :: M a -> (a -> M b) -> M b
(xm $> f) mem =
  let (s1, x, mem') = xm mem in
  let (s2, y, mem'') = (f x) mem' in
  (s1++s2, y, mem'')

output :: String -> M ()
output s mem = (s, (), mem)

new :: Integer -> M Location
new n mem | n < 1 = error "calling new with n < 1 ..." 
new 1 mem = let (a, mem') = fresh mem in ("", a, mem')
new n mem = 
  let (a, mem') = fresh mem
      (_, _, mem'') = new (n-1) mem'
   in ("", a, mem'')

get :: Location -> M Value
get a mem = ("", contents mem a, mem)

put :: Location -> Value -> M ()
put a v mem = ("", (), update mem a v)



data Value = IntVal Integer
  | BoolVal Bool
  | StrVal Text
  | Addr Location
  | Nil | Cons Value Value
  | Function (Value -> M Value)

instance Eq Value where
  IntVal a == IntVal b = a == b
  BoolVal a == BoolVal b = a == b
  StrVal a == StrVal b = a == b
  Addr a == Addr b = a == b
  Nil == Nil = True
  Cons h1 t1 == Cons h2 t2 = h1 == h2 && t1 == t2
  _ == _ = False

instance Show Value where
  show (IntVal n) = show n
  show (BoolVal b) = if b then "true" else "false"
  show (StrVal s) = toS s
  show (Addr a) = "<address " ++ show a ++ ">"
  show Nil = "[]"
  show (Cons h t) = "[" ++ show h ++ shtail t ++ "]"
    where 
      shtail Nil = ""
      shtail (Cons h' t') = ", " ++ show h' ++ shtail t'
      shtail _ = error "undefined behaviour in shtail"
  show (Function _) = "<function>"

type Env = Environment Value
type Mem = Memory Value
type Expr = Expr_ 'LamArray Ident
type Defn = Defn_ 'LamArray Ident
type Program = Program_ 'LamArray Ident



eval :: Expr -> Env -> M Value
eval = undefined


abstract :: Expr -> Env -> Value
abstract (Lambda_ x e@(Lambda_ _ _)) env =
  Function (\a -> result (abstract e (define env x a)))
abstract (Lambda_ x e) env =
  Function (\a -> eval e (define env x a))
abstract _ _ = error "undefined behaviour in abstract"


unify_ :: Expr -> Value -> Maybe [(Ident, Value)]
unify_ (Variable_ i) v = Just [(i,v)]
unify_ (Number_ i)  (IntVal j)  | i == j = Just []
unify_ (Boolean_ b) (BoolVal c) | b == c = Just []
unify_ (StrLit_ s)  (StrVal t) | s == t = Just []
unify_ Nil_ Nil = Just []
unify_ (Cons_ x y) (Cons w v) = do
    u1 <- unify_ x w
    u2 <- unify_ y v
    case (map fst u1) `intersect` (map fst u2) of
        [] -> return $ u1 ++ u2
        xs -> error $ "names " ++ show xs ++ " appear multiple times in a pattern"
unify_ _ _ = Nothing

unify :: [(Maybe Expr, Expr)] -> Value -> Env -> M Value
unify ((Nothing, e):_) _ env = eval e env
unify ((Just p, e):xs) v env = case unify_ p v of
    Just match -> eval e (defargs env (map fst match) (map snd match))
    Nothing -> unify xs v env


elab :: Defn -> Env -> M Env
elab (Val_ x e) env = eval e env $> (\v -> result (define env x v))
elab (Rec_ x e) env =
  case e of
    Lambda_ _ _ ->
      result env' where env' = define env x (abstract e env')
    _ ->
      error "RHS of letrec must be a lambda"


newtype LamArrayEnv  = LamArrayEnv  (Env, Mem)
instance Interpreter 'LamArray where
  type GlobEnv 'LamArray = LamArrayEnv 

  init_env = LamArrayEnv 
    (make_env [
      pureprimBin "+" (funInts (+)),
      pureprimBin "-" (funInts (-)),
      pureprimBin "*" (funInts (*)),
      pureprimBin "/" (funInts $ \ a b -> 
          if b == 0 then error "Dividing by zero" else a `div` b),
      pureprimBin "mod" (funInts $ \ a b -> 
          if b == 0 then error "Dividing by zero" else a `mod` b),
      pureprim "~" (\ (IntVal a) -> IntVal (- a)),
      pureprimBin "<" (\ (IntVal a) (IntVal b) -> BoolVal (a < b)),
      pureprimBin "<=" (\ (IntVal a) (IntVal b) -> BoolVal (a <= b)),
      pureprimBin ">" (\ (IntVal a) (IntVal b) -> BoolVal (a > b)),
      pureprimBin ">=" (\ (IntVal a) (IntVal b) -> BoolVal (a >= b)),
      pureprimBin "==" (\ (a :: Value) (b :: Value) -> BoolVal (a == b)),
      pureprimBin "!=" (\ (a :: Value) (b :: Value) -> BoolVal (a /= b)),
      pureprim "int?" (\ (a :: Value) ->
        case a of IntVal _ -> BoolVal True; _ -> BoolVal False),
      pureprim "head" (\ (Cons h _) -> h),
      pureprim "tail" (\ (Cons _ t) -> t),
      primitive "print" (\ v -> output (show v) $> (\ () -> result v)),
      primitive "println" (\ v -> output (show v ++ "\n") $> (\ () -> result v)),
      primitive "new" (\ x -> case x of 
        Cons (IntVal n) Nil -> new n $> (\a -> result (Addr a)) ; 
        _ -> new 1 $> (\a -> result (Addr a))),
      primitive "!" (\ (Addr a) -> get a),
      primitive "addr" (\ (IntVal a) -> result $ Addr $ Loc a)
      ], init_mem)
    where
      primitive x f = (x, Function f)
      pureprim x f = (x, Function (\args -> result (f args)))
      pureprimBin x f = (x, Function (\a -> result (Function (\b -> result (f a b)))))

      funInts op x y = case (x,y) of 
        (IntVal a,IntVal b) -> IntVal (a `op` b)
        (Addr (Loc a),Addr (Loc b)) -> Addr $ Loc (a `op` b)
        _ -> error "cannot mix int and address arguments; use addr to promote the int argument to an address"

  show_env (LamArrayEnv (Env env, Mem (_,mem))) = 
    "Env:\n" ++
    (intercalate "\n" $ map (\(k,v) -> show k ++ " -> " ++ show v) $ toList env) ++
    "\nMemory:\n" ++
    (intercalate "\n" $ map (\(k,v) -> show k ++ " -> " ++ show v) $ toList mem)

  runProg [] env = return env
  runProg (Calculate_ e:xs) (LamArrayEnv (env, mem), rawEnv) = do
    let (out, v, !mem') = eval e env mem
    putStrLn $ out
    putStrLn $ show v
    runProg xs (LamArrayEnv  (env, mem'), rawEnv)
  runProg (Define_ def:xs) (LamArrayEnv  (env, mem), rawEnv) = do
    let (out, env', !mem') = elab def env mem
    case out of 
      "" -> return ()
      _ -> putStrLn $ out
    runProg xs (LamArrayEnv  (env', mem') , insert (name def) def rawEnv)
    where
      name :: Defn -> Ident
      name (Val_ n _ ) = n
      name (Rec_ n _ ) = n 
