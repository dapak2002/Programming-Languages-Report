{-
 * This file has been copied and adapted from the materials written by 
 * Mike Spivey (https://spivey.oriel.ox.ac.uk/corner/Welcome_to_Spivey%27s_Corner)
 * for the POPL course at the Oxford University CS department.
-}

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DataKinds, TypeFamilies #-}

module LamNatInterpreter() where

import Control.Monad.IO.Class(MonadIO, liftIO)
import LamFunSyntax
import LamFunInterpreter
import Environment
import Data.List(intersect, intercalate)
import Data.Map(toList, insert)

data Value = IntVal Integer
  | BoolVal Bool
  | Nil | Cons Value Value
  | Function (Value -> Value)

instance Eq Value where
  IntVal a == IntVal b = a == b
  BoolVal a == BoolVal b = a == b
  Nil == Nil = True
  Cons h1 t1 == Cons h2 t2 = h1 == h2 && t1 == t2
  _ == _ = False

instance Show Value where
  show (IntVal n) = show n
  show (BoolVal b) = if b then "true" else "false"
  show Nil = "[]"
  show (Cons h t) = "[" ++ show h ++ shtail t ++ "]"
    where 
      shtail Nil = ""
      shtail (Cons h' t') = ", " ++ show h' ++ shtail t'
      shtail _ = error "undefined behaviour in shtail"
  show (Function _) = "<function>"

type Env = Environment Value
type Expr = Expr_ 'LamNat Ident
type Defn = Defn_ 'LamNat Ident
type Program = Program_ 'LamNat Ident



eval :: Expr -> Env -> Value
eval (Number_ n) _ = IntVal n
eval (Boolean_ b) _ = BoolVal b
eval (Variable_ x) env = find env x
eval (App_ f arg) env = case (eval f env, eval arg env) of
    (Function f', es') -> f' es'
    _ -> error "applying a non-function"
eval (Lambda_ x e) env = Function (\a -> eval e (define env x a))
eval Nil_ _ = Nil
eval (Cons_ a b) env = Cons (eval a env) (eval b env)


abstract :: Expr -> Env -> Value
abstract (Lambda_ x e@(Lambda_ _ _)) env =
  Function (\a -> (abstract e (define env x a)))
abstract (Lambda_ x e) env =
  Function (\a -> eval e (define env x a))
abstract _ _ = error "undefined behaviour in abstract"


unify_ :: Expr -> Value -> Maybe [(Ident, Value)]
unify_ (Variable_ i) v = Just [(i,v)]
unify_ (Number_ i) (IntVal j) | i == j = Just []
unify_ (Boolean_ b) (BoolVal c) | b == c = Just []
unify_ Nil_ Nil = Just []
unify_ (Cons_ x y) (Cons w v) = do
    u1 <- unify_ x w
    u2 <- unify_ y v
    case (map fst u1) `intersect` (map fst u2) of
        [] -> return $ u1 ++ u2
        xs -> error $ "names " ++ show xs ++ " appear multiple times in a pattern"
unify_ _ _ = Nothing

unify :: [(Maybe Expr, Expr)] -> Value -> Env -> Value
unify ((Nothing, e):_) _ env = eval e env
unify ((Just p, e):xs) v env = case unify_ p v of
    Just match -> eval e (defargs env (map fst match) (map snd match))
    Nothing -> unify xs v env


elab :: Defn -> Env -> Env
elab (Val_ x e) env = define env x (eval e env)


instance Interpreter 'LamNat where
  type GlobEnv 'LamNat = Env

  init_env =
    make_env [
      pureprim "+" (\ (IntVal a) -> Function (\ (IntVal b) -> IntVal (a + b))),
      pureprim "-" (\ (IntVal a) -> Function (\ (IntVal b) -> IntVal (a - b))),
      pureprim "*" (\ (IntVal a) -> Function (\ (IntVal b) -> IntVal (a * b))),
      pureprim "/" (\ (IntVal a) -> Function (\ (IntVal b) -> 
          if b == 0 then error "Dividing by zero" else IntVal (a `div` b))),
      pureprim "mod" (\ (IntVal a) -> Function (\ (IntVal b) -> 
          if b == 0 then error "Dividing by zero" else IntVal (a `mod` b))),
      pureprim "~" (\ (IntVal a) -> IntVal (- a)),
      pureprim "<" (\ (IntVal a) -> Function (\ (IntVal b) -> BoolVal (a < b))),
      pureprim "<=" (\ (IntVal a) -> Function (\ (IntVal b) -> BoolVal (a <= b))),
      pureprim ">" (\ (IntVal a) -> Function (\ (IntVal b) -> BoolVal (a > b))),
      pureprim ">=" (\ (IntVal a) -> Function (\ (IntVal b) -> BoolVal (a >= b))),
      pureprim "==" (\ (a :: Value) -> Function (\ (b :: Value) -> BoolVal (a == b))),
      pureprim "!=" (\ (a :: Value) -> Function (\ (b :: Value) -> BoolVal (a /= b))),
      pureprim "int?" (\ (a :: Value) ->
        case a of IntVal _ -> BoolVal True; _ -> BoolVal False),
      pureprim "head" (\ (Cons h _) -> h),
      pureprim "tail" (\ (Cons _ t) -> t)
      ]
    where
      constant x v = (x, v)
      pureprim x f = (x, Function (\args -> (f args)))

  show_env (Env env) = intercalate "\n" $ map (\(k,v) -> show k ++ " -> " ++ show v) $ toList env

  runProg [] env = return env
  runProg (Calculate_ e:xs) (env, rawEnv) = do
    let v = eval e env
    putStrLn $ show v
    runProg xs (env, rawEnv)
  runProg (Define_ def:xs) (env, rawEnv) =
    let env' = elab def env in
    runProg xs (env', insert (name def) def rawEnv)
    where
      name :: Defn -> Ident
      name (Val_ n _ ) = n
