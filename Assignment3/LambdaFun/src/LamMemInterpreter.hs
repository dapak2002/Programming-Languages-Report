{-
 * This file has been copied and adapted from the materials written by 
 * Mike Spivey (https://spivey.oriel.ox.ac.uk/corner/Welcome_to_Spivey%27s_Corner)
 * for the POPL course at the Oxford University CS department.
-}

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DataKinds, TypeFamilies, BangPatterns #-}

module LamMemInterpreter() where

--import Control.Monad.IO.Class(MonadIO, liftIO)
import LamFunSyntax
import LamFunInterpreter
import Environment
import Memory
import Data.Text(Text)
import Data.List(intersect, intercalate)
import Data.Map(toList, insert)
import Data.String.Conv(toS)
import Data.Set(fromList, member)


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

new :: M Location
new mem = let (a, mem') = fresh mem in ("", a, mem')

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

showVal _ (IntVal n) = show n
showVal _ (BoolVal b) = if b then "true" else "false"
showVal True (StrVal s) = toS s
showVal False (StrVal s) = show s
showVal _ (Addr a) = "<address " ++ show a ++ ">"
showVal _ Nil = "[]"
showVal b (Cons h t) = "[" ++ showVal b h ++ shtail b t ++ "]"
  where 
    shtail _ Nil = ""
    shtail b (Cons h' t') = ", " ++ showVal b h' ++ shtail b t'
    shtail _ _ = error "undefined behaviour in shtail"
showVal _ (Function _) = "<function>"

type Env = Environment Value
type Mem = Memory Value
type Expr = Expr_ 'LamMem Ident
type Defn = Defn_ 'LamMem Ident
type Program = Program_ 'LamMem Ident



eval :: Expr -> Env -> M Value
eval (Number_ n) _ = result (IntVal n)
eval (Boolean_ b) _ = result (BoolVal b)
eval (StrLit_ b) _ = result (StrVal b)
eval (Variable_ x) env = result (find env x)
eval (App_ f arg) env = 
  eval f env $> (\f' -> case f' of
    Function f'' -> eval arg env $> (\arg' -> f'' arg')
    _ -> error "applying a non-function")
eval (Lambda_ x e) env = result $ Function (\a -> eval e (define env x a))
eval Nil_ _ = result Nil
eval (Cons_ a b) env = eval a env $> (\a' ->
    eval b env $> (\b' -> result (Cons a' b')))
eval (Case_ e cs) env = eval e env $> (\e' -> unify cs e' env)
eval (Let_ d e) env = elab d env $> (\env' -> eval e env')
eval (Contents_ e) env = eval e env $> (\e' -> case e' of
  Addr a -> get a
  _ -> error "trying to access a non-address")
eval (Assign_ e1 e2) env =
  eval e1 env $> (\v1 ->
    case v1 of
      Addr a ->
        eval e2 env $> (\v2 ->
          put a v2 $> (\() -> result v2))
      _ -> error "assigning to a non-address")
eval (Sequence_ e1 e2) env =
  eval e1 env $> (\v -> eval e2 env)
eval (While_ e1 e2) env = u
  where
    u = eval e1 env $> (\v1 ->
      case v1 of
        BoolVal True -> eval e2 env $> (\v2 -> u)
        BoolVal False -> result Nil
        _ -> error "boolean required in while loop")





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


newtype LamMemEnv = LamMemEnv (Env, Mem)
instance Interpreter 'LamMem where
  type GlobEnv 'LamMem = LamMemEnv

  init_env = LamMemEnv
    (make_env [
      pureprimBin "+" (\ (IntVal a) (IntVal b) -> IntVal (a + b)),
      pureprimBin "-" (\ (IntVal a) (IntVal b) -> IntVal (a - b)),
      pureprimBin "*" (\ (IntVal a) (IntVal b) -> IntVal (a * b)),
      pureprimBin "/" (\ (IntVal a) (IntVal b) -> 
          if b == 0 then error "Dividing by zero" else IntVal (a `div` b)),
      pureprimBin "mod" (\ (IntVal a) (IntVal b) -> 
          if b == 0 then error "Dividing by zero" else IntVal (a `mod` b)),
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
      primitive "print" (\ v -> output (showVal True v) $> (\ () -> result v)),
      primitive "println" (\ v -> output (showVal True v ++ "\n") $> (\ () -> result v)),
      primitive "new" (\ _ -> new $> (\a -> result (Addr a))),
      primitive "!" (\ (Addr a) -> get a)
      ], init_mem)
    where
      primitive x f = (x, Function f)
      pureprim x f = (x, Function (\args -> result (f args)))
      pureprimBin x f = (x, Function (\a -> result (Function (\b -> result (f a b)))))

  show_env (LamMemEnv (Env env, Mem (_,mem))) = 
    "Built in functions:\n" ++
    (intercalate ", " $ map (\(k,v) -> toS k) $ toList builtin) ++
    "\n\ESC[32mStack:\ESC[0m\n" ++ -- prints green https://replit.com/@alexhkurz/ColouredHaskellOutput#Main.hs
    (intercalate "\n" $ map (\(k,v) -> toS k ++ " = " ++ showVal False v) $ filter (\(k,_) -> not $ k `member` builtin_set) $ toList env) ++
    "\n\ESC[32mHeap:\ESC[0m\n" ++ -- prints green
    (intercalate "\n" $ map (\(k,v) -> show k ++ " -> " ++ case v of {Just v' -> showVal False v'; _ -> "un-initialized"}) $ toList mem)

    where
      LamMemEnv (Env builtin, _) = init_env
      builtin_set = fromList $ map fst $ toList builtin

  runProg [] env = return env
  runProg (Calculate_ e:xs) (LamMemEnv (env, mem), rawEnv) = do
    let (out, v, !mem') = eval e env mem
    putStrLn $ out
    putStrLn $ showVal False v
    runProg xs (LamMemEnv (env, mem'), rawEnv)
  runProg (Define_ def:xs) (LamMemEnv (env, mem), rawEnv) = do
    let (out, env', !mem') = elab def env mem
    case out of 
      "" -> return ()
      _ -> putStrLn $ out
    runProg xs (LamMemEnv (env', mem') , insert (name def) def rawEnv)
    where
      name :: Defn -> Ident
      name (Val_ n _ ) = n
      name (Rec_ n _ ) = n 
