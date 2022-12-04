{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DataKinds, TypeFamilies, 
  BangPatterns, FlexibleContexts, TypeOperators #-}

module LamInterpreter() where

import Control.Monad.IO.Class(MonadIO, liftIO)
import LamFunSyntax
import LamFunInterpreter
import Environment
import Data.List(intersect, intercalate)
import Data.Map(toList, empty)
import Data.Set(Set)
import qualified Data.Set as S
import Data.String.Conv


type Expr = Expr_ 'LamCBN Ident
newtype Value = Value Expr
type Program = Program_ 'LamCBN Ident


instance Show Value where
  show (Value e) = show_ e
    where
      show_ (Variable_ x) = toS x
      show_ (Lambda_ i e) = "λ" ++ toS i ++ "." ++ show_ e
      show_ (App_ e@(Variable_ _) e') = show_ e ++ " " ++ show_ e'
      show_ (App_ e@(App_ _ _) e') = show_ e ++ " " ++ show_ e'
      show_ (App_ e e') = "(" ++ show_ e ++ ") " ++ show_ e'


evalCBV :: Expr -> Value
evalCBV e@(App_ e1 e2) = case evalCBV e1 of
    -- Haskell is lazy (similar to call-by-name) and to get the correct strict behaviour of
    -- call-by-value, we use the Bang pattern in `!e2'` to explicitly force evaluation.
    -- If we simply wrote `evalCBV (subst i (evalCBV e2) e1')` we would get the same 
    -- behaviour as `evalCBN`.
    Value (Lambda_ i e1') -> let !(Value e2') = evalCBV e2 in evalCBV $ subst i e2' e1'
    Value e1' -> Value $ App_ e1' e2
evalCBV x = Value x


evalCBN :: Expr -> Value
evalCBN (App_ e1 e2) = case evalCBN e1 of
    Value (Lambda_ i e1') -> evalCBN $ subst i e2 e1'
    Value e1' -> Value $ App_ e1' e2
evalCBN x = Value x


-- A quick and dirty way of getting fresh names. Rather inefficient for big terms...
fresh_ :: Expr -> Set Ident
fresh_ (Variable_ i) = S.singleton i
fresh_ (App_ e1 e2) = fresh_ e1 `S.union` fresh_ e2
fresh_ (Lambda_ i e) = S.insert i $ fresh_ e

fresh = (pickFresh $ infList 0) . fresh_
  where
    pickFresh :: [Ident] -> Set Ident -> Ident
    pickFresh (x:xs) ys | x `S.member` ys = pickFresh xs ys
                        | otherwise = x
    infList n = map (\a -> toS $ a : show n) ['a'..'z'] ++ infList (n+1)

subst :: Ident -> Expr -> Expr -> Expr
subst id s (Variable_ id') | id == id' = s
                           | otherwise = Variable_ id'
subst id s (App_ e1 e2) = App_ (subst id s e1) (subst id s e2)
subst id s e@(Lambda_ id' e') = 
    -- to avoid variable capture, we first substitute id' with a fresh name inside the body
    -- of the λ-abstraction, obtaining e''. 
    -- Only then do we proceed to apply substitution of the original s for id in the 
    -- body e''.
    let f = fresh e 
        e'' = subst id' (Variable_ f) e' in 
        Lambda_ f $ subst id s e''


liftExpr :: Expr_ 'LamCBV Ident -> Expr
liftExpr (Variable_ x) = Variable_ x
liftExpr (Lambda_ i x) = Lambda_ i $ liftExpr x
liftExpr (App_ x y) = App_ (liftExpr x) (liftExpr y)


newtype EnvCBN = EnvCBN ()
newtype EnvCBV = EnvCBV ()

instance Interpreter 'LamCBN where
  type GlobEnv 'LamCBN = EnvCBN

  init_env = EnvCBN ()

  show_env _ = ""

  runProg [] _ = return (init_env, empty)
  runProg (Calculate_ e:xs) _ = do
    putStrLn $ show $ evalCBN e
    runProg xs (init_env, empty)

instance Interpreter 'LamCBV where
  type GlobEnv 'LamCBV = EnvCBV

  init_env = EnvCBV ()

  show_env _ = ""

  runProg [] _ = return (init_env, empty)
  runProg (Calculate_ e:xs) _ = do
    putStrLn $ show $ evalCBV $ liftExpr e
    runProg xs (init_env, empty)


