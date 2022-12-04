{-# LANGUAGE KindSignatures, DataKinds, TypeFamilies, TypeFamilyDependencies, 
    PolyKinds, GADTs, RankNTypes, DefaultSignatures #-}

module LamFunInterpreter where
import LamFunSyntax(Version, Program_, Defn_)
import Control.Monad.IO.Class(MonadIO)
import Data.Text(Text)
import Data.Singletons(SingI, Sing)
import Environment(Ident)
import Data.Map(Map)

type RawEnv v = Map Ident (Defn_ v Ident)

class Interpreter (v :: Version) where
    type GlobEnv v = r | r -> v

    init_env :: GlobEnv v
    show_env :: GlobEnv v -> String
    runProg :: [Program_ v Text] -> (GlobEnv v, RawEnv v) -> IO (GlobEnv v, RawEnv v)
