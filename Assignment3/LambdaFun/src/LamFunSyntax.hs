{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveAnyClass, DataKinds, 
  KindSignatures, TypeOperators, TypeFamilies, GADTs, StandaloneDeriving, 
  ExplicitForAll, TypeApplications, ScopedTypeVariables, TemplateHaskell #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module LamFunSyntax where
import Data.Hashable(Hashable)
import GHC.Generics(Generic)
import Unsafe.Coerce(unsafeCoerce)
import Data.Singletons.TH

$(singletons [d| data Version = LamCBN | LamCBV | LamNat | LamRec | LamMem | LamArray |])
deriving instance Show Version
deriving instance Eq Version

type family (a :: Version) <: (b :: Version) where
  a <: a = 'True
  'LamCBN <: a = 'True
  'LamCBV <: a = 'True
  'LamNat <: 'LamRec = 'True
  'LamNat <: 'LamMem = 'True
  'LamNat <: 'LamArray = 'True
  'LamRec <: 'LamMem = 'True
  'LamRec <: 'LamArray = 'True
  'LamMem <: 'LamArray = 'True
  a <: b = 'False

data Expr_ :: Version -> * -> * where
  Variable_ :: a -> Expr_ v a
  Lambda_   :: a  -> Expr_ v a -> Expr_ v a
  App_      :: Expr_ v a -> Expr_ v a -> Expr_ v a
  Number_   :: (LamNat <: v ~ 'True) => Integer -> Expr_ v a 
  Nil_      :: (LamNat <: v ~ 'True) => Expr_ v a
  Cons_     :: (LamNat <: v ~ 'True) => Expr_ v a -> Expr_ v a -> Expr_ v a
  Boolean_  :: (LamNat <: v ~ 'True) => Bool -> Expr_ v a 
  Case_     :: (LamRec <: v ~ 'True) => Expr_ v a -> [(Maybe (Expr_ v a), Expr_ v a)] -> Expr_ v a
  Let_      :: (LamRec <: v ~ 'True) => Defn_ v a -> Expr_ v a -> Expr_ v a
  Assign_   :: (LamMem <: v ~ 'True) => Expr_ v a -> Expr_ v a -> Expr_ v a -- e := e'
  While_    :: (LamMem <: v ~ 'True) => Expr_ v a -> Expr_ v a -> Expr_ v a -- while e do {e'}
  Contents_ :: (LamMem <: v ~ 'True) => Expr_ v a -> Expr_ v a -- '*e'
  Sequence_ :: (LamMem <: v ~ 'True) => Expr_ v a -> Expr_ v a -> Expr_ v a -- e;e'
  StrLit_   :: (LamMem <: v ~ 'True) => a -> Expr_ v a -- "string"


deriving instance Show a => Show (Expr_ v a)
deriving instance Eq a => Eq (Expr_ v a)
deriving instance Ord a => Ord (Expr_ v a)
deriving instance Functor (Expr_ v)

data Defn_ :: Version -> * -> * where
  Val_ :: a -> Expr_ v a -> Defn_ v a
  Rec_ :: (LamRec <: v ~ 'True) =>  a -> Expr_ v a -> Defn_ v a

deriving instance Show a => Show (Defn_ v a)
deriving instance Eq a => Eq (Defn_ v a)
deriving instance Ord a => Ord (Defn_ v a)
deriving instance Functor (Defn_ v)

data Program_ :: Version -> * -> * where
  Calculate_ :: Expr_ v a -> Program_ v a
  Define_    :: (LamNat <: v ~ 'True) => Defn_ v a -> Program_ v a

deriving instance Show a => Show (Program_ v a)
deriving instance Eq a => Eq (Program_ v a)
deriving instance Ord a => Ord (Program_ v a)
deriving instance Functor (Program_ v)



unsafeMkNumber_ :: forall v a. SingI v => Integer -> Expr_ v a
unsafeMkNumber_ b = case sing @v of
    SLamCBN -> error "Trying to make a Number in the wrong version"
    SLamCBV -> error "Trying to make a Number in the wrong version"
    _ -> unsafeCoerce (Number_ :: Integer -> Expr_ 'LamNat a) b

unsafeMkBoolean_ :: forall v a. SingI v => Bool -> Expr_ v a
unsafeMkBoolean_ b = case sing @v of
    SLamCBN -> error "Trying to make a Boolean in the wrong version"
    SLamCBV -> error "Trying to make a Boolean in the wrong version"
    _ -> unsafeCoerce (Boolean_ :: Bool -> Expr_ 'LamNat a) b


unsafeMkNil_ :: forall v a. SingI v => Expr_ v a
unsafeMkNil_ = case sing @v of
    SLamCBN -> error "Trying to make a Nil in the wrong version"
    SLamCBV -> error "Trying to make a Nil in the wrong version"
    _ -> unsafeCoerce (Nil_ :: Expr_ 'LamNat a)

unsafeMkCons_ :: forall v a. SingI v => Expr_ v a ->  Expr_ v a -> Expr_ v a
unsafeMkCons_ b c = case sing @v of
    SLamCBN -> error "Trying to make a Cons in the wrong version"
    SLamCBV -> error "Trying to make a Cons in the wrong version"
    _ -> unsafeCoerce (Cons_ :: Expr_ 'LamNat a -> Expr_ 'LamNat a -> Expr_ 'LamNat a) b c


unsafeMkCase_ :: forall v a. SingI v => Expr_ v a -> [(Maybe (Expr_ v a), Expr_ v a)] -> Expr_ v a
unsafeMkCase_ b cs = case sing @v of
    SLamCBN -> error "Trying to make a Cons in the wrong version"
    SLamCBV -> error "Trying to make a Cons in the wrong version"
    SLamNat -> error "Trying to make a Cons in the wrong version"
    _ -> unsafeCoerce (Case_ :: Expr_ 'LamRec a -> [(Maybe (Expr_ 'LamRec a), Expr_ 'LamRec a)] -> Expr_ 'LamRec a) b cs
   

unsafeMkLet_ :: forall v a. SingI v => Defn_ v a -> Expr_ v a -> Expr_ v a
unsafeMkLet_ a b = case sing @v of
    SLamCBN -> error "Trying to make a Let in the wrong version"
    SLamCBV -> error "Trying to make a Let in the wrong version"
    SLamNat -> error "Trying to make a Let in the wrong version"
    _ -> unsafeCoerce (Let_ :: Defn_ 'LamRec a -> Expr_ 'LamRec a -> Expr_ 'LamRec a) a b
    
unsafeMkAssign_ :: forall v a. SingI v => Expr_ v a -> Expr_ v a -> Expr_ v a
unsafeMkAssign_ a b = case sing @v of
    SLamMem -> Assign_ a b
    SLamArray -> Assign_ a b
    _ -> error "Trying to make an Assign in the wrong version"

unsafeMkContents_ :: forall v a. SingI v => Expr_ v a -> Expr_ v a
unsafeMkContents_ a = case sing @v of
    SLamMem -> Contents_ a
    SLamArray -> Contents_ a
    _ -> error "Trying to make an Contents in the wrong version"

unsafeMkRec_ :: forall v a. SingI v => a -> Expr_ v a -> Defn_ v a
unsafeMkRec_ a b = case sing @v of
    SLamCBN -> error "Trying to make a Let in the wrong version"
    SLamCBV -> error "Trying to make a Let in the wrong version"
    SLamNat -> error "Trying to make a Let in the wrong version"
    _ -> unsafeCoerce (Rec_ :: a -> Expr_ 'LamRec a -> Defn_ 'LamRec a) a b
    
unsafeMkSequence_ :: forall v a. SingI v => Expr_ v a -> Expr_ v a -> Expr_ v a
unsafeMkSequence_ a b = case sing @v of
    SLamMem -> Sequence_ a b
    SLamArray -> Sequence_ a b
    _ -> error "Trying to make an Sequence in the wrong version"

unsafeMkWhile_ :: forall v a. SingI v => Expr_ v a -> Expr_ v a -> Expr_ v a
unsafeMkWhile_ a b = case sing @v of
    SLamMem -> While_ a b
    SLamArray -> While_ a b
    _ -> error "Trying to make an While in the wrong version"

unsafeMkStrLit_ :: forall v a. SingI v => a -> Expr_ v a
unsafeMkStrLit_ a = case sing @v of
    SLamMem -> StrLit_ a
    SLamArray -> StrLit_ a
    _ -> error "Trying to make an StrLit in the wrong version"


unsafeMkDefine_ :: forall v a. SingI v => Defn_ v a -> Program_ v a
unsafeMkDefine_ b = case sing @v of
    SLamCBN -> error "Trying to make a Define in the wrong version"
    SLamCBV -> error "Trying to make a Define in the wrong version"
    _ -> unsafeCoerce (Define_ :: Defn_ 'LamNat a -> Program_ 'LamNat a) b

