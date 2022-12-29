{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Models.List (
  ) where

import Data.Singletons (Sing, SingKind (..), SomeSing (SomeSing), withSomeSing)

data List a
  = Nil
  | Cons a (List a)

data SList :: List a -> Type where
  SNil :: SList 'Nil
  SCons :: Sing x -> SList xs -> SList ( 'Cons x xs)

type instance Sing = SList

instance SingKind k => SingKind (List k) where
  type Demote (List k) = List (Demote k)

  fromSing :: Sing (xs :: List k) -> List (Demote k)
  fromSing = \case
    SNil -> Nil
    SCons x xs -> Cons (fromSing x) (fromSing xs)

  toSing :: List (Demote k) -> SomeSing (List k)
  toSing = \case
    Nil -> SomeSing SNil
    Cons x xs -> withSomeSing x $ \sx ->
      withSomeSing xs $ \sxs ->
        SomeSing $ SCons sx sxs
