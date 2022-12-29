{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Models.DoorState (
  DoorState (..),
  SDoorState (..),
  DoorStateS (..),
  SDoorStateS (..),
) where

import Data.Singletons.TH (genSingletons)

data DoorState
  = Opened
  | Closed
  | Locked
  deriving (Eq, Show)

data SDoorState :: DoorState -> Type where
  SOpened :: SDoorState 'Opened
  SClosed :: SDoorState 'Closed
  SLocked :: SDoorState 'Locked

data DoorStateS
  = OpenedS
  | ClosedS
  | LockedS
  deriving (Eq, Show)

genSingletons [''DoorStateS]
