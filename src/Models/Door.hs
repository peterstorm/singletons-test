{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Models.Door (
  Door (..),
  DoorS (..),
  mkDoor,
  mkDoorS,
  closeDoor,
  closeDoorS,
  doorStatus,
  doorStatus',
  lockAnyDoor,
  lockAnyDoor',
  lockAnyDoorS,
  unlockDoor,
  openAnyDoorS,
  unlockDoorS,
  showAnyDoorS,
) where

import Core.SingDSI (SingDSI (..), withSingDSI)
import Data.Singletons (Sing, SingI (sing))
import Models.DoorState (DoorState (..), DoorStateS (..), SDoorState (..), SDoorStateS (..))

data Door :: DoorState -> Type where
  UnsafeMkDoor :: {doorMaterial :: String} -> Door s

data DoorS :: DoorStateS -> Type where
  UnsafeMkDoorS :: {doorMaterialS :: String} -> DoorS s

mkDoor :: SDoorState s -> String -> Door s
mkDoor _ = UnsafeMkDoor

mkDoorS :: Sing s -> String -> DoorS s
mkDoorS _ = UnsafeMkDoorS

closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor d) = UnsafeMkDoor d

closeDoorS :: DoorS 'OpenedS -> DoorS 'ClosedS
closeDoorS (UnsafeMkDoorS d) = UnsafeMkDoorS d

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor (UnsafeMkDoor d) = UnsafeMkDoor d

lockDoorS :: DoorS 'ClosedS -> DoorS 'LockedS
lockDoorS (UnsafeMkDoorS d) = UnsafeMkDoorS d

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor d) = UnsafeMkDoor d

openDoorS :: DoorS 'ClosedS -> DoorS 'OpenedS
openDoorS (UnsafeMkDoorS d) = UnsafeMkDoorS d

lockAnyDoor :: SDoorState s -> (Door s -> Door 'Locked)
lockAnyDoor = \case
  SOpened -> lockDoor . closeDoor
  SClosed -> lockDoor
  SLocked -> id

lockAnyDoorS :: SDoorStateS s -> (DoorS s -> DoorS 'LockedS)
lockAnyDoorS = \case
  SOpenedS -> lockDoorS . closeDoorS
  SClosedS -> lockDoorS
  SLockedS -> id

lockAnyDoor' :: SingDSI s => Door s -> Door 'Locked
lockAnyDoor' = lockAnyDoor singDS

lockAnyDoor'' :: SDoorState s -> Door s -> Door 'Locked
lockAnyDoor'' s d = withSingDSI s $ lockAnyDoor' d

fromSDoorState :: SDoorState s -> DoorState
fromSDoorState SOpened = Opened
fromSDoorState SClosed = Closed
fromSDoorState SLocked = Locked

doorStatus :: SDoorState s -> Door s -> DoorState
doorStatus s _ = fromSDoorState s

doorStatus' :: SingDSI s => Door s -> DoorState
doorStatus' = doorStatus singDS

doorStatusS :: Sing s -> DoorS s -> DoorStateS
doorStatusS SOpenedS _ = OpenedS
doorStatusS SClosedS _ = ClosedS
doorStatusS SLockedS _ = LockedS

doorStatusS' :: SingI s => DoorS s -> DoorStateS
doorStatusS' = doorStatusS sing

unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)
unlockDoor code (UnsafeMkDoor d) = case code `mod` 2 of
  1 -> Just $ UnsafeMkDoor d
  _ -> Nothing

unlockDoorS :: Int -> DoorS 'LockedS -> Maybe (DoorS 'ClosedS)
unlockDoorS code (UnsafeMkDoorS d) = case code `mod` 2 of
  0 -> Nothing
  1 -> Just $ UnsafeMkDoorS d
  _ -> Nothing

openAnyDoorS :: SingI s => Int -> DoorS s -> Maybe (DoorS 'OpenedS)
openAnyDoorS n = openAnyDoorS' sing
 where
  openAnyDoorS' :: Sing s -> DoorS s -> Maybe (DoorS 'OpenedS)
  openAnyDoorS' = \case
    SOpenedS -> Just
    SClosedS -> Just . openDoorS
    SLockedS -> fmap openDoorS . (unlockDoorS n)

showAnyDoorS :: SingI s => DoorS s -> String
showAnyDoorS d = show $ doorStatusS' d
