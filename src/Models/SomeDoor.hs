{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Models.SomeDoor (
  mkSomeDoor,
  closeSomeOpenedDoor,
  lockAnySomeDoor,
  showSomeDoor,
  withDoor,
) where

import Data.Singletons (Sing, SingI (sing), SingKind (fromSing, toSing), SomeSing (SomeSing), withSingI, withSomeSing)
import Models.Door (DoorS (doorMaterialS), closeDoorS, lockAnyDoorS, mkDoorS, openAnyDoorS, unlockDoorS)
import Models.DoorState (DoorStateS (..), SDoorStateS (..))

data SomeDoor :: Type where
  MkSomeDoor :: Sing s -> DoorS s -> SomeDoor

data OldDoor :: Type where
  MkOldDoor :: DoorStateS -> String -> OldDoor

fromDoor :: Sing s -> DoorS s -> SomeDoor
fromDoor = MkSomeDoor

fromDoor' :: SingI s => DoorS s -> SomeDoor
fromDoor' = fromDoor sing

closeSomeOpenedDoor :: SomeDoor -> Maybe SomeDoor
closeSomeOpenedDoor (MkSomeDoor s d) = case s of
  SOpenedS -> Just . fromDoor' $ closeDoorS d
  SLockedS -> Nothing
  SClosedS -> Nothing

lockAnySomeDoor :: SomeDoor -> SomeDoor
lockAnySomeDoor (MkSomeDoor s d) = fromDoor' $ lockAnyDoorS s d

mkSomeDoor' :: DoorStateS -> String -> SomeDoor
mkSomeDoor' = \case
  OpenedS -> fromDoor' . mkDoorS SOpenedS
  ClosedS -> fromDoor' . mkDoorS SClosedS
  LockedS -> fromDoor' . mkDoorS SLockedS

showSomeDoor :: SomeDoor -> String
showSomeDoor = \case
  MkSomeDoor SOpenedS _ -> "Door is Opened"
  MkSomeDoor SClosedS _ -> "Door is Closed"
  MkSomeDoor SLockedS _ -> "Door is Locked"

withDoor' ::
  DoorStateS ->
  String ->
  (forall s. Sing s -> DoorS s -> r) ->
  r
withDoor' s m f = case s of
  OpenedS -> f SOpenedS $ mkDoorS SOpenedS m
  ClosedS -> f SClosedS $ mkDoorS SClosedS m
  LockedS -> f SLockedS $ mkDoorS SLockedS m

mkSomeDoor :: DoorStateS -> String -> SomeDoor
mkSomeDoor ds m = case toSing ds of
  SomeSing s -> fromDoor s $ mkDoorS s m

withDoor ::
  DoorStateS ->
  String ->
  (forall s. Sing s -> DoorS s -> r) ->
  r
withDoor ds m f = withSomeSing ds $ \s -> f s $ mkDoorS s m

fromOld :: OldDoor -> SomeDoor
fromOld (MkOldDoor ds m) = mkSomeDoor' ds m

toOld :: SomeDoor -> OldDoor
toOld (MkSomeDoor ds d) = MkOldDoor (fromSing ds) (doorMaterialS d)

unlockSomeDoor :: Int -> DoorS 'LockedS -> SomeDoor
unlockSomeDoor n d = case unlockDoorS n d of
  Nothing -> fromDoor' d
  Just v -> fromDoor' v

openAnySomeDoor :: Int -> SomeDoor -> SomeDoor
openAnySomeDoor n sd@(MkSomeDoor ds d) = withSingI ds $
  case openAnyDoorS n d of
    Nothing -> sd
    Just v -> fromDoor' v
