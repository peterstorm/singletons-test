{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module SingletonsTest (
  mainFunc,
) where

import Data.List.Singletons (SList (..))
import Data.Singletons (SingKind (fromSing))
import Models.Door (Door (UnsafeMkDoor), closeDoor, doorStatus, doorStatus', lockAnyDoor, mkDoor, mkDoorS, openAnyDoorS, showAnyDoorS, unlockDoor)
import Models.DoorState (DoorState (..), DoorStateS (OpenedS), SDoorState (..), SDoorStateS (..))
import Models.SomeDoor (mkSomeDoor, showSomeDoor, withDoor)

mainFunc :: IO ()
mainFunc = do
  let list = SOpenedS `SCons` SClosedS `SCons` SLockedS `SCons` SNil
  let sing = fromSing SOpenedS
  let openedDoor = UnsafeMkDoor @'Opened "birch"
  let closedDoor = closeDoor openedDoor
  let doorState = doorStatus SClosed closedDoor
  let doorState' = doorStatus SLocked $ lockAnyDoor SOpened openedDoor
  let doorState'' = doorStatus' openedDoor
  let door = mkDoor SClosed "wood"
  let lockedDoor = mkDoor SLocked "wood"
  let unlockedDoorNothing = unlockDoor 2 lockedDoor
  let unlockedDoorSome = unlockDoor 3 lockedDoor
  let lockedDoorS = mkDoorS SLockedS "wood"
  let someDoor = mkSomeDoor OpenedS "wood"
  putStrLn $ show doorState
  putStrLn $ show doorState'
  putStrLn $ show doorState''
  putStrLn $ show $ doorStatus' door
  putStrLn $ show sing
  putStrLn $ show $ fmap doorStatus' unlockedDoorNothing
  putStrLn $ show $ fmap doorStatus' unlockedDoorSome
  putStrLn $ "This is a singleton door that is: " <> showAnyDoorS lockedDoorS
  putStrLn $ "This door is being opened by openAnyDoorS: " <> (fromMaybe "failed" $ fmap showAnyDoorS $ openAnyDoorS 1 lockedDoorS)
  putStrLn $ "This door is created from 'mkSomeDoor': " <> (showSomeDoor someDoor)
  putStrLn $
    withDoor OpenedS "wood" $ \s _ -> case s of
      SOpenedS -> "Door is Opened"
      SClosedS -> "Door is Closed"
      SLockedS -> "Door is Locked"
