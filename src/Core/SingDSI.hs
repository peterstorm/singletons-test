{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Core.SingDSI (
  SingDSI (..),
  withSingDSI,
) where

import Models.DoorState (DoorState (..), SDoorState (..))

class SingDSI s where
  singDS :: SDoorState s

instance SingDSI 'Opened where
  singDS = SOpened

instance SingDSI 'Closed where
  singDS = SClosed

instance SingDSI 'Locked where
  singDS = SLocked

withSingDSI :: SDoorState s -> (SingDSI s => r) -> r
withSingDSI sng x = case sng of
  SOpened -> x
  SClosed -> x
  SLocked -> x
