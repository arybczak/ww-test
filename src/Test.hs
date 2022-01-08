{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Test where

import GHC.Exts
import Effectful

data E1 :: Effect where
  E1 :: Int# -> E1 m ()

data E2 :: Effect where
  E2 :: Int# -> E2 m ()

data E3 :: Effect where
  E3 :: Int# -> E3 m ()

type instance DispatchOf E1 = 'Dynamic
type instance DispatchOf E2 = 'Dynamic
type instance DispatchOf E3 = 'Dynamic

fun :: (E1 :> es, E2 :> es, E3 :> es) => Int -> Int -> Int -> Eff es ()
fun (I# i1) (I# i2) (I# i3) = do
  send $ E1 i1
  send $ E2 i2
  send $ E3 i3
