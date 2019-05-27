-- |
-- Module      :  Esferixis.Jumpy.Asg
-- Copyright   :  (c) 2019 Ariel Favio Carrizo
-- License     :  BSD-3-Clause
-- Stability   : experimental
-- Portability : ghc

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}

module Esferixis.Jumpy.Var where

import Text.Show
import Data.Word
import Data.Maybe
import Data.Either
import Data.Int

data VarRef x a = VarRef Int

data Var x where
   NewVarRef :: a -> VarRef x a
   DeleteVarRef :: a -> 
