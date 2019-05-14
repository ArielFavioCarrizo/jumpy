-- |
-- Module      :  Esferixis.Jumpy.AST
-- Copyright   :  (c) 2019 Ariel Favio Carrizo
-- License     :  BSD-3-Clause
-- Stability   : experimental
-- Portability : ghc

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}

module Esferixis.Jumpy.Asg where

import Text.Show
import Data.Word
import Data.Maybe
import Data.Int

class (Show ni) => JLocInfo ni

data Entity = 
   TypeClassEntity |
   TypeEntity |
   FunEntity |
   StateEntity |
   VarEntity

data Asg e li o where
   FindEntityByName :: String -> (Entity -> Asg e li o) -> Asg e li o
