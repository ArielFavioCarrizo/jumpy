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

data Module = Module

data ModuleMember = 
   TypeClassModuleMember |
   TypeModuleMember |
   FunModuleMember |
   StateModuleMember |
   VarModuleMember

data Symbol where
   ModuleSymbol :: Module -> Symbol
   ModuleMemberSymbol :: ModuleMember -> Symbol

data Asg e li o where
   FindSymbol :: String -> (ModuleMember -> Asg e li o) -> Asg e li o
