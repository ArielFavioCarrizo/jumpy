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

data TaggedNode ni a = TaggedNode ni a

data ModuleId e = ModuleId Int
data TypeClassId e = TypeClassId Int
data TypeId e = TypeId Int
data FunId e = FunId Int
data StateId e = StateId Int
data VarId e = VarId Int

data Entity e = 
   ModuleEntity (ModuleId e) |
   TypeClassEntity (TypeClassId e) |
   TypeEntity (TypeId e) |
   FunEntity (FunId e) |
   StateEntity (StateId e) |
   VarEntity (VarId e)

data EntityName = GlobalEntityName String | LocalEntityName String

data MaybeInstrScope e ni o where
   JustInstrScope :: Asg e ni o -> MaybeInstrScope e ni o
   NothingInstrScope :: MaybeInstrScope e ni ()

data EntityDecl e ni o =
   ModuleDecl String ( Asg e ni o ) |
   FunDecl String ( MaybeInstrScope e ni o ) -- Incomplete

data Asg e ni o where
   RootModule :: (ModuleId e -> Asg e ni o) -> Asg e ni o
   FindEntityByName :: EntityName -> (Maybe (TaggedNode ni (Entity e)) -> Asg e ni o) -> Asg e ni o
   DeclareEntity :: TaggedNode ni (EntityDecl e ni o) -> Asg e ni o
