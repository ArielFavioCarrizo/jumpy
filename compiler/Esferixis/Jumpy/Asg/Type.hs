-- |
-- Module      :  Esferixis.Jumpy.Ast.Struct
-- Copyright   :  (c) 2019 Ariel Favio Carrizo
-- License     :  BSD-3-Clause
-- Stability   : experimental
-- Portability : ghc

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}

module Esferixis.Jumpy.Asg.Type where

import qualified Data.IntMap as IMap
import qualified Data.IntSet as ISet

import Data.Either
import Data.Maybe

data Id = Id Int
data TypeVar x = TypeVar Id
data Constraint x = Constraint Id (TypeClass x) [(TypeVar x, TypeVar x)]
data FunctionalDependency x = FunctionalDependency Id (TypeVar x) (TypeVar x)
data Type x = Type Id
data TypeClass x = TypeClass Id (TypeVar x) [Constraint x] [FunctionalDependency x]
data TypeInstance x = TypeInstance Id (Constraint x) [TypeVar x]

data TypeVarValue x = TypeVarValue (TypeVar x) (Type x)

data NewInstanceException =
   DependencyHasBeenDefined

data TypeGen x r where
   TypeGenNewTypeVar :: TypeGen x (TypeVar x)
   TypeGenNewConstraint :: (TypeClass x) -> [(TypeVar x, TypeVar x)] -> TypeGen x (Constraint x)
   TypeGenNewFunctionalDependency :: TypeVar x -> TypeVar x -> TypeGen x (FunctionalDependency x)
   TypeGenNewType :: [TypeVar x] -> [Constraint x] -> TypeGen x (Type x)
   TypeGenNewTypeClass :: [TypeVar x] -> [Constraint x] -> [FunctionalDependency x] -> TypeGen x (TypeClass x)
   TypeGenNewInstance :: [Constraint x] -> [TypeVar x] -> TypeGen x (TypeInstance x)
   TypeGenBind :: ( s -> TypeGen x r ) -> TypeGen x r
   TypeGenReturn :: r -> TypeGen x r

newTypeVar = TypeGenNewTypeVar
newConstraint = TypeGenNewConstraint
newFunctionalDependency = TypeGenNewFunctionalDependency
newType = TypeGenNewType
newTypeClass = TypeGenNewTypeClass
newInstance = TypeGenNewInstance

data TypeGenState x = TypeGenState {
   ownedTypeVars :: ISet.IntSet
   }
