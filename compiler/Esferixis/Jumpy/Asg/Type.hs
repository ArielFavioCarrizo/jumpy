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
import Data.Either
import Data.Maybe

data Id = Id Int
data TypeVar x = TypeVar Id
data Constraint x = Constraint Id (TypeClass x) [(TypeVar x, TypeVar x)]
data FunctionalDependency x = FunctionalDependency Id (TypeVar x) (TypeVar x)
data Type x = Type Id
data TypeClass x = TypeClass Id (TypeVar x) [Constraint x] [FunctionalDependency x]
data TypeInstance x = TypeInstance Id (Constraint x) [TypeVar x]

data NewInstanceException =
   DependencyHasBeenDefined

data TypeGen x r where
   NewTypeVar :: TypeGen x (TypeVar x)
   NewConstraint :: (TypeClass x) -> [(TypeVar x, TypeVar x)] -> TypeGen x (Constraint x)
   NewFunctionalDependency :: TypeVar x -> TypeVar x -> TypeGen x (FunctionalDependency x)
   NewType :: [TypeVar x] -> [Constraint x] -> TypeGen x (Type x)
   NewTypeClass :: [TypeVar x] -> [Constraint x] -> [FunctionalDependency x] -> TypeGen x (TypeClass x)
   NewInstance :: [Constraint x] -> [TypeVar x] -> TypeGen x (TypeInstance x)
