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

module Esferixis.Jumpy.Asg.Struct(
   Struct,
   mkStruct,
   memberByName,
   members
   ) where

import Data.HashMap.Lazy as HM
import Data.Maybe

data StructBuildException m =
   StructMemberNameExists m

class StructMember m where
   structMemberName :: m -> String

data Struct m = Struct ( (StructMember m) => HM.HashMap String m )
   
mkStruct :: (StructMember m) => [m] -> Either (StructBuildException m) (Struct m)
mkStruct members = addMembers [] $ Struct empty

memberByName :: (StructMember m) => String -> Struct m -> Maybe m
memberByName name (Struct membersByName) = HM.lookup name membersByName

members :: (StructMember m) => Struct m -> [m]
members (Struct membersByName) = elems membersByName

addMember :: (StructMember m) => Struct m -> m -> Either (StructBuildException m) (Struct m)
addMember (Struct membersByName) member =
   let memberName = structMemberName member
   in
      case ( HM.member memberName membersByName ) of
         True -> Left $ StructMemberNameExists member
         False -> Right $ Struct $ HM.insert memberName member membersByName
         
addMembers :: (StructMember m) => [m] -> Struct m -> Either (StructBuildException m) (Struct m)
addMembers (eachMember:nextMembers) struct = (addMember struct eachMember) >>= addMembers nextMembers
addMembers [] struct = Right $ struct
