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

data Entity x = 
   ModuleEntity (ModuleId x) |
   TypeClassEntity (TypeClassId x) |
   TypeEntity (TypeId x) |
   FunEntity (FunId x) |
   StateEntity (StateId x) |
   VarEntity (VarId x)

data LinkageType =
   PrivateLinkage |
   InternalLinkage |
   ExternalLinkage

data MemberAccess =
   PublicMember |
   PrivateMember

data EntityName = GlobalEntityName String | LocalEntityName String

data ModuleMemberDecl x ni e o where
   ModuleFunDecl :: Maybe (TaggedNode ni LinkageType) -> FunDecl x ni e o -> ModuleMemberDecl x ni e o
   ModuleStateDecl :: Maybe (TaggedNode ni LinkageType) -> StateDecl x ni e o -> ModuleMemberDecl x ni e o

data ModuleCmd x ni e o where
   ModuleCmdMemberDecl :: (TaggedNode ni MemberAccess) -> ModuleMemberDecl x ni e o -> ModuleCmd x ni e o

data InstrScopeCmd x ni e o where
   InstrScopeFunDecl :: FunDecl x ni e o -> InstrScopeCmd x ni e o
   InstrScopeStateDecl :: StateDecl x ni e o -> InstrScopeCmd x ni e o

data MaybeInstrScope x ni e o where
   JustInstrScope :: Asg x ni (InstrScopeCmd x ni) e (ni, o) -> MaybeInstrScope x ni e o
   NothingInstrScope :: MaybeInstrScope x ni e ()

data TypedArgumentDecl e = TypedArgumentDecl String (TypeId e)

data ModuleDecl x ni e o = ModuleDecl (TaggedNode ni String) ( Asg x ni (ModuleCmd x ni) e o)

data CallingConv = CDeclCallingConv | StdCallCallingConv
data StateConv = CDeclStateConv

data FunDecl x ni e o = FunDecl (TaggedNode ni String) (Maybe (TaggedNode ni CallingConv)) [TaggedNode ni (TypedArgumentDecl x)] (MaybeInstrScope x ni e o)

data StateContextDecl x ni = StateContextDecl (Maybe (TaggedNode ni String)) (TaggedNode ni (TypeId x))
data StateDecl x ni e o = StateDecl (TaggedNode ni String) (Maybe (TaggedNode ni StateConv)) (TaggedNode ni (StateContextDecl x ni)) (MaybeInstrScope x ni e o)
data VarDecl x ni e o = VarDecl (TaggedNode ni String) (TaggedNode ni (TypeId x))

data Asg x ni cmd e o where
   RootModule :: (ModuleId x -> Asg x ni cmd e o) -> Asg x ni cmd e o
   FindEntityByName :: EntityName -> (Maybe (TaggedNode ni (Entity x)) -> Asg x ni cmd e o) -> Asg x ni cmd e o
   Throw :: e -> Asg x ni cmd e o
   Try :: Asg x ni cmd e o -> Asg x ni cmd e2 (Either e o)
   ExecuteCommand :: cmd e o -> Asg x ni cmd e o
