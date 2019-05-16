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

data LinkageType =
   PrivateLinkage |
   InternalLinkage |
   ExternalLinkage

data MemberAccess =
   PublicMember |
   PrivateMember

data EntityName = GlobalEntityName String | LocalEntityName String

data ModuleMemberDecl e ni o where
   ModuleFunDecl :: Maybe (TaggedNode ni LinkageType) -> FunDecl e ni o -> ModuleMemberDecl e ni o
   ModuleStateDecl :: Maybe (TaggedNode ni LinkageType) -> StateDecl e ni o -> ModuleMemberDecl e ni o

data ModuleCmd e ni o where
   ModuleCmdMemberDecl :: (TaggedNode ni MemberAccess) -> ModuleMemberDecl e ni o -> ModuleCmd e ni o

data InstrScopeCmd e ni o where
   InstrScopeFunDecl :: FunDecl e ni o -> InstrScopeCmd e ni o
   InstrScopeStateDecl :: StateDecl e ni o -> InstrScopeCmd e ni o

data MaybeInstrScope e ni o where
   JustInstrScope :: Asg e ni (InstrScopeCmd e ni) (ni, o) -> MaybeInstrScope e ni o
   NothingInstrScope :: MaybeInstrScope e ni ()

data TypedArgumentDecl e = TypedArgumentDecl String (TypeId e)

data ModuleDecl e ni o = ModuleDecl (TaggedNode ni String) ( Asg e ni (ModuleCmd e ni) o)

data CallingConv = CDeclCallingConv | StdCallCallingConv
data StateConv = CDeclStateConv

data FunDecl e ni o = FunDecl (TaggedNode ni String) (Maybe (TaggedNode ni CallingConv)) [TaggedNode ni (TypedArgumentDecl e)] (MaybeInstrScope e ni o)

data StateContextDecl e ni = StateContextDecl (Maybe (TaggedNode ni String)) (TaggedNode ni (TypeId e))
data StateDecl e ni o = StateDecl (TaggedNode ni String) (Maybe (TaggedNode ni StateConv)) (TaggedNode ni (StateContextDecl ni e)) (MaybeInstrScope e ni o)
data VarDecl e ni o = VarDecl (TaggedNode ni String) (TaggedNode ni (TypeId e))

data Asg e ni cmd o where
   RootModule :: (ModuleId e -> Asg e ni cmd o) -> Asg e ni cmd o
   FindEntityByName :: EntityName -> (Maybe (TaggedNode ni (Entity e)) -> Asg e ni cmd o) -> Asg e ni cmd o
   ExecuteCommand :: cmd o -> Asg e ni cmd o
