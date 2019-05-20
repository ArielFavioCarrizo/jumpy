-- |
-- Module      :  Esferixis.Jumpy.AST
-- Copyright   :  (c) 2019 Ariel Favio Carrizo
-- License     :  BSD-3-Clause
-- Stability   : experimental
-- Portability : ghc

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}

module Esferixis.Jumpy.Asg where

import Text.Show
import Data.Word
import Data.Maybe
import Data.Either
import Data.Int

class (Show ni) => JLocInfo ni

data TaggedNode ni a = TaggedNode ni a

data ModuleId x = ModuleId Int
data TypeClassId x = TypeClassId Int
data TypeId x = TypeId Int
data FunId x = FunId Int
data StateId x = StateId Int
data VarId x = VarId Int
data LabelId x = LabelId Int

data Entity x = 
   ModuleEntity (ModuleId x) |
   TypeClassEntity (TypeClassId x) |
   TypeEntity (TypeId x) |
   FunEntity (FunId x) |
   StateEntity (StateId x) |
   VarEntity (VarId x) |
   LabelEntity (LabelId x)

data LinkageType =
   PrivateLinkage |
   InternalLinkage |
   ExternalLinkage

data MemberAccess =
   PublicMember |
   PrivateMember
   
data Expression x = Expression x

data EntityName = GlobalEntityName String | LocalEntityName String

data FunDeclException x =
   FunExistsException (FunId x) |
   FunSignatureMismatch (FunId x)
   
data StateDeclException x =
   StateExistsException (StateId x) |
   StateTypeMismatch (StateId x)
   
data LinkageException x =
   LinkageHasBeenDeclared (FunId x)
   
data LinkedDeclException x =
   JustLinkageException (LinkageException x) |
   OtherException x
   
data EntityNotFoundException = EntityNotFoundException EntityName

data ModuleMemberDecl x ni e o where
   ModuleFunDecl :: Maybe (TaggedNode ni LinkageType) -> FunDecl x ni e o -> ModuleMemberDecl x ni ( LinkedDeclException ( Either ( FunDeclException x ) e ) ) o
   ModuleStateDecl :: Maybe (TaggedNode ni LinkageType) -> StateDecl x ni e o -> ModuleMemberDecl x ni ( LinkedDeclException ( Either ( StateDeclException x ) e ) ) o

data ModuleCmd x ni e o where
   ModuleCmdMemberDecl :: (TaggedNode ni MemberAccess) -> ModuleMemberDecl x ni e o -> ModuleCmd x ni e o

data LabelDeclException x =
   LabelExistsException (LabelId x)
   
data IfScopeCmd x ni e o where
   IfScopeIfDecl :: Expression x -> InstrScopeDecl ni e o -> IfScopeCmd x ni e o
   IfScopeElseDecl :: InstrScopeDecl ni e o -> IfScopeCmd x ni e o

data InstrScopeCmd x ni e o where
   InstrScopeFunDecl :: FunDecl x ni e o -> InstrScopeCmd x ni ( Either (FunDeclException x) e ) o
   InstrScopeStateDecl :: StateDecl x ni e o -> InstrScopeCmd x ni ( Either (FunDeclException x) e ) o
   InstrScopeLabelDecl :: String -> InstrScopeCmd x ni (LabelDeclException x) ()
   InstrScopeIfDecl :: Asg x ni (IfScopeCmd x ni) e o -> InstrScopeCmd x ni e o
   InstrScopeWhileDecl :: Expression x -> InstrScopeDecl ni e o -> InstrScopeCmd x ni e o
   InstrScopeDoWhileDecl :: InstrScopeDecl ni e o -> Expression x -> InstrScopeCmd x ni e o
   InstrScopeBreakDecl :: InstrScopeCmd x ni e ()
   InstrScopeContinueDecl :: InstrScopeCmd x ni e ()
   InstrScopeReturnDecl :: Maybe (Expression x) -> InstrScopeCmd x ni e ()
   InstrScopeGoToDecl :: Expression x -> InstrScopeCmd x ni e ()
   InstrScopeAssignmentDecl :: Expression x -> Expression x -> InstrScopeCmd x ni e ()
   
data InstrScopeDecl ni e o = InstrScopeDecl ( forall x. Asg x ni (InstrScopeCmd x ni) e (ni, o) ) -- It is rank-2 type because local declarations are only valid in this scope

data MaybeInstrScope ni e o where
   JustInstrScope :: InstrScopeDecl ni e o -> MaybeInstrScope ni e o
   NothingInstrScope :: MaybeInstrScope ni e ()

data TypedArgumentDecl x = TypedArgumentDecl String (TypeId x)

data ModuleDecl x ni e o = ModuleDecl (TaggedNode ni String) ( Asg x ni (ModuleCmd x ni) e o)

data CallingConv = CDeclCallingConv | StdCallCallingConv
data StateConv = CDeclStateConv

data FunDecl x ni e o = FunDecl (TaggedNode ni String) (Maybe (TaggedNode ni CallingConv)) [TaggedNode ni (TypedArgumentDecl x)] (MaybeInstrScope ni e o)

data StateContextDecl x ni = StateContextDecl (Maybe (TaggedNode ni String)) (TaggedNode ni (TypeId x))
data StateDecl x ni e o = StateDecl (TaggedNode ni String) (Maybe (TaggedNode ni StateConv)) (TaggedNode ni (StateContextDecl x ni)) (MaybeInstrScope ni e o)
data VarDecl x ni e o = VarDecl (TaggedNode ni String) (TaggedNode ni (TypeId x))

data Asg x ni cmd e o where
   AsgFindEntityByName :: EntityName -> Asg x ni cmd EntityNotFoundException (Entity x)
   AsgThrow :: e -> Asg x ni cmd e o
   AsgTry :: Asg x ni cmd e o -> Asg x ni cmd e2 (Either e o)
   AsgExecuteCommand :: cmd e o -> Asg x ni cmd e o
   AsgBind :: Asg x ni cmd e i -> ( i -> Asg x ni cmd e o ) -> Asg x ni cmd e o
   AsgReturn :: o -> Asg x ni cmd e o
