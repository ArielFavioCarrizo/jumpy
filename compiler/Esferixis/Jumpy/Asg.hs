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
data StructMemberId x = StructMemberId x
data UnionMemberId x = UnionMemberId x
data LabelId x = LabelId Int

data Entity x = 
   ModuleEntity (ModuleId x) |
   TypeClassEntity (TypeClassId x) |
   TypeEntity (TypeId x) |
   FunEntity (FunId x) |
   StateEntity (StateId x) |
   VarEntity (VarId x) |
   StructMemberEntity (StructMemberId x) |
   UnionMemberEntity (UnionMemberId x) |
   LabelEntity (LabelId x)

data LinkageType =
   PrivateLinkage |
   InternalLinkage |
   ExternalLinkage

data MemberAccess =
   PublicMember |
   PrivateMember
   
data MaybeDecl decl ni e o =
   JustDecl (decl ni e o) |
   NothingDecl ni e ()
   
data ExprDecl ni e o = ExprDecl ni e o
data TypeExprDecl ni e o = TypeExprDecl ni e o

data EntityName = GlobalEntityName String | LocalEntityName String

data FunDeclException x =
   EntityExistsFunDeclException (Entity x) |
   FunSignatureMismatch (FunId x)
   
data StateDeclException x =
   EntityExistsStateDeclException (Entity x) |
   StateTypeMismatch (StateId x)
   
data VarDeclException x =
   EntityExistsVarDeclException (Entity x)
   
data StructDeclException x =
   EntityExistsStructDeclException (Entity x)
  
data UnionDeclException x =
   EntityExistsUnionDeclException (Entity x)
   
data LinkageException x =
   LinkageHasBeenDeclared (FunId x)
   
data LinkedDeclException x =
   JustLinkageException (LinkageException x) |
   OtherException x
   
data EntityNotFoundException = EntityNotFoundException EntityName

data DeclCmd cmd ni e o = DeclCmd ( forall x. Asg x ni (InstrScopeCmd x ni) e (ni, o) ) -- It is rank-2 type because local declarations are only valid in this scope

data ModuleMemberDecl x ni e o where
   ModuleFunDecl :: Maybe (TaggedNode ni LinkageType) -> FunDecl ni e o -> ModuleMemberDecl x ni ( LinkedDeclException ( Either ( FunDeclException x ) e ) ) o
   ModuleStateDecl :: Maybe (TaggedNode ni LinkageType) -> StateDecl ni e o -> ModuleMemberDecl x ni ( LinkedDeclException ( Either ( StateDeclException x ) e ) ) o
   ModuleStructDecl :: StructDecl ni e o -> ModuleMemberDecl x ni ( Either (StructDeclException x) e ) o
   ModuleUnionDecl :: UnionDecl ni e o -> ModuleMemberDecl x ni ( Either (StructDeclException x) e ) o

data ModuleCmd x ni e o where
   ModuleCmdMemberDecl :: (TaggedNode ni MemberAccess) -> ModuleMemberDecl x ni e o -> ModuleCmd x ni e o

data StructMemberDeclException x =
   StructMemberExistsException (StructMemberId x)

data StructCmd x ni e o where
   StructMemberDecl :: TypeExprDecl ni e o -> ( o -> (ni,String,o2) ) -> StructCmd x ni ( Either (StructMemberDeclException x) e ) o2
   
type StructDecl ni e o = DeclCmd StructCmd ni e o
   
data UnionMemberDeclException x =
   UnionMemberExistsException (UnionMemberId x)
   
data UnionCmd x ni e o where
   UnionMemberDecl :: TypeExprDecl ni e o -> ( o -> (TaggedNode ni String,o2) ) -> UnionCmd x ni ( Either (UnionMemberDeclException x) e ) o2
   
type UnionDecl ni e o = DeclCmd UnionCmd ni e o

data LabelDeclException x =
   LabelExistsException (LabelId x)
   
data IfStDecl ni e o where
   IfStDecl :: (ExprDecl ni e o) -> ( o -> InstrScopeDecl ni e2 o2 ) -> IfStDecl ni e2 o2
   ElseDecl :: InstrScopeDecl ni e o -> IfStDecl ni e o
   EndIfDecl :: IfStDecl ni e o   
   
type InstrScopeDecl ni e o = DeclCmd InstrScopeCmd ni e o

data InstrScopeCmd x ni e o where
   InstrScopeFunDecl :: FunDecl ni e o -> InstrScopeCmd x ni ( Either (FunDeclException x) e ) o
   InstrScopeStateDecl :: StateDecl ni e o -> InstrScopeCmd x ni ( Either (FunDeclException x) e ) o
   InstrScopeVarDecl :: VarDecl ni e o -> InstrScopeCmd x ni ( Either (VarDeclException x) e ) o
   InstrScopeStructDecl :: StructDecl ni e o -> InstrScopeCmd x ni ( Either (StructDeclException x) e ) o
   InstrScopeUnionDecl :: UnionDecl ni e o -> InstrScopeCmd x ni ( Either (UnionDeclException x) e ) o
   InstrScopeLabelDecl :: TaggedNode ni String -> InstrScopeCmd x ni (LabelDeclException x) ()
   InstrScopeIfDecl :: IfStDecl ni e o -> InstrScopeCmd x ni e o
   InstrScopeWhileDecl :: ExprDecl ni e o -> ( o -> InstrScopeDecl ni e2 o2 ) -> InstrScopeCmd x ni e2 o2
   InstrScopeDoWhileDecl :: InstrScopeDecl ni e o -> ( o -> ExprDecl ni e2 o2 ) -> InstrScopeCmd x ni e2 o2
   InstrScopeBreakDecl :: ni -> InstrScopeCmd x ni e ()
   InstrScopeContinueDecl :: ni -> InstrScopeCmd x ni e ()
   InstrScopeReturnDecl :: ni -> MaybeDecl ExprDecl ni e o -> InstrScopeCmd x ni e o
   InstrScopeGoToDecl :: ExprDecl ni e o -> InstrScopeCmd x ni e o

data ModuleDecl x ni e o = ModuleDecl (TaggedNode ni String) ( Asg x ni (ModuleCmd x ni) e o)

data CallingConv = CDeclCallingConv | StdCallCallingConv
data StateConv = CDeclStateConv

data FunDecl ni e o = FunDecl ni e o -- Incomplete
data StateDecl ni e o = StateDecl ni e o -- Incomplete

data VarDecl ni e o where 
   VarDecl :: TypeExprDecl ni e o -> ( o -> (TaggedNode ni String,o2) ) -> VarDecl ni e o2

data Asg x ni cmd e o where
   AsgFindEntityByName :: EntityName -> Asg x ni cmd EntityNotFoundException (Entity x)
   AsgThrow :: e -> Asg x ni cmd e o
   AsgTry :: Asg x ni cmd e o -> Asg x ni cmd e2 (Either e o)
   AsgExecuteCommand :: cmd e o -> Asg x ni cmd e o
   AsgBind :: Asg x ni cmd e i -> ( i -> Asg x ni cmd e o ) -> Asg x ni cmd e o
   AsgReturn :: o -> Asg x ni cmd e o
