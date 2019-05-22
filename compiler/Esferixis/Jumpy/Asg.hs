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
data StructId x = StructId Int
data UnionId x = UnionId Int
data StructMemberId x = StructMemberId Int
data UnionMemberId x = UnionMemberId Int
data FunId x = FunId Int
data StateId x = StateId Int
data VarId x = VarId Int
data LabelId x = LabelId Int

data EntityId x = 
   ModuleEntity (ModuleId x) |
   TypeClassEntity (TypeClassId x) |
   StructEntity (StructId x) |
   UnionEntity (UnionId x) |
   StructMemberEntity (StructMemberId x) |
   UnionMemberEntity (UnionMemberId x) |
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
   
data ExprDecl ni e o = ExprDecl ni e o
data ExprId x = ExprId Int

data TypeExprDecl ni e o = TypeExprDecl ni e o
data TypeExprId x = TypeExprId Int

data EntityName = GlobalEntityName String | LocalEntityName String

data FunDeclException x =
   EntityExistsFunDeclException (EntityId x) |
   FunSignatureMismatch (FunId x)
   
data StateDeclException x =
   EntityExistsStateDeclException (EntityId x) |
   StateTypeMismatch (StateId x)
   
data VarDeclException x =
   EntityExistsVarDeclException (EntityId x)
   
data StructDeclException x =
   EntityExistsStructDeclException (EntityId x)
  
data UnionDeclException x =
   EntityExistsUnionDeclException (EntityId x)
   
data LinkageException x =
   LinkageHasBeenDeclared (FunId x)
   
data LinkedDeclException x =
   JustLinkageException (LinkageException x) |
   OtherException x
   
data EntityNotFoundException = EntityNotFoundException EntityName

data MDecl cmd ni e o = NestedDecl ( forall x. Asg x ni ( cmd x ni ) e (ni, o) ) -- It is rank-2 type because local declarations are only valid in this scope

data MaybeDecl decl ni e o where
   JustDecl :: decl ni e o -> MaybeDecl decl ni e o
   NothingDecl :: MaybeDecl decl ni e ()

data ModuleMemberDecl x ni e o where
   ModuleFunDecl :: Maybe (TaggedNode ni LinkageType) -> FunDecl x ni e o -> ModuleMemberDecl x ni ( LinkedDeclException ( FunDeclException x ) ) ()
   ModuleStateDecl :: Maybe (TaggedNode ni LinkageType) -> StateDecl x ni e o -> ModuleMemberDecl x ni ( LinkedDeclException ( StateDeclException x ) ) ()
   ModuleStructDecl :: StructDecl x ni -> ModuleMemberDecl x ni (Either (StructDeclException x) e) o
   ModuleUnionDecl :: UnionDecl x ni -> ModuleMemberDecl x ni (Either (StructDeclException x) e) o

data ModuleCmd x ni e o where
   ModuleCmdMemberDecl :: (TaggedNode ni MemberAccess) -> ModuleMemberDecl x ni e o -> ModuleCmd x ni e o
   
data StructDecl x ni = StructDecl ni [FieldDecl x ni]
data UnionDecl x ni = UnionDecl ni [FieldDecl x ni]

data FieldDecl x ni = FieldDecl ni (TypeExprId x) (TaggedNode ni String)

data LabelDeclException x =
   LabelExistsException (LabelId x)
   
data InstrScopeId x = InstrScopeId Int
data InstrScopeDecl ni e o = InstrScopeDecl (MDecl InstrScopeCmd ni e o)

data InstrScopeCmd x ni e o where
   InstrScopeExprDecl :: ExprDecl ni e o -> InstrScopeCmd x ni e (ExprId x, o)
   InstrScopeInstrScopeDecl :: InstrScopeDecl ni e o -> InstrScopeCmd x ni e (InstrScopeId x)
   InstrScopeFunDecl :: FunDecl x ni e o -> InstrScopeCmd x ni (Either (FunDeclException x) e) o
   InstrScopeStateDecl :: StateDecl x ni e o -> InstrScopeCmd x ni (Either (StateDeclException x) e) o
   InstrScopeVarDecl :: VarDecl x ni -> InstrScopeCmd x ni (VarDeclException x) ()
   InstrScopeStructDecl :: StructDecl x ni -> InstrScopeCmd x ni (StructDeclException x) ()
   InstrScopeUnionDecl :: UnionDecl x ni -> InstrScopeCmd x ni (UnionDeclException x) ()
   InstrScopeLabelDecl :: TaggedNode ni String -> InstrScopeCmd x ni (LabelDeclException x) ()
   InstrScopeIfDecl :: ni -> [((ExprId x), (InstrScopeId x))] -> Maybe (ExprId x) -> InstrScopeCmd x ni e ()
   InstrScopeWhileDecl :: ni -> ExprId x -> InstrScopeId x -> InstrScopeCmd x ni e ()
   InstrScopeDoWhileDecl :: ni -> InstrScopeId x -> ExprId x -> InstrScopeCmd x ni e ()
   InstrScopeBreakDecl :: ni -> InstrScopeCmd x ni e ()
   InstrScopeContinueDecl :: ni -> InstrScopeCmd x ni e ()
   InstrScopeReturnDecl :: ni -> Maybe (ExprId x) -> InstrScopeCmd x ni e ()
   InstrScopeGoToDecl :: ExprId x -> InstrScopeCmd x ni e ()
   InstrScopePutExprSt :: ExprId x -> InstrScopeCmd x ni e ()

data ModuleDecl x ni e o = ModuleDecl (TaggedNode ni String) ( Asg x ni (ModuleCmd x ni) e o)

data CallingConv = CDeclCallingConv | StdCallCallingConv
data StateConv = CDeclStateConv

data FunDecl x ni e o = FunDecl ni [FunArg x ni] ( MaybeDecl InstrScopeDecl ni e o )

data FunArg x ni = FunArg ni (TypeExprId x) String

data StateDecl x ni e o = StateDecl (StateContextDecl x ni) ( MaybeDecl InstrScopeDecl ni e o )
data StateContextDecl ni x = AnonymusStateContextDecl (TypeExprId x) | NamedStateContextDecl (TypeExprId x) (TaggedNode ni String)

data VarDecl x ni = VarDecl ni (TypeExprId x) String

data MemberContainer x m where
   ModuleContainer :: ModuleId x -> MemberContainer x ( EntityId x )
   StructContainer :: StructId x -> MemberContainer x ( StructMemberId x )
   UnionContainer :: UnionId x -> MemberContainer x ( UnionMemberId x)

data Asg x ni cmd e o where
   AsgFindEntityByName :: EntityName -> Asg x ni cmd EntityNotFoundException (EntityId x)
   AsgFindMember :: String -> MemberContainer x o -> Asg x ni cmd e o
   AsgTypeExprDecl :: TypeExprDecl ni e o -> Asg x ni cmd e (TypeExprId x, o)
   AsgThrow :: e -> Asg x ni cmd e o
   AsgTry :: Asg x ni cmd e o -> Asg x ni cmd e2 (Either e o)
   AsgExecuteCommand :: cmd e o -> Asg x ni cmd e o
   AsgBind :: Asg x ni cmd e i -> ( i -> Asg x ni cmd e o ) -> Asg x ni cmd e o
   AsgReturn :: o -> Asg x ni cmd e o
