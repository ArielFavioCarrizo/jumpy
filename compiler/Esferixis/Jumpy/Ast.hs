-- |
-- Module      :  Language.C.Syntax.AST
-- Copyright   :  (c) 2019 Ariel Favio Carrizo
-- License     :  BSD-3-Clause
-- Stability   : experimental
-- Portability : ghc

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}

module Esferixis.Jumpy.Ast where

import Text.Show
import Data.Word
import Data.Maybe

class (Show ni) => JLocInfo ni

data JNode n ni = JNode n ni

data JTranslUnit ni = JTranslUnit ([JNode (JNamespaceScopeSt ni) ni])

data JNamespaceScopeSt ni =
   JNamespaceDeclSt (JNamespaceDecl ni) |
   JUserTypeDeclSt (JUserTypeDecl ni) |
   JFunDeclSt (JFunDecl ni) |
   JContScopeSt (JContScopeDesc ni)

data JNamespaceDecl ni = JNamespaceDecl {
   jNamespaceDeclName :: JNode String ni,
   jNamespaceDeclStatements :: [JNode (JNamespaceScopeSt ni) ni]
   }

data JVarDeclDesc ni = JVarDeclDesc {
   jVarDeclDescName :: JNode String ni,
   jVarDeclDescType :: JNode (JDataTypeId ni) ni
   }

data JUserTypeDecl ni = JTypeDecl {
   jUserTypeDeclName :: JNode String ni,
   jUserTypeDeclGenericParams :: [JNode String ni],
   jUserTypeDeclDesc :: JNode (JUserTypeDeclDesc ni) ni
   }

data JUserTypeDeclDesc ni =
   JStructDeclDesc [JFieldDeclDesc ni] |
   JUnionDeclDesc [JFieldDeclDesc ni] |
   JAliasDeclDesc (JDataTypeId ni)

data JFieldDeclDesc ni = JFieldDeclDesc {
   jFieldDeclDescName :: JNode String ni,
   jFieldDeclDescType :: JNode (JDataTypeId ni) ni
   }

data JDataTypeId ni =
   JVoidId |
   JBoolId |
   JCharId |
   JI8Id |
   JI16Id |
   JI32Id |
   JI64Id |
   JISizeId |
   JU8Id |
   JU16Id |
   JU32Id |
   JU64Id |
   JUSizeId |
   JF32Id |
   JF64Id |
   JFunTypeId (JFunTypeIdDesc ni) |
   JContTypeId JCallingConvId |
   JArrayId (JArrayIdDesc ni) |
   JUserTypeId (String) |
   JPtrId (JDataTypeId ni) |
   JGenericDataTypeId (JGenericDataTypeIdDesc ni)

data JGenericDataTypeIdDesc ni = JGenericDataTypeIdDesc {
   jGenericDataTypeIdDescGenericParams :: [JNode (JDataTypeId ni) ni],
   jGenericDataTypeIdDescWrappedType :: JNode (JDataTypeId ni) ni
}

data JArrayIdDesc ni = JArrayIdDesc {
   jArrayIdElementType :: JNode (JDataTypeId ni) ni,
   jArrayIdElementSize :: [JNode Word64 ni]
   }

data JFunTypeIdDesc ni = JFunTypeIdDesc {
   jFunTypeIdCallingConv :: JNode JCallingConvId ni,
   jFunTypeIdArguments :: [JNode (JDataTypeId ni) ni],
   jFunTypeIdReturnType :: JNode (JDataTypeId ni) ni
   }

data JCallingConvId = JCDeclCallingConvId

data JFunDecl ni = JFunDecl {
   jFunDeclName :: JNode String ni,
   jFunDeclCallingConv :: Maybe (JNode JCallingConvId ni),
   jFunDeclArguments :: [JNode (JArgDeclDesc ni) ni],
   jFunDeclReturnType :: JNode (JDataTypeId ni) ni,
   jFunDeclBody :: Maybe (JNode (JInstrScope ni) ni)
   }

data JArgDeclDesc ni = JArgDeclDesc {
   jArgDeclDescName :: JNode String ni,
   jArgDeclDescType :: JNode (JDataTypeId ni) ni
   }

data JContScopeDesc ni = JContScopeDesc {
   jContScopeDataDecl :: JNode (JContScopeDataDecl ni) ni,
   jContScopeLabelStatements :: [JNode (JContScopeLabelDecl ni) ni]
   }

data JContScopeDataDecl ni =
   JContScopeLexicalScopedDataDecl (JDataTypeId ni) |
   JContScopeNamedDataDecl (JVarDeclDesc ni)

data JContScopeLabelDecl ni = JContScopeLabelDecl {
   jContScopeLabelDeclName :: JNode String ni,
   jContScopeLabelDeclInstrScope :: JNode (JInstrScope ni) ni
   }

data JInstrScope ni = JInstrScope [JNode (JInstrScopeSt ni) ni]

data JInstrScopeSt ni =
   JInstrScopeUserTypeDeclSt (JUserTypeDecl ni) |
   JInstrScopeFunDeclSt (JFunDecl ni) |
   JInstrScopeVarDeclSt (JVarDeclDesc ni) |
   JAssignationSt (JAssignationStDesc ni) |
   JIfSt (JIfStDesc ni) |
   JWhileSt (JWhileStDesc ni) |
   JDoWhileSt (JWhileStDesc ni) |
   JForSt (JForStDesc ni) |
   JBreakSt |
   JContinueSt |
   JReturnSt |
   JGoToSt (JValSrcSt ni)

data JAssignationStDesc ni = JAssignationStDesc {
   jAssignationStDstOp :: JNode (JValDstSt ni) ni,
   jAssignationStSrcOp :: JNode (JValSrcSt ni) ni
   }

data JIfStDesc ni = JIfStDesc {
   jIfStSrcOp :: JValSrcSt ni,
   jIfStTrueScope :: JNode (JInstrScope ni) ni,
   jIfStFalseCase :: Maybe ( JNode (JIfAnotherCaseSt ni) ni )
   }

data JIfAnotherCaseSt ni =
   JElseIfSt ( JIfStDesc ni ) |
   JElseSt ( JNode (JInstrScope ni) ni )

data JWhileStDesc ni = JWhileStDesc {
   jWhileStDescConditionValue :: JNode (JValSrcSt ni) ni,
   jWhileStDescInstrScope :: JNode (JInstrScopeSt ni) ni
   }

data JForStDesc ni = JForStDesc {
   jForStDescInitialScope :: JNode (JInstrScopeSt ni) ni,
   jForStDescIterationScope :: JNode (JInstrScopeSt ni) ni,
   jForStDescPreNextIterationScope :: JNode (JInstrScopeSt ni) ni
   }

data JValDstSt ni = JValDstSt ni

data JValSrcSt ni = JValSrcSt {
   jValSrcStAddSt :: (JNode (JValSrcSt ni) ni, JNode (JValSrcSt ni) ni),
   jValSrcStSubSt :: (JNode (JValSrcSt ni) ni, JNode (JValSrcSt ni) ni),
   jValSrcStMulSt :: (JNode (JValSrcSt ni) ni, JNode (JValSrcSt ni) ni),
   jValSrcStDivSt :: (JNode (JValSrcSt ni) ni, JNode (JValSrcSt ni) ni),
   jValSrcStModSt :: (JNode (JValSrcSt ni) ni, JNode (JValSrcSt ni) ni),
   jValSrcStPostIncrementSt :: JNode (JValSrcSt ni) ni,
   jValSrcStPreIncrementSt :: JNode (JValSrcSt ni) ni,
   jValSrcStPostDecrementSt :: JNode (JValSrcSt ni) ni,
   jValSrcStPreDecrementSt :: JNode (JValSrcSt ni) ni,
   jValSrcStBitwiseAndOp :: (JNode (JValSrcSt ni) ni, JNode (JValSrcSt ni) ni),
   jValSrcStBitwiseOrOp :: (JNode (JValSrcSt ni) ni, JNode (JValSrcSt ni) ni),
   jValSrcStBitwiseXorOp :: (JNode (JValSrcSt ni) ni, JNode (JValSrcSt ni) ni),
   jValSrcStBitwiseNotOp :: JNode (JValSrcSt ni) ni,
   jValSrcStBitwiseLeftShiftOp :: (JNode (JValSrcSt ni) ni, JNode (JValSrcSt ni) ni),
   jValSrcStBitwiseRightShiftOp :: (JNode (JValSrcSt ni) ni, JNode (JValSrcSt ni) ni)
   }
