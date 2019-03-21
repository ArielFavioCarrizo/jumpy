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

module Esferixis.Jumpy.Ast where

import Text.Show
import Data.Word
import Data.Maybe
import Data.Int

class (Show ni) => JLocInfo ni

data JNode n ni = JNode n ni

data JTranslUnit ni = JTranslUnit (JNode (JNamespaceScopeSt ni) ni)

data JLinkageType =
   JPrivate |
   JInternal |
   JExternal

data JNamespaceScopeSt ni =
   JNamespaceDeclSt (JNamespaceDecl ni) |
   JNamespaceUserTypeDeclSt (JUserTypeDecl ni) |
   JNamespaceFunDeclSt (JFunDecl ni, JLinkageType) |
   JNamespaceContextScopeSt (JContextScope ni) |
   JNamespaceVarDeclSt (JVarDecl ni, JLinkageType)

data JNamespaceDecl ni = JNamespaceDecl {
   jNamespaceDeclName :: JNode String ni,
   jNamespaceDeclStatements :: [JNode (JNamespaceScopeSt ni) ni]
   }

data JVarDecl ni = JVarDeclDesc {
   jVarDeclDescName :: JNode String ni,
   jVarDeclDescType :: JNode (JDataCellTypeId ni) ni,
   jVarDeclDescValue :: JNode (JDataCellSt ni) ni
   }

data JTypeDeclParam ni =
   JTypeDeclGenericParam (JNode String ni) |
   JTypeDeclTemplateParam (JNode String ni)

data JUserTypeDecl ni = JTypeDecl {
   jUserTypeDeclName :: JNode String ni,
   jUserTypeDeclParams :: [JNode (JTypeDeclParam ni) ni],
   jUserTypeDeclDesc :: Maybe (JNode (JUserTypeDeclDesc ni) ni)
   }

data JUserTypeDeclDesc ni =
   JStructDeclDesc [JFieldDeclDesc ni] |
   JUnionDeclDesc [JFieldDeclDesc ni] |
   JAliasDeclDesc (JDataTypeId ni)

data JFieldDeclDesc ni = JFieldDeclDesc {
   jFieldDeclDescName :: JNode String ni,
   jFieldDeclDescType :: JNode (JDataTypeId ni) ni
   }

data JDataCellTypeId ni =
   JConstId ( JNode (JDataTypeId ni) ni ) |
   JMutableId ( JNode (JDataTypeId ni) ni )

data JDataTypeId ni =
   JVoidId |
   JBoolId |
   JI8Id |
   JI16Id |
   JI32Id |
   JI64Id |
   JU8Id |
   JU16Id |
   JU32Id |
   JU64Id |
   JF32Id |
   JF64Id |
   JFunTypeId (JFunTypeIdDesc ni) |
   JContTypeId JCallingConvId |
   JArrayTypeId (JArrayTypeIdDesc ni) |
   JUserTypeId (String) |
   JPtrId (JDataCellTypeId ni) |
   JParametrizedDataTypeId (JParametrizedDataTypeIdDesc ni)

data JParametrizedDataTypeIdDesc ni = JParametrizedDataTypeIdDesc {
   jGenericDataTypeIdDescGenericParams :: [JNode (JDataTypeId ni) ni],
   jGenericDataTypeIdDescWrappedType :: JNode (JDataTypeId ni) ni
}

data JArrayTypeIdDesc ni = JArrayIdDesc {
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
   jFunDeclArguments :: [JNode (JFunArgDecl ni) ni],
   jFunDeclReturnType :: JNode (JDataTypeId ni) ni,
   jFunDeclBody :: Maybe (JNode (JInstrScope ni) ni)
   }

data JFunArgDecl ni = JArgDeclDesc {
   jArgDeclDescName :: JNode String ni,
   jArgDeclDescType :: JNode (JDataCellTypeId ni) ni
   }

data JContextScope ni = JContextScope {
   jContextScopeDataType :: JNode (JDataTypeId ni) ni,
   jContextScopeLabelStatements :: [JNode (JContextScopeLabelDecl ni) ni]
   }

data JContextScopeLabelDecl ni = JContScopeContDecl {
   jContScopeLabelDeclName :: JNode String ni,
   jContScopeLabelDeclInstrScope :: JNode (JInstrScope ni) ni
   }

data JInstrScope ni = JInstrScope [JNode (JInstrScopeSt ni) ni]

data JInstrScopeSt ni =
   JInstrScopeUserTypeDeclSt (JUserTypeDecl ni) |
   JInstrScopeFunDeclSt (JFunDecl ni) |
   JInstrScopeContextScopeSt (JContextScope ni) |
   JInstrScopeVarDeclSt (JVarDecl ni) |
   JInstrScopeLabelDeclSt (JNode String ni) |
   JAssignationSt (JAssignationStDesc ni) |
   JIfSt (JIfStDesc ni) |
   JWhileSt (JWhileStDesc ni) |
   JDoWhileSt (JWhileStDesc ni) |
   JForSt (JForStDesc ni) |
   JBreakSt |
   JContinueSt |
   JReturnSt |
   JInstrScopeCallSt (JCallStDesc ni) |
   JGoToSt (JDataCellSt ni)

data JAssignationStDesc ni = JAssignationStDesc {
   jAssignationStDstOp :: JNode (JDataCellSt ni) ni,
   jAssignationStSrcOp :: JNode (JDataCellSt ni) ni
   }

data JIfStDesc ni = JIfStDesc {
   jIfStSrcOp :: JNode (JDataCellSt ni) ni,
   jIfStTrueScope :: JNode (JInstrScope ni) ni,
   jIfStFalseCase :: Maybe ( JNode (JIfAnotherCaseSt ni) ni )
   }

data JIfAnotherCaseSt ni =
   JElseIfSt ( JNode (JIfStDesc ni) ni ) |
   JElseSt ( JNode (JInstrScope ni) ni )

data JWhileStDesc ni = JWhileStDesc {
   jWhileStDescConditionValue :: JNode (JDataCellSt ni) ni,
   jWhileStDescInstrScope :: JNode (JInstrScopeSt ni) ni
   }

data JForStDesc ni = JForStDesc {
   jForStDescInitialScope :: JNode (JInstrScopeSt ni) ni,
   jForStDescIterationScope :: JNode (JInstrScopeSt ni) ni,
   jForStDescPreNextIterationScope :: JNode (JInstrScopeSt ni) ni
   }

data JCallStDesc ni = JCallStDesc {
   jCallStFunction :: JNode ( JDataCellSt ni ) ni,
   jCallStArguments :: [ JNode ( JCallArg ni ) ni ]
   }
   
data JCallArg ni = JCallArg {
   jCallArgName :: Maybe (JNode String ni),
   jCallArgValue :: JNode (JDataCellSt ni) ni
}

data JDataCellSt ni =
   JValSrcLiteralSt (JLiteralSt ni) |
   JValSrcIdentifierSt String |
   JValSrcContValSt (JContVal ni) |
   JValSrcDereferencePtrSt (JNode (JDataCellSt ni) ni) |
   JValSrcFieldSt (JFieldId ni) |
   JValSrcCallSt ( JCallStDesc ni ) |
   JValSrcAddOpSt (JNode (JDataCellSt ni) ni, JNode (JDataCellSt ni) ni) |
   JValSrcSubOpSt (JNode (JDataCellSt ni) ni, JNode (JDataCellSt ni) ni) |
   JValSrcMulOpSt (JNode (JDataCellSt ni) ni, JNode (JDataCellSt ni) ni) |
   JValSrcDivOpSt (JNode (JDataCellSt ni) ni, JNode (JDataCellSt ni) ni) |
   JValSrcModOpSt (JNode (JDataCellSt ni) ni, JNode (JDataCellSt ni) ni) |
   JValSrcPostIncrementSt ( JNode (JDataCellSt ni) ni ) |
   JValSrcPreIncrementSt ( JNode (JDataCellSt ni) ni ) |
   JValSrcPostDecrementSt ( JNode (JDataCellSt ni) ni ) |
   JValSrcPreDecrementSt ( JNode (JDataCellSt ni) ni ) |
   JValSrcBitwiseAndOpSt (JNode (JDataCellSt ni) ni, JNode (JDataCellSt ni) ni) |
   JValSrcBitwiseOrOpSt (JNode (JDataCellSt ni) ni, JNode (JDataCellSt ni) ni) |
   JValSrcBitwiseXorOpSt (JNode (JDataCellSt ni) ni, JNode (JDataCellSt ni) ni) |
   JValSrcBitwiseNotOpSt ( JNode (JDataCellSt ni) ni ) |
   JValSrcBitwiseLeftShiftOpSt (JNode (JDataCellSt ni) ni, JNode (JDataCellSt ni) ni) |
   JValSrcBitwiseRightShiftOpSt (JNode (JDataCellSt ni) ni, JNode (JDataCellSt ni) ni) |
   JValSrcTernaryOpSt ( JValSrcTernaryOpStDesc ni )

data JLiteralSt ni =
   JBoolLiteralSt Bool |
   JI8LiteralSt Int8 |
   J16LiteralSt Int16 |
   JI32LiteralSt Int32 |
   JI64LiteralSt Int64 |
   JSizeOfLiteralSt ( JDataTypeId ni ) |
   JU8LiteralSt Word8 |
   JU16LiteralSt Word16 |
   JU32LiteralSt Word32 |
   JU64LiteralSt Word64 |
   JF32LiteralSt Float |
   JF64LiteralSt Double
   
data JContVal ni = JContVal {
   jContValIP :: JNode (JDataCellSt ni) ni,
   jContValDataPtr :: JNode (JDataCellSt ni) ni
}

data JValSrcTernaryOpStDesc ni = JValSrcTernaryOpStDesc {
   jValSrcTernaryOpStDescSrcValue :: JNode ( JDataCellSt ni ) ni,
   jValSrcTernaryOpStDescTrueConditionValue :: JNode ( JDataCellSt ni ) ni,
   jValSrcTernaryOpStDescFalseConditionValue :: JNode ( JDataCellSt ni ) ni
   }

data JFieldId ni = JFieldId {
   jFieldIdContainer :: JNode ( JDataCellSt ni ) ni,
   jFieldIdName :: JNode String ni
   }
