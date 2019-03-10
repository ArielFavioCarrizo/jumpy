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
import Data.Int

class (Show ni) => JLocInfo ni

data JNode n ni = JNode n ni

data JTranslUnit ni = JTranslUnit ([JNode (JNamespaceScopeSt ni) ni])

data JLinkageType =
   JPrivate |
   JInternal |
   JExternal

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
   jVarDeclDescType :: JNode (JDataCellTypeId ni) ni,
   jVarDeclLinkageType :: JLinkageType
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
   jFunDeclArguments :: [JNode (JArgDeclDesc ni) ni],
   jFunDeclReturnType :: JNode (JDataTypeId ni) ni,
   jFunDeclBody :: Maybe (JNode (JInstrScope ni) ni),
   jFunDeclLinkageType :: JLinkageType
   }

data JArgDeclDesc ni = JArgDeclDesc {
   jArgDeclDescName :: JNode String ni,
   jArgDeclDescType :: JNode (JDataCellTypeId ni) ni
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
   JInstrScopeCallSt (JCallStDesc ni) |
   JGoToSt (JValSrcSt ni)

data JAssignationStDesc ni = JAssignationStDesc {
   jAssignationStDstOp :: JNode (JDataCellRefSt ni) ni,
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

data JCallStDesc ni = JCallStDesc {
   jCallStFunction :: JNode ( JValSrcSt ni ) ni,
   jCallStArguments :: [ JNode ( JValSrcSt ni ) ni ]
   }

data JDataCellRefSt ni =
   JValDataCellRefDereferencePtrSt ( JNode (JValSrcSt ni) ni ) |
   JValDataCellRefFieldStDesc ( JFieldStDesc ni ) |
   JValDataCellRefIdentifierSt ( JNode String ni )

data JValSrcSt ni =
   JValSrcLiteralSt (JLiteralSt ni) |
   JValSrcDataCellSt (JDataCellRefSt ni) |
   JValSrcFieldSt (JFieldStDesc ni) |
   JValSrcCallSt ( JCallStDesc ni ) |
   JValSrcAddOpSt (JNode (JValSrcSt ni) ni, JNode (JValSrcSt ni) ni) |
   JValSrcSubOpSt (JNode (JValSrcSt ni) ni, JNode (JValSrcSt ni) ni) |
   JValSrcMulOpSt (JNode (JValSrcSt ni) ni, JNode (JValSrcSt ni) ni) |
   JValSrcDivOpSt (JNode (JValSrcSt ni) ni, JNode (JValSrcSt ni) ni) |
   JValSrcModOpSt (JNode (JValSrcSt ni) ni, JNode (JValSrcSt ni) ni) |
   JValSrcPostIncrementSt ( JNode (JValSrcSt ni) ni ) |
   JValSrcPreIncrementSt ( JNode (JValSrcSt ni) ni ) |
   JValSrcPostDecrementSt ( JNode (JValSrcSt ni) ni ) |
   JValSrcPreDecrementSt ( JNode (JValSrcSt ni) ni ) |
   JValSrcBitwiseAndOpSt (JNode (JValSrcSt ni) ni, JNode (JValSrcSt ni) ni) |
   JValSrcBitwiseOrOpSt (JNode (JValSrcSt ni) ni, JNode (JValSrcSt ni) ni) |
   JValSrcBitwiseXorOpSt (JNode (JValSrcSt ni) ni, JNode (JValSrcSt ni) ni) |
   JValSrcBitwiseNotOpSt ( JNode (JValSrcSt ni) ni ) |
   JValSrcBitwiseLeftShiftOpSt (JNode (JValSrcSt ni) ni, JNode (JValSrcSt ni) ni) |
   JValSrcBitwiseRightShiftOpSt (JNode (JValSrcSt ni) ni, JNode (JValSrcSt ni) ni) |
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

data JValSrcTernaryOpStDesc ni = JValSrcTernaryOpStDesc {
   jValSrcTernaryOpStDescSrcValue :: JNode ( JValSrcSt ni ) ni,
   jValSrcTernaryOpStDescTrueConditionValue :: JNode ( JValSrcSt ni ) ni,
   jValSrcTernaryOpStDescFalseConditionValue :: JNode ( JValSrcSt ni ) ni
   }

data JFieldStDesc ni = JValSrcFieldStDesc {
   jValDataCellFieldStDescContainer :: JNode ( JValSrcSt ni ) ni,
   jValDataCellFieldStDescFieldName :: JNode String ni
   }
