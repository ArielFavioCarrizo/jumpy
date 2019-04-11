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

data JTranslUnit ni = JTranslUnit [JNode (JModuleDecl ni) ni]

data JLinkageType =
   JPrivateLinkage |
   JInternalLinkage |
   JExternalLinkage
   
data JMemberAccess =
   JPublicMember |
   JPrivateMember

data JModuleDecl ni = JModuleDecl {
   jModuleDeclName :: JNode String ni,
   jModuleDeclMembers :: [JNode (JModuleSt ni) ni]
   }
   
data JModuleSt ni =
   JModuleMemberSt JMemberAccess (JModuleMemberStDesc ni) |
   JModuleTypeInstanceSt (JTypeInstanceDecl ni)
   
data JModuleMemberStDesc ni =
   JModuleTypeClassDecl (JTypeClassDecl ni) |
   JModuleUserTypeDecl (JUserTypeDecl ni) |
   JModuleFunDecl JLinkageType (JFunDecl ni) |
   JModuleContextScopeSt (JContextScope ni) |
   JModuleVarDecl JLinkageType (JVarDecl ni)
   
data JTypeConstraint ni = JTypeConstraint {
   jTypeConstraintClassName :: JNode String ni, -- It could be a type class or a variant
   jTypeConstraintParams :: [JNode String ni]
   }
   
data JTypeClassDecl ni = JTypeClassDecl {
   jTypeClassDeclName :: JNode String ni,
   jTypeClassParams :: [JNode String ni],
   jTypeClassMembers :: JFunPrototype ni
   }
   
data JTypeInstanceDecl ni = JTypeInstanceDecl {
   jTypeInstanceTypeConstraints :: [JNode (JTypeConstraint ni) ni],
   jTypeInstanceMembers :: [JNode (JFunDecl ni) ni]
   }

data JVarDecl ni = JVarDeclDesc {
   jVarDeclDescName :: JNode String ni,
   jVarDeclDescType :: JNode (JDataCellTypeId ni) ni,
   jVarDeclDescValue :: JNode (JOperandSt ni) ni
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
   JVariant (JVariantDesc ni) |
   JStructDeclDesc [JFieldDeclDesc ni] |
   JUnionDeclDesc [JFieldDeclDesc ni] |
   JAliasDeclDesc (JDataTypeId ni)

data JVariantDesc ni = JVariantDesc {
   jVariantDescMembers :: [JNode (JDataTypeId ni) ni]
}

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

data JFunPrototype ni = JFunPrototype {
   jFunName :: JNode String ni,
   jFunCallingConv :: Maybe (JNode JCallingConvId ni),
   jFunArguments :: [JNode (JFunArgDecl ni) ni],
   jFunReturnType :: JNode (JDataTypeId ni) ni
   }

data JFunDecl ni = JFunDecl {
   jFunDeclTypeConstraints :: [JNode (JTypeConstraint ni) ni],
   jFunDeclPrototype :: JNode (JFunPrototype ni) ni,
   jFunDeclBody :: Maybe (JNode (JInstrScope ni) ni)
   }

data JFunArgDecl ni = JArgDeclDesc {
   jArgDeclDescName :: JNode String ni,
   jArgDeclDescType :: JNode (JDataCellTypeId ni) ni
   }

data JContextScope ni = JContextScope {
   jContextScopeTypeConstraints :: [JNode (JTypeConstraint ni) ni],
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
   JGoToSt (JOperandSt ni)

data JAssignationStDesc ni = JAssignationStDesc {
   jAssignationStDstOp :: JNode (JOperandSt ni) ni,
   jAssignationStSrcOp :: JNode (JOperandSt ni) ni
   }

data JIfStDesc ni = JIfStDesc {
   jIfStSrcOp :: JNode (JOperandSt ni) ni,
   jIfStTrueScope :: JNode (JInstrScope ni) ni,
   jIfStFalseCase :: Maybe ( JNode (JIfAnotherCaseSt ni) ni )
   }

data JIfAnotherCaseSt ni =
   JElseIfSt ( JNode (JIfStDesc ni) ni ) |
   JElseSt ( JNode (JInstrScope ni) ni )

data JWhileStDesc ni = JWhileStDesc {
   jWhileStDescConditionValue :: JNode (JOperandSt ni) ni,
   jWhileStDescInstrScope :: JNode (JInstrScopeSt ni) ni
   }

data JForStDesc ni = JForStDesc {
   jForStDescInitialScope :: JNode (JInstrScopeSt ni) ni,
   jForStDescIterationScope :: JNode (JInstrScopeSt ni) ni,
   jForStDescPreNextIterationScope :: JNode (JInstrScopeSt ni) ni
   }

data JCallStDesc ni = JCallStDesc {
   jCallStFunction :: JNode ( JOperandSt ni ) ni,
   jCallStArguments :: [ JNode ( JCallArg ni ) ni ]
   }
   
data JCallArg ni = JCallArg {
   jCallArgName :: Maybe (JNode String ni),
   jCallArgValue :: JNode (JOperandSt ni) ni
}

data JOperandSt ni =
   JOperandLiteralSt (JLiteralSt ni) |
   JOperandIdentifierSt String |
   JOperandContValSt (JContVal ni) |
   JOperandDereferencePtrSt (JNode (JOperandSt ni) ni) |
   JOperandFieldSt (JFieldId ni) |
   JOperandCallSt ( JCallStDesc ni ) |
   JOperandAddOpSt (JNode (JOperandSt ni) ni, JNode (JOperandSt ni) ni) |
   JOperandSubOpSt (JNode (JOperandSt ni) ni, JNode (JOperandSt ni) ni) |
   JOperandMulOpSt (JNode (JOperandSt ni) ni, JNode (JOperandSt ni) ni) |
   JOperandDivOpSt (JNode (JOperandSt ni) ni, JNode (JOperandSt ni) ni) |
   JOperandModOpSt (JNode (JOperandSt ni) ni, JNode (JOperandSt ni) ni) |
   JOperandPostIncrementSt ( JNode (JOperandSt ni) ni ) |
   JOperandPreIncrementSt ( JNode (JOperandSt ni) ni ) |
   JOperandPostDecrementSt ( JNode (JOperandSt ni) ni ) |
   JOperandPreDecrementSt ( JNode (JOperandSt ni) ni ) |
   JOperandBitwiseAndOpSt (JNode (JOperandSt ni) ni, JNode (JOperandSt ni) ni) |
   JOperandBitwiseOrOpSt (JNode (JOperandSt ni) ni, JNode (JOperandSt ni) ni) |
   JOperandBitwiseXorOpSt (JNode (JOperandSt ni) ni, JNode (JOperandSt ni) ni) |
   JOperandBitwiseNotOpSt ( JNode (JOperandSt ni) ni ) |
   JOperandBitwiseLeftShiftOpSt (JNode (JOperandSt ni) ni, JNode (JOperandSt ni) ni) |
   JOperandBitwiseRightShiftOpSt (JNode (JOperandSt ni) ni, JNode (JOperandSt ni) ni) |
   JOperandTernaryOpSt ( JOperandTernaryOpStDesc ni )

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
   jContValIP :: JNode (JOperandSt ni) ni,
   jContValDataPtr :: JNode (JOperandSt ni) ni
}

data JOperandTernaryOpStDesc ni = JOperandTernaryOpStDesc {
   jOperandTernaryOpStDescSrcValue :: JNode ( JOperandSt ni ) ni,
   jOperandTernaryOpStDescTrueConditionValue :: JNode ( JOperandSt ni ) ni,
   jOperandTernaryOpStDescFalseConditionValue :: JNode ( JOperandSt ni ) ni
   }

data JFieldId ni = JFieldId {
   jFieldIdContainer :: JNode ( JOperandSt ni ) ni,
   jFieldIdName :: JNode String ni
   }
