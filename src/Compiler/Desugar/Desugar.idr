module Compiler.Desugar.Desugar

import Data.List1
import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Syntax.Attribute
import Frontend.Syntax.AST
import Frontend.Syntax.Contract
import Frontend.Syntax.Common
import Frontend.Syntax.Doc
import Frontend.Syntax.Literal
import Frontend.Syntax.Name
import Frontend.Syntax.Operator
import Frontend.Syntax.Pattern
import Frontend.Syntax.Type

%default total

desugarAstNode : {a : Type} -> AstNode SurfaceAstPhase a -> AstNode CanonicalAstPhase a
desugarAstNode (MkAstNode docInfo metadata value) = canonicalAstNode docInfo Written value

desugarAttribute : SurfaceAttribute -> CanonicalAttribute
desugarAttribute (MkAstNode attributeInfo metadata (MkAttributeNode name arguments)) =
  canonicalAstNode attributeInfo Written $
    MkAttributeNode
      (desugarAstNode name)
      (map (map desugarAstNode) arguments)

desugarPath : SurfacePath -> CanonicalPath
desugarPath (MkAstNode pathAstInfo metadata (MkPathNode firstSegment remainingSegments)) =
  canonicalAstNode pathAstInfo Written $
    MkPathNode (desugarAstNode firstSegment) (map desugarAstNode remainingSegments)

desugarPattern : SurfacePattern -> CanonicalPattern
desugarPattern (MkAstNode patternInfo _ patternNode) =
  canonicalAstNode patternInfo Written $
    case patternNode of
      PatternWildcard =>
        PatternWildcard
      PatternName mutability binderName =>
        PatternName mutability (desugarAstNode binderName)
      PatternPath valuePath =>
        PatternPath (desugarPath valuePath)
      PatternLiteral literal =>
        PatternLiteral (desugarAstNode literal)
      PatternParenthesized innerPattern =>
        PatternParenthesized (recur innerPattern)
      PatternTuple elementPatterns =>
        PatternTuple (map recur elementPatterns)
      PatternArray elementPatterns =>
        PatternArray (map recur elementPatterns)
      PatternStruct structPath fieldPatterns =>
        PatternStruct
          (desugarPath structPath)
          (map desugarStructPatternField fieldPatterns)
      PatternEnumTuple variantPath argumentPatterns =>
        PatternEnumTuple
          (desugarPath variantPath)
          (map recur argumentPatterns)
  where
    recur : SurfacePattern -> CanonicalPattern
    recur pattern =
      desugarPattern (assert_smaller patternNode pattern)

    desugarStructPatternField : SurfaceStructPatternField -> CanonicalStructPatternField
    desugarStructPatternField (MkAstNode fieldInfo _ fieldNode) =
      canonicalAstNode fieldInfo Written $
        case fieldNode of
          StructPatternFieldShorthand mutability fieldAndBinderName =>
            StructPatternFieldShorthand
              mutability
              (desugarAstNode fieldAndBinderName)
          StructPatternFieldExplicit fieldName fieldPattern =>
            StructPatternFieldExplicit
              (desugarAstNode fieldName)
              (recur fieldPattern)

mutual
  desugarExpressionNode : ExpressionNode SurfaceAstPhase -> ExpressionNode CanonicalAstPhase
  desugarExpressionNode expression =
    case expression of
      ExprLiteral literal => ExprLiteral (desugarAstNode literal)
      ExprName name => ExprName (desugarAstNode name)
      ExprPath path => ExprPath (desugarPath path)
      ExprBuiltin builtin => ExprBuiltin builtin
      ExprSelf => ExprSelf
      ExprParenthesized inner => ExprParenthesized (desugarNestedExpression inner)
      ExprTuple elements => ExprTuple (map desugarNestedExpression elements)
      ExprArray elements => ExprArray (map desugarNestedExpression elements)
      ExprRepeatedArray element count => ExprRepeatedArray (desugarNestedExpression element) (desugarNestedExpression count)
      ExprStructLiteral path fields => assert_total $ idris_crash "Desugar.idr: desugarExpressionNode: ExprStructLiteral not implemented"
      ExprCall callee arguments => ExprCall (desugarNestedExpression callee) (map desugarNestedExpression arguments)
      ExprMethodCall receiver name arguments => ExprMethodCall (desugarNestedExpression receiver) (desugarAstNode name) (map desugarNestedExpression arguments)
      ExprField object name => ExprField (desugarNestedExpression object) (desugarAstNode name)
      ExprTupleIndex tuple indexText => ExprTupleIndex (desugarNestedExpression tuple) indexText
      ExprIndex object index => ExprIndex (desugarNestedExpression object) (desugarNestedExpression index)
      ExprUnary operator operand => ExprUnary (desugarAstNode operator) (desugarNestedExpression operand)
      ExprBinary operator left right => ExprBinary (desugarAstNode operator) (desugarNestedExpression left) (desugarNestedExpression right)
      ExprRange start operator end => ExprRange (map desugarNestedExpression start) (desugarAstNode operator) (map desugarNestedExpression end)
      ExprCast operand target => ExprCast (desugarNestedExpression operand) (desugarType target)
      ExprBlock block => ExprBlock (desugarBlockExpression block)
      ExprIf ifNode => ExprIf (desugarIfNode ifNode)
      ExprQIf ifNode => assert_total $ idris_crash "Desugar.idr: desugarExpressionNode: ExprQIf not implemented"
      ExprSIf ifNode => assert_total $ idris_crash "Desugar.idr: desugarExpressionNode: ExprSIf not implemented"
      ExprMatch matchNode => assert_total $ idris_crash "Desugar.idr: desugarExpressionNode: ExprMatch not implemented"
      ExprQMatch matchNode => assert_total $ idris_crash "Desugar.idr: desugarExpressionNode: ExprQMatch not implemented"
      ExprSMatch matchNode => assert_total $ idris_crash "Desugar.idr: desugarExpressionNode: ExprSMatch not implemented"
      ExprLoop body => ExprLoop (desugarBlockExpression body)
      ExprWhile condition body => ExprWhile (desugarNestedExpression condition) (desugarBlockExpression body)
      ExprFor pattern iterator body => ExprFor (desugarPattern pattern) (desugarNestedExpression iterator) (desugarBlockExpression body)
      ExprBreak value => ExprBreak (map desugarNestedExpression value)
      ExprContinue => ExprContinue
      ExprReturn value => ExprReturn (map desugarNestedExpression value)
      ExprCtrl control => ExprCtrl (desugarControlExpressionNode control)
      ExprAdjoint adjoint => ExprAdjoint (desugarAdjointExpressionNode adjoint)
    where
      desugarNestedExpression : SurfaceExpr -> CanonicalExpr
      desugarNestedExpression nestedExpression =
        desugarExpression (assert_smaller expression nestedExpression)
      desugarBlockExpression : SurfaceBlock -> CanonicalBlock
      desugarBlockExpression 
        (MkAstNode blockAstInfo _ (MkBlockNode blockInnerDocs blockStatements finalExpression)) =
        canonicalAstNode blockAstInfo Written $
          MkBlockNode 
            (map desugarAstNode blockInnerDocs) 
            (map (\statement => desugarStatement (assert_smaller expression statement)) blockStatements) 
            (map desugarNestedExpression finalExpression)
      desugarIfNode : ClassicalIfNode SurfaceAstPhase -> ClassicalIfNode CanonicalAstPhase
      desugarIfNode ifNode@(MkClassicalIfNode ifCondition ifThenBlock ifElseBranch) =
        MkClassicalIfNode
          (desugarNestedExpression ifCondition)
          (desugarBlockExpression ifThenBlock)
          (map desugarElseNode ifElseBranch)
        where
          desugarElseNode : ClassicalElseNode SurfaceAstPhase -> ClassicalElseNode CanonicalAstPhase
          desugarElseNode (ElseBlock elseBlock) =
            ElseBlock (desugarBlockExpression elseBlock)
          desugarElseNode (ElseChainedIf (MkAstNode chainedIfInfo _ chainedIfNode)) =
            ElseChainedIf $
              canonicalAstNode chainedIfInfo Written $
                desugarIfNode (assert_smaller ifNode chainedIfNode)
      desugarControlExpressionNode : ControlExpressionNode SurfaceAstPhase -> ControlExpressionNode CanonicalAstPhase
      desugarControlExpressionNode (ControlledCallable controlQubits onBasisRaw controlledCallable) =
        ControlledCallable
          (map desugarNestedExpression controlQubits)
          (map desugarAstNode onBasisRaw)
          (desugarNestedExpression controlledCallable)
      desugarControlExpressionNode (ControlledBlock controlQubits onBasisRaw controlledBlock) =
        ControlledBlock
          (map desugarNestedExpression controlQubits)
          (map desugarAstNode onBasisRaw)
          (desugarBlockExpression controlledBlock)
      desugarAdjointExpressionNode : AdjointExpressionNode SurfaceAstPhase -> AdjointExpressionNode CanonicalAstPhase
      desugarAdjointExpressionNode (AdjointOfCallable adjointedCallable) = AdjointOfCallable (desugarNestedExpression adjointedCallable)
      desugarAdjointExpressionNode (AdjointBlock adjointedBlock) = AdjointBlock (desugarBlockExpression adjointedBlock)

  desugarExpression : SurfaceExpr -> CanonicalExpr
  desugarExpression (MkAstNode expressionInfo metadata expressionNode) =
    canonicalAstNode expressionInfo Written (desugarExpressionNode expressionNode)

  desugarType : Ty SurfaceAstPhase (Expr SurfaceAstPhase) -> Ty CanonicalAstPhase (Expr CanonicalAstPhase)
  desugarType (MkAstNode tyAstInfo metadata typeNode) =
    canonicalAstNode tyAstInfo Written $
      case typeNode of
        TyPrimitive primitiveName =>
          TyPrimitive primitiveName
        TyPath typePath =>
          TyPath (desugarPath typePath)
        TyUnit =>
          TyUnit
        TyParenthesized innerType =>
          TyParenthesized (desugarNestedType innerType)
        TyTuple elementTypes =>
          TyTuple (map desugarNestedType elementTypes)
        TyArray elementType sizeExpression =>
          TyArray 
            (desugarNestedType elementType) 
            (desugarExpression sizeExpression)
        TySlice elementType =>
          TySlice (desugarNestedType elementType)
        TyReference borrowKind referencedType =>
          TyReference 
            (desugarAstNode borrowKind) 
            (desugarNestedType referencedType)
        TyQualified storageQualifiers qualifiedType =>
          TyQualified (map desugarAstNode storageQualifiers) (desugarNestedType qualifiedType)
        TyFunction functionEffect functionParameters returnType =>
          TyFunction
            (map desugarAstNode functionEffect)
            (map desugarParameter functionParameters)
            (map desugarNestedType returnType)
      where
        desugarNestedType : SurfaceTy -> CanonicalTy
        desugarNestedType nestedType =
          desugarType (assert_smaller typeNode nestedType)
        desugarParameter : SurfaceAstNode (FunctionTypeParameterNode SurfaceAstPhase (SurfaceAstNode (ExpressionNode SurfaceAstPhase))) ->
          CanonicalAstNode (FunctionTypeParameterNode CanonicalAstPhase (CanonicalAstNode (ExpressionNode CanonicalAstPhase)))
        desugarParameter (MkAstNode parameterAstInfo metadata (MkFunctionTypeParameterNode parameterName parameterType)) =
          canonicalAstNode parameterAstInfo Written $ 
            MkFunctionTypeParameterNode (desugarAstNode parameterName) (desugarNestedType parameterType)

  desugarFunctionParameter: AstNode SurfaceAstPhase (FunctionParameterNode SurfaceAstPhase) -> AstNode CanonicalAstPhase (FunctionParameterNode CanonicalAstPhase)
  desugarFunctionParameter (MkAstNode parameterInfo metadata (NormalParameter parameterDocs parameterMutability parameterName parameterType)) =
    canonicalAstNode parameterInfo Written $
      NormalParameter
        (map desugarAstNode parameterDocs)
        (map desugarAstNode parameterMutability)
        (desugarAstNode parameterName)
        (desugarType parameterType)
  desugarFunctionParameter (MkAstNode parameterInfo metadata (ReceiverParameter receiverDocs receiverBorrow)) =
    canonicalAstNode parameterInfo Written $
      ReceiverParameter
        (map desugarAstNode receiverDocs)
        (map desugarAstNode receiverBorrow)

  desugarSignedPauliTerm : SurfaceSignedPauliTerm -> SignedPauliTerm CanonicalAstPhase
  desugarSignedPauliTerm (MkAstNode termInfo metadata (MkSignedPauliTermNode sign pauliString)) =
    canonicalAstNode termInfo Written $
      MkSignedPauliTermNode sign (desugarAstNode pauliString)

  desugarContractPredicate : SurfaceContractPredicate -> CanonicalContractPredicate
  desugarContractPredicate (MkAstNode predicateInfo metadata predicateNode) =
    canonicalAstNode predicateInfo Written $
      case predicateNode of
        ContractClean qubitArgument =>
          ContractClean (desugarExpression qubitArgument)
        ContractBasis qubitArgument pauliString =>
          ContractBasis
            (desugarExpression qubitArgument)
            (desugarAstNode pauliString)
        ContractSeparable qubitArgument =>
          ContractSeparable (desugarExpression qubitArgument)
        ContractIsolated qubitArgument =>
          ContractIsolated (desugarExpression qubitArgument)
        ContractProduct firstQubitSet otherQubitSets =>
          ContractProduct
            (desugarExpression firstQubitSet)
            (map desugarExpression otherQubitSets)
        ContractStabilized qubitArgument stabilizerTerms =>
          ContractStabilized
            (desugarExpression qubitArgument)
            (map desugarSignedPauliTerm stabilizerTerms)

  desugarContractClause : ContractClause SurfaceAstPhase (Expr SurfaceAstPhase) -> ContractClause CanonicalAstPhase (Expr CanonicalAstPhase) 
  desugarContractClause (MkAstNode contractAstInfo metadata contractClauseNode) =
    canonicalAstNode contractAstInfo Written $
      case contractClauseNode of
        RequiresClause predicate => RequiresClause (desugarContractPredicate predicate)
        EnsuresClause predicate => EnsuresClause (desugarContractPredicate predicate)

  desugarLetInitializer : LetInitializerNode SurfaceAstPhase -> LetInitializerNode CanonicalAstPhase
  desugarLetInitializer (MkLetInitializerNode marker value) =
    MkLetInitializerNode (desugarAstNode marker) (desugarExpression value)

  desugarAssignmentTarget : SurfaceAstNode (AssignmentTargetNode SurfaceAstPhase) -> CanonicalAstNode (AssignmentTargetNode CanonicalAstPhase)
  desugarAssignmentTarget (MkAstNode assignmentTargetAstInfo metadata assignmentTargetNode) =
    canonicalAstNode assignmentTargetAstInfo Written $
      case assignmentTargetNode of
        AssignTargetName targetName =>
          AssignTargetName (desugarAstNode targetName)
        AssignTargetIndex targetObject indexExpression =>
          AssignTargetIndex
            (desugarExpression targetObject)
            (desugarExpression indexExpression)
        AssignTargetField targetObject fieldName =>
          AssignTargetField
            (desugarExpression targetObject)
            (desugarAstNode fieldName)
        AssignTargetTupleIndex targetObject tupleIndexRawText =>
          AssignTargetTupleIndex
            (desugarExpression targetObject)
            tupleIndexRawText

  desugarStatement : Statement SurfaceAstPhase -> Statement CanonicalAstPhase
  desugarStatement (MkAstNode statementAstInfo metadata statementNode) =
    canonicalAstNode statementAstInfo Written $
      case statementNode of
        StatementLet (MkLetBindingNode qualifiers pattern typeAnnotation initializer) =>
          StatementLet $
            MkLetBindingNode
              (map desugarAstNode qualifiers)
              (desugarPattern pattern)
              (map (\ty => desugarType (assert_smaller statementNode ty)) typeAnnotation)
              (map (\init => desugarLetInitializer (assert_smaller statementNode init)) initializer)
        StatementAssignment (MkAssignmentNode assignmentTarget assignmentOperator assignmentValue) =>
          StatementAssignment $
            MkAssignmentNode
              (desugarAssignmentTarget assignmentTarget)
              (desugarAstNode assignmentOperator)
              (desugarExpression assignmentValue)
        StatementSemiExpression statementExpression =>
          StatementSemiExpression (desugarExpression statementExpression)
        StatementExpression statementExpression =>
          StatementExpression (desugarExpression statementExpression)
 
desugarFunctionBody : Block SurfaceAstPhase -> Block CanonicalAstPhase
desugarFunctionBody (MkAstNode functionBodyAstInfo metadata (MkBlockNode blockInnerDocs blockStatements finalExpression)) =
  canonicalAstNode functionBodyAstInfo Written $
    MkBlockNode
      (map desugarAstNode blockInnerDocs)
      (map desugarStatement blockStatements)
      (map desugarExpression finalExpression)

desugarItem : SurfaceItem -> CanonicalItem
desugarItem (MkAstNode itemInfo metadata item) =
  canonicalAstNode itemInfo Written $
    case item of
      ItemModule declaration => assert_total $ idris_crash "Desugar.idr: desugarItem: ItemModule not implemented."
      ItemUse declaration => assert_total $ idris_crash "Desugar.idr: desugarItem: ItemUse not implemented."
      ItemConst declaration => ItemConst $ desugarConstDeclaration declaration
      ItemEnum declaration => assert_total $ idris_crash "Desugar.idr: desugarItem: ItemEnum not implemented"
      ItemQEnum declaration => assert_total $ idris_crash "Desugar.idr: desugarItem: ItemQEnum not implemented"
      ItemStruct declaration => assert_total $ idris_crash "Desugar.idr: desugarItem: ItemStruct not implemented"
      ItemImpl declaration => assert_total $ idris_crash "Desugar.idr: desugarItem: ItemImpl not implemented"
      ItemFunction declaration => ItemFunction $ desugarFunctionDeclaration declaration
    where
      desugarConstDeclaration : ConstDeclarationNode SurfaceAstPhase -> ConstDeclarationNode CanonicalAstPhase
      desugarConstDeclaration
          (MkConstDeclarationNode
            constDocs
            constVisibility
            (MkAstNode constNameInfo constNameMetadata constNameNode)
            constType
            constValue) =
              MkConstDeclarationNode
                (map desugarAstNode constDocs)
                (map desugarAstNode constVisibility)
                (canonicalAstNode constNameInfo Written constNameNode)
                (desugarType constType)
                (desugarExpression constValue)
      desugarFunctionDeclaration : FunctionDeclarationNode SurfaceAstPhase -> FunctionDeclarationNode CanonicalAstPhase
      desugarFunctionDeclaration
          (MkFunctionDeclarationNode
            functionDocs
            functionAttributes
            functionVisibility
            functionConstness
            functionEffect
            functionName
            functionParameters
            returnType
            supportClause
            contractClauses
            functionBody
          ) = MkFunctionDeclarationNode
                (map desugarAstNode functionDocs)
                (map desugarAttribute functionAttributes)
                (map desugarAstNode functionVisibility)
                (map desugarAstNode functionConstness)
                (desugarFunctionEffect functionEffect)
                (desugarAstNode functionName)
                (map desugarFunctionParameter functionParameters)
                (map desugarType returnType)
                (map desugarAstNode supportClause)
                (map desugarContractClause contractClauses)
                (desugarFunctionBody functionBody)
              where
                getAstInfo : Name SurfaceAstPhase -> AstInfo
                getAstInfo (MkAstNode astInfo x value) =
                  MkAstInfo
                    (MkNodeId astInfo.nodeId.surfaceId (astInfo.nodeId.desugarId + 1))
                    astInfo.span
                desugarFunctionEffect : Maybe (AstNode SurfaceAstPhase FunctionEffect) -> Maybe (AstNode CanonicalAstPhase FunctionEffect)
                desugarFunctionEffect Nothing = Just $ canonicalAstNode (getAstInfo functionName) InferredDefaultFunctionEffect EffectGeneral
                desugarFunctionEffect (Just functionEffectNode) = Just $ desugarAstNode functionEffectNode


desugarSurfaceSyntax : SurfaceSourceFile -> CanonicalSourceFile
desugarSurfaceSyntax
    (MkAstNode fileInfo metadata (MkSourceFileNode docs items)) =
  canonicalAstNode fileInfo Written $
    MkSourceFileNode
      (map desugarAstNode docs)
      (map desugarItem items)
