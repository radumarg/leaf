module Frontend.Desugar.Desugar

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

desugarType : Ty SurfaceAstPhase (Expr SurfaceAstPhase) -> Ty CanonicalAstPhase (Expr CanonicalAstPhase)
desugarType (MkAstNode tyInfo metadata typeNode) =
  canonicalAstNode tyInfo Written $
    case typeNode of
      TyPrimitive primitiveName =>
        TyPrimitive primitiveName
      TyPath typePath =>
        TyPath ?xx_2
      TyUnit =>
        TyUnit
      TyParenthesized innerType =>
        ?xx_4
      TyTuple elementTypes =>
        ?xx_5
      TyArray elementType sizeExpression =>
        ?xx_6
      TySlice elementType =>
        ?xx_7
      TyReference borrowKind referencedType =>
        ?xx_8
      TyQualified storageQualifiers qualifiedType =>
        ?xx_9
      TyFunction functionEffect functionParameters returnType =>
        ?xx_10

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

desugarExpressionNode : ExpressionNode SurfaceAstPhase -> ExpressionNode CanonicalAstPhase
desugarExpressionNode expression =
  case expression of
    ExprLiteral literal => ExprLiteral (desugarAstNode literal)
    ExprName name => ExprName (desugarAstNode name)
    ExprPath path => ?desugar_expr_path
    ExprBuiltin builtin => ExprBuiltin builtin
    ExprSelf => ExprSelf
    ExprParenthesized inner => ?desugar_expr_parenthesized
    ExprTuple elements => ?desugar_expr_tuple
    ExprArray elements => ?desugar_expr_array
    ExprRepeatedArray element count => ?desugar_expr_repeated_array
    ExprStructLiteral path fields => ?desugar_expr_struct_literal
    ExprCall callee arguments => ?desugar_expr_call
    ExprMethodCall receiver name arguments => ?desugar_expr_method_call
    ExprField object name => ?desugar_expr_field
    ExprTupleIndex tuple indexText => ?desugar_expr_tuple_index
    ExprIndex object index => ?desugar_expr_index
    ExprUnary operator operand => ?desugar_expr_unary
    ExprBinary operator left right => ?desugar_expr_binary
    ExprRange start operator end => ?desugar_expr_range
    ExprCast operand target => ?desugar_expr_cast
    ExprBlock block => ?desugar_expr_block
    ExprIf ifNode => ?desugar_expr_if
    ExprQIf ifNode => assert_total $ idris_crash "Desugar.idr: desugar_expression_node: ExprQIf not implemented"
    ExprSIf ifNode => assert_total $ idris_crash "Desugar.idr: desugar_expression_node: ExprSIf not implemented"
    ExprMatch matchNode => assert_total $ idris_crash "Desugar.idr: desugar_expression_node: ExprMatch not implemented"
    ExprQMatch matchNode => assert_total $ idris_crash "Desugar.idr: desugar_expression_node: ExprQMatch not implemented"
    ExprSMatch matchNode => assert_total $ idris_crash "Desugar.idr: desugar_expression_node: ExprSMatch not implemented"
    ExprLoop body => ?desugar_expr_loop
    ExprWhile condition body => ?desugar_expr_while
    ExprFor pattern iterator body => ?desugar_expr_for
    ExprBreak value => ?desugar_expr_break
    ExprContinue => ExprContinue
    ExprReturn value => ?desugar_expr_return
    ExprCtrl control => ?desugar_expr_ctrl
    ExprAdjoint adjoint => ?desugar_expr_adjoint

desugarExpression : SurfaceExpr -> CanonicalExpr
desugarExpression (MkAstNode expressionInfo metadata expressionNode) =
  canonicalAstNode expressionInfo Written (desugarExpressionNode expressionNode)

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

desugarPath : SurfacePath -> CanonicalPath 
desugarPath (MkAstNode pathAstInfo metadata (MkPathNode firstSegment remainingSegments)) =
  canonicalAstNode pathAstInfo Written $
    MkPathNode (desugarAstNode firstSegment) (map desugarAstNode remainingSegments)

desugarLetInitializer : LetInitializerNode SurfaceAstPhase -> LetInitializerNode CanonicalAstPhase
desugarLetInitializer (MkLetInitializerNode marker value) =
  MkLetInitializerNode (desugarAstNode marker) (desugarExpression value)

desugarLetPattern : Pattern SurfaceAstPhase -> Pattern CanonicalAstPhase
desugarLetPattern (MkAstNode letPatternAstInfo metadata patternNode) =
  canonicalAstNode letPatternAstInfo Written $
    case patternNode of
      PatternWildcard =>
        PatternWildcard
      PatternName mutability binderName =>
        PatternName mutability (desugarAstNode binderName)
      PatternPath valuePath =>
        PatternPath (desugarPath valuePath)
      PatternLiteral literal =>
        ?xx_12
      PatternParenthesized innerPattern =>
        ?xx_13
      PatternTuple elementPatterns =>
        ?xx_14
      PatternArray elementPatterns =>
        ?xx_15
      PatternStruct structPath fieldPatterns =>
        ?xx_16
      PatternEnumTuple variantPath argumentPatterns =>
        ?xx_17

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
            (desugarLetPattern pattern)
            (map desugarType typeAnnotation)
            (map desugarLetInitializer initializer)
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
      ItemModule declaration => ItemModule ?desugar_module_declaration
      ItemUse declaration => ItemUse ?desugar_use_declaration
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
            (MkAstNode constValueInfo constValueMetadata constValueNode)) =
              MkConstDeclarationNode
                (map desugarAstNode constDocs)
                (map desugarAstNode constVisibility)
                (canonicalAstNode constNameInfo Written constNameNode)
                (desugarType constType)
                (canonicalAstNode constValueInfo Written (desugarExpressionNode constValueNode))
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
                (map desugarAstNode functionEffect)
                (desugarAstNode functionName)
                (map desugarFunctionParameter functionParameters)
                (map desugarType returnType)
                (map desugarAstNode supportClause)
                (map desugarContractClause contractClauses)
                (desugarFunctionBody functionBody)

desugarSurfaceSyntax : SurfaceSourceFile -> CanonicalSourceFile
desugarSurfaceSyntax
    (MkAstNode fileInfo metadata (MkSourceFileNode docs items)) =
  canonicalAstNode fileInfo Written $
    MkSourceFileNode
      (map desugarAstNode docs)
      (map desugarItem items)
