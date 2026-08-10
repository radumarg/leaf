module Frontend.Desugar.Desugar

import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Syntax.Attribute
import Frontend.Syntax.AST
import Frontend.Syntax.Common
import Frontend.Syntax.Doc
import Frontend.Syntax.Literal
import Frontend.Syntax.Name
import Frontend.Syntax.Type

%default total

desugarAstNode : {a : Type} -> AstNode SurfaceAstPhase a -> AstNode CanonicalAstPhase a
desugarAstNode (MkAstNode docInfo _ value) = canonicalAstNode docInfo Written value

desugarAttribute : SurfaceAttribute -> CanonicalAttribute
desugarAttribute (MkAstNode info _ (MkAttributeNode name arguments)) =
  canonicalAstNode info Written $
    MkAttributeNode
      (desugarAstNode name)
      (map (map desugarAstNode) arguments)

desugarFunctionParameter: AstNode SurfaceAstPhase (FunctionParameterNode SurfaceAstPhase) -> AstNode CanonicalAstPhase (FunctionParameterNode CanonicalAstPhase)
desugarFunctionParameter (MkAstNode parameterInfo _ (NormalParameter parameterDocs parameterMutability parameterName parameterType)) =
        ?desugar_normal_function_parameter
desugarFunctionParameter (MkAstNode parameterInfo _ (ReceiverParameter receiverDocs receiverBorrow)) =
        ?desugar_receiver_function_parameter

desugar_expression_node :
     ExpressionNode SurfaceAstPhase
  -> ExpressionNode CanonicalAstPhase
desugar_expression_node expression =
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

desugar_type_node : TyNode SurfaceAstPhase SurfaceExpr -> TyNode CanonicalAstPhase CanonicalExpr
desugar_type_node (TyPrimitive primitiveName) = TyPrimitive primitiveName
desugar_type_node (TyPath typePath) = ?desugar_type_node_impl_1
desugar_type_node TyUnit = TyUnit
desugar_type_node (TyParenthesized innerType) = ?desugar_type_node_impl_3
desugar_type_node (TyTuple elementTypes) = ?desugar_type_node_impl_4
desugar_type_node (TyArray elementType sizeExpression) = ?desugar_type_node_impl_5
desugar_type_node (TySlice elementType) = ?desugar_type_node_impl_6
desugar_type_node (TyReference borrowKind referencedType) = ?desugar_type_node_impl_7
desugar_type_node (TyQualified storageQualifiers qualifiedType) = ?desugar_type_node_impl_8
desugar_type_node (TyFunction functionEffect functionParameters returnType) = ?desugar_type_node_impl_9

desugarItem : SurfaceItem -> CanonicalItem
desugarItem (MkAstNode itemInfo _ item) =
  canonicalAstNode itemInfo Written $
    case item of
      ItemModule declaration => ItemModule ?desugar_module_declaration
      ItemUse declaration => ItemUse ?desugar_use_declaration
      ItemConst declaration => ItemConst $ desugar_const_declaration declaration
      ItemEnum declaration => assert_total $ idris_crash "Desugar.idr: desugarItem: ItemEnum not implemented"
      ItemQEnum declaration => assert_total $ idris_crash "Desugar.idr: desugarItem: ItemQEnum not implemented"
      ItemStruct declaration => assert_total $ idris_crash "Desugar.idr: desugarItem: ItemStruct not implemented"
      ItemImpl declaration => assert_total $ idris_crash "Desugar.idr: desugarItem: ItemImpl not implemented"
      ItemFunction declaration => ItemFunction $ desugar_function_declaration declaration
    where
      desugar_const_declaration : ConstDeclarationNode SurfaceAstPhase -> ConstDeclarationNode CanonicalAstPhase
      desugar_const_declaration
          (MkConstDeclarationNode
            constDocs
            constVisibility
            (MkAstNode constNameInfo _ constNameNode)
            (MkAstNode constTypeInfo _ constTypeNode)
            (MkAstNode constValueInfo _ constValueNode)) =
              MkConstDeclarationNode
                (map desugarAstNode constDocs)
                (map desugarAstNode constVisibility)
                (canonicalAstNode constNameInfo Written constNameNode)
                (canonicalAstNode constTypeInfo Written (desugar_type_node constTypeNode))
                (canonicalAstNode constValueInfo Written (desugar_expression_node constValueNode))

      desugar_function_declaration : FunctionDeclarationNode SurfaceAstPhase -> FunctionDeclarationNode CanonicalAstPhase
      desugar_function_declaration
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
            functionBody) =
              MkFunctionDeclarationNode
                (map desugarAstNode functionDocs)
                (map desugarAttribute functionAttributes)
                (map desugarAstNode functionVisibility)
                (map desugarAstNode functionConstness)
                (map desugarAstNode functionEffect)
                (desugarAstNode functionName)
                (map desugarFunctionParameter functionParameters)
                (map ?desugar_type_node2 returnType)
                ?h1_9
                ?h1_10
                ?h1_11

desugarSurfaceSyntax : SurfaceSourceFile -> CanonicalSourceFile
desugarSurfaceSyntax
    (MkAstNode fileInfo _ (MkSourceFileNode docs items)) =
  canonicalAstNode fileInfo Written $
    MkSourceFileNode
      (map desugarAstNode docs)
      (map desugarItem items)
