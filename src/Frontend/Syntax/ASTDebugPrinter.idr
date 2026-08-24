module Frontend.Syntax.ASTDebugPrinter

import Data.List1

import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Token
import Frontend.Syntax.AST
import Frontend.Syntax.ASTPrettyPrinter
import Frontend.Syntax.Common
import Frontend.Syntax.Name
import Frontend.Syntax.Operator
import Frontend.Syntax.Pattern
import Frontend.Syntax.Type

-- This diagnostic traversal follows the same mutually-recursive graph as the
-- AST.  Its source fragments are still produced by the total strict printer.
%default covering

-- Structural output for inspecting the surface AST.  Every `strict` value is
-- delegated to PrettyStrict, which adds all syntactically optional
-- parentheses.  This module does not change the source-oriented Show instances.

indent : Nat -> String
indent Z = ""
indent (S n) = "  " ++ indent n

astNodeNumber : AstInfo -> Nat
astNodeNumber (MkAstInfo (MkNodeId surfaceId desugarId) _) = surfaceId

node : Nat -> String -> String -> AstInfo -> String -> String
node depth ty ctor info strict =
  indent depth ++ "SurfaceAstNode<" ++ ty ++ "> #" ++ show (astNodeNumber info) ++
  " " ++ ctor ++ "\n" ++ indent (S depth) ++ "strict = " ++ show strict ++ "\n"

field : Nat -> String -> String -> String
field depth name value = indent depth ++ name ++ " = " ++ show value ++ "\n"

nameText : SurfaceName -> String
nameText (MkAstNode _ _ (MkNameNode text)) = text

leafText : Show a => SurfaceAstNode a -> String
leafText (MkAstNode _ _ value) = show value

exprCtor : ExpressionNode SurfaceAstPhase -> String
exprCtor e = case e of
  ExprLiteral _ => "ExprLiteral"
  ExprName _ => "ExprName"
  ExprPath _ => "ExprPath"
  ExprBuiltin _ => "ExprBuiltin"
  ExprSelf => "ExprSelf"
  ExprParenthesized _ => "ExprParenthesized"
  ExprTuple _ => "ExprTuple"
  ExprArray _ => "ExprArray"
  ExprRepeatedArray _ _ => "ExprRepeatedArray"
  ExprStructLiteral _ _ => "ExprStructLiteral"
  ExprCall _ _ => "ExprCall"
  ExprMethodCall _ _ _ => "ExprMethodCall"
  ExprField _ _ => "ExprField"
  ExprTupleIndex _ _ => "ExprTupleIndex"
  ExprIndex _ _ => "ExprIndex"
  ExprUnary _ _ => "ExprUnary"
  ExprBinary _ _ _ => "ExprBinary"
  ExprRange _ _ _ => "ExprRange"
  ExprCast _ _ => "ExprCast"
  ExprBlock _ => "ExprBlock"
  ExprIf _ => "ExprIf"
  ExprQIf _ => "ExprQIf"
  ExprSIf _ => "ExprSIf"
  ExprMatch _ => "ExprMatch"
  ExprQMatch _ => "ExprQMatch"
  ExprSMatch _ => "ExprSMatch"
  ExprLoop _ => "ExprLoop"
  ExprWhile _ _ => "ExprWhile"
  ExprFor _ _ _ => "ExprFor"
  ExprBreak _ => "ExprBreak"
  ExprContinue => "ExprContinue"
  ExprReturn _ => "ExprReturn"
  ExprCtrl _ => "ExprCtrl"
  ExprAdjoint _ => "ExprAdjoint"

mutual
  debugExpr : Nat -> SurfaceExpr -> String
  debugExpr depth expression@(MkAstNode info _ value) =
    node depth "ExpressionNode" (exprCtor value) info (showExprStrict expression) ++
    debugExprChildren (S depth) value

  debugExprChildren : Nat -> ExpressionNode SurfaceAstPhase -> String
  debugExprChildren depth e = case e of
    ExprParenthesized x => debugExpr depth x
    ExprTuple xs => debugExprs depth (forget xs)
    ExprArray xs => debugExprs depth xs
    ExprRepeatedArray x n => debugExpr depth x ++ debugExpr depth n
    ExprCall f xs => debugExpr depth f ++ debugExprs depth xs
    ExprMethodCall x method xs =>
      field depth "method" (nameText method) ++ debugExpr depth x ++ debugExprs depth xs
    ExprField x name => field depth "field" (nameText name) ++ debugExpr depth x
    ExprTupleIndex x index => field depth "index" index ++ debugExpr depth x
    ExprIndex x index => debugExpr depth x ++ debugExpr depth index
    ExprUnary _ x => debugExpr depth x
    ExprBinary _ x y => debugExpr depth x ++ debugExpr depth y
    ExprRange x _ y => debugMaybeExpr depth x ++ debugMaybeExpr depth y
    ExprCast x ty => debugExpr depth x ++ debugTy depth ty
    ExprBlock block => debugBlock depth block
    ExprLoop block => debugBlock depth block
    ExprWhile condition block => debugExpr depth condition ++ debugBlock depth block
    ExprFor pattern iterable block =>
      field depth "pattern" (showPattern pattern) ++ debugExpr depth iterable ++ debugBlock depth block
    ExprBreak x => debugMaybeExpr depth x
    ExprReturn x => debugMaybeExpr depth x
    _ => ""

  debugExprs : Nat -> List SurfaceExpr -> String
  debugExprs _ [] = ""
  debugExprs depth (x :: xs) = debugExpr depth x ++ debugExprs depth xs

  debugMaybeExpr : Nat -> Maybe SurfaceExpr -> String
  debugMaybeExpr _ Nothing = ""
  debugMaybeExpr depth (Just x) = debugExpr depth x

  debugTy : Nat -> SurfaceTy -> String
  debugTy depth ty@(MkAstNode info _ value) = case value of
    TyPrimitive primitive =>
      node depth "TyNode SurfaceExpr" "TyPrimitive" info (showTyStrict ty) ++
      field (S depth) "primitive" (showTypPrimLeaf primitive)
    TyPath _ => node depth "TyNode SurfaceExpr" "TyPath" info (showTyStrict ty)
    TyUnit => node depth "TyNode SurfaceExpr" "TyUnit" info (showTyStrict ty)
    TyParenthesized x =>
      node depth "TyNode SurfaceExpr" "TyParenthesized" info (showTyStrict ty) ++ debugTy (S depth) x
    TyTuple xs =>
      node depth "TyNode SurfaceExpr" "TyTuple" info (showTyStrict ty) ++ debugTys (S depth) (forget xs)
    TyArray element size =>
      node depth "TyNode SurfaceExpr" "TyArray" info (showTyStrict ty) ++
      debugTy (S depth) element ++ debugExpr (S depth) size
    TySlice x => node depth "TyNode SurfaceExpr" "TySlice" info (showTyStrict ty) ++ debugTy (S depth) x
    TyReference _ x => node depth "TyNode SurfaceExpr" "TyReference" info (showTyStrict ty) ++ debugTy (S depth) x
    TyQualified _ x => node depth "TyNode SurfaceExpr" "TyQualified" info (showTyStrict ty) ++ debugTy (S depth) x
    TyFunction _ _ result =>
      node depth "TyNode SurfaceExpr" "TyFunction" info (showTyStrict ty) ++ debugMaybeTy (S depth) result

  debugTys : Nat -> List SurfaceTy -> String
  debugTys _ [] = ""
  debugTys depth (x :: xs) = debugTy depth x ++ debugTys depth xs

  debugMaybeTy : Nat -> Maybe SurfaceTy -> String
  debugMaybeTy _ Nothing = ""
  debugMaybeTy depth (Just x) = debugTy depth x

  debugBlock : Nat -> SurfaceBlock -> String
  debugBlock depth block@(MkAstNode info _ (MkBlockNode _ statements final)) =
    node depth "BlockNode" "MkBlockNode" info (showBlockStrict block) ++
    debugStatements (S depth) statements ++ debugMaybeExpr (S depth) final

  debugStatements : Nat -> List SurfaceStatement -> String
  debugStatements _ [] = ""
  debugStatements depth (x :: xs) = debugStatement depth x ++ debugStatements depth xs

  debugStatement : Nat -> SurfaceStatement -> String
  debugStatement depth (MkAstNode info _ statement) = case statement of
    StatementLet binding =>
      node depth "StatementNode" "StatementLet" info "let ...;" ++ debugLet (S depth) binding
    StatementAssignment assignment =>
      node depth "StatementNode" "StatementAssignment" info "<assignment>"
    StatementSemiExpression expression =>
      node depth "StatementNode" "StatementSemiExpression" info (showExprStrict expression ++ ";") ++
      debugExpr (S depth) expression
    StatementExpression expression =>
      node depth "StatementNode" "StatementExpression" info (showExprStrict expression) ++
      debugExpr (S depth) expression

  debugLet : Nat -> LetBindingNode SurfaceAstPhase -> String
  debugLet depth (MkLetBindingNode _ pattern annotation initializer) =
    indent depth ++ "LetBindingNode MkLetBindingNode\n" ++
    field (S depth) "pattern" (showPattern pattern) ++ debugMaybeTy (S depth) annotation ++
    debugInitializer (S depth) initializer

  debugInitializer : Nat -> Maybe (LetInitializerNode SurfaceAstPhase) -> String
  debugInitializer _ Nothing = ""
  debugInitializer depth (Just (MkLetInitializerNode marker value)) =
    indent depth ++ "LetInitializerNode MkLetInitializerNode\n" ++
    field (S depth) "marker" (leafText marker) ++ debugExpr (S depth) value

  debugParameter : Nat -> SurfaceFunctionParameter -> String
  debugParameter depth (MkAstNode info _ parameter) = case parameter of
    NormalParameter _ mutability name ty =>
      node depth "FunctionParameterNode" "NormalParameter" info
        (maybe "" (\m => leafText m ++ " ") mutability ++ nameText name ++ ": " ++ showTyStrict ty) ++
      field (S depth) "name" (nameText name) ++ debugTy (S depth) ty
    ReceiverParameter _ borrow =>
      node depth "FunctionParameterNode" "ReceiverParameter" info
        (maybe "self" (\b => leafText b ++ "self") borrow)

  debugParameters : Nat -> List SurfaceFunctionParameter -> String
  debugParameters _ [] = ""
  debugParameters depth (x :: xs) = debugParameter depth x ++ debugParameters depth xs

  debugFunction : Nat -> AstInfo -> SurfaceItem -> FunctionDeclarationNode SurfaceAstPhase -> String
  debugFunction depth info item function = case function of
    MkFunctionDeclarationNode _ _ _ _ _ name params result _ _ body =>
      node depth "ItemNode" "ItemFunction(FunctionDeclarationNode)" info (showItemStrict item) ++
      field (S depth) "functionName" (nameText name) ++ debugParameters (S depth) params ++
      debugMaybeTy (S depth) result ++ debugBlock (S depth) body

  debugItem : Nat -> SurfaceItem -> String
  debugItem depth item@(MkAstNode info _ value) = case value of
    ItemFunction function => debugFunction depth info item function
    ItemModule _ => node depth "ItemNode" "ItemModule" info (showItemStrict item)
    ItemUse _ => node depth "ItemNode" "ItemUse" info (showItemStrict item)
    ItemConst _ => node depth "ItemNode" "ItemConst" info (showItemStrict item)
    ItemEnum _ => node depth "ItemNode" "ItemEnum" info (showItemStrict item)
    ItemQEnum _ => node depth "ItemNode" "ItemQEnum" info (showItemStrict item)
    ItemStruct _ => node depth "ItemNode" "ItemStruct" info (showItemStrict item)
    ItemImpl _ => node depth "ItemNode" "ItemImpl" info (showItemStrict item)

  debugItems : Nat -> List SurfaceItem -> String
  debugItems _ [] = ""
  debugItems depth (x :: xs) = debugItem depth x ++ debugItems depth xs

public export
showAstDebug : SurfaceSourceFile -> String
showAstDebug source@(MkAstNode info _ (MkSourceFileNode _ items)) =
  node 0 "SourceFileNode" "MkSourceFileNode" info (showSourceFileStrict source) ++ debugItems 1 items
