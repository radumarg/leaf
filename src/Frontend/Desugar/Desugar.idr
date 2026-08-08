module Frontend.Desugar.Desugar

import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Syntax.AST
import Frontend.Syntax.Doc


%default total

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
      ItemFunction declaration => ItemFunction ?desugar_function_declaration
    where
      desugar_const_declaration : ConstDeclarationNode SurfaceAstPhase -> ConstDeclarationNode CanonicalAstPhase
      desugar_const_declaration ?x = ?desugar_const_declaration


desugarDocComment : SurfaceDocComment -> CanonicalDocComment
desugarDocComment (MkAstNode docInfo _ docComment) =
  canonicalAstNode docInfo Written docComment

desugarSurfaceSyntax : SurfaceSourceFile -> CanonicalSourceFile
desugarSurfaceSyntax
    (MkAstNode fileInfo _ (MkSourceFileNode docs items)) =
  canonicalAstNode fileInfo Written $
    MkSourceFileNode
      (map desugarDocComment docs)
      (map desugarItem items)
