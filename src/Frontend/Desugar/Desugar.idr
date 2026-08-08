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
      ItemConst declaration => ItemConst ?desugar_const_declaration
      ItemEnum declaration => ItemEnum ?desugar_enum_declaration
      ItemQEnum declaration => ItemQEnum ?desugar_qenum_declaration
      ItemStruct declaration => ItemStruct ?desugar_struct_declaration
      ItemImpl declaration => ItemImpl ?desugar_impl_declaration
      ItemFunction declaration => ItemFunction ?desugar_function_declaration

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
