module Frontend.Syntax.Doc

import Frontend.ASTPhases

%default total

--------------------------------------------------------------------------------
-- Documentation comments in the AST
--------------------------------------------------------------------------------
-- Normal comments (`//`, `/* */`) are discarded by the lexer and never reach
-- the AST. Documentation comments, however, are part of the surface syntax:
--
--   /// outer line doc      -- documents the item that FOLLOWS
--   /** outer block doc */  -- documents the item that FOLLOWS
--   //! inner line doc      -- documents the ENCLOSING module/block
--   /*! inner block doc */  -- documents the ENCLOSING module/block
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Doc comment shape
--------------------------------------------------------------------------------

-- Whether the doc comment was written in line form or block form.
public export
data DocCommentKind
  = DocCommentLine   -- ///  or  //!
  | DocCommentBlock  -- /** ... */  or  /*! ... */

-- What the doc comment attaches to, as written in source.
public export
data DocCommentPlacement
  = DocCommentOuter  -- ///  or  /** ... */  : documents the following item,
                     -- field, variant, or parameter
  | DocCommentInner  -- //!  or  /*! ... */  : documents the enclosing
                     -- module or block

--------------------------------------------------------------------------------
-- Doc comment node
-- A single documentation comment as it appeared in source.
--------------------------------------------------------------------------------

public export
record DocCommentNode where
  constructor MkDocCommentNode
  docCommentKind      : DocCommentKind
  docCommentPlacement : DocCommentPlacement
  docCommentRawText   : String

--------------------------------------------------------------------------------
-- Phase wrappers
--------------------------------------------------------------------------------

public export
DocComment : AstPhase -> Type
DocComment phase = AstNode phase DocCommentNode

public export
SurfaceDocComment : Type
SurfaceDocComment = DocComment SurfaceAstPhase

public export
CanonicalDocComment : Type
CanonicalDocComment = DocComment CanonicalAstPhase

public export
ResolvedDocComment : Type
ResolvedDocComment = DocComment ResolvedAstPhase

public export
TypedDocComment : Type
TypedDocComment = DocComment TypedAstPhase