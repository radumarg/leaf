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
--
-- Design choices:
--
--   * The AST records the raw spelling of every doc comment, so a
--     pretty-printer or doc generator can round-trip the exact source text.
--   * `DocCommentPlacement` is stored on the node even though the attachment
--     site (a `List DocComment` on an item vs. on a module/block) already
--     implies it. Keeping it explicit preserves what the user actually wrote,
--     which matters for diagnostics such as "inner doc comment is only
--     allowed at the start of a module or block".
--   * Attachment itself is NOT modeled here. Items, fields, variants, and
--     parameters carry their own `List SurfaceDocComment` (outer docs);
--     modules and blocks carry theirs (inner docs). This module only defines
--     what a single doc comment is.
--
-- Relationship to the lexer: `TokOuterDoc`/`TokInnerDoc` carry the raw text
-- and already fix the placement. The line-vs-block distinction is recovered
-- by the parser from the leading characters of the raw spelling ("///" or
-- "//!" vs. "/**" or "/*!"). This assumes the lexer preserves the comment
-- delimiters in the token payload; if the lexer ever strips them, the
-- kind must instead be carried on the token itself.
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
--------------------------------------------------------------------------------
-- A single documentation comment as it appeared in source.
--
-- `docCommentRawText` is the full raw spelling, delimiters included, exactly
-- as the lexer captured it. No trimming, no de-indentation, no stripping of
-- leading `*` in block docs -- all of that is presentation-layer work for a
-- later documentation pass, not the AST's concern.
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
-- Doc comments are phase-invariant payloads: resolution and type checking
-- never change their content, only the wrapper node changes as the comment is
-- carried along on the declarations it documents.
--------------------------------------------------------------------------------

public export
SurfaceDocComment : Type
SurfaceDocComment = SurfaceAstNode DocCommentNode

public export
CanonicalDocComment : Type
CanonicalDocComment = CanonicalAstNode DocCommentNode

public export
ResolvedDocComment : Type
ResolvedDocComment = ResolvedAstNode DocCommentNode

public export
TypedDocComment : Type
TypedDocComment = TypedAstNode DocCommentNode