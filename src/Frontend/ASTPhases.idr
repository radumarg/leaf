module Frontend.ASTPhases

import Frontend.ASTData
import Frontend.Source
import Frontend.Syntax.Common

%default total

--------------------------------------------------------------------------------
-- Phase-specific AST wrappers
--------------------------------------------------------------------------------
-- POLICY: which node families get all four phase aliases, and which do not.
--
-- NON-RECURSIVE payloads (NameNode, DocCommentNode, LiteralNode, ...) get
-- Surface/Canonical/Resolved/Typed aliases wherever they make sense: the
-- wrapper sits entirely outside a flat value, so an alias costs three lines
-- and nothing else.
--
-- RECURSIVE node families (types, expressions, statements, patterns) are
-- SURFACE-PHASE ONLY: their recursion goes through the located wrapper
-- (children are `SurfaceAstNode (Node ...)`), and abstracting that wrapper
-- into a `wrapper : Type -> Type` parameter is not strictly positive --
-- Idris 2's totality checker rejects `wrapper (Node wrapper ...)` for the
-- same reason it rejects `data Fix f = MkFix (f (Fix f))`.
--
-- This is not just a workaround; it matches the pipeline's intent. The
-- phases should not share grammars:
--
--   * Canonical is a SMALLER language: desugaring removes for-loops,
--     compound assignment, method calls, paren nodes. Where a payload is
--     genuinely unchanged (types are not rewritten by desugaring), the
--     canonical tree may simply embed surface nodes directly.
--   * Resolved changes CONTENT, not decoration: e.g. a path type stops
--     holding a written path and starts holding a SymbolId target. No
--     aliasing scheme expresses that.
--   * Typed output is largely SEMANTIC, not syntactic: checked types are
--     the checker's own representation, carried in the TypedAstNode
--     wrapper, not a re-decorated syntax tree.
--
-- So each later phase defines its own node families inside the pass that
-- produces them, when that pass's real requirements are known. The absence
-- of Canonical/Resolved/Typed aliases for recursive families is deliberate.
--------------------------------------------------------------------------------

-- parser output
public export
record SurfaceAstNode a where
  constructor MkSurfaceAstNode
  astInfo : AstInfo
  value   : a

-- desugaring output
public export
record CanonicalAstNode a where
  constructor MkCanonicalAstNode
  astInfo : AstInfo
  origin  : NodeOrigin
  value   : a

-- name resolution output
public export
record ResolvedAstNode a where
  constructor MkResolvedAstNode
  astInfo : AstInfo
  origin  : NodeOrigin
  value   : a

-- type checking output
public export
record TypedAstNode a where
  constructor MkTypedAstNode
  astInfo : AstInfo
  origin  : NodeOrigin
  value   : a
  -- add type information here

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- The span of an AstInfo, as a plain function. This exists because
-- downstream passes (Validate, and every pass after it) bind AstInfo values
-- from wrapper patterns without importing AstInfo's defining module, so its
-- record dot-projections are not in scope there. One accessor here -- where
-- that module IS in scope -- serves them all.
public export
astInfoSpan : AstInfo -> SourceSpan
astInfoSpan (MkAstInfo _ nodeSpan) = nodeSpan

public export
interface HasNodeValue (wrapper : Type -> Type) where
  getNodeValue : wrapper a -> a

public export
HasNodeValue SurfaceAstNode where
  getNodeValue (MkSurfaceAstNode _ value) = value

public export
HasNodeValue CanonicalAstNode where
  getNodeValue (MkCanonicalAstNode _ _ value) = value

public export
HasNodeValue ResolvedAstNode where
  getNodeValue (MkResolvedAstNode _ _ value) = value

public export
HasNodeValue TypedAstNode where
  getNodeValue (MkTypedAstNode _ _ value) = value

--------------------------------------------------------------------------------
-- Show instances: print the payload only, never the bookkeeping
--------------------------------------------------------------------------------
-- A pretty-printer wants source-shaped output, not node ids and spans, so
-- these instances unconditionally skip AstInfo (and NodeOrigin, on the later
-- phases) and defer to `Show` on the wrapped value alone. Every node family
-- that wants to be `Show` therefore only needs a `Show` instance on its own
-- (un-located) payload type -- see Frontend.Syntax.AstNodePrettyPrinter for
-- the surface AST's payload instances.
--------------------------------------------------------------------------------

public export
Show a => Show (SurfaceAstNode a) where
  show (MkSurfaceAstNode _ v) = show v

public export
Show a => Show (CanonicalAstNode a) where
  show (MkCanonicalAstNode _ _ v) = show v

public export
Show a => Show (ResolvedAstNode a) where
  show (MkResolvedAstNode _ _ v) = show v

public export
Show a => Show (TypedAstNode a) where
  show (MkTypedAstNode _ _ v) = show v