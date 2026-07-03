module Frontend.Syntax.AST

import Frontend.Syntax.Common

%default total

--------------------------------------------------------------------------------
-- Phase-specific AST wrappers
--------------------------------------------------------------------------------

public export
record SurfaceAstNode a where
  constructor MkSurfaceAstNode
  astInfo : AstInfo
  value   : a

public export
record CanonicalAstNode a where
  constructor MkCanonicalAstNode
  astInfo : AstInfo
  origin  : NodeOrigin
  value   : a

public export
record ResolvedAstNode a where
  constructor MkResolvedAstNode
  astInfo : AstInfo
  origin  : NodeOrigin
  value   : a

public export
record TypedAstNode a where
  constructor MkTypedAstNode
  astInfo : AstInfo
  origin  : NodeOrigin
  value   : a
  -- add type information here



-- Scopes are introduced by:
-- - modules;
-- - functions;
-- - blocks;
-- - lambda expressions if Leaf has them;
-- - match arms;

-- record ResolvedBlock where
--   constructor MkResolvedBlock
--   info    : AstInfo
--   scopeId : ScopeId
--   stmts   : List ResolvedStmt

-- Surface AST
--   - SourceSpan
--   - NodeId
--   - user-written syntax
--   - unresolved names as String / SurfaceName

--   ↓ desugaring

-- Canonical AST
--   - SourceSpan
--   - NodeId
--   - Origin information
--   - fewer syntactic forms
--   - still unresolved names

--   ↓ name resolution

-- Resolved AST
--   - same canonical structure
--   - declarations have SymbolId
--   - name uses have SymbolId
--   - symbol table produced separately
--   - scope tree produced separately

--   ↓ type checking

-- Typed AST
--   - resolved names
--   - every expression has Type
--   - declarations have type signatures / schemes
--   - optional compact effect/linearity annotations


-- Resolved AST:
--   AstInfo
--   ResolvedName
--   SymbolId on declarations
--   ScopeId on scope-introducing nodes

-- Typed AST:
--   AstInfo
--   ResolvedName
--   Type on every expression
--   function types on declarations
--   compact effect/linearity annotations only where needed

-- Leaf pipeline

-- AST node:
--   NodeId
--   SourceSpan
--   Origin

-- Name resolution:
--   adds SymbolId to names
--   builds SymbolTable
--   builds ScopeTree

-- Type checking:
--   adds Type to expressions
--   maybe adds Effect/Usage annotations

-- Optional indexing pass:
--   builds ParentMap
--   builds NodeId → Span map
--   builds NodeId → enclosing function map
--   builds NodeId → enclosing scope map