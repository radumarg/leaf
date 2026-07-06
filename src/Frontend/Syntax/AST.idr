module Frontend.Syntax.AST

import Frontend.Syntax.Common

%default total

--------------------------------------------------------------------------------
-- Phase-specific AST wrappers
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


-- SymbolId = what binding/name?
-- ScopeId  = where is the binding visible?
-- NodeId   = which AST node?


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


-- Attach SymbolId only to AST places that are about name binding or name reference.

-- So:

-- function declaration     → has SymbolId
-- parameter declaration    → has SymbolId
-- let-bound variable       → has SymbolId
-- variable use             → has SymbolId
-- function call callee     → usually has SymbolId if it is a named function
-- type name use            → has SymbolId
-- module/import name       → may have SymbolId
-- literal                  → no SymbolId
-- binary expression        → no SymbolId, unless operators are resolved as symbols
-- if expression            → no SymbolId
-- block                    → no SymbolId, but may have ScopeId
-- return statement         → no SymbolId

-- Use SymbolId here:

-- ResolvedFnDecl.symbolId
-- ResolvedParam.symbolId
-- ResolvedLetBinding.symbolId
-- ResolvedName.symbolId
-- ResolvedTypeName.symbolId

-- Use ScopeId here:

-- ResolvedProgram.scopeId
-- ResolvedModule.scopeId
-- ResolvedFnDecl.body.scopeId
-- ResolvedBlock.scopeId

-- NodeId     → every AST node
-- SymbolId   → declarations and resolved name uses
-- ScopeId    → nodes that introduce scopes
-- Type       → expressions in the typed AST