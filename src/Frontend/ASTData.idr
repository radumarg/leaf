module Frontend.ASTData

import Frontend.Source

%default total

--------------------------------------------------------------------------------
-- Compiler ids
--------------------------------------------------------------------------------

-- Unique node id for 
-- each AST node.
public export
record NodeId where
  constructor MkNodeId
  surfaceId : Nat
  desugarId: Nat

-- Unique id for a name/binding, 
-- introduced by the program.
-- Not all nodes introduce a name.
-- Not all names are introduced by a node, 
-- e.g. builtins, imports are not.
public export
record SymbolId where
  constructor MkSymbolId
  id : Nat

-- Blocks, functions, modules, 
-- can introduce scopes.
-- Unique id for a lexical scope.
public export
record ScopeId where
  constructor MkScopeId
  id : Nat

--------------------------------------------------------------------------------
-- Node provenance: written/desugaring/type-checker
--------------------------------------------------------------------------------

public export
data NodeProvenance
  = Written
  | DesugarQubitQualifier
  | DesugarFunctionEffect
  | DesugarFunctionReturnType


public export
Show NodeProvenance where
  show Written = "user written code"
  show DesugarQubitQualifier = "auto-generated default qubit qualifier"
  show DesugarFunctionEffect = "auto-generated default function effect"
  show DesugarFunctionReturnType = "auto-generated default function return type"

--------------------------------------------------------------------------------
-- Common AST information
--------------------------------------------------------------------------------

public export
record AstInfo where
  constructor MkAstInfo
  nodeId : NodeId
  span   : SourceSpan

--------------------------------------------------------------------------------
-- Scope information
--
-- Scope data live in a scope tree / resolver output, not directly on every AST node.
-- Nodes that introduce scopes carry their own ScopeId in their payload.
--------------------------------------------------------------------------------

public export
record Scope where
  constructor MkScope
  id      : ScopeId
  parent  : Maybe ScopeId
  symbols : List SymbolId

--------------------------------------------------------------------------------
-- Helpers for AST nodes
--
-- Creates a NodeId with current Id, and returns the next Id for the next node.
--------------------------------------------------------------------------------

public export
reserveNodeId : Nat -> (NodeId, Nat)
reserveNodeId current = (MkNodeId current 0, S current)
