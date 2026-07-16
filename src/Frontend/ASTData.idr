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
  id : Nat

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
-- Desugaring / generation provenance
--------------------------------------------------------------------------------

-- Type of desugaring that 
--was applied to a node.
public export
data DesugarKind
  = ForLoopDesugar
  | MethodCallDesugar
  | CompoundAssignmentDesugar
  -- other desugarings will be added here

-- Type of generated node that was created 
-- by the compiler during desugaring.
public export
data GeneratedKind
  = SyntheticReturn
  | SyntheticBlock
  | TemporaryBinding
  | BuiltinExpansion
  | OtherGenerated String

-- Type of origin for a node in the AST
-- Written: node was written by the user
-- Desugared: node was created by the 
-- compiler during desugaring.
-- Generated: node was created by the 
-- compiler during code generation.
public export
data NodeOrigin
  = Written
  | Desugared DesugarKind
  | Generated GeneratedKind

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
--------------------------------------------------------------------------------

public export
reserveNodeId : Nat -> (NodeId, Nat)
reserveNodeId current =
    (MkNodeId current, S current)