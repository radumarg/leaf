module Frontend.ASTPhases

import Frontend.ASTData
import Frontend.Source
import Frontend.Type

%default total

--------------------------------------------------------------------------------
-- Phase-specific AST wrappers
--------------------------------------------------------------------------------

data AstPhase     -- The four phases of the AST, each with its own metadata.
  = Surface       -- parser output
  | Canonical     -- desugaring output
  | Resolved      -- name and scope resolution output
  | Typed         -- type checking output

record CanonicalMetadata where
  constructor MkCanonicalMetadata
  origin : NodeOrigin

record ResolvedMetadata where
  constructor MkResolvedMetadata
  origin : NodeOrigin

record TypedMetadata where
  constructor MkTypedMetadata
  origin       : NodeOrigin
  inferredType : LeafType

MetadataFor : AstPhase -> Type
MetadataFor Surface   = ()
MetadataFor Canonical = CanonicalMetadata
MetadataFor Resolved  = ResolvedMetadata
MetadataFor Typed     = TypedMetadata

public export
record AstNode (phase : AstPhase) a where
  constructor MkAstNode
  astInfo  : AstInfo
  metadata : MetadataFor phase
  value    : a

--------------------------------------------------------------------------------
-- Name aliases for the four phases
--------------------------------------------------------------------------------

public export
SurfaceAstNode : Type -> Type
SurfaceAstNode = AstNode Surface

public export
CanonicalAstNode : Type -> Type
CanonicalAstNode = AstNode Canonical

public export
ResolvedAstNode : Type -> Type
ResolvedAstNode = AstNode Resolved

public export
TypedAstNode : Type -> Type
TypedAstNode = AstNode Typed

--------------------------------------------------------------------------------
-- Smart constructors for the four phases
--------------------------------------------------------------------------------

public export
surfaceAstNode : AstInfo -> a -> SurfaceAstNode a
surfaceAstNode astInfo value =
  MkAstNode astInfo () value

public export
canonicalAstNode : AstInfo -> NodeOrigin -> a -> CanonicalAstNode a
canonicalAstNode astInfo origin value =
  MkAstNode astInfo (MkCanonicalMetadata origin) value

public export
resolvedAstNode : AstInfo -> NodeOrigin -> a -> ResolvedAstNode a
resolvedAstNode astInfo origin value =
  MkAstNode astInfo (MkResolvedMetadata origin) value

public export
typedAstNode : AstInfo -> NodeOrigin -> LeafType -> a -> TypedAstNode a
typedAstNode astInfo origin inferredType value =
  MkAstNode astInfo (MkTypedMetadata origin inferredType) value

--------------------------------------------------------------------------------
-- Show instance: print the payload only, never the bookkeeping
--------------------------------------------------------------------------------

public export
Show a => Show (AstNode phase a) where
  show (MkAstNode _ _ value) = show value
