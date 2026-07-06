module Frontend.Syntax.Name

import Frontend.Syntax.AST
import Frontend.Syntax.Common

%default total

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
-- Names in the surface AST
--------------------------------------------------------------------------------
-- This module represents textual, unresolved names as they appear in source code.
--
-- Important design choice:
--
--   * This module does NOT resolve names.
--   * It does NOT decide whether a name denotes a local variable, function,
--     type, enum variant, module, field, method, or associated function.
--   * It preserves source locations so later phases can report diagnostics
--     against the exact written name/path.
--
-- Examples represented by these types:
--
--   x
--   q
--   helper
--   my_library::helper
--   Data::Left
--   Person::new
--
-- Builtins such as qalloc, measr, reset, ctrl, apply, etc. are not modeled here
-- as ordinary names if the lexer classifies them as TokBuiltin. Expression/callee
-- syntax can later decide how to represent builtin callees.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Simple unresolved names
--------------------------------------------------------------------------------

public export
record NameNode where
  constructor MkNameNode
  nameNodeText : String

public export
SurfaceName : Type
SurfaceName = SurfaceAstNode NameNode

public export
CanonicalName : Type
CanonicalName = CanonicalAstNode NameNode

--------------------------------------------------------------------------------
-- Simple resolved names
--
-- The SymbolId tells which binding/program entity this name denotes.
--------------------------------------------------------------------------------

public export
record ResolvedNameNode where
  constructor MkResolvedNameNode
  resolvedNameNodeText     : String
  resolvedNameNodeSymbolId : SymbolId

public export
ResolvedName : Type
ResolvedName = ResolvedAstNode ResolvedNameNode

public export
TypedName : Type
TypedName = TypedAstNode ResolvedNameNode

--------------------------------------------------------------------------------
-- Path segments
--------------------------------------------------------------------------------
-- A path segment is one component of a Rust-style :: path.
--
-- Examples:
--
--   my_library::helper
--   ^^^^^^^^^^  ^^^^^^
--   segment     segment
--
--   Data::Left
--   ^^^^  ^^^^
--   segment segment
--
-- The `self` keyword is syntactically special in Leaf and lexed as a keyword,
-- not an ordinary identifier, so it gets its own segment constructor.
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Unresolved path segments
--------------------------------------------------------------------------------

public export
data PathSegmentNode
  = PathSegmentName String
  | PathSegmentSelf

public export
SurfacePathSegment : Type
SurfacePathSegment = SurfaceAstNode PathSegmentNode

public export
CanonicalPathSegment : Type
CanonicalPathSegment = CanonicalAstNode PathSegmentNode


--------------------------------------------------------------------------------
-- Paths
--------------------------------------------------------------------------------
-- A Path represents one or more path segments separated by `::`.
--
-- The AST enforces the basic syntactic invariant that a path is non-empty.
--
-- Examples:
--
--   x
--   my_library::helper
--   Data::Left
--   Person::new
--
-- A single identifier like `x` can be represented as a path with one segment,
-- but local binders and declarations should usually use `Name` directly.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Unresolved paths
--------------------------------------------------------------------------------

public export
record PathNode segment where
  constructor MkPathNode
  firstSegment      : segment
  remainingSegments : List segment

public export
SurfacePath : Type
SurfacePath = SurfaceAstNode (PathNode SurfacePathSegment)

public export
CanonicalPath : Type
CanonicalPath = CanonicalAstNode (PathNode CanonicalPathSegment)


--------------------------------------------------------------------------------
-- Resolved paths
--------------------------------------------------------------------------------
-- Keep resolved paths intentionally simple.
--
-- The resolver preserves the written path as text and records the final symbol
-- the whole path resolved to.
--
-- Example:
--
--   Data::Left
--
-- becomes something like:
--
--   firstPathSegmentText      = "Data"
--   remainingPathSegmentTexts = ["Left"]
--   resolvedPathTargetSymbolId = SymbolId for Left
--------------------------------------------------------------------------------

public export
record ResolvedPathNode where
  constructor MkResolvedPathNode
  firstPathSegmentText       : String
  remainingPathSegmentTexts  : List String
  resolvedPathTargetSymbolId : SymbolId

public export
ResolvedPath : Type
ResolvedPath = ResolvedAstNode ResolvedPathNode

public export
TypedPath : Type
TypedPath = TypedAstNode ResolvedPathNode

--------------------------------------------------------------------------------
-- Qualified names
--------------------------------------------------------------------------------
-- A QualifiedName is useful when later AST nodes want to distinguish:
--
--   optional qualifier path + final name/segment
--
-- This is often convenient for enum variants, qenum variants, associated
-- functions, and imported names.
--
-- Examples:
--
--   x
--     qualifierPath = Nothing
--     finalSegment  = x
--
--   Data::Left
--     qualifierPath = Just Data
--     finalSegment  = Left
--
--   Person::new
--     qualifierPath = Just Person
--     finalSegment  = new
--
-- This is still unresolved. `Person::new` is not known to be a method here,
-- and `Data::Left` is not known to be an enum/qenum variant here.
--------------------------------------------------------------------------------

public export
record QualifiedNameNode path name where
  constructor MkQualifiedNameNode
  qualifierPath : Maybe path
  finalName     : name

public export
SurfaceQualifiedName : Type
SurfaceQualifiedName =
  SurfaceAstNode (QualifiedNameNode SurfacePath SurfaceName)

public export
CanonicalQualifiedName : Type
CanonicalQualifiedName =
  CanonicalAstNode (QualifiedNameNode CanonicalPath CanonicalName)

public export
ResolvedQualifiedName : Type
ResolvedQualifiedName =
  ResolvedAstNode (QualifiedNameNode ResolvedPath ResolvedName)

public export
TypedQualifiedName : Type
TypedQualifiedName =
  TypedAstNode (QualifiedNameNode TypedPath TypedName)
