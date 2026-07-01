module Frontend.Syntax.Name

import Frontend.Source

%default total

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
-- Simple textual names
--------------------------------------------------------------------------------

public export
data NameNode
  = MkNameNode String

public export
Name : Type
Name = Located NameNode


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

public export
data PathSegmentNode
  = PathSegmentName String
  | PathSegmentSelf

public export
PathSegment : Type
PathSegment = Located PathSegmentNode


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

public export
record PathNode where
  constructor MkPathNode
  firstSegment      : PathSegment
  remainingSegments : List PathSegment

public export
Path : Type
Path = Located PathNode


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
record QualifiedNameNode where
  constructor MkQualifiedNameNode
  qualifierPath : Maybe Path
  finalSegment  : PathSegment

public export
QualifiedName : Type
QualifiedName = Located QualifiedNameNode


--------------------------------------------------------------------------------
-- Small query helpers
--------------------------------------------------------------------------------
-- These helpers are intentionally simple. They are useful in tests, parser code,
-- diagnostics, pretty-printers, and later compiler passes.
--------------------------------------------------------------------------------

public export
nameText : Name -> String
nameText locatedName =
  case value locatedName of
    MkNameNode text => text


public export
pathSegmentText : PathSegment -> String
pathSegmentText locatedSegment =
  case value locatedSegment of
    PathSegmentName text => text
    PathSegmentSelf      => "self"


public export
pathSegments : Path -> List PathSegment
pathSegments locatedPath =
  let node = value locatedPath in
    firstSegment node :: remainingSegments node


public export
qualifiedNameSegments : QualifiedName -> List PathSegment
qualifiedNameSegments locatedQualifiedName =
  let node = value locatedQualifiedName in
    case qualifierPath node of
      Nothing =>
        [finalSegment node]

      Just qualifier =>
        pathSegments qualifier ++ [finalSegment node]


public export
isSingleSegmentPath : Path -> Bool
isSingleSegmentPath locatedPath =
  case remainingSegments (value locatedPath) of
    [] => True
    _  => False


public export
isUnqualifiedName : QualifiedName -> Bool
isUnqualifiedName locatedQualifiedName =
  case qualifierPath (value locatedQualifiedName) of
    Nothing => True
    Just _  => False