module Frontend.Syntax.Type

import Data.List1
import Frontend.Token
import Frontend.ASTPhases
import Frontend.Syntax.Common
import Frontend.Syntax.Name
import Frontend.Syntax.Operator

%default total

--------------------------------------------------------------------------------
-- Written type syntax
--------------------------------------------------------------------------------
-- Represents types exactly as the USER WROTE THEM: annotations on lets,
-- parameters, fields, returns, casts. No inference, no checking, no
-- canonicalization -- `[i32; 2 + 2]` stays `2 + 2`, it does not become 4.
--
-- Breaking the Ty/Expr module cycle:
--
--   * a type can contain an expression   ([T; N] -- N is written as an
--     expression; that it must be CONST is a semantic check, not syntax)
--   * an expression can contain a type   (casts `e as T`, typed lets)
--
-- Rather than making the modules mutually recursive, `TyNode` is
-- parameterized over the expression type used for array sizes. Expr.idr
-- ties the knot:
--
--   SurfaceTy : Type
--   SurfaceTy = Ty SurfaceAstPhase SurfaceExpr
--
-- The parameter does NOT leak beyond that point: every module downstream of
-- Expr.idr (Stmt, Contract, Decl, ...) uses the concrete SurfaceTy alias and
-- never mentions arraySizeExpr.
--
-- Representable examples:
--
--   qubit                          TyPrimitive
--   Person                         TyPath
--   my_module::Config              TyPath
--   ()                             TyUnit
--   (i32, f64)                     TyTuple
--   (qubit,)                       TyTuple (one element; trailing comma)
--   [i32; 2 + 2]                   TyArray
--   &[i32]  &mut [i32]             TyReference around TySlice
--   &qubit                         TyReference
--   affine qubit                   TyQualified
--   scratch linear qubit           TyQualified (source order preserved)
--   unitary fn(qs: [qubit; 4], target: qubit) -> ([qubit; 4], qubit)
--                                  TyFunction
--
-- Semantic rules that are deliberately REPRESENTABLE here and rejected later,
-- with the relevant spans available for diagnostics:
--
--   * `&mut qubit` / `&mut [qubit]`   -- mut is never written on qubit refs
--   * `[T; N]` with non-const N       -- const-ness of the size expression
--   * `linear affine qubit`           -- mutually exclusive qualifiers
--------------------------------------------------------------------------------

mutual

  public export
  data TyNode : (phase : AstPhase) -> (arraySizeExpr : Type) -> Type where

    -- Built-in primitive type name: qubit, qstate, bit, i32, f64, angle64,
    -- param, bool, ... Reuses the lexer's authoritative enumeration.
    TyPrimitive :
         (primitiveName : TypPrimName)
      -> TyNode phase arraySizeExpr

    -- User-defined named type: struct, enum, or qenum, possibly behind a
    -- module path (Person, my_module::Config). Which of those it actually
    -- names is resolution's job; here it is just a written path.
    --
    -- NOTE: there is deliberately no type-argument slot here. Leaf has no
    -- generics; if it ever grows them, TyPath acquires an argument list and
    -- every consumer changes. That is a language-design decision to make
    -- explicitly, not one to pre-wire.
    TyPath :
         (typePath : Path phase)
      -> TyNode phase arraySizeExpr

    -- The unit TYPE `()`. A dedicated constructor: `()` is never an
    -- empty tuple, so TyTuple below cannot represent it (List1 requires at
    -- least one element).
    TyUnit :
         TyNode phase arraySizeExpr

    -- A parenthesized type `(T)`. Kept explicit in the surface AST because
    -- Leaf has one-element tuple types `(T,)`: with both in the grammar,
    -- `(T)` and `(T,)` differ by a single token, and diagnostics like
    -- "help: `(T)` is a parenthesized type, not a 1-tuple -- add a trailing
    -- comma" need the written form. Discarded during canonicalization.
    TyParenthesized :
         (innerType : AstNode phase (TyNode phase arraySizeExpr))
      -> TyNode phase arraySizeExpr

    -- Tuple type with AT LEAST one element: (i32, f64), (qubit,).
    -- The List1 shape makes a zero-element tuple type unrepresentable --
    -- that source form is TyUnit. Whether the user wrote a trailing comma
    -- is only semantically visible in the one-element case, where it is
    -- the entire difference between TyTuple and TyParenthesized.
    TyTuple :
         (elementTypes : List1 (AstNode phase (TyNode phase arraySizeExpr)))
      -> TyNode phase arraySizeExpr

    -- Fixed-size array type [T; N]. The size is stored as a WRITTEN
    -- EXPRESSION (integer literal, `2 + 2`, a named constant N, ...);
    -- requiring it to be a const expression is a later pass's check.
    TyArray :
         (elementType    : AstNode phase (TyNode phase arraySizeExpr))
      -> (sizeExpression : arraySizeExpr)
      -> TyNode phase arraySizeExpr

    -- Slice type [T]. In well-formed Leaf source this only occurs behind a
    -- reference (&[T], &mut [T]) -- as TyReference wrapping TySlice -- but
    -- the AST does not enforce "slices only behind references"; a bare
    -- `[T]` annotation is a semantic (sizedness) error with a good span.
    TySlice :
         (elementType : AstNode phase (TyNode phase arraySizeExpr))
      -> TyNode phase arraySizeExpr

    -- Reference type: &T or &mut T, reusing BorrowKind so shared-vs-mutable
    -- is spelled once for both expressions and types. The borrow kind is
    -- located so "`mut` is never written on a qubit reference" can point at
    -- the `&mut` itself rather than the whole type.
    TyReference :
         (borrowKind     : AstNode phase BorrowKind)
      -> (referencedType : AstNode phase (TyNode phase arraySizeExpr))
      -> TyNode phase arraySizeExpr

    -- Quantum-qualified type: linear qubit, affine qubit, scratch linear
    -- qubit, ... Qualifiers are non-empty (a TyQualified node exists only
    -- because at least one qualifier was written), kept in SOURCE ORDER,
    -- and individually located, so `scratch linear` round-trips distinctly
    -- from `linear scratch` and a validation pass can point at the exact
    -- offending keyword in `linear affine qubit`.
    TyQualified :
         (storageQualifiers : List1 (AstNode phase QuantumStorageQualifier))
      -> (qualifiedType     : AstNode phase (TyNode phase arraySizeExpr))
      -> TyNode phase arraySizeExpr

    -- Function type, as used for higher-order parameters:
    --
    --   unitary fn(qs: [qubit; 4], target: qubit) -> ([qubit; 4], qubit)
    --
    -- The effect is optional exactly as on declarations: `Nothing` means no
    -- qualifier written, `Just` a located EffectGeneral means the user
    -- explicitly wrote `general`. The return type is optional: `Nothing`
    -- means no `->` was written, distinct from an explicit `-> ()`.
    TyFunction :
         (functionEffect     : Maybe (AstNode phase FunctionEffect))
      -> (functionParameters : List (AstNode phase (FunctionTypeParameterNode phase arraySizeExpr)))
      -> (returnType         : Maybe (AstNode phase (TyNode phase arraySizeExpr)))
      -> TyNode phase arraySizeExpr

  -- One parameter inside a FUNCTION TYPE: `qs: [qubit; 4]`. The name is
  -- required because every function-type parameter in the spec is written
  -- name-first; if Leaf ever admits Rust-style anonymous fn-type parameters
  -- (fn(i32) -> i32), this becomes `Maybe (Name phase)` -- a one-line change.
  -- Distinct from the (richer) declaration-side parameter in Decl.idr, which
  -- additionally carries doc comments and mutability.
  public export
  record FunctionTypeParameterNode (phase : AstPhase) (arraySizeExpr : Type) where
    constructor MkFunctionTypeParameterNode
    parameterName : Name phase
    parameterType : AstNode phase (TyNode phase arraySizeExpr)

--------------------------------------------------------------------------------
-- AST-wrapped type
--------------------------------------------------------------------------------
-- Expr.idr / AST.idr tie the knot by instantiating `arraySizeExpr` with the
-- located expression type at the same phase:
--
--   SurfaceTy : Type
--   SurfaceTy = Ty SurfaceAstPhase SurfaceExpr
--------------------------------------------------------------------------------

public export
Ty : (phase : AstPhase) -> (arraySizeExpr : Type) -> Type
Ty phase arraySizeExpr = AstNode phase (TyNode phase arraySizeExpr)
