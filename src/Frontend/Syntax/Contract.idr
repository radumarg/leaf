module Frontend.Syntax.Contract

import Data.List1
import Derive.Prelude
import Language.Reflection
import Frontend.ASTPhases

%default total
%language ElabReflection

--------------------------------------------------------------------------------
-- Quantum contract clauses
--------------------------------------------------------------------------------
-- Preserves the requires/ensures annotations between a function signature and
-- its body, WITHOUT validating them:
--
--   requires clean(q)
--   requires basis([q1, q2], XX)
--   requires isolated(qs)
--   ensures separable([q1, q2])
--   ensures product([q1, q2], [q3, q4], qs)
--   ensures stabilized([q0, q1, q2], [+XXX, +ZZI, +IZZ])
--
-- Qubit arguments are ordinary EXPRESSIONS (a name `q1`, an array literal
-- `[q1, q2]`, an index `qs[2]`), so this module is parameterized over the
-- expression type exactly like TyNode's arraySizeExpr -- Decl.idr ties the
-- knot with the concrete SurfaceExpr. Which expressions are ACCEPTABLE
-- contract arguments (lvalue-ish qubit designators, not arbitrary
-- computation) is a validation pass's check against the preserved spans.
--
-- Pauli strings: BY LANGUAGE RULING, contract Pauli strings use only the
-- single-letter alphabet I, X, Y, Z. That makes segmentation of a term like
-- ZZI trivial (one operator per character) and lets the AST store Pauli
-- strings DECODED rather than as raw text: a PauliStringNode is a non-empty
-- list of PauliOperator, so a malformed string ("XQZ") is UNREPRESENTABLE --
-- the parser rejects the bad character with its exact position. Nothing is
-- lost versus raw text: each operator prints as exactly one character, so
-- decoded strings round-trip verbatim.
--
-- (Lexically, XX and ZZI arrive as plain identifier tokens and +XXX as a
-- plus symbol followed by an identifier; the parser reinterprets them in
-- contract position. The spec's list of gates for stabilizer expressions
-- should be amended to match this ruling -- and note the spec spelled
-- identity "Id" in that list but "I" in every actual string; with the
-- I/X/Y/Z ruling the one-letter "I" is the syntax.)
--
-- Deliberately representable, rejected later:
--   * Pauli string length vs. qubit count      (basis([q1, q2], XXX))
--   * ensures preceding requires               (spec: requires come first)
--   * non-designator argument expressions      (clean(1 + 2))
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Pauli operators, strings, and signed stabilizer terms
--------------------------------------------------------------------------------

-- One single-qubit Pauli operator, as ruled: I, X, Y, Z only.
public export
data PauliOperator
  = PauliI
  | PauliX
  | PauliY
  | PauliZ

%runElab derive "PauliOperator" [Eq]

public export
showPauliOperatorLeaf : PauliOperator -> String
showPauliOperatorLeaf p =
  case p of
    PauliI => "I"
    PauliX => "X"
    PauliY => "Y"
    PauliZ => "Z"

public export
implementation Show PauliOperator where
  show = showPauliOperatorLeaf

-- The single authoritative character classification, for the parser
-- production that decodes an identifier token into a Pauli string.
public export
pauliOperatorFromChar : Char -> Maybe PauliOperator
pauliOperatorFromChar c =
  case c of
    'I' => Just PauliI
    'X' => Just PauliX
    'Y' => Just PauliY
    'Z' => Just PauliZ
    _   => Nothing

-- A Pauli string as written: X, XX, ZZI, XYZ. Decoded and non-empty; the
-- written spelling is recovered exactly by concatenating the operators'
-- one-character spellings.
public export
record PauliStringNode where
  constructor MkPauliStringNode
  pauliOperators : List1 PauliOperator

public export
SurfacePauliString : Type
SurfacePauliString = SurfaceAstNode PauliStringNode

-- The sign of a stabilizer term. MANDATORY, because the spec writes it on
-- every term ([ +ZI, -ZZ ], [ -Z ]); if unsigned terms defaulting to + are
-- ever ruled legal, this field on SignedPauliTermNode becomes a Maybe.
public export
data StabilizerSign
  = StabilizerPlus   -- +
  | StabilizerMinus  -- -

%runElab derive "StabilizerSign" [Eq]

-- One signed stabilizer term: +XXX, -ZZ. The sign is stored plain (the
-- term's own span starts at the sign character); the Pauli string is
-- located so length-mismatch diagnostics can point at it precisely.
public export
record SignedPauliTermNode where
  constructor MkSignedPauliTermNode
  stabilizerSign  : StabilizerSign
  stabilizerPauli : SurfacePauliString

public export
SurfaceSignedPauliTerm : Type
SurfaceSignedPauliTerm = SurfaceAstNode SignedPauliTermNode

--------------------------------------------------------------------------------
-- Contract predicates
--------------------------------------------------------------------------------
-- The six predicate forms, each with its own correctly-shaped argument list.
-- `expr` is the (located) surface expression type, supplied by Decl.idr.
--------------------------------------------------------------------------------

public export
data ContractPredicateNode : (expr : Type) -> Type where

  -- clean(q) / clean([q1, q2]) -- qubits in |0>, separated from the rest.
  ContractClean :
       (qubitArgument : expr)
    -> ContractPredicateNode expr

  -- basis(q1, X) / basis([q1, q2], XX) -- separable eigenstate of the
  -- given Pauli string, separated from the rest.
  ContractBasis :
       (qubitArgument : expr)
    -> (pauliString   : SurfacePauliString)
    -> ContractPredicateNode expr

  -- separable(qs) -- not entangled among themselves, separated from rest.
  ContractSeparable :
       (qubitArgument : expr)
    -> ContractPredicateNode expr

  -- isolated(qs) -- not entangled with the REST of the program, though
  -- possibly entangled among themselves.
  ContractIsolated :
       (qubitArgument : expr)
    -> ContractPredicateNode expr

  -- product(q1, q2, qs) / product([q1, q2], [q3, q4], qs) -- the given
  -- qubit sets are mutually unentangled. Product is a relation between AT
  -- LEAST TWO sets, and the first + List1 shape encodes exactly that:
  -- a one-argument product(x) is unrepresentable.
  ContractProduct :
       (firstQubitSet  : expr)
    -> (otherQubitSets : List1 expr)
    -> ContractPredicateNode expr

  -- stabilized(qs, [ +ZI, -ZZ ]) -- exact stabilizer state. At least one
  -- stabilizer term (an empty bracket list would assert nothing). Note the
  -- bracket list is contract-specific syntax parsed by a dedicated
  -- production, NOT an ordinary array-literal expression: +ZI is not an
  -- expression.
  ContractStabilized :
       (qubitArgument   : expr)
    -> (stabilizerTerms : List1 SurfaceSignedPauliTerm)
    -> ContractPredicateNode expr

--------------------------------------------------------------------------------
-- Contract clauses
--------------------------------------------------------------------------------
-- requires <predicate> / ensures <predicate>. A function declaration stores
-- `List` of located clauses IN SOURCE ORDER: the spec's rule that requires
-- clauses precede ensures clauses is a validation check (with the offending
-- clause's span), not an AST shape -- encoding it as two separate lists
-- would erase the written interleaving that the diagnostic needs to show.
--------------------------------------------------------------------------------

public export
data ContractClauseNode : (expr : Type) -> Type where

  RequiresClause :
       (predicate : SurfaceAstNode (ContractPredicateNode expr))
    -> ContractClauseNode expr

  EnsuresClause :
       (predicate : SurfaceAstNode (ContractPredicateNode expr))
    -> ContractClauseNode expr

--------------------------------------------------------------------------------
-- Located aliases (still parameterized; Decl.idr instantiates with the
-- concrete SurfaceExpr)
--------------------------------------------------------------------------------

public export
LocatedContractPredicate : (expr : Type) -> Type
LocatedContractPredicate expr = SurfaceAstNode (ContractPredicateNode expr)

public export
LocatedContractClause : (expr : Type) -> Type
LocatedContractClause expr = SurfaceAstNode (ContractClauseNode expr)