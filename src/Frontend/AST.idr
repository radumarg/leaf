module Frontend.AST

import Derive.Prelude
import Language.Reflection

%default total
%language ElabReflection

------------------------------------------------------------------------------
-- GateName: enumerates the built-in quantum gates.
-- In the lexer, strings like "H" becomes TokGate GateH
------------------------------------------------------------------------------
public export
data GateName
  = GateId | GateX | GateY | GateZ | GateH
  | GateS | GateSDG | GateT | GateTDG
  | GateSX | GateSXDG
  | GateRX | GateRY | GateRZ
  | GateU1 | GateU2 | GateU3
  | GateCNOT | GateCX
  | GateCY | GateCZ | GateCS | GateCSDG | GateCT | GateCTDG
  | GateCSX | GateCSXDG
  | GateCRX | GateCRY | GateCRZ
  | GateCU1 | GateCU2 | GateCU3
  | GateSWAP
  | GateRXX | GateRYY | GateRZZ
  | GateCCX | GateCSWAP
  | GateGPI | GateGPI2 | GateMS

------------------------------------------------------------------------------
-- BuiltinName: built-in runtime-like functions (keywords).
--   qalloc()      -> qubit
--   qalloc(8)     -> [qubit; 8]
--   measr(q)      -> (bit, qubit)
--   reset(q)      -> qubit
--   math helpers: abs, acos, asin, atan, ceil, cos, exp, floor, ln, log2, log10, pow, round, sin, sqrt, tan
--   quantum helpers: adjoint, discard, measr, qalloc, reset, uncompute
--   other helpers: max, min
------------------------------------------------------------------------------
public export
data BuiltinName
  = BuiltinAbs
  | BuiltinAdjoint
  | BuiltinAcos
  | BuiltinAsin
  | BuiltinAtan
  | BuiltinBarrier
  | BuiltinCeil
  | BuiltinCos
  | BuiltinDiscard
  | BuiltinExp
  | BuiltinFloor
  | BuiltinImport
  | BuiltinLn
  | BuiltinLog10
  | BuiltinLog2
  | BuiltinMax
  | BuiltinMeasr
  | BuiltinMin
  | BuiltinPow
  | BuiltinQAlloc
  | BuiltinReset
  | BuiltinRound
  | BuiltinSin
  | BuiltinSqrt
  | BuiltinTan
  | BuiltinUncompute

------------------------------------------------------------------------------
-- AssignOp: statement-level assignment operators
--   x = ...
--   x += ...
--   x %= ...
------------------------------------------------------------------------------
public export
data AssignOp : Type where
  AssignEq    : AssignOp
  AssignAddEq : AssignOp
  AssignSubEq : AssignOp
  AssignMulEq : AssignOp
  AssignDivEq : AssignOp
  AssignRemEq : AssignOp
  AssignWalrusEq : AssignOp

------------------------------------------------------------------------------
-- UnaryOp: prefix operators
--   -x
--   !flag
------------------------------------------------------------------------------
public export
data UnaryOp : Type where
  UnaryNeg : UnaryOp     -- -x
  UnaryNot : UnaryOp     -- !x

------------------------------------------------------------------------------
-- BinaryOp: infix operators.
-- Used inside expressions:
--  2 + 3
-- Range operators:
--  a .. b    (exclusive)
--  a ..= b   (inclusive)
------------------------------------------------------------------------------
public export
data BinaryOp : Type where
  OpAdd       : BinaryOp  -- +
  OpSub       : BinaryOp  -- -
  OpMul       : BinaryOp  -- *
  OpDiv       : BinaryOp  -- /
  OpRem       : BinaryOp  -- %

  OpLt        : BinaryOp  -- <
  OpLe        : BinaryOp  -- <=
  OpGt        : BinaryOp  -- >
  OpGe        : BinaryOp  -- >=

  OpEqEq      : BinaryOp  -- ==
  OpNotEq     : BinaryOp  -- !=

  OpAndAnd    : BinaryOp  -- &&
  OpOrOr      : BinaryOp  -- ||

  OpBitOr     : BinaryOp  -- |
  OpBitXor    : BinaryOp  -- ^

  OpRangeExcl : BinaryOp  -- ..
  OpRangeIncl : BinaryOp  -- ..=

------------------------------------------------------------------------------
-- Literal: user-written literal values.
-- Store int/float as RAW STRINGS to preserve formatting and decide 
-- later wether to allow underscores, hex, scientific, etc.
------------------------------------------------------------------------------
public export
data Literal : Type where
  LitIntRaw     : String -> Literal
  LitFloatRaw   : String -> Literal
  LitBool       : Bool -> Literal
  LitString     : String -> Literal
  LitBitString  : String -> Literal
  LitUnit       : Literal

-- Primitive built-in types
public export
data TypPrimName
  = TypPrimUnit
  | TypPrimAngle
  | TypPrimBit
  | TypPrimBool
  | TypPrimF32
  | TypPrimF64
  | TypPrimI8
  | TypPrimI16
  | TypPrimI32
  | TypPrimI64
  | TypPrimI128
  | TypPrimU8
  | TypPrimU16
  | TypPrimU32
  | TypPrimU64
  | TypPrimU128
  | TypPrimQubit

------------------------------------------------------------------------------
-- TypExpr: type expressions in annotations and function signatures.
--   ()                => TypUnit
--   i32               => TypPrim TypPrimI32
--   (i32, bool)       => TypTuple [...]
--   [i32; 4]          => TypArrayFixed (TypPrim TypPrimI32) 4
--   [i32; 4]          => TypArrayFixed (TypPrim TypPrimI32) (SizeNat 4)
--   [i32; n]          => TypArrayFixed (TypPrim TypPrimI32) (SizeVar "n")
------------------------------------------------------------------------------
public export
data SizeExpr : Type where
  SizeNat : Nat -> SizeExpr
  SizeVar : String -> SizeExpr

-- Type expressions
public export
data TypExpr : Type where
  -- Unit type: ()
  TypUnit : TypExpr

  -- Primitive type keyword: int, bool, float, qubit, bit, ...
  TypPrim : TypPrimName -> TypExpr

  -- Tuple types: (int, bool)
  TypTuple : List TypExpr -> TypExpr

  -- Fixed-size arrays: [T; n]
  TypArrayFixed : (typElementTypExpr : TypExpr)
               -> (typLengthSizeExpr : SizeExpr)
               -> TypExpr

------------------------------------------------------------------------------
-- Pattern: used in let bindings and match arms.
--   let x = ...
--   let (a, b, _) = ...
--   match day { 1 => ..., _ => ... }
------------------------------------------------------------------------------
public export
data Pattern : Type where
  PatWildcard : Pattern                 -- _
  PatVarName  : String -> Pattern       -- x
  PatLit      : Literal -> Pattern      -- 1, true, "hi"
  PatUnit     : Pattern                 -- ()
  PatTuple    : List Pattern -> Pattern -- (a,b,_)

------------------------------------------------------------------------------
-- Control prefix syntax for quantum controls:
--   ctrl(q0) H(q1);
--   negctrl(q0) H(q1);
--   ctrl(q0 = true, q1 = false) H(q2);
--   ctrl(q0, q1) { H(q2); CX(q2,q3); }
-- Controls can be positional expressions or named args (q0 = true).
------------------------------------------------------------------------------
public export
record ControlNamedArg where
  constructor MkControlNamedArg
  controlName : String
  controlPolarity : Bool

mutual
  public export
  data ControlArg
    = ControlArgExpr   Expr
    | ControlArgNamed  ControlNamedArg

  public export
  data ControlPrefix
    = PrefixCtrl    (List ControlArg)
    | PrefixNegCtrl (List ControlArg)

  ----------------------------------------------------------------------------
  -- Expressions.
  --  * Gate calls are expressions: H(q0) is an expression
  --  * Blocks are expressions: { ... } is EBlock
  --  * ctrl(...) can apply to a gate or to a block (EControlBlock)
  --  * "if" and "loop/while/for/match" are expressions (like Rust)
  ----------------------------------------------------------------------------
  public export
  data Expr : Type where

    -- Variables like `x`, `q0`
    EVarName : String -> Expr

    -- Literals like 1, 3.14, true, "hi", ()
    ELit : Literal -> Expr

    -- Tuples: (1, true, x)
    ETuple : List Expr -> Expr

    -- Array literal: [1,2,3]
    EArrayLiteral : List Expr -> Expr

    -- Array repeat: [0; 8]
    EArrayRepeat : (typRepeatedValueExpr : Expr) -> (typRepeatCountNat : Nat) -> Expr

    -- Indexing: a[0]
    EIndex : (typTargetExpr : Expr) -> (typIndexExpr : Expr) -> Expr

    -- Field access: a.len()
    EField : (typTargetExpr : Expr) -> (typFieldName : String) -> Expr

    -- Tuple positional indexing: t.0, t.1, ...
    ETupleIndex : (typTargetExpr : Expr) -> (typIndexNat : Nat) -> Expr

    -- Function call: f(x,y)
    ECall : (typCalleeExpr : Expr) -> (typArgs : List Expr) -> Expr

    -- Macro call: println!(...)
    EMacroCall : (macroName : String) -> (typArgs : List Expr) -> Expr

    -- Prefix operators: -x, !flag
    EUnary : UnaryOp -> Expr -> Expr

    -- Infix operators: x + y, a .. b, x == y, flag && flag2, x | y, x ^ y, ...
    EBinary : BinaryOp -> Expr -> Expr -> Expr

    -- if condition { ... } else { ... }
    EIf : (typConditionExpr : Expr)
      -> (typThenBlock : BlockExpr)
      -> (typElseBranch : Maybe Expr)
      -> Expr

    -- qif condition { ... } qelse { ... }
    EQIf : (typConditionExpr : Expr)
      -> (typThenBlock : BlockExpr)
      -> (typElseBranch : Maybe Expr)
      -> Expr

    -- loop { ... }  (may return a value via break value)
    ELoop : BlockExpr -> Expr

    -- while condition { ... }
    EWhile : (typConditionExpr : Expr) -> (typBodyBlock : BlockExpr) -> Expr

    -- for pat in iterable { ... }
    EFor : (typPattern : Pattern)
        -> (typIterableExpr : Expr)
        -> (typBodyBlock : BlockExpr)
        -> Expr

    -- match scrutinee { pat => expr, ... }
    EMatch : (typScrutineeExpr : Expr) -> (typArms : List MatchArm) -> Expr

    -- qmatch scrutinee { 0 => { ... } b"01" => { ... } }
    EQMatch : (typScrutineeExpr : Expr) -> (typArms : List QMatchArm) -> Expr

    -- { statements; tailExpr? }
    EBlock : BlockExpr -> Expr

    -- cast: x as int
    ECastAs : (typValueExpr : Expr) -> (typCastTarget : TypExpr) -> Expr

    -- builtins: qalloc(...), measr(...), reset(...)
    EBuiltinCall : BuiltinName -> List Expr -> Expr

    -- apply gate with optional control prefixes:
    --   ctrl(q0) negctrl(q1) H(q2)
    EGateApply : (typControlPrefixes : List ControlPrefix)
              -> (typGateName : GateName)
              -> (typArgs : List Expr)
              -> Expr

    -- control prefixes apply to a block:
    --   ctrl(q0,q1) { H(q2); CX(q2,q3); }
    EControlBlock : (typControlPrefixes : List ControlPrefix) -> (typBlockExpr : BlockExpr) -> Expr

  public export
  data QMatchLabel : Type where
    QMatchLabelNat       : Nat -> QMatchLabel
    QMatchLabelBitString : String -> QMatchLabel

  public export
  record QMatchArm where
    constructor MkQMatchArm
    qmatchLabel    : QMatchLabel
    qmatchBodyExpr : BlockExpr

  public export
  record MatchArm where
    constructor MkMatchArm
    matchPattern : Pattern
    matchBodyExpr : Expr

  ---------------------------------------------------------------------------------
  -- Blocks and statements.
  --
  -- In Rust style:
  --   - expressions followed by `;` become statements
  --   - the final expression in a block may omit `;` and becomes the block's value
  ---------------------------------------------------------------------------------
  public export
  record BlockExpr where
    constructor MkBlockExpr
    blockStatements : List Stmt
    blockTailExpr : Maybe Expr

  public export
  data Stmt : Type where
    -- let pattern [: type]? = expr ;
    StmtLet : (typBindingPattern : Pattern)
          -> (typAnnotationMaybe : Maybe TypExpr)
          -> (typValueExpr : Expr)
          -> Stmt

    -- assignment statement: lhs (=|+=|...) rhs ;
    StmtAssign : (typLeftHandSideExpr : Expr)
             -> (typAssignOp : AssignOp)
             -> (typRightHandSideExpr : Expr)
             -> Stmt

    -- expression statement
    -- typHadSemicolon=True means it ended with ';'
    StmtExpr : (typExpr : Expr) -> (typHadSemicolon : Bool) -> Stmt

    -- break [expr]? [;]?
    StmtBreak : (typBreakValueMaybe : Maybe Expr) -> Stmt

    -- continue [;]?
    StmtContinue : Stmt

    -- return [expr]? [;]?
    StmtReturn : (typReturnValueMaybe : Maybe Expr) -> Stmt

------------------------------------------------------------------------------
-- Function declarations.
--   fn f(x: int) -> int { x + 1 }
------------------------------------------------------------------------------
public export
record FnParam where
  constructor MkFnParam
  paramName : String
  paramType : TypExpr

public export
record FnDecl where
  constructor MkFnDecl
  fnName : String
  fnParams : List FnParam
  fnReturnTypeMaybe : Maybe TypExpr
  fnBodyBlock : BlockExpr

-------------------------------------------------------------------------------
-- Top-level items: allow either function declarations or top-level statements.
-------------------------------------------------------------------------------
public export
data Item
  = ItemFnDecl FnDecl
  | ItemStmt   Stmt

public export
record Program where
  constructor MkProgram
  programItems : List Item

------------------------------------------------------------------------------
-- Derivations for debugging/testing
------------------------------------------------------------------------------
-- Keep only non-recursive derives here. The recursive AST family rooted at
-- Expr is implemented manually in Frontend.ExprPrettyPrinter to avoid cycles
-- in derived Show/Eq resolution.

%runElab derive "GateName" [Show, Eq]
%runElab derive "BuiltinName" [Show, Eq]
%runElab derive "AssignOp" [Show, Eq]
%runElab derive "UnaryOp" [Show, Eq]
%runElab derive "BinaryOp" [Show, Eq]
%runElab derive "Literal" [Show, Eq]
%runElab derive "TypPrimName" [Show, Eq]
%runElab derive "SizeExpr" [Show, Eq]
%runElab derive "TypExpr" [Show, Eq]
%runElab derive "Pattern" [Show, Eq]
%runElab derive "ControlNamedArg" [Show, Eq]
%runElab derive "QMatchLabel" [Show, Eq]
%runElab derive "FnParam" [Show, Eq]