module Frontend.ExprPrettyPrinter

import Derive.Prelude
import Language.Reflection

import Frontend.AST

%default total
%language ElabReflection

------------------------------------------------------------------------------
-- Pretty-printing style for Expr
------------------------------------------------------------------------------
public export
data PrettyStyle
  = PrettyLax
  | PrettyStrict

joinWith : String -> List String -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith sep (x :: xs) = x ++ sep ++ joinWith sep xs

parens : String -> String
parens s = "(" ++ s ++ ")"

brackets : String -> String
brackets s = "[" ++ s ++ "]"

braces : String -> String
braces s = "{ " ++ s ++ " }"

showBoolLower : Bool -> String
showBoolLower True = "true"
showBoolLower False = "false"

escapeStringChars : List Char -> String
escapeStringChars [] = ""
escapeStringChars ('\n' :: cs) = "\\n" ++ escapeStringChars cs
escapeStringChars ('\t' :: cs) = "\\t" ++ escapeStringChars cs
escapeStringChars ('\r' :: cs) = "\\r" ++ escapeStringChars cs
escapeStringChars ('\"' :: cs) = "\\\"" ++ escapeStringChars cs
escapeStringChars ('\\' :: cs) = "\\\\" ++ escapeStringChars cs
escapeStringChars (c :: cs) = singleton c ++ escapeStringChars cs

escapeString : String -> String
escapeString s = escapeStringChars (unpack s)

showLiteralLeaf : Literal -> String
showLiteralLeaf (LitIntRaw s) = s
showLiteralLeaf (LitFloatRaw s) = s
showLiteralLeaf (LitBool b) = showBoolLower b
showLiteralLeaf (LitString s) = "\"" ++ escapeString s ++ "\""
showLiteralLeaf (LitBitString s) = "b\"" ++ s ++ "\""
showLiteralLeaf LitUnit = "()"

showTypPrimLeaf : TypPrimName -> String
showTypPrimLeaf  TypPrimUnit  = "()"
showTypPrimLeaf  TypPrimAngle32 = "angle32"
showTypPrimLeaf  TypPrimAngle64 = "angle64"
showTypPrimLeaf  TypPrimBit   = "bit"
showTypPrimLeaf  TypPrimBool  = "bool"
showTypPrimLeaf  TypPrimQubit = "qubit"
showTypPrimLeaf  TypPrimF32   = "f32"
showTypPrimLeaf  TypPrimF64   = "f64"
showTypPrimLeaf  TypPrimI8   = "i8"
showTypPrimLeaf  TypPrimI16  = "i16"
showTypPrimLeaf  TypPrimI32  = "i32"
showTypPrimLeaf  TypPrimI64  = "i64"
showTypPrimLeaf  TypPrimI128 = "i128"
showTypPrimLeaf  TypPrimParam = "param"
showTypPrimLeaf  TypPrimU8   = "u8"
showTypPrimLeaf  TypPrimU16  = "u16"
showTypPrimLeaf  TypPrimU32  = "u32"
showTypPrimLeaf  TypPrimU64  = "u64"
showTypPrimLeaf  TypPrimU128 = "u128"

showSizeExprLeaf : SizeExpr -> String
showSizeExprLeaf (SizeNat n) = show n
showSizeExprLeaf (SizeVar x) = x

showTupleLike : List String -> String
showTupleLike [] = "()"
showTupleLike [x] = "(" ++ x ++ ",)"
showTupleLike xs = "(" ++ joinWith ", " xs ++ ")"

mutual
  showTypExprLeaf : TypExpr -> String
  showTypExprLeaf TypUnit = "()"
  showTypExprLeaf (TypPrim p) = showTypPrimLeaf p
  showTypExprLeaf (TypTuple ts) = showTupleLike (showTypExprList ts)
  showTypExprLeaf (TypArrayFixed elemTy n) =
    "[" ++ showTypExprLeaf elemTy ++ "; " ++ showSizeExprLeaf n ++ "]"

  showTypExprList : List TypExpr -> List String
  showTypExprList [] = []
  showTypExprList (t :: ts) = showTypExprLeaf t :: showTypExprList ts

mutual
  showPatternQualifierLeaf : PatternQualifier -> String
  showPatternQualifierLeaf PatQualScratch = "scratch"
  showPatternQualifierLeaf PatQualLinear = "linear"
  showPatternQualifierLeaf PatQualAffine = "affine"

  showPatternQualifierList : List PatternQualifier -> List String
  showPatternQualifierList [] = []
  showPatternQualifierList (qualifier :: qualifiers) =
    showPatternQualifierLeaf qualifier :: showPatternQualifierList qualifiers

  showPatternLeaf : Pattern -> String
  showPatternLeaf PatWildcard = "_"
  showPatternLeaf (PatVarName x) = x
  showPatternLeaf (PatLit lit) = showLiteralLeaf lit
  showPatternLeaf PatUnit = "()"
  showPatternLeaf (PatQualified qualifiers pat) =
    let qualifierPrefix = joinWith " " (showPatternQualifierList qualifiers) in
      if qualifierPrefix == ""
         then showPatternLeaf pat
         else qualifierPrefix ++ " " ++ showPatternLeaf pat
  showPatternLeaf (PatTuple ps) = showTupleLike (showPatternList ps)

  showPatternList : List Pattern -> List String
  showPatternList [] = []
  showPatternList (p :: ps) = showPatternLeaf p :: showPatternList ps

showTypedBindingPatternLeaf : Pattern -> String
showTypedBindingPatternLeaf (PatQualified qualifiers pat) =
  showPatternLeaf (PatQualified qualifiers pat)
showTypedBindingPatternLeaf pat = showPatternLeaf pat ++ " "

showBuiltinNameLeaf : BuiltinName -> String
showBuiltinNameLeaf BuiltinAbs       = "abs"
showBuiltinNameLeaf BuiltinAdjoint   = "adjoint"
showBuiltinNameLeaf BuiltinAcos      = "acos"
showBuiltinNameLeaf BuiltinAsin      = "asin"
showBuiltinNameLeaf BuiltinAtan      = "atan"
showBuiltinNameLeaf BuiltinBarrier   = "barrier"
showBuiltinNameLeaf BuiltinCeil      = "ceil"
showBuiltinNameLeaf BuiltinCos       = "cos"
showBuiltinNameLeaf BuiltinDiscard   = "discard"
showBuiltinNameLeaf BuiltinExp       = "exp"
showBuiltinNameLeaf BuiltinFloor     = "floor"
showBuiltinNameLeaf BuiltinImport    = "import"
showBuiltinNameLeaf BuiltinLn        = "ln"
showBuiltinNameLeaf BuiltinLog10     = "log10"
showBuiltinNameLeaf BuiltinLog2      = "log2"
showBuiltinNameLeaf BuiltinMax       = "max"
showBuiltinNameLeaf BuiltinMeasr     = "measr"
showBuiltinNameLeaf BuiltinMin       = "min"
showBuiltinNameLeaf BuiltinParam     = "Param"
showBuiltinNameLeaf BuiltinQAlloc    = "qalloc"
showBuiltinNameLeaf BuiltinReset     = "reset"
showBuiltinNameLeaf BuiltinRound     = "round"
showBuiltinNameLeaf BuiltinSin       = "sin"
showBuiltinNameLeaf BuiltinSqrt      = "sqrt"
showBuiltinNameLeaf BuiltinTan       = "tan"
showBuiltinNameLeaf BuiltinUncompute = "uncompute"
showBuiltinNameLeaf BuiltinWeaken    = "weaken"

showGateNameLeaf : GateName -> String
showGateNameLeaf GateId    = "Id"
showGateNameLeaf GateX     = "X"
showGateNameLeaf GateY     = "Y"
showGateNameLeaf GateZ     = "Z"
showGateNameLeaf GateH     = "H"
showGateNameLeaf GateS     = "S"
showGateNameLeaf GateSDG   = "SDG"
showGateNameLeaf GateT     = "T"
showGateNameLeaf GateTDG   = "TDG"
showGateNameLeaf GateSX    = "SX"
showGateNameLeaf GateSXDG  = "SXDG"
showGateNameLeaf GateRX    = "RX"
showGateNameLeaf GateRY    = "RY"
showGateNameLeaf GateRZ    = "RZ"
showGateNameLeaf GateU1    = "U1"
showGateNameLeaf GateU2    = "U2"
showGateNameLeaf GateU3    = "U3"
showGateNameLeaf GateCNOT  = "CNOT"
showGateNameLeaf GateCX    = "CX"
showGateNameLeaf GateCY    = "CY"
showGateNameLeaf GateCZ    = "CZ"
showGateNameLeaf GateCS    = "CS"
showGateNameLeaf GateCSDG  = "CSDG"
showGateNameLeaf GateCT    = "CT"
showGateNameLeaf GateCTDG  = "CTDG"
showGateNameLeaf GateCSX   = "CSX"
showGateNameLeaf GateCSXDG = "CSXDG"
showGateNameLeaf GateCRX   = "CRX"
showGateNameLeaf GateCRY   = "CRY"
showGateNameLeaf GateCRZ   = "CRZ"
showGateNameLeaf GateCU1   = "CU1"
showGateNameLeaf GateCU2   = "CU2"
showGateNameLeaf GateCU3   = "CU3"
showGateNameLeaf GateSWAP  = "SWAP"
showGateNameLeaf GateRXX   = "RXX"
showGateNameLeaf GateRYY   = "RYY"
showGateNameLeaf GateRZZ   = "RZZ"
showGateNameLeaf GateCCX   = "CCX"
showGateNameLeaf GateCSWAP = "CSWAP"
showGateNameLeaf GateGPI   = "GPI"
showGateNameLeaf GateGPI2  = "GPI2"
showGateNameLeaf GateMS    = "MS"
showGateNameLeaf GateZZ    = "ZZ"

showAssignOpLeaf : AssignOp -> String
showAssignOpLeaf AssignEq       = "="
showAssignOpLeaf AssignAddEq    = "+="
showAssignOpLeaf AssignSubEq    = "-="
showAssignOpLeaf AssignMulEq    = "*="
showAssignOpLeaf AssignDivEq    = "/="
showAssignOpLeaf AssignRemEq    = "%="
showAssignOpLeaf AssignWalrusEq = ":="

showUnaryOpLeaf : UnaryOp -> String
showUnaryOpLeaf UnaryNeg = "-"
showUnaryOpLeaf UnaryNot = "!"

showBinaryOpLeaf : BinaryOp -> String
showBinaryOpLeaf OpAdd       = "+"
showBinaryOpLeaf OpSub       = "-"
showBinaryOpLeaf OpMul       = "*"
showBinaryOpLeaf OpDiv       = "/"
showBinaryOpLeaf OpRem       = "%"
showBinaryOpLeaf OpLt        = "<"
showBinaryOpLeaf OpLe        = "<="
showBinaryOpLeaf OpGt        = ">"
showBinaryOpLeaf OpGe        = ">="
showBinaryOpLeaf OpEqEq      = "=="
showBinaryOpLeaf OpNotEq     = "!="
showBinaryOpLeaf OpAndAnd    = "&&"
showBinaryOpLeaf OpOrOr      = "||"
showBinaryOpLeaf OpBitOr     = "|"
showBinaryOpLeaf OpBitXor    = "^"
showBinaryOpLeaf OpRangeExcl = ".."
showBinaryOpLeaf OpRangeIncl = "..="

binaryPrec : BinaryOp -> Nat
binaryPrec OpRangeExcl = 10
binaryPrec OpRangeIncl = 10
binaryPrec OpOrOr      = 20
binaryPrec OpAndAnd    = 30
binaryPrec OpBitOr     = 40
binaryPrec OpBitXor    = 50
binaryPrec OpEqEq      = 60
binaryPrec OpNotEq     = 60
binaryPrec OpLt        = 70
binaryPrec OpLe        = 70
binaryPrec OpGt        = 70
binaryPrec OpGe        = 70
binaryPrec OpAdd       = 80
binaryPrec OpSub       = 80
binaryPrec OpMul       = 90
binaryPrec OpDiv       = 90
binaryPrec OpRem       = 90

exprPrec : Expr -> Nat
exprPrec (EVarName _) = 120
exprPrec (ELit _) = 120
exprPrec (ETuple _) = 120
exprPrec (EArrayLiteral _) = 120
exprPrec (EArrayRepeat _ _) = 120
exprPrec (ECall _ _) = 110
exprPrec (EMacroCall _ _) = 110
exprPrec (EIndex _ _) = 110
exprPrec (EField _ _) = 110
exprPrec (ETupleIndex _ _) = 110
exprPrec (EBuiltinCall _ _) = 110
exprPrec (EGateApply _ _ _) = 110
exprPrec (EUnary _ _) = 100
exprPrec (ECastAs _ _) = 95
exprPrec (EBinary op _ _) = binaryPrec op
exprPrec (EIf _ _ _) = 0
exprPrec (EQIf _ _ _) = 0
exprPrec (ELoop _) = 0
exprPrec (EWhile _ _) = 0
exprPrec (EFor _ _ _) = 0
exprPrec (EMatch _ _) = 0
exprPrec (EQMatch _ _) = 0
exprPrec (EBlock _) = 0
exprPrec (EControlBlock _ _) = 0

needsParensLax : Nat -> Expr -> Bool
needsParensLax outer e = outer > exprPrec e

wrapByStyle : PrettyStyle -> Nat -> Expr -> String -> String
wrapByStyle PrettyStrict _ _ s = parens s
wrapByStyle PrettyLax outer e s =
  if needsParensLax outer e then parens s else s

prettyFuelExhausted : String
prettyFuelExhausted = "<pretty-fuel-exhausted>"

mutual
  exprSize : Expr -> Nat
  exprSize (EVarName _) = 1
  exprSize (ELit _) = 1
  exprSize (ETuple es) = S (exprListSize es)
  exprSize (EArrayLiteral es) = S (exprListSize es)
  exprSize (EArrayRepeat val _) = S (exprSize val)
  exprSize (EIndex target ix) = S (exprSize target + exprSize ix)
  exprSize (EField target _) = S (exprSize target)
  exprSize (ETupleIndex target _) = S (exprSize target)
  exprSize (ECall callee args) = S (exprSize callee + exprListSize args)
  exprSize (EMacroCall _ args) = S (exprListSize args)
  exprSize (EUnary _ e) = S (exprSize e)
  exprSize (EBinary _ lhs rhs) = S (exprSize lhs + exprSize rhs)
  exprSize (EIf cond thenBlk elseBranch) = S (exprSize cond + blockSize thenBlk + maybeExprSize elseBranch)
  exprSize (EQIf cond thenBlk elseBranch) = S (exprSize cond + blockSize thenBlk + maybeExprSize elseBranch)
  exprSize (ELoop body) = S (blockSize body)
  exprSize (EWhile cond body) = S (exprSize cond + blockSize body)
  exprSize (EFor _ iterable body) = S (exprSize iterable + blockSize body)
  exprSize (EMatch scrutinee arms) = S (exprSize scrutinee + matchArmListSize arms)
  exprSize (EQMatch scrutinee arms) = S (exprSize scrutinee + qMatchArmListSize arms)
  exprSize (EBlock blk) = S (blockSize blk)
  exprSize (ECastAs value _) = S (exprSize value)
  exprSize (EBuiltinCall _ args) = S (exprListSize args)
  exprSize (EGateApply prefixes _ args) = S (controlPrefixListSize prefixes + exprListSize args)
  exprSize (EControlBlock prefixes blk) = S (controlPrefixListSize prefixes + blockSize blk)

  exprListSize : List Expr -> Nat
  exprListSize [] = 0
  exprListSize (e :: es) = exprSize e + exprListSize es

  maybeExprSize : Maybe Expr -> Nat
  maybeExprSize Nothing = 0
  maybeExprSize (Just e) = exprSize e

  controlArgSize : ControlArg -> Nat
  controlArgSize (ControlArgExpr e) = S (exprSize e)
  controlArgSize (ControlArgNamed _) = 1

  controlArgListSize : List ControlArg -> Nat
  controlArgListSize [] = 0
  controlArgListSize (arg :: args) = controlArgSize arg + controlArgListSize args

  controlPrefixSize : ControlPrefix -> Nat
  controlPrefixSize (PrefixCtrl args) = S (controlArgListSize args)
  controlPrefixSize (PrefixNegCtrl args) = S (controlArgListSize args)

  controlPrefixListSize : List ControlPrefix -> Nat
  controlPrefixListSize [] = 0
  controlPrefixListSize (ctrlPrefix :: prefixes) =
    controlPrefixSize ctrlPrefix + controlPrefixListSize prefixes

  matchArmSize : MatchArm -> Nat
  matchArmSize (MkMatchArm _ body) = S (exprSize body)

  matchArmListSize : List MatchArm -> Nat
  matchArmListSize [] = 0
  matchArmListSize (arm :: arms) = matchArmSize arm + matchArmListSize arms

  qMatchArmSize : QMatchArm -> Nat
  qMatchArmSize (MkQMatchArm _ body) = S (blockSize body)

  qMatchArmListSize : List QMatchArm -> Nat
  qMatchArmListSize [] = 0
  qMatchArmListSize (arm :: arms) = qMatchArmSize arm + qMatchArmListSize arms

  stmtSize : Stmt -> Nat
  stmtSize (StmtLet _ _ val) = S (exprSize val)
  stmtSize (StmtAssign lhs _ rhs) = S (exprSize lhs + exprSize rhs)
  stmtSize (StmtExpr e _) = S (exprSize e)
  stmtSize (StmtBreak maybeExpr) = S (maybeExprSize maybeExpr)
  stmtSize StmtContinue = 1
  stmtSize (StmtReturn maybeExpr) = S (maybeExprSize maybeExpr)

  stmtListSize : List Stmt -> Nat
  stmtListSize [] = 0
  stmtListSize (stmt :: stmts) = stmtSize stmt + stmtListSize stmts

  blockSize : BlockExpr -> Nat
  blockSize (MkBlockExpr stmts tailExpr) = S (stmtListSize stmts + maybeExprSize tailExpr)

showQMatchLabelLeaf : QMatchLabel -> String
showQMatchLabelLeaf (QMatchLabelNat n) = show n
showQMatchLabelLeaf (QMatchLabelBitString s) = "b\"" ++ s ++ "\""

mutual
  showExprListFuel : Nat -> PrettyStyle -> List Expr -> List String
  showExprListFuel _ _ [] = []
  showExprListFuel fuel style (e :: es) =
    showExprWithFuel fuel style e :: showExprListFuel fuel style es

  showControlArgListFuel : Nat -> PrettyStyle -> List ControlArg -> List String
  showControlArgListFuel _ _ [] = []
  showControlArgListFuel fuel style (arg :: args) =
    showControlArgWithFuel fuel style arg :: showControlArgListFuel fuel style args

  showControlPrefixListFuel : Nat -> PrettyStyle -> List ControlPrefix -> List String
  showControlPrefixListFuel _ _ [] = []
  showControlPrefixListFuel fuel style (ctrlPrefix :: prefixes) =
    showControlPrefixWithFuel fuel style ctrlPrefix ::
    showControlPrefixListFuel fuel style prefixes

  showMatchArmListFuel : Nat -> PrettyStyle -> List MatchArm -> List String
  showMatchArmListFuel _ _ [] = []
  showMatchArmListFuel fuel style (arm :: arms) =
    showMatchArmWithFuel fuel style arm :: showMatchArmListFuel fuel style arms

  showQMatchArmListFuel : Nat -> PrettyStyle -> List QMatchArm -> List String
  showQMatchArmListFuel _ _ [] = []
  showQMatchArmListFuel fuel style (arm :: arms) =
    showQMatchArmWithFuel fuel style arm :: showQMatchArmListFuel fuel style arms

  showStmtListFuel : Nat -> PrettyStyle -> List Stmt -> List String
  showStmtListFuel _ _ [] = []
  showStmtListFuel fuel style (stmt :: stmts) =
    showStmtWithFuel fuel style stmt :: showStmtListFuel fuel style stmts

  showPostfixBaseFuel : Nat -> PrettyStyle -> Expr -> String
  showPostfixBaseFuel Z _ _ = prettyFuelExhausted
  showPostfixBaseFuel (S fuel) style e =
    if exprPrec e < 110
      then parens (showExprWithFuel fuel style e)
      else showExprPrecWithFuel fuel style 110 e

  showExprWithFuel : Nat -> PrettyStyle -> Expr -> String
  showExprWithFuel fuel style e = showExprPrecWithFuel fuel style 0 e

  showControlArgWithFuel : Nat -> PrettyStyle -> ControlArg -> String
  showControlArgWithFuel Z _ _ = prettyFuelExhausted
  showControlArgWithFuel (S fuel) style (ControlArgExpr e) = showExprWithFuel fuel style e
  showControlArgWithFuel (S _) _ (ControlArgNamed (MkControlNamedArg nm pol)) =
    nm ++ " = " ++ showBoolLower pol

  showControlPrefixWithFuel : Nat -> PrettyStyle -> ControlPrefix -> String
  showControlPrefixWithFuel Z _ _ = prettyFuelExhausted
  showControlPrefixWithFuel (S fuel) style (PrefixCtrl args) =
    "ctrl(" ++ joinWith ", " (showControlArgListFuel fuel style args) ++ ")"
  showControlPrefixWithFuel (S fuel) style (PrefixNegCtrl args) =
    "negctrl(" ++ joinWith ", " (showControlArgListFuel fuel style args) ++ ")"

  showMatchArmWithFuel : Nat -> PrettyStyle -> MatchArm -> String
  showMatchArmWithFuel Z _ _ = prettyFuelExhausted
  showMatchArmWithFuel (S fuel) style (MkMatchArm pat body) =
    showPatternLeaf pat ++ " => " ++ showExprWithFuel fuel style body

  showQMatchArmWithFuel : Nat -> PrettyStyle -> QMatchArm -> String
  showQMatchArmWithFuel Z _ _ = prettyFuelExhausted
  showQMatchArmWithFuel (S fuel) style (MkQMatchArm lbl body) =
    showQMatchLabelLeaf lbl ++ " => " ++ showBlockWithFuel fuel style body

  showStmtWithFuel : Nat -> PrettyStyle -> Stmt -> String
  showStmtWithFuel Z _ _ = prettyFuelExhausted
  showStmtWithFuel (S fuel) style (StmtLet pat Nothing val) =
    "let " ++ showPatternLeaf pat ++ " = " ++ showExprWithFuel fuel style val ++ ";"
  showStmtWithFuel (S fuel) style (StmtLet pat (Just ty) val) =
    "let " ++ showTypedBindingPatternLeaf pat ++ ": " ++ showTypExprLeaf ty ++
    " = " ++ showExprWithFuel fuel style val ++ ";"
  showStmtWithFuel (S fuel) style (StmtAssign lhs op rhs) =
    showExprWithFuel fuel style lhs ++ " " ++ showAssignOpLeaf op ++ " " ++
    showExprWithFuel fuel style rhs ++ ";"
  showStmtWithFuel (S fuel) style (StmtExpr e True) = showExprWithFuel fuel style e ++ ";"
  showStmtWithFuel (S fuel) style (StmtExpr e False) = showExprWithFuel fuel style e
  showStmtWithFuel (S _) _ (StmtBreak Nothing) = "break;"
  showStmtWithFuel (S fuel) style (StmtBreak (Just e)) =
    "break " ++ showExprWithFuel fuel style e ++ ";"
  showStmtWithFuel (S _) _ StmtContinue = "continue;"
  showStmtWithFuel (S _) _ (StmtReturn Nothing) = "return;"
  showStmtWithFuel (S fuel) style (StmtReturn (Just e)) =
    "return " ++ showExprWithFuel fuel style e ++ ";"

  showBlockWithFuel : Nat -> PrettyStyle -> BlockExpr -> String
  showBlockWithFuel Z _ _ = prettyFuelExhausted
  showBlockWithFuel (S fuel) style (MkBlockExpr stmts tailExpr) =
    let stmtStrs = showStmtListFuel fuel style stmts
        tailStrs =
          case tailExpr of
            Nothing => []
            Just e  => [showExprWithFuel fuel style e]
        parts = stmtStrs ++ tailStrs
    in case parts of
         [] => "{ }"
         _  => braces (joinWith " " parts)

  showExprPrecWithFuel : Nat -> PrettyStyle -> Nat -> Expr -> String
  showExprPrecWithFuel Z _ _ _ = prettyFuelExhausted
  showExprPrecWithFuel (S fuel) style outer expr =
    case expr of
      EVarName x =>
        x

      ELit lit =>
        showLiteralLeaf lit

      ETuple es =>
        showTupleLike (showExprListFuel fuel style es)

      EArrayLiteral es =>
        brackets (joinWith ", " (showExprListFuel fuel style es))

      EArrayRepeat val n =>
        "[" ++ showExprWithFuel fuel style val ++ "; " ++ show n ++ "]"

      EIndex target ix =>
        showPostfixBaseFuel fuel style target ++ "[" ++ showExprWithFuel fuel style ix ++ "]"

      EField target fld =>
        showPostfixBaseFuel fuel style target ++ "." ++ fld

      ETupleIndex target n =>
        showPostfixBaseFuel fuel style target ++ "." ++ show n

      ECall callee args =>
        showPostfixBaseFuel fuel style callee ++
        "(" ++ joinWith ", " (showExprListFuel fuel style args) ++ ")"

      EMacroCall macroName args =>
        macroName ++ "!(" ++ joinWith ", " (showExprListFuel fuel style args) ++ ")"

      EUnary op e =>
        let body = showUnaryOpLeaf op ++ showExprPrecWithFuel fuel style 100 e
        in wrapByStyle style outer expr body

      EBinary op lhs rhs =>
        let p = binaryPrec op
            leftStr  = showExprPrecWithFuel fuel style p lhs
            rightStr = showExprPrecWithFuel fuel style (S p) rhs
            body = leftStr ++ " " ++ showBinaryOpLeaf op ++ " " ++ rightStr
        in wrapByStyle style outer expr body

      EIf cond thenBlk elseBranch =>
        let base = "if " ++ showExprWithFuel fuel style cond ++ " " ++ showBlockWithFuel fuel style thenBlk
        in case elseBranch of
             Nothing => base
             Just e  => base ++ " else " ++ showExprWithFuel fuel style e

      EQIf cond thenBlk elseBranch =>
        let base = "qif " ++ showExprWithFuel fuel style cond ++ " " ++ showBlockWithFuel fuel style thenBlk
        in case elseBranch of
             Nothing => base
             Just e  => base ++ " qelse " ++ showExprWithFuel fuel style e

      ELoop body =>
        "loop " ++ showBlockWithFuel fuel style body

      EWhile cond body =>
        "while " ++ showExprWithFuel fuel style cond ++ " " ++ showBlockWithFuel fuel style body

      EFor pat iterable body =>
        "for " ++ showPatternLeaf pat ++ " in " ++
        showExprWithFuel fuel style iterable ++ " " ++ showBlockWithFuel fuel style body

      EMatch scrutinee arms =>
        "match " ++ showExprWithFuel fuel style scrutinee ++ " { " ++
        joinWith ", " (showMatchArmListFuel fuel style arms) ++ " }"

      EQMatch scrutinee arms =>
        "qmatch " ++ showExprWithFuel fuel style scrutinee ++ " { " ++
        joinWith ", " (showQMatchArmListFuel fuel style arms) ++ " }"

      EBlock blk =>
        showBlockWithFuel fuel style blk

      ECastAs value targetTy =>
        let body = showExprPrecWithFuel fuel style 95 value ++ " as " ++ showTypExprLeaf targetTy
        in wrapByStyle style outer expr body

      EBuiltinCall builtin args =>
        showBuiltinNameLeaf builtin ++
        "(" ++ joinWith ", " (showExprListFuel fuel style args) ++ ")"

      EGateApply prefixes gate args =>
        let prefixPart =
              case prefixes of
                [] => ""
                _  => joinWith " " (showControlPrefixListFuel fuel style prefixes) ++ " "
        in prefixPart ++ showGateNameLeaf gate ++
           "(" ++ joinWith ", " (showExprListFuel fuel style args) ++ ")"

      EControlBlock prefixes blk =>
        let prefixPart =
              case prefixes of
                [] => ""
                _  => joinWith " " (showControlPrefixListFuel fuel style prefixes) ++ " "
        in prefixPart ++ showBlockWithFuel fuel style blk

showPostfixBase : PrettyStyle -> Expr -> String
showPostfixBase style e = showPostfixBaseFuel (exprSize e) style e

showExprWith : PrettyStyle -> Expr -> String
showExprWith style e = showExprWithFuel (exprSize e) style e

showControlArgWith : PrettyStyle -> ControlArg -> String
showControlArgWith style arg = showControlArgWithFuel (controlArgSize arg) style arg

showControlPrefixWith : PrettyStyle -> ControlPrefix -> String
showControlPrefixWith style ctrlPrefix =
  showControlPrefixWithFuel (controlPrefixSize ctrlPrefix) style ctrlPrefix

showMatchArmWith : PrettyStyle -> MatchArm -> String
showMatchArmWith style arm = showMatchArmWithFuel (matchArmSize arm) style arm

showQMatchArmWith : PrettyStyle -> QMatchArm -> String
showQMatchArmWith style arm = showQMatchArmWithFuel (qMatchArmSize arm) style arm

showStmtWith : PrettyStyle -> Stmt -> String
showStmtWith style stmt = showStmtWithFuel (stmtSize stmt) style stmt

showBlockWith : PrettyStyle -> BlockExpr -> String
showBlockWith style blk = showBlockWithFuel (blockSize blk) style blk

showExprPrecWith : PrettyStyle -> Nat -> Expr -> String
showExprPrecWith style outer expr = showExprPrecWithFuel (exprSize expr) style outer expr

public export
showExprLax : Expr -> String
showExprLax = showExprWith PrettyLax

public export
showExprStrict : Expr -> String
showExprStrict = showExprWith PrettyStrict

showFnParamLeaf : FnParam -> String
showFnParamLeaf (MkFnParam name typ) = name ++ ": " ++ showTypExprLeaf typ

showFnDeclLax : FnDecl -> String
showFnDeclLax (MkFnDecl name params maybeRet body) =
  let retPart =
        case maybeRet of
          Nothing => ""
          Just retTyp => " -> " ++ showTypExprLeaf retTyp
  in "fn " ++ name ++ "(" ++ joinWith ", " (map showFnParamLeaf params) ++ ")" ++
     retPart ++ " " ++ showBlockWith PrettyLax body

showFnDeclStrict : FnDecl -> String
showFnDeclStrict (MkFnDecl name params maybeRet body) =
  let retPart =
        case maybeRet of
          Nothing => ""
          Just retTyp => " -> " ++ showTypExprLeaf retTyp
  in "fn " ++ name ++ "(" ++ joinWith ", " (map showFnParamLeaf params) ++ ")" ++
     retPart ++ " " ++ showBlockWith PrettyStrict body

showItemLax : Item -> String
showItemLax (ItemFnDecl fnDecl) = showFnDeclLax fnDecl
showItemLax (ItemStmt stmt) = showStmtWith PrettyLax stmt

showItemStrict : Item -> String
showItemStrict (ItemFnDecl fnDecl) = showFnDeclStrict fnDecl
showItemStrict (ItemStmt stmt) = showStmtWith PrettyStrict stmt

public export
showProgramLax : Program -> String
showProgramLax (MkProgram items) = joinWith "\n" (map showItemLax items)

public export
showProgramStrict : Program -> String
showProgramStrict (MkProgram items) = joinWith "\n" (map showItemStrict items)

mutual
  eqExprList : List Expr -> List Expr -> Bool
  eqExprList [] [] = True
  eqExprList (x :: xs) (y :: ys) = eqExprStrict x y && eqExprList xs ys
  eqExprList _ _ = False

  eqMaybeExpr : Maybe Expr -> Maybe Expr -> Bool
  eqMaybeExpr Nothing Nothing = True
  eqMaybeExpr (Just x) (Just y) = eqExprStrict x y
  eqMaybeExpr _ _ = False

  eqControlArgList : List ControlArg -> List ControlArg -> Bool
  eqControlArgList [] [] = True
  eqControlArgList (x :: xs) (y :: ys) = eqControlArgStrict x y && eqControlArgList xs ys
  eqControlArgList _ _ = False

  eqControlPrefixList : List ControlPrefix -> List ControlPrefix -> Bool
  eqControlPrefixList [] [] = True
  eqControlPrefixList (x :: xs) (y :: ys) =
    eqControlPrefixStrict x y && eqControlPrefixList xs ys
  eqControlPrefixList _ _ = False

  eqMatchArmList : List MatchArm -> List MatchArm -> Bool
  eqMatchArmList [] [] = True
  eqMatchArmList (x :: xs) (y :: ys) = eqMatchArmStrict x y && eqMatchArmList xs ys
  eqMatchArmList _ _ = False

  eqQMatchArmList : List QMatchArm -> List QMatchArm -> Bool
  eqQMatchArmList [] [] = True
  eqQMatchArmList (x :: xs) (y :: ys) =
    eqQMatchArmStrict x y && eqQMatchArmList xs ys
  eqQMatchArmList _ _ = False

  eqStmtList : List Stmt -> List Stmt -> Bool
  eqStmtList [] [] = True
  eqStmtList (x :: xs) (y :: ys) = eqStmtStrict x y && eqStmtList xs ys
  eqStmtList _ _ = False

  public export
  eqControlArgStrict : ControlArg -> ControlArg -> Bool
  eqControlArgStrict (ControlArgExpr x) (ControlArgExpr y) = eqExprStrict x y
  eqControlArgStrict (ControlArgNamed x) (ControlArgNamed y) = x == y
  eqControlArgStrict _ _ = False

  public export
  eqControlPrefixStrict : ControlPrefix -> ControlPrefix -> Bool
  eqControlPrefixStrict (PrefixCtrl xs) (PrefixCtrl ys) = eqControlArgList xs ys
  eqControlPrefixStrict (PrefixNegCtrl xs) (PrefixNegCtrl ys) = eqControlArgList xs ys
  eqControlPrefixStrict _ _ = False

  public export
  eqMatchArmStrict : MatchArm -> MatchArm -> Bool
  eqMatchArmStrict (MkMatchArm patX bodyX) (MkMatchArm patY bodyY) =
    patX == patY && eqExprStrict bodyX bodyY

  public export
  eqQMatchArmStrict : QMatchArm -> QMatchArm -> Bool
  eqQMatchArmStrict (MkQMatchArm labelX bodyX) (MkQMatchArm labelY bodyY) =
    labelX == labelY && eqBlockExprStrict bodyX bodyY

  public export
  eqStmtStrict : Stmt -> Stmt -> Bool
  eqStmtStrict (StmtLet patX typX valX) (StmtLet patY typY valY) =
    patX == patY && typX == typY && eqExprStrict valX valY
  eqStmtStrict (StmtAssign lhsX opX rhsX) (StmtAssign lhsY opY rhsY) =
    eqExprStrict lhsX lhsY && opX == opY && eqExprStrict rhsX rhsY
  eqStmtStrict (StmtExpr exprX semiX) (StmtExpr exprY semiY) =
    eqExprStrict exprX exprY && semiX == semiY
  eqStmtStrict (StmtBreak maybeX) (StmtBreak maybeY) = eqMaybeExpr maybeX maybeY
  eqStmtStrict StmtContinue StmtContinue = True
  eqStmtStrict (StmtReturn maybeX) (StmtReturn maybeY) = eqMaybeExpr maybeX maybeY
  eqStmtStrict _ _ = False

  public export
  eqBlockExprStrict : BlockExpr -> BlockExpr -> Bool
  eqBlockExprStrict (MkBlockExpr stmtsX tailX) (MkBlockExpr stmtsY tailY) =
    eqStmtList stmtsX stmtsY && eqMaybeExpr tailX tailY

  public export
  eqExprStrict : Expr -> Expr -> Bool
  eqExprStrict (EVarName x) (EVarName y) = x == y
  eqExprStrict (ELit x) (ELit y) = x == y
  eqExprStrict (ETuple xs) (ETuple ys) = eqExprList xs ys
  eqExprStrict (EArrayLiteral xs) (EArrayLiteral ys) = eqExprList xs ys
  eqExprStrict (EArrayRepeat valX nX) (EArrayRepeat valY nY) =
    eqExprStrict valX valY && nX == nY
  eqExprStrict (EIndex targetX indexX) (EIndex targetY indexY) =
    eqExprStrict targetX targetY && eqExprStrict indexX indexY
  eqExprStrict (EField targetX fieldX) (EField targetY fieldY) =
    eqExprStrict targetX targetY && fieldX == fieldY
  eqExprStrict (ETupleIndex targetX indexX) (ETupleIndex targetY indexY) =
    eqExprStrict targetX targetY && indexX == indexY
  eqExprStrict (ECall calleeX argsX) (ECall calleeY argsY) =
    eqExprStrict calleeX calleeY && eqExprList argsX argsY
  eqExprStrict (EMacroCall nameX argsX) (EMacroCall nameY argsY) =
    nameX == nameY && eqExprList argsX argsY
  eqExprStrict (EUnary opX exprX) (EUnary opY exprY) =
    opX == opY && eqExprStrict exprX exprY
  eqExprStrict (EBinary opX lhsX rhsX) (EBinary opY lhsY rhsY) =
    opX == opY && eqExprStrict lhsX lhsY && eqExprStrict rhsX rhsY
  eqExprStrict (EIf condX thenX elseX) (EIf condY thenY elseY) =
    eqExprStrict condX condY && eqBlockExprStrict thenX thenY && eqMaybeExpr elseX elseY
  eqExprStrict (EQIf condX thenX elseX) (EQIf condY thenY elseY) =
    eqExprStrict condX condY && eqBlockExprStrict thenX thenY && eqMaybeExpr elseX elseY
  eqExprStrict (ELoop bodyX) (ELoop bodyY) = eqBlockExprStrict bodyX bodyY
  eqExprStrict (EWhile condX bodyX) (EWhile condY bodyY) =
    eqExprStrict condX condY && eqBlockExprStrict bodyX bodyY
  eqExprStrict (EFor patX iterableX bodyX) (EFor patY iterableY bodyY) =
    patX == patY && eqExprStrict iterableX iterableY && eqBlockExprStrict bodyX bodyY
  eqExprStrict (EMatch scrutineeX armsX) (EMatch scrutineeY armsY) =
    eqExprStrict scrutineeX scrutineeY && eqMatchArmList armsX armsY
  eqExprStrict (EQMatch scrutineeX armsX) (EQMatch scrutineeY armsY) =
    eqExprStrict scrutineeX scrutineeY && eqQMatchArmList armsX armsY
  eqExprStrict (EBlock blockX) (EBlock blockY) = eqBlockExprStrict blockX blockY
  eqExprStrict (ECastAs valueX typX) (ECastAs valueY typY) =
    eqExprStrict valueX valueY && typX == typY
  eqExprStrict (EBuiltinCall builtinX argsX) (EBuiltinCall builtinY argsY) =
    builtinX == builtinY && eqExprList argsX argsY
  eqExprStrict (EGateApply prefixesX gateX argsX) (EGateApply prefixesY gateY argsY) =
    eqControlPrefixList prefixesX prefixesY && gateX == gateY && eqExprList argsX argsY
  eqExprStrict (EControlBlock prefixesX blockX) (EControlBlock prefixesY blockY) =
    eqControlPrefixList prefixesX prefixesY && eqBlockExprStrict blockX blockY
  eqExprStrict _ _ = False

eqFnDeclStrict : FnDecl -> FnDecl -> Bool
eqFnDeclStrict (MkFnDecl nameX paramsX retX bodyX) (MkFnDecl nameY paramsY retY bodyY) =
  nameX == nameY && paramsX == paramsY && retX == retY && eqBlockExprStrict bodyX bodyY

eqItemStrict : Item -> Item -> Bool
eqItemStrict (ItemFnDecl fnX) (ItemFnDecl fnY) = eqFnDeclStrict fnX fnY
eqItemStrict (ItemStmt stmtX) (ItemStmt stmtY) = eqStmtStrict stmtX stmtY
eqItemStrict _ _ = False

eqItemList : List Item -> List Item -> Bool
eqItemList [] [] = True
eqItemList (x :: xs) (y :: ys) = eqItemStrict x y && eqItemList xs ys
eqItemList _ _ = False

eqProgramStrict : Program -> Program -> Bool
eqProgramStrict (MkProgram itemsX) (MkProgram itemsY) = eqItemList itemsX itemsY

------------------------------------------------------------------------------
-- Show instances for recursive AST types
------------------------------------------------------------------------------
-- These instances live here instead of Frontend.AST because the recursive AST
-- family needs custom structural traversal for Show/Eq and does not derive
-- cleanly with ElabReflection.
public export
implementation Show ControlArg where
  showPrec _ arg = showControlArgWith PrettyLax arg

public export
implementation Show ControlPrefix where
  showPrec _ ctrlPrefix = showControlPrefixWith PrettyLax ctrlPrefix

public export
implementation Show MatchArm where
  showPrec _ arm = showMatchArmWith PrettyLax arm

public export
implementation Show QMatchArm where
  showPrec _ arm = showQMatchArmWith PrettyLax arm

public export
implementation Show Stmt where
  showPrec _ stmt = showStmtWith PrettyLax stmt

public export
implementation Show BlockExpr where
  showPrec _ blk = showBlockWith PrettyLax blk

public export
implementation Show Expr where
  showPrec _ e = showExprLax e

public export
implementation Show FnDecl where
  showPrec _ fnDecl = showFnDeclLax fnDecl

public export
implementation Show Item where
  showPrec _ item = showItemLax item

public export
implementation Show Program where
  showPrec _ program = showProgramLax program

------------------------------------------------------------------------------
-- Eq instances for recursive AST types
------------------------------------------------------------------------------
public export
implementation Eq ControlArg where
  (==) = eqControlArgStrict

public export
implementation Eq ControlPrefix where
  (==) = eqControlPrefixStrict

public export
implementation Eq MatchArm where
  (==) = eqMatchArmStrict

public export
implementation Eq QMatchArm where
  (==) = eqQMatchArmStrict

public export
implementation Eq Stmt where
  (==) = eqStmtStrict

public export
implementation Eq BlockExpr where
  (==) = eqBlockExprStrict

public export
implementation Eq Expr where
  (==) = eqExprStrict

public export
implementation Eq FnDecl where
  (==) = eqFnDeclStrict

public export
implementation Eq Item where
  (==) = eqItemStrict

public export
implementation Eq Program where
  (==) = eqProgramStrict
