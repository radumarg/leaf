module Frontend.ExprPrettyPrinter

-- import Derive.Prelude
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
showTypPrimLeaf TypPrimUnit  = "()"
showTypPrimLeaf TypPrimAngle = "angle"
showTypPrimLeaf TypPrimBit   = "bit"
showTypPrimLeaf TypPrimBool  = "bool"
showTypPrimLeaf TypPrimFloat = "float"
showTypPrimLeaf TypPrimInt   = "int"
showTypPrimLeaf TypPrimUInt  = "uint"
showTypPrimLeaf TypPrimQubit = "qubit"

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
  showPatternLeaf : Pattern -> String
  showPatternLeaf PatWildcard = "_"
  showPatternLeaf (PatVarName x) = x
  showPatternLeaf (PatLit lit) = showLiteralLeaf lit
  showPatternLeaf PatUnit = "()"
  showPatternLeaf (PatTuple ps) = showTupleLike (showPatternList ps)

  showPatternList : List Pattern -> List String
  showPatternList [] = []
  showPatternList (p :: ps) = showPatternLeaf p :: showPatternList ps

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
showBuiltinNameLeaf BuiltinPow       = "pow"
showBuiltinNameLeaf BuiltinQAlloc    = "qalloc"
showBuiltinNameLeaf BuiltinReset     = "reset"
showBuiltinNameLeaf BuiltinRound     = "round"
showBuiltinNameLeaf BuiltinSin       = "sin"
showBuiltinNameLeaf BuiltinSqrt      = "sqrt"
showBuiltinNameLeaf BuiltinTan       = "tan"
showBuiltinNameLeaf BuiltinUncompute = "uncompute"

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


  
