module Frontend.Syntax.ASTPrettyPrinter

import Data.List1

import Frontend.Token
import Frontend.ASTPhases
import Frontend.Syntax.AST
import Frontend.Syntax.Attribute
import Frontend.Syntax.Common
import Frontend.Syntax.Contract
import Frontend.Syntax.Doc
import Frontend.Syntax.Literal
import Frontend.Syntax.Name
import Frontend.Syntax.Operator
import Frontend.Syntax.Pattern
import Frontend.Syntax.Type

%default total

--------------------------------------------------------------------------------
-- Pretty-printing for the surface AST (Frontend.Syntax.AST and friends).
--------------------------------------------------------------------------------
-- Two styles, selected by PrettyStyle:
--
--   PrettyLax    (default) -- parentheses are printed only where the source
--                              would otherwise re-parse differently, i.e. the
--                              usual "minimal parenthesization" pretty-printer
--   PrettyStrict            -- every syntactically OPTIONAL parenthesis
--                              (around unary/binary/cast/range operands) is
--                              printed explicitly, regardless of precedence
--
-- Both styles still print MANDATORY parentheses -- e.g. around a block-like
-- expression (if/match/loop/...) used as the base of a postfix operation --
-- since those are not "optional", they are required for the output to
-- re-parse to the same tree.
--
-- TOTALITY: this module follows the discipline documented at the top of
-- Frontend.PostParseValidation ("learned the hard way"): Idris 2's size-change checker
-- credits CONSTRUCTOR PATTERNS ONLY. Every record is destructured via its
-- constructor in the pattern head (never through a `.field` projection) when
-- the projected value feeds a recursive call, and every List/List1 that is
-- walked with a function from the same mutual block is walked with a
-- hand-written recursive helper (never `map`/`concatMap`/`maybe`, which hide
-- the recursive call behind a higher-order function the checker cannot see
-- through). Expressions and types are mutually recursive (casts embed types,
-- array-size positions embed expressions), and blocks/statements/items hang
-- off of them, so the whole family below is ONE mutual block -- exactly the
-- shape validated by Frontend.PostParseValidation, and, like that module, it needs no
-- fuel parameter to be accepted as total.
--
-- Patterns (Frontend.Syntax.Pattern) do not depend on expressions, so they
-- get their own, smaller mutual block, checked independently and reused
-- freely (via ordinary, non-recursive calls) from the big block below.
--------------------------------------------------------------------------------

public export
data PrettyStyle
  = PrettyLax
  | PrettyStrict

isPrettyStrict : PrettyStyle -> Bool
isPrettyStrict PrettyStrict = True
isPrettyStrict PrettyLax    = False

--------------------------------------------------------------------------------
-- Generic string helpers
--------------------------------------------------------------------------------

joinWith : String -> List String -> String
joinWith _   []        = ""
joinWith _   [x]       = x
joinWith sep (x :: xs) = x ++ sep ++ joinWith sep xs

parens : String -> String
parens s = "(" ++ s ++ ")"

brackets : String -> String
brackets s = "[" ++ s ++ "]"

braces : String -> String
braces s = "{ " ++ s ++ " }"

-- "" stays "", anything else gets a trailing space -- for optional keyword
-- prefixes such as `pub `/`mut ` whose Show instance prints "" when absent.
prefixSpace : String -> String
prefixSpace ""  = ""
prefixSpace str = str ++ " "

showBoolLower : Bool -> String
showBoolLower True  = "true"
showBoolLower False = "false"

-- Whether a rendered fragment starts with `&`. A bare `&` prefix (SharedBorrow,
-- no trailing space) glued directly onto a fragment that itself starts with
-- `&` would re-lex as the single `&&` token instead of two `&` tokens, so
-- callers use this to decide whether a defensive space is needed.
startsWithAmp : String -> Bool
startsWithAmp s =
  case unpack s of
    ('&' :: _) => True
    _          => False

-- `&`/`&mut` prefixed onto an already-rendered body, safe against the `&&`
-- collision above. `&mut` already carries its own trailing space (a word
-- boundary is required regardless), so only the bare `&` case needs the check.
showBorrowPrefixed : BorrowKind -> String -> String
showBorrowPrefixed SharedBorrow  body = "&" ++ (if startsWithAmp body then " " else "") ++ body
showBorrowPrefixed MutableBorrow body = "&mut " ++ body

-- (), (x,), (x, y, ...) -- the shared tuple-printing shape used by
-- expression tuples, type tuples, and pattern tuples alike.
showTupleLike : List String -> String
showTupleLike []  = "()"
showTupleLike [x] = "(" ++ x ++ ",)"
showTupleLike xs  = "(" ++ joinWith ", " xs ++ ")"

--------------------------------------------------------------------------------
-- Names and paths (Frontend.Syntax.Name) -- not recursive into expressions,
-- so plain (non-mutual) functions, freely callable from anywhere below.
--------------------------------------------------------------------------------

showName : SurfaceName -> String
showName (MkAstNode _ _ (MkNameNode text)) = text

showPathSegment : SurfacePathSegment -> String
showPathSegment (MkAstNode _ _ seg) =
  case seg of
    PathSegmentName s => s
    PathSegmentSelf   => "self"

showPath : SurfacePath -> String
showPath (MkAstNode _ _ (MkPathNode first rest)) =
  joinWith "::" (showPathSegment first :: map showPathSegment rest)

--------------------------------------------------------------------------------
-- Literals (Frontend.Syntax.Literal) -- every payload is already the raw
-- source spelling (or a value with exactly one spelling), so this is a flat
-- lookup, no recursion.
--------------------------------------------------------------------------------

showLiteral : SurfaceLiteral -> String
showLiteral (MkAstNode _ _ lit) =
  case lit of
    LiteralIntegerRaw s     => s
    LiteralFloatRaw s       => s
    LiteralStringRaw s      => s
    LiteralByteRaw s        => s
    LiteralByteStringRaw s  => s
    LiteralBasisStringRaw s => s
    LiteralBoolean b        => showBoolLower b
    LiteralUnit             => "()"
    LiteralQuantumState st  => show st

--------------------------------------------------------------------------------
-- Doc comments (Frontend.Syntax.Doc) -- the raw spelling already includes
-- delimiters, so printing one back out is verbatim.
--------------------------------------------------------------------------------

showDocComment : SurfaceDocComment -> String
showDocComment (MkAstNode _ _ (MkDocCommentNode _ _ rawText)) = rawText

-- A block of doc comments, one per line, immediately preceding whatever
-- follows -- "" when there are none.
docsPrefix : List SurfaceDocComment -> String
docsPrefix docs = concatMap (\d => showDocComment d ++ "\n") docs

--------------------------------------------------------------------------------
-- Attributes (Frontend.Syntax.Attribute)
--------------------------------------------------------------------------------

showAttributeArgument : SurfaceAttributeArgument -> String
showAttributeArgument (MkAstNode _ _ (AttributeArgumentStringLit s)) = s

showAttribute : SurfaceAttribute -> String
showAttribute (MkAstNode _ _ (MkAttributeNode nm margs)) =
  "#[" ++ showName nm ++
    (case margs of
       Nothing   => ""
       Just args => "(" ++ joinWith ", " (map showAttributeArgument args) ++ ")") ++
  "]"

attrsPrefix : List SurfaceAttribute -> String
attrsPrefix attrs = concatMap (\a => showAttribute a ++ "\n") attrs

--------------------------------------------------------------------------------
-- Small leaf-list helpers shared by later sections. None of these walk a
-- structure that is part of the big mutual block, so `map` is safe here.
--------------------------------------------------------------------------------

visPrefix : VisibilityQualifier -> String
visPrefix v = prefixSpace (show v)

optionalVisPrefix : Maybe (SurfaceAstNode VisibilityQualifier) -> String
optionalVisPrefix Nothing = ""
optionalVisPrefix (Just (MkAstNode _ _ visibility)) = visPrefix visibility

showQualifiersPrefix : List (SurfaceAstNode QuantumStorageQualifier) -> String
showQualifiersPrefix []    = ""
showQualifiersPrefix quals =
  joinWith " " (map (\(MkAstNode _ _ q) => show q) quals) ++ " "

showOnBasis : Maybe (SurfaceAstNode String) -> String
showOnBasis Nothing                    = ""
showOnBasis (Just (MkAstNode _ _ raw)) = ".on(" ++ raw ++ ")"

showQualifiersPrefix1 : List1 (SurfaceAstNode QuantumStorageQualifier) -> String
showQualifiersPrefix1 quals =
  joinWith " " (map (\(MkAstNode _ _ q) => show q) (forget quals)) ++ " "

showMutabilityPrefix : Maybe (SurfaceAstNode Mutability) -> String
showMutabilityPrefix Nothing = ""
showMutabilityPrefix (Just (MkAstNode _ _ mutability)) =
  prefixSpace (show mutability)

--------------------------------------------------------------------------------
-- Patterns (Frontend.Syntax.Pattern) -- self-recursive, but independent of
-- expressions, so this is its own mutual block.
--------------------------------------------------------------------------------

mutual

  public export
  showPattern : SurfacePattern -> String
  showPattern (MkAstNode _ _ pat) = showPatternNode pat

  showPatternNode : PatternNode -> String
  showPatternNode pat =
    case pat of
      PatternWildcard          => "_"
      PatternName mutability nm =>
        maybe "" (\m => prefixSpace (show m)) mutability ++ showName nm
      PatternPath p             => showPath p
      PatternLiteral lit        => showLiteral lit
      PatternParenthesized inner => parens (showPattern inner)
      PatternTuple elems         => showTupleLike (showPatternList1 elems)
      PatternArray elems         => brackets (joinWith ", " (showPatternList elems))
      PatternStruct p fields     =>
        showPath p ++ " " ++ braces (joinWith ", " (showStructPatternFieldList fields))
      PatternEnumTuple p args    =>
        showPath p ++ parens (joinWith ", " (showPatternList args))

  showPatternList : List SurfacePattern -> List String
  showPatternList []        = []
  showPatternList (p :: ps) = showPattern p :: showPatternList ps

  showPatternList1 : List1 SurfacePattern -> List String
  showPatternList1 (p ::: ps) = showPattern p :: showPatternList ps

  showStructPatternField : SurfaceStructPatternField -> String
  showStructPatternField (MkAstNode _ _ f) =
    case f of
      StructPatternFieldShorthand mutability nm =>
        prefixSpace (show mutability) ++ showName nm
      StructPatternFieldExplicit nm pat =>
        showName nm ++ ": " ++ showPattern pat

  showStructPatternFieldList : List SurfaceStructPatternField -> List String
  showStructPatternFieldList []        = []
  showStructPatternFieldList (f :: fs) =
    showStructPatternField f :: showStructPatternFieldList fs

-- Quantum match patterns: flat, not self-recursive.
public export
showQuantumMatchPattern : SurfaceQuantumMatchPattern -> String
showQuantumMatchPattern (MkAstNode _ _ pat) =
  case pat of
    QuantumPatternBasisStringRaw s => s
    QuantumPatternIntegerRaw s     => s
    QuantumPatternWildcard         => "_"
    QuantumPatternQenumVariant p names =>
      showPath p ++ parens (joinWith ", " (map showName names))

--------------------------------------------------------------------------------
-- Pauli strings and stabilizer terms (Frontend.Syntax.Contract) -- these do
-- not mention expressions, so they too sit outside the big mutual block.
--------------------------------------------------------------------------------

showPauliString : SurfacePauliString -> String
showPauliString (MkAstNode _ _ (MkPauliStringNode ops)) =
  joinWith "" (map show (forget ops))

showStabilizerSign : StabilizerSign -> String
showStabilizerSign StabilizerPlus  = "+"
showStabilizerSign StabilizerMinus = "-"

showSignedPauliTerm : SurfaceSignedPauliTerm -> String
showSignedPauliTerm (MkAstNode _ _ (MkSignedPauliTermNode sign pauli)) =
  showStabilizerSign sign ++ showPauliString pauli

--------------------------------------------------------------------------------
-- Expression precedence table
--------------------------------------------------------------------------------
-- Higher binds tighter. Only Unary/Cast/Binary/Range are "operator class":
-- these are the constructs with OPTIONAL parentheses, the ones PrettyStrict
-- forces open. Everything else either never needs parenthesizing (atoms,
-- postfix forms, ctrl/adjoint -- all self-delimited by their own syntax) or
-- always needs it when a mandatory position demands higher precedence than
-- it has (block-like control flow used as a postfix base), independent of
-- style.
--------------------------------------------------------------------------------

precAtomic, precPostfix, precUnary, precCast : Nat
precAtomic  = 110
precPostfix = 100
precUnary   = 90
precCast    = 85

precMul, precAdd, precShift, precBitAnd, precBitXor, precBitOr : Nat
precMul    = 80
precAdd    = 75
precShift  = 70
precBitAnd = 65
precBitXor = 60
precBitOr  = 55

precCompare, precLogicalAnd, precLogicalOr, precRange, precLowest : Nat
precCompare    = 50
precLogicalAnd = 45
precLogicalOr  = 40
precRange      = 30
precLowest     = 0

binaryOperatorPrecedence : BinaryOperator -> Nat
binaryOperatorPrecedence op =
  case op of
    BinaryMultiply     => precMul
    BinaryDivide       => precMul
    BinaryRemainder    => precMul
    BinaryAdd          => precAdd
    BinarySubtract     => precAdd
    BinaryShiftLeft    => precShift
    BinaryShiftRight   => precShift
    BinaryBitAnd       => precBitAnd
    BinaryBitXor       => precBitXor
    BinaryBitOr        => precBitOr
    BinaryEqual        => precCompare
    BinaryNotEqual     => precCompare
    BinaryGreater      => precCompare
    BinaryGreaterEqual => precCompare
    BinaryLess         => precCompare
    BinaryLessEqual    => precCompare
    BinaryLogicalAnd   => precLogicalAnd
    BinaryLogicalOr    => precLogicalOr

exprOwnPrecedence : ExpressionNode -> Nat
exprOwnPrecedence e =
  case e of
    ExprLiteral _         => precAtomic
    ExprName _             => precAtomic
    ExprPath _              => precAtomic
    ExprBuiltin _            => precAtomic
    ExprSelf                  => precAtomic
    ExprParenthesized _        => precAtomic
    ExprTuple _                 => precAtomic
    ExprArray _                  => precAtomic
    ExprRepeatedArray _ _         => precAtomic
    ExprStructLiteral _ _          => precAtomic
    ExprCtrl _                      => precAtomic
    ExprAdjoint _                    => precAtomic
    ExprCall _ _                      => precPostfix
    ExprMethodCall _ _ _                => precPostfix
    ExprField _ _                         => precPostfix
    ExprTupleIndex _ _                      => precPostfix
    ExprIndex _ _                             => precPostfix
    ExprUnary _ _                               => precUnary
    ExprCast _ _                                  => precCast
    ExprBinary (MkAstNode _ _ op) _ _               => binaryOperatorPrecedence op
    ExprRange _ _ _                                   => precRange
    ExprBlock _                                         => precLowest
    ExprIf _                                              => precLowest
    ExprQIf _                                               => precLowest
    ExprSIf _                                                 => precLowest
    ExprMatch _                                                 => precLowest
    ExprQMatch _                                                  => precLowest
    ExprSMatch _                                                    => precLowest
    ExprLoop _                                                        => precLowest
    ExprWhile _ _                                                       => precLowest
    ExprFor _ _ _                                                         => precLowest
    ExprBreak _                                                             => precLowest
    ExprContinue                                                              => precLowest
    ExprReturn _                                                                => precLowest

isOperatorClassExpr : ExpressionNode -> Bool
isOperatorClassExpr e =
  case e of
    ExprUnary _ _   => True
    ExprCast _ _    => True
    ExprBinary _ _ _ => True
    ExprRange _ _ _  => True
    _                => False

--------------------------------------------------------------------------------
-- The big mutual block: types, expressions (and every node family they
-- embed), blocks, statements, contracts, items, and declarations. All of
-- these are one strongly-connected recursive family: casts embed types,
-- array sizes embed expressions, function declarations embed blocks and
-- contract clauses, impl/mod declarations embed items which embed function
-- declarations again.
--------------------------------------------------------------------------------

mutual

  ------------------------------------------------------------------
  -- Types (Frontend.Syntax.Type)
  ------------------------------------------------------------------

  showTyNode : PrettyStyle -> TyNode SurfaceExpr -> String
  showTyNode style ty =
    case ty of
      TyPrimitive p         => showTypPrimLeaf p
      TyPath p               => showPath p
      TyUnit                   => "()"
      TyParenthesized inner      => parens (showTy style inner)
      TyTuple elems                => showTupleLike (showTyList1 style elems)
      TyArray elemTy sizeE           =>
        brackets (showTy style elemTy ++ "; " ++ showExprAt style 0 sizeE)
      TySlice elemTy                   => brackets (showTy style elemTy)
      TyReference (MkAstNode _ _ borrow) innerTy =>
        showBorrowPrefixed borrow (showTy style innerTy)
      TyQualified quals innerTy =>
        showQualifiersPrefix1 quals ++ showTy style innerTy
      TyFunction effect params retTy =>
        (case effect of
           Nothing                     => ""
           Just (MkAstNode _ _ eff) => show eff ++ " ") ++
        "fn(" ++ joinWith ", " (showFunctionTypeParameterList style params) ++ ")" ++
        (case retTy of
           Nothing => ""
           Just t  => " -> " ++ showTy style t)

  public export
  showTy : PrettyStyle -> SurfaceTy -> String
  showTy style (MkAstNode _ _ ty) = showTyNode style ty

  showTyList : PrettyStyle -> List SurfaceTy -> List String
  showTyList style []        = []
  showTyList style (t :: ts) = showTy style t :: showTyList style ts

  showTyList1 : PrettyStyle -> List1 SurfaceTy -> List String
  showTyList1 style (t ::: ts) = showTy style t :: showTyList style ts

  showFunctionTypeParameterList :
       PrettyStyle
    -> List (SurfaceAstNode (FunctionTypeParameterNode SurfaceExpr))
    -> List String
  showFunctionTypeParameterList style [] = []
  showFunctionTypeParameterList style (MkAstNode _ _ (MkFunctionTypeParameterNode nm ty) :: rest) =
    (showName nm ++ ": " ++ showTy style ty) :: showFunctionTypeParameterList style rest

  ------------------------------------------------------------------
  -- Expressions
  ------------------------------------------------------------------

  -- The single entry point that decides whether a child expression needs to
  -- be parenthesized, given the precedence its syntactic position requires.
  -- Exported for callers that need explicit control over the ambient
  -- precedence; `showExpr` below is the common case (no ambient requirement).
  public export
  showExprAt : PrettyStyle -> Nat -> SurfaceExpr -> String
  showExprAt style outerReq (MkAstNode _ _ value) = wrapExpr style outerReq value

  public export
  showExpr : PrettyStyle -> SurfaceExpr -> String
  showExpr style = showExprAt style 0

  wrapExpr : PrettyStyle -> Nat -> ExpressionNode -> String
  wrapExpr style outerReq value =
    let body     = showExpressionNodeBody style value
        own      = exprOwnPrecedence value
        mustWrap = (own < outerReq) || (isPrettyStrict style && isOperatorClassExpr value)
    in if mustWrap then parens body else body

  -- Top-level (unconstrained) rendering of a raw ExpressionNode -- used by
  -- the Show instance further down.
  showExpressionNode : PrettyStyle -> ExpressionNode -> String
  showExpressionNode style value = wrapExpr style 0 value

  showExpressionNodeBody : PrettyStyle -> ExpressionNode -> String
  showExpressionNodeBody style value =
    case value of
      ExprLiteral lit => showLiteral lit
      ExprName nm     => showName nm
      ExprPath p      => showPath p
      ExprBuiltin b   => show b
      ExprSelf        => "self"

      ExprParenthesized inner => parens (showExprAt style 0 inner)
      ExprTuple elems          => showTupleLike (showExprList1 style elems)
      ExprArray elems           => brackets (joinWith ", " (showExprList style elems))
      ExprRepeatedArray elemE countE =>
        brackets (showExprAt style 0 elemE ++ "; " ++ showExprAt style 0 countE)

      ExprStructLiteral p fields =>
        showPath p ++ " " ++ braces (joinWith ", " (showFieldInitList style fields))

      ExprCall callee args =>
        showExprAt style precPostfix callee ++
          "(" ++ joinWith ", " (showExprList style args) ++ ")"

      ExprMethodCall recv methodName args =>
        showExprAt style precPostfix recv ++ "." ++ showName methodName ++
          "(" ++ joinWith ", " (showExprList style args) ++ ")"

      ExprField obj fld      => showExprAt style precPostfix obj ++ "." ++ showName fld
      ExprTupleIndex obj idx => showExprAt style precPostfix obj ++ "." ++ idx
      ExprIndex obj idx      =>
        showExprAt style precPostfix obj ++ "[" ++ showExprAt style 0 idx ++ "]"

      ExprUnary (MkAstNode _ _ op) operand =>
        let operandStr = showExprAt style precUnary operand in
        case op of
          UnaryNegate     => "-" ++ operandStr
          UnaryLogicalNot => "!" ++ operandStr
          UnaryBorrow b   => showBorrowPrefixed b operandStr

      ExprBinary (MkAstNode _ _ op) lhs rhs =>
        let p = binaryOperatorPrecedence op in
        showExprAt style p lhs ++ " " ++ show op ++ " " ++ showExprAt style (S p) rhs

      ExprRange start (MkAstNode _ _ op) end =>
        (case start of
           Nothing => ""
           Just s  => showExprAt style (S precRange) s) ++
        show op ++
        (case end of
           Nothing => ""
           Just e  => showExprAt style (S precRange) e)

      ExprCast operand ty => showExprAt style precCast operand ++ " as " ++ showTy style ty

      ExprBlock blk => showBlock style blk

      ExprIf ifNode => showClassicalIfNode style ifNode

      ExprQIf (MkQuantumIfNode qcond thenBranch elseBranch) =>
        "qif " ++ showExprAt style 0 qcond ++ " " ++ showQuantumBranch style thenBranch ++
          (case elseBranch of
             Nothing => ""
             Just b  => " qelse " ++ showQuantumBranch style b)

      ExprSIf (MkStateIfNode scond thenE elseE) =>
        "sif " ++ showExprAt style 0 scond ++ " then " ++ showExprAt style 0 thenE ++
          " selse " ++ showExprAt style 0 elseE

      ExprMatch (MkClassicalMatchNode scrut arms) =>
        "match " ++ showExprAt style 0 scrut ++ " " ++
          braces (joinWith ", " (showClassicalMatchArmList style arms))

      ExprQMatch (MkQuantumMatchNode scrut arms) =>
        "qmatch " ++ showExprAt style 0 scrut ++ " " ++
          braces (joinWith ", " (showQuantumMatchArmList style arms))

      ExprSMatch (MkStateMatchNode scrut arms) =>
        "smatch " ++ showExprAt style 0 scrut ++ " " ++
          braces (joinWith ", " (showQuantumMatchArmList style arms))

      ExprLoop body => "loop " ++ showBlock style body

      ExprWhile cond body =>
        "while " ++ showExprAt style 0 cond ++ " " ++ showBlock style body

      ExprFor pat iterE body =>
        "for " ++ showPattern pat ++ " in " ++ showExprAt style 0 iterE ++
          " " ++ showBlock style body

      ExprBreak Nothing  => "break"
      ExprBreak (Just e) => "break " ++ showExprAt style 0 e

      ExprContinue => "continue"

      ExprReturn Nothing  => "return"
      ExprReturn (Just e) => "return " ++ showExprAt style 0 e

      ExprCtrl c    => showControlExpr style c
      ExprAdjoint a => showAdjointExpr style a

  showExprList : PrettyStyle -> List SurfaceExpr -> List String
  showExprList style []        = []
  showExprList style (e :: es) = showExprAt style 0 e :: showExprList style es

  showExprList1 : PrettyStyle -> List1 SurfaceExpr -> List String
  showExprList1 style (e ::: es) = showExprAt style 0 e :: showExprList style es

  showFieldInit : PrettyStyle -> SurfaceAstNode FieldInitializerNode -> String
  showFieldInit style (MkAstNode _ _ f) =
    case f of
      FieldInitShorthand nm => showName nm
      FieldInitExplicit nm e => showName nm ++ ": " ++ showExprAt style 0 e

  showFieldInitList :
       PrettyStyle -> List (SurfaceAstNode FieldInitializerNode) -> List String
  showFieldInitList style []        = []
  showFieldInitList style (f :: fs) = showFieldInit style f :: showFieldInitList style fs

  ------------------------------------------------------------------
  -- if / qif / sif
  ------------------------------------------------------------------

  showClassicalIfNode : PrettyStyle -> ClassicalIfNode -> String
  showClassicalIfNode style (MkClassicalIfNode cond thenBlk elseBranch) =
    "if " ++ showExprAt style 0 cond ++ " " ++ showBlock style thenBlk ++
      (case elseBranch of
         Nothing                                          => ""
         Just (ElseBlock b)                                => " else " ++ showBlock style b
         Just (ElseChainedIf (MkAstNode _ _ chained)) =>
           " else " ++ showClassicalIfNode style chained)

  showQuantumBranch : PrettyStyle -> QuantumBranchNode -> String
  showQuantumBranch style branch =
    case branch of
      QuantumBranchBlock b      => showBlock style b
      QuantumBranchExpression e => showExprAt style 0 e

  ------------------------------------------------------------------
  -- match / qmatch / smatch
  ------------------------------------------------------------------

  showClassicalMatchArmNode : PrettyStyle -> ClassicalMatchArmNode -> String
  showClassicalMatchArmNode style (MkClassicalMatchArmNode pat guard armBody) =
    showPattern pat ++
      (case guard of
         Nothing => ""
         Just g  => " if " ++ showExprAt style 0 g) ++
      " => " ++ showExprAt style 0 armBody

  public export
  showClassicalMatchArm : PrettyStyle -> SurfaceAstNode ClassicalMatchArmNode -> String
  showClassicalMatchArm style (MkAstNode _ _ arm) = showClassicalMatchArmNode style arm

  showClassicalMatchArmList :
       PrettyStyle -> List (SurfaceAstNode ClassicalMatchArmNode) -> List String
  showClassicalMatchArmList style []        = []
  showClassicalMatchArmList style (a :: as) =
    showClassicalMatchArm style a :: showClassicalMatchArmList style as

  showQuantumMatchArmNode : PrettyStyle -> QuantumMatchArmNode -> String
  showQuantumMatchArmNode style (MkQuantumMatchArmNode pat armBody) =
    showQuantumMatchPattern pat ++ " => " ++ showExprAt style 0 armBody

  public export
  showQuantumMatchArm : PrettyStyle -> SurfaceAstNode QuantumMatchArmNode -> String
  showQuantumMatchArm style (MkAstNode _ _ arm) = showQuantumMatchArmNode style arm

  showQuantumMatchArmList :
       PrettyStyle -> List (SurfaceAstNode QuantumMatchArmNode) -> List String
  showQuantumMatchArmList style []        = []
  showQuantumMatchArmList style (a :: as) =
    showQuantumMatchArm style a :: showQuantumMatchArmList style as

  ------------------------------------------------------------------
  -- ctrl and adjoint
  ------------------------------------------------------------------

  showControlExpr : PrettyStyle -> ControlExpressionNode -> String
  showControlExpr style c =
    case c of
      ControlledCallable controls onBasis callable =>
        "ctrl(" ++ joinWith ", " (showExprList1 style controls) ++ ")" ++
          showOnBasis onBasis ++ ".apply(" ++ showExprAt style 0 callable ++ ")"
      ControlledBlock controls onBasis body =>
        "ctrl(" ++ joinWith ", " (showExprList1 style controls) ++ ")" ++
          showOnBasis onBasis ++ " " ++ showBlock style body

  showAdjointExpr : PrettyStyle -> AdjointExpressionNode -> String
  showAdjointExpr style a =
    case a of
      AdjointOfCallable callable => "adjoint(" ++ showExprAt style 0 callable ++ ")"
      AdjointBlock body          => "adjoint " ++ showBlock style body

  ------------------------------------------------------------------
  -- Blocks and statements
  ------------------------------------------------------------------

  showBlockNode : PrettyStyle -> BlockNode -> String
  showBlockNode style (MkBlockNode innerDocs stmts finalE) =
    let docsStrs  = map showDocComment innerDocs
        stmtsStrs = showStatementList style stmts
        tailStrs  = case finalE of
                      Nothing => []
                      Just e  => [showExprAt style 0 e]
        parts     = docsStrs ++ stmtsStrs ++ tailStrs
    in case parts of
         [] => "{ }"
         _  => braces (joinWith " " parts)

  public export
  showBlock : PrettyStyle -> SurfaceBlock -> String
  showBlock style (MkAstNode _ _ blk) = showBlockNode style blk

  showStatementNode : PrettyStyle -> StatementNode -> String
  showStatementNode style stmt =
    case stmt of
      StatementLet letBinding         => showLetBindingNode style letBinding ++ ";"
      StatementAssignment assignment  => showAssignmentNode style assignment ++ ";"
      StatementSemiExpression e       => showExprAt style 0 e ++ ";"
      StatementExpression e           => showExprAt style 0 e

  public export
  showStatement : PrettyStyle -> SurfaceStatement -> String
  showStatement style (MkAstNode _ _ stmt) = showStatementNode style stmt

  showStatementList : PrettyStyle -> List SurfaceStatement -> List String
  showStatementList style []        = []
  showStatementList style (s :: ss) = showStatement style s :: showStatementList style ss

  showLetBindingNode : PrettyStyle -> LetBindingNode -> String
  showLetBindingNode style (MkLetBindingNode quals pat tyAnn maybeInit) =
    "let " ++ showQualifiersPrefix quals ++ showPattern pat ++
      (case tyAnn of
         Nothing => ""
         Just t  => ": " ++ showTy style t) ++
      (case maybeInit of
         Nothing                                       => ""
         Just (MkLetInitializerNode (MkAstNode _ _ marker) val) =>
           " " ++ show marker ++ " " ++ showExprAt style 0 val)

  public export
  showLetBinding : PrettyStyle -> SurfaceLetBinding -> String
  showLetBinding style (MkAstNode _ _ lb) = showLetBindingNode style lb

  showAssignmentNode : PrettyStyle -> AssignmentNode -> String
  showAssignmentNode style (MkAssignmentNode (MkAstNode _ _ target) (MkAstNode _ _ op) val) =
    showAssignmentTargetNode style target ++ " " ++ show op ++ " " ++ showExprAt style 0 val

  showAssignmentTargetNode : PrettyStyle -> AssignmentTargetNode -> String
  showAssignmentTargetNode style target =
    case target of
      AssignTargetName nm      => showName nm
      AssignTargetIndex obj ix =>
        showExprAt style precPostfix obj ++ "[" ++ showExprAt style 0 ix ++ "]"
      AssignTargetField obj fld => showExprAt style precPostfix obj ++ "." ++ showName fld
      AssignTargetTupleIndex obj ix => showExprAt style precPostfix obj ++ "." ++ ix

  public export
  showAssignmentTarget : PrettyStyle -> SurfaceAssignmentTarget -> String
  showAssignmentTarget style (MkAstNode _ _ t) = showAssignmentTargetNode style t

  ------------------------------------------------------------------
  -- Contracts: requires/ensures clauses and their predicates
  ------------------------------------------------------------------

  showContractPredicateNode : PrettyStyle -> ContractPredicateNode SurfaceExpr -> String
  showContractPredicateNode style predicate =
    case predicate of
      ContractClean e        => "clean(" ++ showExprAt style 0 e ++ ")"
      ContractBasis e pauli  =>
        "basis(" ++ showExprAt style 0 e ++ ", " ++ showPauliString pauli ++ ")"
      ContractSeparable e    => "separable(" ++ showExprAt style 0 e ++ ")"
      ContractIsolated e     => "isolated(" ++ showExprAt style 0 e ++ ")"
      ContractProduct e (x ::: xs) =>
        "product(" ++
          joinWith ", " (showExprAt style 0 e :: showExprAt style 0 x :: showExprList style xs) ++
          ")"
      ContractStabilized e terms =>
        "stabilized(" ++ showExprAt style 0 e ++ ", [" ++
          joinWith ", " (map showSignedPauliTerm (forget terms)) ++ "])"

  public export
  showContractPredicate : PrettyStyle -> SurfaceContractPredicate -> String
  showContractPredicate style (MkAstNode _ _ p) = showContractPredicateNode style p

  showContractClauseNode : PrettyStyle -> ContractClauseNode SurfaceExpr -> String
  showContractClauseNode style clause =
    case clause of
      RequiresClause p => "requires " ++ showContractPredicate style p
      EnsuresClause p  => "ensures " ++ showContractPredicate style p

  public export
  showContractClause : PrettyStyle -> SurfaceContractClause -> String
  showContractClause style (MkAstNode _ _ c) = showContractClauseNode style c

  showContractClauseList : PrettyStyle -> List SurfaceContractClause -> List String
  showContractClauseList style []        = []
  showContractClauseList style (c :: cs) =
    showContractClause style c :: showContractClauseList style cs

  ------------------------------------------------------------------
  -- Function declarations and parameters
  ------------------------------------------------------------------

  showFunctionParameterNode : PrettyStyle -> FunctionParameterNode -> String
  showFunctionParameterNode style p =
    case p of
      NormalParameter docs mutability nm ty =>
        docsPrefix docs ++ showMutabilityPrefix mutability ++
        showName nm ++ ": " ++ showTy style ty
      ReceiverParameter docs Nothing => docsPrefix docs ++ "self"
      ReceiverParameter docs (Just (MkAstNode _ _ borrow)) =>
        docsPrefix docs ++
          (case borrow of
             SharedBorrow  => "&self"
             MutableBorrow => "&mut self")

  public export
  showFunctionParameter : PrettyStyle -> SurfaceFunctionParameter -> String
  showFunctionParameter style (MkAstNode _ _ p) = showFunctionParameterNode style p

  showFunctionParameterList : PrettyStyle -> List SurfaceFunctionParameter -> List String
  showFunctionParameterList style []        = []
  showFunctionParameterList style (p :: ps) =
    showFunctionParameter style p :: showFunctionParameterList style ps

  showFunctionDeclarationNode : PrettyStyle -> FunctionDeclarationNode -> String
  showFunctionDeclarationNode style
    (MkFunctionDeclarationNode docs attrs vis constness effect nm params retTy supports contracts body) =
    let visibilityStr = case vis of
                          Nothing                       => ""
                          Just (MkAstNode _ _ visibility) => visPrefix visibility
        effectStr = case effect of
                      Nothing                        => ""
                      Just (MkAstNode _ _ eff)  => show eff ++ " "
        constStr    = case constness of
                        Nothing                         => ""
                        Just (MkAstNode _ _ qualifier) => show qualifier ++ " "
        paramsStr   = joinWith ", " (showFunctionParameterList style params)
        retStr      = case retTy of
                        Nothing => ""
                        Just t  => " -> " ++ showTy style t
        supportsStr = case supports of
                        [] => ""
                        _  => " supports " ++
                              joinWith ", " (map (\(MkAstNode _ _ k) => show k) supports)
        contractsStr = concatMap (\c => " " ++ c) (showContractClauseList style contracts)
    in docsPrefix docs ++ attrsPrefix attrs ++ visibilityStr ++ constStr ++ effectStr ++
       "fn " ++ showName nm ++ "(" ++ paramsStr ++ ")" ++ retStr ++ supportsStr ++
       contractsStr ++ " " ++ showBlock style body

  public export
  showFunctionDeclaration : PrettyStyle -> SurfaceFunctionDeclaration -> String
  showFunctionDeclaration style (MkAstNode _ _ fd) = showFunctionDeclarationNode style fd

  showImplFunctionList :
       PrettyStyle -> List (SurfaceAstNode FunctionDeclarationNode) -> List String
  showImplFunctionList style []        = []
  showImplFunctionList style (MkAstNode _ _ fd :: rest) =
    showFunctionDeclarationNode style fd :: showImplFunctionList style rest

  ------------------------------------------------------------------
  -- struct / enum / qenum declarations
  ------------------------------------------------------------------

  showStructField : PrettyStyle -> SurfaceAstNode StructFieldNode -> String
  showStructField style (MkAstNode _ _ (MkStructFieldNode docs nm ty)) =
    docsPrefix docs ++ showName nm ++ ": " ++ showTy style ty

  showStructFieldList : PrettyStyle -> List (SurfaceAstNode StructFieldNode) -> List String
  showStructFieldList style []        = []
  showStructFieldList style (f :: fs) = showStructField style f :: showStructFieldList style fs

  showStructDeclarationNode : PrettyStyle -> StructDeclarationNode -> String
  showStructDeclarationNode style (MkStructDeclarationNode docs attrs vis nm fields) =
    docsPrefix docs ++ attrsPrefix attrs ++ optionalVisPrefix vis ++ "struct " ++ showName nm ++
      " " ++ braces (joinWith ", " (showStructFieldList style fields))

  public export
  showStructDeclaration : PrettyStyle -> SurfaceStructDeclaration -> String
  showStructDeclaration style (MkAstNode _ _ sd) = showStructDeclarationNode style sd

  showEnumVariantBody : PrettyStyle -> EnumVariantBody -> String
  showEnumVariantBody style body =
    case body of
      VariantUnit          => ""
      VariantTuple tys      => parens (joinWith ", " (showTyList1 style tys))
      VariantStruct fields   => " " ++ braces (joinWith ", " (showStructFieldList style fields))

  showEnumVariant : PrettyStyle -> SurfaceAstNode EnumVariantNode -> String
  showEnumVariant style (MkAstNode _ _ (MkEnumVariantNode docs nm body)) =
    docsPrefix docs ++ showName nm ++ showEnumVariantBody style body

  showEnumVariantList : PrettyStyle -> List (SurfaceAstNode EnumVariantNode) -> List String
  showEnumVariantList style []        = []
  showEnumVariantList style (v :: vs) = showEnumVariant style v :: showEnumVariantList style vs

  showEnumDeclarationNode : PrettyStyle -> EnumDeclarationNode -> String
  showEnumDeclarationNode style (MkEnumDeclarationNode docs attrs vis nm variants) =
    docsPrefix docs ++ attrsPrefix attrs ++ optionalVisPrefix vis ++ "enum " ++ showName nm ++
      " " ++ braces (joinWith ", " (showEnumVariantList style variants))

  public export
  showEnumDeclaration : PrettyStyle -> SurfaceEnumDeclaration -> String
  showEnumDeclaration style (MkAstNode _ _ ed) = showEnumDeclarationNode style ed

  showQEnumVariant : PrettyStyle -> SurfaceAstNode QEnumVariantNode -> String
  showQEnumVariant style (MkAstNode _ _ (MkQEnumVariantNode docs nm payloadTys)) =
    docsPrefix docs ++ showName nm ++ parens (joinWith ", " (showTyList1 style payloadTys))

  showQEnumVariantList : PrettyStyle -> List (SurfaceAstNode QEnumVariantNode) -> List String
  showQEnumVariantList style []        = []
  showQEnumVariantList style (v :: vs) = showQEnumVariant style v :: showQEnumVariantList style vs

  showQEnumDeclarationNode : PrettyStyle -> QEnumDeclarationNode -> String
  showQEnumDeclarationNode style (MkQEnumDeclarationNode docs attrs vis nm variants) =
    docsPrefix docs ++ attrsPrefix attrs ++ optionalVisPrefix vis ++ "qenum " ++ showName nm ++
      " " ++ braces (joinWith ", " (showQEnumVariantList style variants))

  public export
  showQEnumDeclaration : PrettyStyle -> SurfaceQEnumDeclaration -> String
  showQEnumDeclaration style (MkAstNode _ _ qd) = showQEnumDeclarationNode style qd

  ------------------------------------------------------------------
  -- impl / const / use / mod declarations
  ------------------------------------------------------------------

  showImplDeclarationNode : PrettyStyle -> ImplDeclarationNode -> String
  showImplDeclarationNode style (MkImplDeclarationNode docs target fns) =
    docsPrefix docs ++ "impl " ++ showPath target ++ " " ++
      braces (joinWith " " (showImplFunctionList style fns))

  public export
  showImplDeclaration : PrettyStyle -> SurfaceImplDeclaration -> String
  showImplDeclaration style (MkAstNode _ _ impl) = showImplDeclarationNode style impl

  showConstDeclarationNode : PrettyStyle -> ConstDeclarationNode -> String
  showConstDeclarationNode style (MkConstDeclarationNode docs vis nm ty val) =
    docsPrefix docs ++ optionalVisPrefix vis ++ "const " ++ showName nm ++ ": " ++ showTy style ty ++
      " = " ++ showExprAt style 0 val ++ ";"

  public export
  showConstDeclaration : PrettyStyle -> SurfaceConstDeclaration -> String
  showConstDeclaration style (MkAstNode _ _ cd) = showConstDeclarationNode style cd

  showUseDeclarationNode : PrettyStyle -> UseDeclarationNode -> String
  showUseDeclarationNode style (MkUseDeclarationNode docs vis path) =
    docsPrefix docs ++ optionalVisPrefix vis ++ "use " ++ showPath path ++ ";"

  public export
  showUseDeclaration : PrettyStyle -> SurfaceUseDeclaration -> String
  showUseDeclaration style (MkAstNode _ _ ud) = showUseDeclarationNode style ud

  showModuleDeclarationNode : PrettyStyle -> ModuleDeclarationNode -> String
  showModuleDeclarationNode style (MkModuleDeclarationNode docs vis nm body) =
    docsPrefix docs ++ optionalVisPrefix vis ++ "mod " ++ showName nm ++ showModuleBody style body

  showModuleBody : PrettyStyle -> ModuleBody -> String
  showModuleBody style body =
    case body of
      ModuleInline innerDocs items =>
        " " ++ braces (joinWith "\n" (map showDocComment innerDocs ++ showItemList style items))
      ModuleExternal => ";"

  public export
  showModuleDeclaration : PrettyStyle -> SurfaceModuleDeclaration -> String
  showModuleDeclaration style (MkAstNode _ _ md) = showModuleDeclarationNode style md

  ------------------------------------------------------------------
  -- Items and the source file
  ------------------------------------------------------------------

  showItemNode : PrettyStyle -> ItemNode -> String
  showItemNode style item =
    case item of
      ItemFunction fd => showFunctionDeclarationNode style fd
      ItemStruct sd   => showStructDeclarationNode style sd
      ItemEnum ed     => showEnumDeclarationNode style ed
      ItemQEnum qd    => showQEnumDeclarationNode style qd
      ItemImpl impl   => showImplDeclarationNode style impl
      ItemConst cd    => showConstDeclarationNode style cd
      ItemUse ud      => showUseDeclarationNode style ud
      ItemModule md   => showModuleDeclarationNode style md

  public export
  showItem : PrettyStyle -> SurfaceItem -> String
  showItem style (MkAstNode _ _ item) = showItemNode style item

  showItemList : PrettyStyle -> List SurfaceItem -> List String
  showItemList style []        = []
  showItemList style (i :: is) = showItem style i :: showItemList style is

  showSourceFileNode : PrettyStyle -> SourceFileNode -> String
  showSourceFileNode style (MkSourceFileNode innerDocs items) =
    joinWith "\n" (map showDocComment innerDocs ++ showItemList style items)

  public export
  showSourceFile : PrettyStyle -> SurfaceSourceFile -> String
  showSourceFile style (MkAstNode _ _ sf) = showSourceFileNode style sf

--------------------------------------------------------------------------------
-- Public convenience wrappers
--------------------------------------------------------------------------------

public export
showSourceFileLax : SurfaceSourceFile -> String
showSourceFileLax = showSourceFile PrettyLax

public export
showSourceFileStrict : SurfaceSourceFile -> String
showSourceFileStrict = showSourceFile PrettyStrict

public export
showExprLax : SurfaceExpr -> String
showExprLax = showExprAt PrettyLax 0

public export
showExprStrict : SurfaceExpr -> String
showExprStrict = showExprAt PrettyStrict 0

public export
showBlockLax : SurfaceBlock -> String
showBlockLax = showBlock PrettyLax

public export
showBlockStrict : SurfaceBlock -> String
showBlockStrict = showBlock PrettyStrict

public export
showItemLax : SurfaceItem -> String
showItemLax = showItem PrettyLax

public export
showItemStrict : SurfaceItem -> String
showItemStrict = showItem PrettyStrict

public export
showTyLax : SurfaceTy -> String
showTyLax = showTy PrettyLax

public export
showTyStrict : SurfaceTy -> String
showTyStrict = showTy PrettyStrict

--------------------------------------------------------------------------------
-- Show instances on the raw (un-located) payload types. Frontend.ASTPhases
-- implements `Show a => Show (SurfaceAstNode a)` (and the Canonical/Resolved/
-- Typed equivalents) generically by printing `value` alone and skipping
-- AstInfo/NodeOrigin -- so giving each raw type below a Show instance is
-- enough for every SurfaceXxx alias in Frontend.Syntax.AST to be `Show`
-- automatically, always in PrettyLax style. Call showXxxStrict (or
-- showXxxWith-style functions above) directly when strict style is wanted.
--------------------------------------------------------------------------------

public export
implementation Show ExpressionNode where
  show = showExpressionNode PrettyLax

public export
implementation Show BlockNode where
  show = showBlockNode PrettyLax

public export
implementation Show StatementNode where
  show = showStatementNode PrettyLax

public export
implementation Show ItemNode where
  show = showItemNode PrettyLax

public export
implementation Show (TyNode SurfaceExpr) where
  show = showTyNode PrettyLax

public export
implementation Show (ContractClauseNode SurfaceExpr) where
  show = showContractClauseNode PrettyLax

public export
implementation Show (ContractPredicateNode SurfaceExpr) where
  show = showContractPredicateNode PrettyLax

public export
implementation Show SourceFileNode where
  show = showSourceFileNode PrettyLax

public export
implementation Show FunctionDeclarationNode where
  show = showFunctionDeclarationNode PrettyLax

public export
implementation Show FunctionParameterNode where
  show = showFunctionParameterNode PrettyLax

public export
implementation Show StructDeclarationNode where
  show = showStructDeclarationNode PrettyLax

public export
implementation Show EnumDeclarationNode where
  show = showEnumDeclarationNode PrettyLax

public export
implementation Show QEnumDeclarationNode where
  show = showQEnumDeclarationNode PrettyLax

public export
implementation Show ImplDeclarationNode where
  show = showImplDeclarationNode PrettyLax

public export
implementation Show ConstDeclarationNode where
  show = showConstDeclarationNode PrettyLax

public export
implementation Show UseDeclarationNode where
  show = showUseDeclarationNode PrettyLax

public export
implementation Show ModuleDeclarationNode where
  show = showModuleDeclarationNode PrettyLax

public export
implementation Show LetBindingNode where
  show = showLetBindingNode PrettyLax

public export
implementation Show AssignmentTargetNode where
  show = showAssignmentTargetNode PrettyLax

public export
implementation Show ClassicalMatchArmNode where
  show = showClassicalMatchArmNode PrettyLax

public export
implementation Show QuantumMatchArmNode where
  show = showQuantumMatchArmNode PrettyLax
