module Frontend.Parser.Helper

import Text.Bounds
import Text.Parse.Manual

import Frontend.Source
import Frontend.Token
import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Parser.Error
import Frontend.Syntax.AST
import Frontend.Syntax.Common
import Frontend.Syntax.Doc
import Frontend.Syntax.Name
import Frontend.Syntax.Operator

%default total

-- Text.Bounds positions use zero-based lines and columns, while Leaf's
-- SourcePos uses one-based lines and columns. There is also an endpoint
-- mismatch specific to bounds emitted by ILex: they originate as ByteBounds,
-- whose end is the position of the token's last byte, and integer literal preserves
-- that inclusive end. Leaf's SourceSpan instead uses a half-open end position.
--
-- Start and end must consequently be converted differently. The start only
-- changes index base; the end changes index base and advances past the final
-- character. This conversion is for ILex-produced Bounds, not arbitrary
-- Text.Bounds values. Advancing the column is safe as long as tokens do not
-- end with a newline which is actually the case for tokens in Token.idr.

sourceStartPos : Position -> SourcePos
sourceStartPos (P line column) =
  MkSourcePos (S line) (S column) 0

sourceEndPos : Position -> SourcePos
sourceEndPos (P line column) =
  MkSourcePos (S line) (S (S column)) 0

public export
sourceSpan : Bounds -> SourceSpan
sourceSpan NoBounds =
  let start = MkSourcePos 1 1 0
  in MkSourceSpan "" start start

sourceSpan (BS start end) =
  MkSourceSpan "" (sourceStartPos start) (sourceEndPos end)

lastItemSpan : SurfaceItem -> List SurfaceItem -> SourceSpan
lastItemSpan item [] =
    item.astInfo.span
lastItemSpan _ (item :: rest) =
    lastItemSpan item rest

public export
sourceFileInfo : String -> NodeId -> List SurfaceItem -> AstInfo
sourceFileInfo sourceFileName nodeId [] =
    let start = MkSourcePos 1 1 0 in
        MkAstInfo nodeId (MkSourceSpan sourceFileName start start)
sourceFileInfo _ nodeId (first :: rest) =
    let firstSpan = first.astInfo.span
        lastSpan = lastItemSpan first rest
     in MkAstInfo nodeId (mergeSpans firstSpan lastSpan)

public export
record ExpressionTupleTail where
  constructor MkExpressionTupleTail
  tupleTailElements : List SurfaceExpr
  tupleCloseBounds  : Bounds

public export
record CallArguments where
  constructor MkCallArguments
  callArgumentValues : List SurfaceExpr
  callCloseBounds     : Bounds

public export
record ArrayElements where
  constructor MkArrayElements
  arrayElementValues : List SurfaceExpr
  arrayCloseBounds   : Bounds

public export
failWithCustomError : CustomParseError -> Bounds -> Res isStrict Token tokens CustomParseError a
failWithCustomError customParseError bounds = Fail0 (B (Custom customParseError) bounds)

public export
parameterStartSpan :
     List SurfaceDocComment
  -> Maybe (SurfaceAstNode Mutability)
  -> SurfaceName
  -> SourceSpan
parameterStartSpan (doc :: _) _ _ = doc.astInfo.span
parameterStartSpan [] (Just mutability) _ = mutability.astInfo.span
parameterStartSpan [] Nothing name = name.astInfo.span

public export
assignmentOperator : Symbol -> Maybe AssignmentOperator
assignmentOperator SymEq        = Just AssignValue
assignmentOperator SymPlusEq    = Just AssignAdd
assignmentOperator SymMinusEq   = Just AssignSubtract
assignmentOperator SymStarEq    = Just AssignMultiply
assignmentOperator SymSlashEq   = Just AssignDivide
assignmentOperator SymPercentEq = Just AssignRemainder
assignmentOperator SymAndEq     = Just AssignBitAnd
assignmentOperator SymOrEq      = Just AssignBitOr
assignmentOperator SymCaretEq   = Just AssignBitXor
assignmentOperator SymShlEq     = Just AssignShiftLeft
assignmentOperator SymShrEq     = Just AssignShiftRight
assignmentOperator _            = Nothing

public export
binaryOperator : Symbol -> Maybe (BinaryOperator, Nat)
binaryOperator SymStar   = Just (BinaryMultiply, 80)
binaryOperator SymSlash  = Just (BinaryDivide, 80)
binaryOperator SymPercent = Just (BinaryRemainder, 80)
binaryOperator SymPlus   = Just (BinaryAdd, 75)
binaryOperator SymMinus  = Just (BinarySubtract, 75)
binaryOperator SymShl    = Just (BinaryShiftLeft, 70)
binaryOperator SymShr    = Just (BinaryShiftRight, 70)
binaryOperator SymAmp    = Just (BinaryBitAnd, 65)
binaryOperator SymCaret  = Just (BinaryBitXor, 60)
binaryOperator SymPipe   = Just (BinaryBitOr, 55)
binaryOperator SymLt     = Just (BinaryLess, 50)
binaryOperator SymLe     = Just (BinaryLessEqual, 50)
binaryOperator SymGt     = Just (BinaryGreater, 50)
binaryOperator SymGe     = Just (BinaryGreaterEqual, 50)
binaryOperator SymEqEq   = Just (BinaryEqual, 50)
binaryOperator SymNotEq  = Just (BinaryNotEqual, 50)
binaryOperator SymAndAnd = Just (BinaryLogicalAnd, 45)
binaryOperator SymOrOr   = Just (BinaryLogicalOr, 40)
binaryOperator _         = Nothing
