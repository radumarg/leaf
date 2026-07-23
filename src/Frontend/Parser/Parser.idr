module Frontend.Parser.Parser

import Text.Bounds
import Text.Parse.Manual
import Text.Parse.Syntax
import Data.List1

import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Source
import Frontend.Token
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
import Frontend.Parser.Error
import Frontend.Parser.Helper

%default total

0 Rule : Bool -> Type -> Type
Rule strict result =
     (nextNodeId : Nat)
  -> (tokens : List (Bounded Token))
  -> (0 acc : SuffixAcc tokens)
  -> Res strict Token tokens CustomParseError (result, Nat)

||| Parses a complete expression, including ranges and every tighter-precedence form.
||| Tested by: `fn arithmetic() {1 + 2 * 3}`.
parseExpression : Rule True SurfaceExpr

||| Returns whether an operator belongs to Leaf's non-associative comparison group.
||| All six operators share precedence, but unlike arithmetic operators they may not
||| be folded left-associatively into a chain.
isComparisonOperator : BinaryOperator -> Bool
isComparisonOperator BinaryEqual = True
isComparisonOperator BinaryNotEqual = True
isComparisonOperator BinaryGreater = True
isComparisonOperator BinaryGreaterEqual = True
isComparisonOperator BinaryLess = True
isComparisonOperator BinaryLessEqual = True
isComparisonOperator _ = False

||| Detects a comparison at the outermost level of an already-parsed expression.
||| Parenthesized comparisons intentionally do not match because their outer node is
||| a parenthesized expression, making the programmer's grouping explicit.
isUnparenthesizedComparison : SurfaceExpr -> Bool
isUnparenthesizedComparison (MkAstNode _ _ (ExprBinary operator _ _)) =
  isComparisonOperator operator.value
isUnparenthesizedComparison _ = False

||| Parses an identifier into a named AST node with source bounds.
||| Tested by: `fn names() {value; result}`.
parseName : String -> Rule True SurfaceName
parseName _ _ [] acc = Fail0 (B EOI NoBounds)
parseName expectedNameDescription nodeId ((B token bounds) :: remaining) acc =
    let (nameNodeId, nextNodeId) = reserveNodeId nodeId
    in case token of
        TokIdent name =>
            let functionNameNode =
                    surfaceAstNode
                        (MkAstInfo nameNodeId (sourceSpan bounds))
                        (MkNameNode name)
            in Succ0 (functionNameNode, nextNodeId) remaining

        _ =>
            Fail0 (B (Expected [ expectedNameDescription ] (show token)) bounds)

mutual
  ||| Dispatches to the parser for a primitive, path, reference, array, tuple,
  ||| qualified, or function type.
  ||| Tested by: `fn use_types(person: Person, config: my_module::Config) {}`.
  parseType : Rule True SurfaceTy
  parseType _ [] _ = Fail0 (B EOI NoBounds)
  parseType nodeId ((B token bounds) :: remaining) acc@(SA recur) =
    let (typeNodeId, nextNodeId) = reserveNodeId nodeId in
    case token of
      TokTypPrim primitiveName =>
        Succ0
          (surfaceAstNode (MkAstInfo typeNodeId (sourceSpan bounds)) (TyPrimitive primitiveName), nextNodeId)
          remaining
      TokSym SymLParen =>
        succT $ parseParenType typeNodeId bounds nextNodeId remaining recur
      TokSym SymLBracket =>
        succT $ parseArrayType typeNodeId bounds nextNodeId remaining recur
      TokSym SymAmp =>
        succT $
          parseReferenceType typeNodeId bounds nextNodeId remaining recur
      TokIdent name =>
        succT $ parsePathType typeNodeId name bounds nextNodeId remaining recur
      TokKw keyword =>
        case storageQualifierFromKeyword keyword of
          Just qualifier =>
            let (qualifierNodeId, afterQualifierNodeId) = reserveNodeId nextNodeId
             in succT $ parseQualifiedType typeNodeId
                  (surfaceAstNode
                    (MkAstInfo qualifierNodeId (sourceSpan bounds)) qualifier ::: [])
                  bounds afterQualifierNodeId remaining recur
          Nothing =>
            case functionEffectFromKeyword keyword of
              Just effect =>
                succT $ parseEffectFunctionType typeNodeId effect bounds
                  nextNodeId remaining recur
              Nothing =>
                case keyword of
                  KwFn =>
                    succT $ parseFunctionType typeNodeId Nothing bounds
                      nextNodeId remaining recur
                  _ => Fail0 (B (Expected ["a type declaration"] (show token)) bounds)
      _ => Fail0 (B (Expected ["a type declaration"] (show token)) bounds)

  ||| Parses the `::name` segments following the first name of a type path.
  ||| The first segment is handled by `parsePathType`; this helper consumes only
  ||| complete `:: identifier` pairs and deliberately succeeds without consuming
  ||| anything when the next token is not another segment. Recursion returns the
  ||| final segment's bounds so the caller can span the complete path. The explicit
  ||| suffix composition proves that every returned token list is a suffix of the
  ||| original input.
  ||| Tested by: `fn use_types(config: my_module::Config) {}`.
  parseTypePathTail : Rule False TypePathTail
  parseTypePathTail nodeId
      ((B (TokSym SymDoubleColon) colonBounds) ::
       (B (TokIdent name) nameBounds) :: remaining) acc =
    let (segmentNodeId, nextNodeId) = reserveNodeId nodeId
        segment = surfaceAstNode
          (MkAstInfo segmentNodeId (sourceSpan nameBounds))
          (PathSegmentName name)
     in case parseTypePathTail nextNodeId remaining suffixAcc of
          Fail0 err => Fail0 err
          Succ0 (MkTypePathTail segments lastBounds, finalNodeId)
                finalTokens @{tailSuffix} =>
            let finalBounds = case segments of
                  [] => nameBounds
                  _ => lastBounds
             in Succ0 (MkTypePathTail (segment :: segments) finalBounds, finalNodeId)
              finalTokens
              @{Data.List.Suffix.weaken $
                Data.List.Suffix.trans tailSuffix $
                Data.List.Suffix.trans
                  (the (Suffix True remaining
                    (B (TokIdent name) nameBounds :: remaining))
                    (Uncons Same))
                  (the (Suffix True
                    (B (TokIdent name) nameBounds :: remaining)
                    (B (TokSym SymDoubleColon) colonBounds ::
                     B (TokIdent name) nameBounds :: remaining))
                    (Uncons Same))}
  parseTypePathTail nodeId tokens _ =
    Succ0 (MkTypePathTail [] NoBounds, nodeId) tokens @{Same}

  ||| Builds a named or qualified path type from its first identifier and tail.
  ||| Tested by: `fn use_types(person: Person, config: my_module::Config) {}`.
  parsePathType :
       NodeId -> String -> Bounds -> Rule False SurfaceTy
  parsePathType typeNodeId firstName firstBounds nodeId tokens acc =
    let (pathNodeId, afterPathNodeId) = reserveNodeId nodeId
        (segmentNodeId, afterSegmentNodeId) = reserveNodeId afterPathNodeId
        firstSegment = surfaceAstNode
          (MkAstInfo segmentNodeId (sourceSpan firstBounds))
          (PathSegmentName firstName)
     in case parseTypePathTail afterSegmentNodeId tokens acc of
          Fail0 err => Fail0 err
          Succ0 (MkTypePathTail segments lastBounds, finalNodeId)
                finalTokens @{tailSuffix} =>
            let pathBounds = case segments of
                  [] => firstBounds
                  _ => firstBounds <+> lastBounds
                path = surfaceAstNode
                  (MkAstInfo pathNodeId (sourceSpan pathBounds))
                  (MkPathNode firstSegment segments)
                ty = surfaceAstNode
                  (MkAstInfo typeNodeId (sourceSpan pathBounds))
                  (TyPath path)
             in Succ0 (ty, finalNodeId) finalTokens @{tailSuffix}

  ||| Parses shared and mutable reference types beginning with `&`.
  ||| Tested by: `fn borrow(person: &Person, mutable: &mut Person) {}`.
  parseReferenceType : NodeId -> Bounds -> Rule True SurfaceTy
  parseReferenceType typeNodeId ampBounds nodeId
      ((B (TokKw KwMut) mutBounds) :: remaining) (SA recur) =
    let (borrowNodeId, afterBorrowNodeId) = reserveNodeId nodeId
        borrowBounds = ampBounds <+> mutBounds
        borrow = surfaceAstNode
          (MkAstInfo borrowNodeId (sourceSpan borrowBounds)) MutableBorrow
     in case assert_total $ parseType afterBorrowNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (inner, finalNodeId) finalTokens @{innerSuffix} =>
            let ty = surfaceAstNode
                  (MkAstInfo typeNodeId
                    (mergeSpans (sourceSpan ampBounds) inner.astInfo.span))
                  (TyReference borrow inner)
             in succT $ Succ0 (ty, finalNodeId) finalTokens @{innerSuffix}
  parseReferenceType typeNodeId ampBounds nodeId tokens acc =
    let (borrowNodeId, afterBorrowNodeId) = reserveNodeId nodeId
        borrow = surfaceAstNode
          (MkAstInfo borrowNodeId (sourceSpan ampBounds)) SharedBorrow
     in case assert_total $ parseType afterBorrowNodeId tokens acc of
          Fail0 err => Fail0 err
          Succ0 (inner, finalNodeId) finalTokens @{innerSuffix} =>
            let ty = surfaceAstNode
                  (MkAstInfo typeNodeId
                    (mergeSpans (sourceSpan ampBounds) inner.astInfo.span))
                  (TyReference borrow inner)
             in Succ0 (ty, finalNodeId) finalTokens @{innerSuffix}

  ||| Collects consecutive quantum-storage qualifiers and parses their inner type.
  ||| Tested by: `fn qualified(q: affine qubit, pair: (scratch linear qubit, affine qubit)) {}`.
  parseMoreTypeQualifiers :
       NodeId
    -> List1 (SurfaceAstNode QuantumStorageQualifier)
    -> Bounds
    -> Rule True SurfaceTy
  parseMoreTypeQualifiers typeNodeId qualifiers firstBounds nodeId
      ((B (TokKw keyword) bounds) :: remaining) (SA recur) =
    case storageQualifierFromKeyword keyword of
      Just qualifier =>
        let (qualifierNodeId, nextNodeId) = reserveNodeId nodeId
            located = surfaceAstNode
              (MkAstInfo qualifierNodeId (sourceSpan bounds)) qualifier
         in succT $ parseMoreTypeQualifiers typeNodeId
              (snocList1 qualifiers located) firstBounds nextNodeId remaining recur
      Nothing =>
        case assert_total $
               parseType nodeId (B (TokKw keyword) bounds :: remaining) (SA recur) of
          Fail0 err => Fail0 err
          Succ0 (inner, finalNodeId) finalTokens @{innerSuffix} =>
            Succ0
              (surfaceAstNode
                (MkAstInfo typeNodeId
                  (mergeSpans (sourceSpan firstBounds) inner.astInfo.span))
                (TyQualified qualifiers inner),
               finalNodeId)
              finalTokens @{innerSuffix}
  parseMoreTypeQualifiers typeNodeId qualifiers firstBounds nodeId tokens acc =
    case assert_total $ parseType nodeId tokens acc of
      Fail0 err => Fail0 err
      Succ0 (inner, finalNodeId) finalTokens @{innerSuffix} =>
        Succ0
          (surfaceAstNode
            (MkAstInfo typeNodeId
              (mergeSpans (sourceSpan firstBounds) inner.astInfo.span))
            (TyQualified qualifiers inner),
           finalNodeId)
          finalTokens @{innerSuffix}

  ||| Enters qualified-type parsing with the qualifier already consumed.
  ||| Tested by: `fn qualified(q: affine qubit) {}`.
  parseQualifiedType :
       NodeId
    -> List1 (SurfaceAstNode QuantumStorageQualifier)
    -> Bounds
    -> Rule True SurfaceTy
  parseQualifiedType = parseMoreTypeQualifiers

  ||| Parses an effect-qualified function type, requiring `fn` after the effect.
  ||| Tested by:
  ||| `general fn phase_kickback(oracle: unitary fn(qs: [qubit; 4], target: qubit) -> ([qubit; 4], qubit)) {}`.
  parseEffectFunctionType :
       NodeId -> FunctionEffect -> Bounds -> Rule True SurfaceTy
  parseEffectFunctionType typeNodeId effect effectBounds nodeId
      ((B (TokKw KwFn) fnBounds) :: remaining) (SA recur) =
    let (effectNodeId, nextNodeId) = reserveNodeId nodeId
        locatedEffect = surfaceAstNode
          (MkAstInfo effectNodeId (sourceSpan effectBounds)) effect
     in succT $ parseFunctionType typeNodeId (Just locatedEffect)
          (effectBounds <+> fnBounds) nextNodeId remaining recur
  parseEffectFunctionType _ effect _ _ ((B token bounds) :: _) _ =
    Fail0 (B (Expected ["`fn` after `" ++ show effect ++ "`"] (show token)) bounds)
  parseEffectFunctionType _ _ _ _ [] _ = Fail0 (B EOI NoBounds)

  ||| Parses one named and typed parameter inside a function type.
  ||| Tested by: `fn callback(f: fn(value: i32)) {}`.
  parseFunctionTypeParameter : Rule True
      (SurfaceAstNode (FunctionTypeParameterNode SurfaceExpr))
  parseFunctionTypeParameter nodeId tokens acc =
    let (parameterNodeId, afterParameterNodeId) = reserveNodeId nodeId in
    case parseName "function type parameter name" afterParameterNodeId tokens acc of
      Fail0 err => Fail0 err
      Succ0 (name, afterNameNodeId) afterName @{nameSuffix} =>
        case afterName of
          (B (TokSym SymColon) colonBounds :: afterColon) =>
            case assert_total $ parseType afterNameNodeId afterColon suffixAcc of
              Fail0 err => Fail0 err
              Succ0 (parameterType, finalNodeId) finalTokens @{typeSuffix} =>
                let parameter = surfaceAstNode
                      (MkAstInfo parameterNodeId
                        (mergeSpans name.astInfo.span parameterType.astInfo.span))
                      (MkFunctionTypeParameterNode name parameterType)
                 in Succ0 (parameter, finalNodeId) finalTokens
                      @{Data.List.Suffix.trans typeSuffix $
                        Data.List.Suffix.trans
                          (the (Suffix True afterColon
                            (B (TokSym SymColon) colonBounds :: afterColon))
                            (Uncons Same))
                          nameSuffix}
          [] => Fail0 (B EOI NoBounds)
          (B unexpected unexpectedBounds :: _) =>
            Fail0 (B (Expected [":"] (show unexpected)) unexpectedBounds)

  ||| Parses the comma-separated parameter list and closing parenthesis of a function type.
  ||| Parameters accumulate in a `SnocList` so source order is preserved without
  ||| repeatedly appending to an ordinary list. The closing `)` belongs to this
  ||| helper: it is consumed here, while its bounds are retained for the function
  ||| type's enclosing source span. A comma commits the parser to another parameter.
  ||| Tested by: `fn callback(f: fn(value: i32)) {}`.
  parseFunctionTypeParameterList :
       SnocList (SurfaceAstNode (FunctionTypeParameterNode SurfaceExpr))
    -> Rule True FunctionTypeParameters
  parseFunctionTypeParameterList parsed _ [] _ = Fail0 (B EOI NoBounds)
  parseFunctionTypeParameterList parsed nodeId
      ((B (TokSym SymRParen) closeBounds) :: remaining) _ =
    Succ0 (MkFunctionTypeParameters (parsed <>> []) closeBounds, nodeId) remaining
  parseFunctionTypeParameterList parsed nodeId tokens acc =
    case parseFunctionTypeParameter nodeId tokens acc of
      Fail0 err => Fail0 err
      Succ0 (parameter, nextNodeId) afterParameter @{parameterSuffix} =>
        case afterParameter of
          (B (TokSym SymComma) commaBounds :: afterComma) =>
            case assert_total $ parseFunctionTypeParameterList (parsed :< parameter)
                   nextNodeId afterComma suffixAcc of
              Fail0 err => Fail0 err
              Succ0 result finalTokens @{restSuffix} =>
                Succ0 result finalTokens
                  @{Data.List.Suffix.trans restSuffix $
                    Data.List.Suffix.trans
                      (the (Suffix True afterComma
                        (B (TokSym SymComma) commaBounds :: afterComma))
                        (Uncons Same))
                      parameterSuffix}
          (B (TokSym SymRParen) closeBounds :: remaining) =>
            Succ0
              (MkFunctionTypeParameters (parsed <>> [parameter]) closeBounds,
               nextNodeId)
              remaining
              @{Data.List.Suffix.trans
                  (the (Suffix True remaining
                    (B (TokSym SymRParen) closeBounds :: remaining))
                    (Uncons Same))
                  parameterSuffix}
          [] => Fail0 (B EOI NoBounds)
          (B unexpected unexpectedBounds :: _) =>
            Fail0 (B (Expected [",", ")"] (show unexpected)) unexpectedBounds)

  ||| Parses a function type, its optional effect, parameters, and optional return type.
  ||| Tested by: `fn callback(f: fn(value: i32)) {}`.
  parseFunctionType :
       NodeId
    -> Maybe (SurfaceAstNode FunctionEffect)
    -> Bounds
    -> Rule True SurfaceTy
  parseFunctionType _ _ _ _ [] _ = Fail0 (B EOI NoBounds)
  parseFunctionType typeNodeId effect startBounds nodeId
      ((B (TokSym SymLParen) _ :: remaining)) (SA recur) =
    case parseFunctionTypeParameterList [<] nodeId remaining recur of
      Fail0 err => Fail0 err
      Succ0 (MkFunctionTypeParameters functionParams closeBounds, afterParamsNodeId)
            afterParams @{paramsSuffix} =>
        case afterParams of
          (B (TokSym SymArrow) arrowBounds :: afterArrow) =>
            case assert_total $ parseType afterParamsNodeId afterArrow suffixAcc of
              Fail0 err => Fail0 err
              Succ0 (returnType, finalNodeId) finalTokens @{returnSuffix} =>
                let ty = surfaceAstNode
                      (MkAstInfo typeNodeId
                        (mergeSpans (sourceSpan startBounds) returnType.astInfo.span))
                      (TyFunction effect functionParams (Just returnType))
                 in succT $ Succ0 (ty, finalNodeId) finalTokens
                      @{Data.List.Suffix.trans
                          (Data.List.Suffix.trans returnSuffix
                            (the (Suffix True afterArrow
                              (B (TokSym SymArrow) arrowBounds :: afterArrow))
                              (Uncons Same)))
                          paramsSuffix}
          _ =>
            let ty = surfaceAstNode
                  (MkAstInfo typeNodeId
                    (sourceSpan (startBounds <+> closeBounds)))
                  (TyFunction effect functionParams Nothing)
             in succT $ Succ0 (ty, afterParamsNodeId) afterParams @{paramsSuffix}
  parseFunctionType _ _ _ _ ((B token bounds) :: _) _ =
    Fail0 (B (Expected ["`(` after `fn`"] (show token)) bounds)

  ||| Parses slice types `[T]` and fixed-length array types `[T; expression]`.
  ||| Tested by: `fn arrays() { let b: [i32; 2 + 2]; }`.
  parseArrayType : NodeId -> Bounds -> Rule True SurfaceTy
  parseArrayType _ _ _ [] _ = Fail0 (B EOI NoBounds)
  parseArrayType arrayNodeId openBounds nodeId tokens acc =
    case assert_total $ parseType nodeId tokens acc of
      Fail0 err => Fail0 err
      Succ0 (elementType, afterElementNodeId) afterElement @{elementSuffix} =>
        case afterElement of
          [] => Fail0 (B EOI NoBounds)
          (B (TokSym SymRBracket) closeBounds) :: finalTokens =>
            let sliceType = surfaceAstNode
                  (MkAstInfo arrayNodeId
                    (sourceSpan (openBounds <+> closeBounds)))
                  (TySlice elementType)
             in Succ0 (sliceType, afterElementNodeId) finalTokens
                  @{Data.List.Suffix.trans
                      (the (Suffix True finalTokens
                        (B (TokSym SymRBracket) closeBounds :: finalTokens))
                        (Uncons Same))
                      elementSuffix}
          _ :: _ =>
            case (exact (TokSym SymSemi) *>
                  Text.Parse.Manual.acc
                    (assert_total $ parseExpression afterElementNodeId)) afterElement of
              Fail0 err => Fail0 err
              Succ0 (length, finalNodeId) afterLength @{lengthSuffix} =>
                case afterLength of
                  [] => Fail0 (B EOI NoBounds)
                  (B (TokSym SymRBracket) closeBounds) :: finalTokens =>
                    let arrayType = surfaceAstNode
                          (MkAstInfo arrayNodeId
                            (sourceSpan (openBounds <+> closeBounds)))
                          (TyArray elementType length)
                     in Succ0 (arrayType, finalNodeId) finalTokens
                          @{Data.List.Suffix.trans
                              (the (Suffix True finalTokens
                                      (B (TokSym SymRBracket) closeBounds :: finalTokens))
                                   (Uncons Same))
                              (Data.List.Suffix.trans lengthSuffix elementSuffix)}
                  (B unexpected unexpectedBounds) :: _ =>
                    Fail0 (B (Expected ["]"] (show unexpected)) unexpectedBounds)

  ||| Parses unit, parenthesized, and tuple types beginning with `(`.
  ||| Tested by: `fn add(point: (i32, i32)) {}`.
  parseParenType : NodeId -> Bounds -> Rule True SurfaceTy
  parseParenType _ _ _ [] _ = Fail0 (B EOI NoBounds)
  parseParenType typeNodeId openBounds nodeId
      ((B (TokSym SymRParen) closeBounds) :: remaining) _ =
    Succ0 (surfaceAstNode
             (MkAstInfo typeNodeId (sourceSpan (openBounds <+> closeBounds))) TyUnit,
           nodeId) remaining
  parseParenType typeNodeId openBounds nodeId tokens acc =
    case assert_total $ parseType nodeId tokens acc of
      Fail0 err => Fail0 err
      Succ0 (firstType, afterFirstNodeId) afterFirst @{firstSuffix} =>
        case afterFirst of
          [] => Fail0 (B EOI NoBounds)
          (B (TokSym SymRParen) closeBounds) :: remaining =>
            let ty = surfaceAstNode
                       (MkAstInfo typeNodeId (sourceSpan (openBounds <+> closeBounds)))
                       (TyParenthesized firstType)
             in Succ0 (ty, afterFirstNodeId) remaining
                  @{Data.List.Suffix.trans
                      (the (Suffix True remaining
                              (B (TokSym SymRParen) closeBounds :: remaining))
                           (Uncons Same))
                      firstSuffix}
          (B (TokSym SymComma) commaBounds) :: afterComma =>
            case parseTupleTail afterFirstNodeId afterComma suffixAcc of
              Fail0 err => Fail0 err
              Succ0 (tail, finalNodeId) finalTokens @{tailSuffix} =>
                let ty = surfaceAstNode
                           (MkAstInfo typeNodeId
                             (sourceSpan (openBounds <+> tail.closingBounds)))
                           (TyTuple (firstType ::: tail.elementTypes))
                 in Succ0 (ty, finalNodeId) finalTokens
                      @{Data.List.Suffix.trans tailSuffix $
                        Data.List.Suffix.trans
                          (the (Suffix True afterComma
                                  (B (TokSym SymComma) commaBounds :: afterComma))
                               (Uncons Same))
                          firstSuffix}
          (B unexpected unexpectedBounds) :: _ =>
            Fail0 (B (Expected [",", ")"] (show unexpected)) unexpectedBounds)

  ||| Parses the remaining comma-separated elements and closing `)` of a tuple type.
  ||| This helper starts after the tuple's first type and therefore also handles the
  ||| trailing-comma case. It consumes the closing delimiter and returns its bounds
  ||| separately, allowing `parseParenType` to distinguish grouping from tuple
  ||| syntax and to span the complete tuple.
  ||| Tested by: `fn qualified(pair: (scratch linear qubit, affine qubit)) {}`.
  parseTupleTail : Rule True TupleTail
  parseTupleTail _ [] _ = Fail0 (B EOI NoBounds)
  parseTupleTail nodeId ((B token bounds) :: remaining) acc@(SA recur) =
    case token of
      TokSym SymRParen => Succ0 (MkTupleTail [] bounds, nodeId) remaining
      _ =>
        case assert_total $ parseType nodeId (B token bounds :: remaining) acc of
          Fail0 err => Fail0 err
          Succ0 (elementType, nextNodeId) afterElement @{elementSuffix} =>
            case afterElement of
              [] => Fail0 (B EOI NoBounds)
              (B (TokSym SymRParen) closeBounds) :: finalTokens =>
                Succ0 (MkTupleTail [elementType] closeBounds, nextNodeId) finalTokens
                  @{Data.List.Suffix.trans
                      (the (Suffix True finalTokens
                              (B (TokSym SymRParen) closeBounds :: finalTokens))
                           (Uncons Same))
                      elementSuffix}
              (B (TokSym SymComma) commaBounds) :: afterComma =>
                case assert_total $ parseTupleTail nextNodeId afterComma suffixAcc of
                  Fail0 err => Fail0 err
                  Succ0 (tail, finalNodeId) finalTokens @{tailSuffix} =>
                    Succ0 (MkTupleTail (elementType :: tail.elementTypes)
                                             tail.closingBounds,
                           finalNodeId) finalTokens
                      @{Data.List.Suffix.trans tailSuffix $
                        Data.List.Suffix.trans
                          (the (Suffix True afterComma
                                  (B (TokSym SymComma) commaBounds :: afterComma))
                               (Uncons Same))
                          elementSuffix}
              (B unexpected unexpectedBounds) :: _ =>
                Fail0 (B (Expected [",", ")"] (show unexpected)) unexpectedBounds)

||| Detects an outer documentation comment before a parameter and reports that
||| function outer documentation comments are not supported yet. Otherwise it
||| leaves the token stream untouched and returns no comments.
||| The related unsupported-comment test uses: `/// docs\n`.
parseParameterDocComments : Rule False (List SurfaceDocComment)
parseParameterDocComments nodeId [] _ =
    Succ0 ([], nodeId) []
parseParameterDocComments nodeId ((B token bounds) :: remaining) _ =
    case token of
        TokOuterDoc _ =>
            failWithCustomError
                (UnsupportedFeature "Function outer doc comments are not yet supported.")
                bounds
        _ =>
            Succ0 ([], nodeId) (B token bounds :: remaining)

||| Parses an optional `mut` modifier on a function parameter.
||| Tested by: `fn increment(mut x: i32) -> i32 { x += 1; x }`.
parseParameterMutability : Rule False (Maybe (SurfaceAstNode Mutability))
parseParameterMutability nodeId [] _ =
    Succ0 (Nothing, nodeId) []
parseParameterMutability nodeId ((B token bounds) :: remaining) _ =
    case token of
        TokKw KwMut =>
            let (mutabilityNodeId, nextNodeId) = reserveNodeId nodeId
                mutability = surfaceAstNode
                    (MkAstInfo mutabilityNodeId (sourceSpan bounds))
                    Mutable
             in Succ0 (Just mutability, nextNodeId) remaining
        _ =>
            Succ0 (Nothing, nodeId) (B token bounds :: remaining)

||| Parses one function parameter, including docs, mutability, name, and type.
||| Tested by: `fn increment(mut x: i32) -> i32 { x += 1; x }`.
parseFunctionParameter : Rule True (SurfaceAstNode FunctionParameterNode)
parseFunctionParameter nodeId tokens acc =
    let (parameterNodeId, nextNodeId) = reserveNodeId nodeId
    in case parseParameterDocComments nextNodeId tokens acc of
        Fail0 err => Fail0 err
        Succ0 (docs, afterDocsNodeId) afterDocs @{docsSuffix} =>
            case parseParameterMutability afterDocsNodeId afterDocs suffixAcc of
                Fail0 err => Fail0 err
                Succ0 (mutability, afterMutabilityNodeId)
                      afterMutability @{mutabilitySuffix} =>
                    case parseName
                           "parameter name" afterMutabilityNodeId afterMutability suffixAcc of
                        Fail0 err => Fail0 err
                        Succ0 (name, afterNameNodeId) afterName @{nameSuffix} =>
                            case afterName of
                                [] => Fail0 (B EOI NoBounds)
                                _ :: _ =>
                                    case (exact (TokSym SymColon) *>
                                          Text.Parse.Manual.acc (parseType afterNameNodeId)) afterName of
                                        Fail0 err => Fail0 err
                                        Succ0 (parameterType, finalNodeId)
                                              finalTokens @{typeSuffix} =>
                                            let parameterSpan =
                                                    mergeSpans
                                                        (parameterStartSpan docs mutability name)
                                                        parameterType.astInfo.span
                                                parameter =
                                                    surfaceAstNode
                                                        (MkAstInfo
                                                            parameterNodeId
                                                            parameterSpan)
                                                        (NormalParameter
                                                            docs
                                                            mutability
                                                            name
                                                            parameterType)
                                             in Succ0
                                                    (parameter, finalNodeId)
                                                    finalTokens
                                                    @{Data.List.Suffix.trans typeSuffix $
                                                      Data.List.Suffix.trans nameSuffix $
                                                      Data.List.Suffix.trans mutabilitySuffix $
                                                      docsSuffix}

||| Parses comma-separated function parameters until the closing `)`.
||| Parsed parameters accumulate in a `SnocList`, then become an ordinary list only
||| at the delimiter. The helper consumes both commas and the closing parenthesis;
||| after a comma, failure to parse another parameter is an error rather than an
||| empty-list success.
||| Tested by: `fn add(i: i32, point: (i32, i32)) {}`.
parseFunctionParameterList : 
    SnocList (SurfaceAstNode FunctionParameterNode) ->
    Rule False (List (SurfaceAstNode FunctionParameterNode))
parseFunctionParameterList parsed nodeId [] _ = Fail0 (B EOI NoBounds)
parseFunctionParameterList parsed nodeId
    ((B token bounds) :: remaining) acc@(SA recur) =
    case token of
        TokSym SymRParen =>
            Succ0 (parsed <>> [], nodeId) remaining
        _ =>
            case parseFunctionParameter
                    nodeId (B token bounds :: remaining) acc of
                Fail0 err => Fail0 err
                Succ0 (parameter, nextNodeId) afterParameter =>
                    case afterParameter of
                        [] => 
                            Fail0 (B EOI NoBounds)

                        (B (TokSym SymComma) _) :: afterComma =>
                            succF $
                                parseFunctionParameterList
                                    (parsed :< parameter)
                                    nextNodeId
                                    afterComma
                                    recur

                        (B (TokSym SymRParen) closeBounds) :: afterClose =>
                            Succ0
                                (parsed <>> [parameter], nextNodeId)
                                afterClose

                        (B unexpected unexpectedBounds) :: _ =>
                            Fail0
                                (B
                                    (Expected [",", ")"] (show unexpected))
                                    unexpectedBounds)

||| Parses a function declaration's parenthesized parameter list.
||| Tested by: `fn add(i: i32, point: (i32, i32)) {}`.
parseFunctionParameters : Rule True (List (SurfaceAstNode FunctionParameterNode))
parseFunctionParameters _ [] _ = Fail0 (B EOI NoBounds)
parseFunctionParameters nodeId tokens@(_ :: _) _ =
    (exact (TokSym SymLParen) *> acc (parseFunctionParameterList [<] nodeId)) tokens

||| Parses an optional `-> type` return annotation.
||| Tested by: `fn empty() -> () {}`.
parseOptionalReturnType : Rule False (Maybe SurfaceTy)
parseOptionalReturnType nodeId [] _ =
    Succ0 (Nothing, nodeId) []
parseOptionalReturnType nodeId
    ((B token bounds) :: remaining) acc@(SA recur) =
    case token of
        TokSym SymArrow =>
            case parseType nodeId remaining recur of
                Fail0 err => Fail0 err
                Succ0 (returnType, nextNodeId) finalTokens @{typeSuffix} =>
                    Succ0
                        (Just returnType, nextNodeId)
                        finalTokens
                        @{Data.List.Suffix.weaken $
                          Data.List.Suffix.trans typeSuffix $
                          the
                            (Suffix True
                              remaining
                              (B (TokSym SymArrow) bounds :: remaining))
                            (Uncons Same)}
        _ =>
            Succ0
                (Nothing, nodeId)
                (B token bounds :: remaining)

||| Detects an optional `supports` clause and reports it as unsupported. When no
||| clause is present, it leaves the token stream untouched and returns an empty list.
||| The lexer test supplying this syntax uses:
||| `unitary fn f(q: &qubit) supports adjoint, ctrl {}`.
parseOptionalSupportClause : Rule False (List (SurfaceAstNode SupportKind))
parseOptionalSupportClause nodeId [] _ =
    Succ0 ([], nodeId) []
parseOptionalSupportClause nodeId ((B token bounds) :: remaining) _ =
    case token of
        TokKw KwSupports =>
            failWithCustomError
                (UnsupportedFeature "Function 'supports' clauses are not yet supported.")
                bounds

        _ =>
            Succ0 ([], nodeId) (B token bounds :: remaining)

||| Detects a leading `requires` or `ensures` contract clause and reports quantum
||| contracts as unsupported. Otherwise it leaves the tokens untouched.
||| The lexer test supplying these clauses uses:
||| `requires clean(q) ensures basis(q, X)`.
parseContractClauses : Rule False (List SurfaceContractClause)
parseContractClauses nodeId [] _ =
    Succ0 ([], nodeId) []
parseContractClauses nodeId ((B token bounds) :: remaining) _ =
    case token of
        TokKw KwRequires => unsupportedContract bounds
        TokKw KwEnsures  => unsupportedContract bounds
        _ => Succ0 ([], nodeId) (B token bounds :: remaining)
  where
    unsupportedContract : Bounds ->
                          Res False Token tokens CustomParseError
                              (List SurfaceContractClause, Nat)
    unsupportedContract bounds =
        failWithCustomError
            (UnsupportedFeature
                "Quantum contracts 'requires' and/or 'ensures' are not yet supported.")
            bounds

||| Collects quantum-storage qualifiers preceding a `let` pattern.
||| Tested by: `fn allocate() {let scratch linear qs: [qubit; 2] = qalloc(2);}`.
parseLetQualifiers :
     SnocList (SurfaceAstNode QuantumStorageQualifier)
  -> Rule False (List (SurfaceAstNode QuantumStorageQualifier))
parseLetQualifiers qualifiers nodeId
    ((B (TokKw keyword) qualifierBounds) :: remaining) (SA recur) =
  case keyword of
    KwLinear  => consume QualifierLinear
    KwAffine  => consume QualifierAffine
    KwScratch => consume QualifierScratch
    _ => Succ0 (qualifiers <>> [], nodeId)
           (B (TokKw keyword) qualifierBounds :: remaining) @{Same}
  where
    consume :
         QuantumStorageQualifier
      -> Res False Token
           (B (TokKw keyword) qualifierBounds :: remaining)
           CustomParseError
           (List (SurfaceAstNode QuantumStorageQualifier), Nat)
    consume qualifier =
      let (qualifierNodeId, nextNodeId) = reserveNodeId nodeId
          located = surfaceAstNode
            (MkAstInfo qualifierNodeId (sourceSpan qualifierBounds)) qualifier
       in succF $
            parseLetQualifiers (qualifiers :< located) nextNodeId remaining recur
parseLetQualifiers qualifiers nodeId tokens _ =
  Succ0 (qualifiers <>> [], nodeId) tokens @{Same}

mutual
  ||| Parses name, wildcard, tuple, and array patterns used by `let`.
  ||| Tested by:
  ||| `fn destructure() {let (a, b, c) = (1, 2, 3); let (x, _, z) = (1, 2, 3);}`.
  parsePattern : Rule True SurfacePattern
  parsePattern _ [] _ = Fail0 (B EOI NoBounds)
  parsePattern nodeId ((B TokUnderscore bounds) :: remaining) _ =
    let (patternNodeId, nextNodeId) = reserveNodeId nodeId
     in Succ0
          (surfaceAstNode (MkAstInfo patternNodeId (sourceSpan bounds))
            PatternWildcard,
           nextNodeId)
          remaining
  parsePattern nodeId ((B (TokIdent text) bounds) :: remaining) _ =
    let (patternNodeId, afterPatternNodeId) = reserveNodeId nodeId
        (nameNodeId, nextNodeId) = reserveNodeId afterPatternNodeId
        name = surfaceAstNode (MkAstInfo nameNodeId (sourceSpan bounds))
                              (MkNameNode text)
     in Succ0
          (surfaceAstNode (MkAstInfo patternNodeId (sourceSpan bounds))
            (PatternName Nothing name),
           nextNodeId)
          remaining
  parsePattern nodeId
      ((B (TokKw KwMut) mutBounds) ::
       (B (TokIdent text) nameBounds) :: remaining) _ =
    let (patternNodeId, afterPatternNodeId) = reserveNodeId nodeId
        (nameNodeId, nextNodeId) = reserveNodeId afterPatternNodeId
        name = surfaceAstNode (MkAstInfo nameNodeId (sourceSpan nameBounds))
                              (MkNameNode text)
     in Succ0
          (surfaceAstNode
            (MkAstInfo patternNodeId (sourceSpan (mutBounds <+> nameBounds)))
            (PatternName (Just Mutable) name),
           nextNodeId)
          remaining
  parsePattern _ ((B (TokKw KwMut) mutBounds) :: _) _ =
    failWithCustomError
      (ParseErrorWithMessage "`mut` must be followed by a binder name.")
      mutBounds
  parsePattern nodeId
      ((B (TokSym SymLParen) openBounds) ::
       (B (TokSym SymRParen) closeBounds) :: remaining) _ =
    let (patternNodeId, afterPatternNodeId) = reserveNodeId nodeId
        (literalNodeId, nextNodeId) = reserveNodeId afterPatternNodeId
        span = sourceSpan (openBounds <+> closeBounds)
        literal = surfaceAstNode (MkAstInfo literalNodeId span) LiteralUnit
     in Succ0
          (surfaceAstNode (MkAstInfo patternNodeId span)
            (PatternLiteral literal),
           nextNodeId)
          remaining
  parsePattern nodeId
      ((B (TokSym SymLParen) openBounds) :: remaining) (SA recur) =
    let (patternNodeId, afterPatternNodeId) = reserveNodeId nodeId
     in case parsePattern afterPatternNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (first, afterFirstNodeId) afterFirst @{firstSuffix} =>
            case afterFirst of
              (B (TokSym SymRParen) closeBounds) :: finalTokens =>
                Succ0
                  (surfaceAstNode
                    (MkAstInfo patternNodeId
                      (sourceSpan (openBounds <+> closeBounds)))
                    (PatternParenthesized first),
                   afterFirstNodeId)
                  finalTokens
                    @{Data.List.Suffix.trans
                        (Data.List.Suffix.trans
                          (the (Suffix True finalTokens
                                  (B (TokSym SymRParen) closeBounds :: finalTokens))
                               (Uncons Same))
                          firstSuffix)
                        (the (Suffix True remaining
                                (B (TokSym SymLParen) openBounds :: remaining))
                             (Uncons Same))}
              (B (TokSym SymComma) commaBounds) :: afterComma =>
                case assert_total $
                           parseTuplePatternTail afterFirstNodeId afterComma suffixAcc of
                  Fail0 err => Fail0 err
                  Succ0 (tail, finalNodeId) finalTokens @{tailSuffix} =>
                    Succ0
                      (surfaceAstNode
                        (MkAstInfo patternNodeId
                          (sourceSpan (openBounds <+> tail.patternCloseBounds)))
                        (PatternTuple (first ::: tail.patternTailValues)),
                       finalNodeId)
                      finalTokens
                        @{Data.List.Suffix.trans
                            (Data.List.Suffix.trans
                              (Data.List.Suffix.trans tailSuffix
                                (the (Suffix True afterComma
                                        (B (TokSym SymComma) commaBounds :: afterComma))
                                     (Uncons Same)))
                              firstSuffix)
                            (the (Suffix True remaining
                                    (B (TokSym SymLParen) openBounds :: remaining))
                                 (Uncons Same))}
              (B unexpected bounds) :: _ =>
                Fail0 (B (Expected [",", ")"] (show unexpected)) bounds)
              [] => Fail0 (B EOI NoBounds)
  parsePattern nodeId
      ((B (TokSym SymLBracket) openBounds) :: remaining) (SA recur) =
    let (patternNodeId, afterPatternNodeId) = reserveNodeId nodeId
     in case parseArrayPatternElements afterPatternNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (elements, finalNodeId) finalTokens @{elementsSuffix} =>
            Succ0
              (surfaceAstNode
                (MkAstInfo patternNodeId
                  (sourceSpan (openBounds <+> elements.patternCloseBounds)))
                (PatternArray elements.patternTailValues),
               finalNodeId)
              finalTokens
                @{Data.List.Suffix.trans elementsSuffix
                    (the (Suffix True remaining
                            (B (TokSym SymLBracket) openBounds :: remaining))
                         (Uncons Same))}
  parsePattern _ ((B token bounds) :: _) _ =
    Fail0 (B (Expected ["a pattern"] (show token)) bounds)

  ||| Parses the remaining elements and closing `)` of a tuple pattern.
  ||| Tested by: `fn destructure() {let (a, b, c) = (1, 2, 3);}`.
  parseTuplePatternTail : Rule True PatternTail
  parseTuplePatternTail _ [] _ = Fail0 (B EOI NoBounds)
  parseTuplePatternTail nodeId
      ((B (TokSym SymRParen) closeBounds) :: remaining) _ =
    Succ0 (MkPatternTail [] closeBounds, nodeId) remaining
  parseTuplePatternTail nodeId tokens acc =
    case assert_total $ parsePattern nodeId tokens acc of
      Fail0 err => Fail0 err
      Succ0 (pattern, afterPatternNodeId) afterPattern @{patternSuffix} =>
        case afterPattern of
          (B (TokSym SymRParen) closeBounds) :: finalTokens =>
            Succ0 (MkPatternTail [pattern] closeBounds, afterPatternNodeId)
              finalTokens
                @{Data.List.Suffix.trans
                    (the (Suffix True finalTokens
                            (B (TokSym SymRParen) closeBounds :: finalTokens))
                         (Uncons Same))
                    patternSuffix}
          (B (TokSym SymComma) commaBounds) :: afterComma =>
            case assert_total $
                       parseTuplePatternTail afterPatternNodeId afterComma suffixAcc of
              Fail0 err => Fail0 err
              Succ0 (tail, finalNodeId) finalTokens @{tailSuffix} =>
                Succ0
                  (MkPatternTail (pattern :: tail.patternTailValues)
                    tail.patternCloseBounds,
                   finalNodeId)
                  finalTokens
                    @{Data.List.Suffix.trans
                        (Data.List.Suffix.trans tailSuffix
                          (the (Suffix True afterComma
                                  (B (TokSym SymComma) commaBounds :: afterComma))
                               (Uncons Same)))
                        patternSuffix}
          (B unexpected bounds) :: _ =>
            Fail0 (B (Expected [",", ")"] (show unexpected)) bounds)
          [] => Fail0 (B EOI NoBounds)

  ||| Parses the comma-separated elements and closing `]` of an array pattern.
  ||| Tested by: `fn measure() {let [b0, b1, b2]: [bit; 3] = measr(qs);}`.
  parseArrayPatternElements : Rule True PatternTail
  parseArrayPatternElements _ [] _ = Fail0 (B EOI NoBounds)
  parseArrayPatternElements nodeId
      ((B (TokSym SymRBracket) closeBounds) :: remaining) _ =
    Succ0 (MkPatternTail [] closeBounds, nodeId) remaining
  parseArrayPatternElements nodeId tokens acc =
    case assert_total $ parsePattern nodeId tokens acc of
      Fail0 err => Fail0 err
      Succ0 (pattern, afterPatternNodeId) afterPattern @{patternSuffix} =>
        case afterPattern of
          (B (TokSym SymRBracket) closeBounds) :: finalTokens =>
            Succ0 (MkPatternTail [pattern] closeBounds, afterPatternNodeId)
              finalTokens
                @{Data.List.Suffix.trans
                    (the (Suffix True finalTokens
                            (B (TokSym SymRBracket) closeBounds :: finalTokens))
                         (Uncons Same))
                    patternSuffix}
          (B (TokSym SymComma) commaBounds) :: afterComma =>
            case assert_total $
                       parseArrayPatternElements afterPatternNodeId afterComma suffixAcc of
              Fail0 err => Fail0 err
              Succ0 (tail, finalNodeId) finalTokens @{tailSuffix} =>
                Succ0
                  (MkPatternTail (pattern :: tail.patternTailValues)
                    tail.patternCloseBounds,
                   finalNodeId)
                  finalTokens
                    @{Data.List.Suffix.trans
                        (Data.List.Suffix.trans tailSuffix
                          (the (Suffix True afterComma
                                  (B (TokSym SymComma) commaBounds :: afterComma))
                               (Uncons Same)))
                        patternSuffix}
          (B unexpected bounds) :: _ =>
            Fail0 (B (Expected [",", "]"] (show unexpected)) bounds)
          [] => Fail0 (B EOI NoBounds)

||| Parses statements and an optional trailing expression until a block's closing brace.
||| A semicolon turns an expression into `StatementSemiExpression`; an expression
||| immediately followed by `}` becomes the block's result value. Block-like
||| expressions may omit a semicolon in non-final position and are stored as
||| `StatementExpression`. Assignment operators are recognized only after parsing
||| their left expression, then delegated to `parseAssignmentStatement`.
||| Expressions are entered through `parseStatementExpression`, which prevents an
||| unparenthesized statement-leading block form from absorbing a following operator
||| or postfix token. The block-like check is performed before symbol/assignment
||| dispatch so the untouched token begins the next statement.
|||
||| `blockNodeId`, the opening bounds, and accumulated statements remain stable
||| across recursion. Each recursive call advances the token suffix and threads the
||| next free node ID returned by the parser that just succeeded.
||| Tested by: `fn simple() {let i: i32 = 1;}`.
parseBlockContents :
     NodeId
  -> Bounds
  -> SnocList SurfaceStatement
  -> Rule True SurfaceBlock

||| Requires and parses the braced body of a function or block-like construct.
||| Tested by: `fn empty() {}`.
parseFunctionBody : Rule True SurfaceBlock

mutual
  parseExpression = parseRangeExpression

  ||| Parses an expression at the start of a statement.
  ||| Unparenthesized expressions with blocks are complete statements in Leaf, so
  ||| this entry point parses them directly and does not let the general postfix,
  ||| cast, binary, or range parsers consume following statement tokens.
  ||| Callable `ctrl` and `adjoint` forms are not block statements; after inspecting
  ||| their AST form, they resume through `parseExpressionContinuation`.
  parseStatementExpression : Rule True SurfaceExpr
  parseStatementExpression nodeId
      tokens@((B (TokSym SymLBrace) _) :: _) acc =
    parseBlockExpression nodeId tokens acc
  parseStatementExpression nodeId
      tokens@((B (TokKw KwIf) _) :: _) acc =
    parseIfExpression nodeId tokens acc
  parseStatementExpression nodeId
      tokens@((B (TokKw KwLoop) _) :: _) acc =
    parseLoopExpression nodeId tokens acc
  parseStatementExpression nodeId
      tokens@((B (TokKw KwWhile) _) :: _) acc =
    parseWhileExpression nodeId tokens acc
  parseStatementExpression nodeId
      tokens@((B (TokKw KwFor) _) :: _) acc =
    parseForExpression nodeId tokens acc
  parseStatementExpression nodeId
      ((B (TokBuiltin BuiltinCtrl) bounds) :: remaining) (SA recur) =
    case parseControlExpression bounds nodeId remaining recur of
      Fail0 err => Fail0 err
      Succ0 (control, afterControlNodeId) afterControl @{controlSuffix} =>
        if isBlockLikeExpression control
          then succT $ Succ0 (control, afterControlNodeId) afterControl
                 @{controlSuffix}
          else
            case parseExpressionContinuation control afterControlNodeId
                   afterControl suffixAcc of
              Fail0 err => Fail0 err
              Succ0 result finalTokens @{continuationSuffix} =>
                succT $ Succ0 result finalTokens
                  @{Data.List.Suffix.trans continuationSuffix controlSuffix}
  parseStatementExpression nodeId
      ((B (TokKw KwAdjoint) bounds) :: remaining) (SA recur) =
    case parseAdjointExpression bounds nodeId remaining recur of
      Fail0 err => Fail0 err
      Succ0 (adjoint, afterAdjointNodeId) afterAdjoint @{adjointSuffix} =>
        if isBlockLikeExpression adjoint
          then succT $ Succ0 (adjoint, afterAdjointNodeId) afterAdjoint
                 @{adjointSuffix}
          else
            case parseExpressionContinuation adjoint afterAdjointNodeId
                   afterAdjoint suffixAcc of
              Fail0 err => Fail0 err
              Succ0 result finalTokens @{continuationSuffix} =>
                succT $ Succ0 result finalTokens
                  @{Data.List.Suffix.trans continuationSuffix adjointSuffix}
  parseStatementExpression nodeId tokens acc =
    parseExpression nodeId tokens acc

  ||| Continues an expression whose primary node has already been parsed.
  ||| Postfix operations bind first, followed by casts, binary operators, and
  ||| finally a possible range. Statement parsing uses this for callable `ctrl` and
  ||| `adjoint` forms only; their block forms deliberately bypass continuation.
  parseExpressionContinuation : SurfaceExpr -> Rule False SurfaceExpr
  parseExpressionContinuation primary nodeId tokens acc =
    case assert_total $
               parsePostfixExpression primary nodeId tokens acc of
      Fail0 err => Fail0 err
      Succ0 (postfix, afterPostfixNodeId) afterPostfix @{postfixSuffix} =>
        case assert_total $
                   parseCastExpressionRest postfix afterPostfixNodeId
                     afterPostfix suffixAcc of
          Fail0 err => Fail0 err
          Succ0 (cast, afterCastNodeId) afterCast @{castSuffix} =>
            case assert_total $
                       parseBinaryExpressionRest 0 cast afterCastNodeId
                         afterCast suffixAcc of
              Fail0 err => Fail0 err
              Succ0 (binary, afterBinaryNodeId) afterBinary @{binarySuffix} =>
                case assert_total $
                           parseRangeExpressionRest binary afterBinaryNodeId
                             afterBinary suffixAcc of
                  Fail0 err => Fail0 err
                  Succ0 result finalTokens @{rangeSuffix} =>
                    Succ0 result finalTokens
                      @{Data.List.Suffix.trans rangeSuffix $
                        Data.List.Suffix.trans binarySuffix $
                        Data.List.Suffix.trans castSuffix postfixSuffix}

  ||| Parses a range expression or delegates to binary-expression parsing.
  ||| Tested by: `fn ranges() {1..5; 1..; ..5; ..=5; ..}`.
  parseRangeExpression : Rule True SurfaceExpr
  parseRangeExpression nodeId
      ((B (TokSym SymDotDot) operatorBounds) :: remaining) acc@(SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
        (operatorNodeId, afterOperatorNodeId) = reserveNodeId afterExpressionNodeId
        operator = surfaceAstNode
          (MkAstInfo operatorNodeId (sourceSpan operatorBounds)) RangeExclusive
     in case remaining of
          (B (TokSym SymSemi) _) :: _ =>
            Succ0
              (surfaceAstNode
                (MkAstInfo expressionNodeId (sourceSpan operatorBounds))
                (ExprRange Nothing operator Nothing),
               afterOperatorNodeId)
              remaining
          (B (TokSym SymComma) _) :: _ =>
            Succ0
              (surfaceAstNode
                (MkAstInfo expressionNodeId (sourceSpan operatorBounds))
                (ExprRange Nothing operator Nothing),
               afterOperatorNodeId)
              remaining
          (B (TokSym SymRParen) _) :: _ =>
            Succ0
              (surfaceAstNode
                (MkAstInfo expressionNodeId (sourceSpan operatorBounds))
                (ExprRange Nothing operator Nothing),
               afterOperatorNodeId)
              remaining
          (B (TokSym SymRBracket) _) :: _ =>
            Succ0
              (surfaceAstNode
                (MkAstInfo expressionNodeId (sourceSpan operatorBounds))
                (ExprRange Nothing operator Nothing),
               afterOperatorNodeId)
              remaining
          (B (TokSym SymRBrace) _) :: _ =>
            Succ0
              (surfaceAstNode
                (MkAstInfo expressionNodeId (sourceSpan operatorBounds))
                (ExprRange Nothing operator Nothing),
               afterOperatorNodeId)
              remaining
          _ =>
            case assert_total $
                       parseBinaryExpression 0 afterOperatorNodeId remaining recur of
              Fail0 err => Fail0 err
              Succ0 (end, finalNodeId) finalTokens @{endSuffix} =>
                let expression = surfaceAstNode
                      (MkAstInfo expressionNodeId
                        (mergeSpans (sourceSpan operatorBounds) end.astInfo.span))
                      (ExprRange Nothing operator (Just end))
                 in Succ0 (expression, finalNodeId) finalTokens
                      @{Data.List.Suffix.trans endSuffix
                          (the (Suffix True remaining
                                  (B (TokSym SymDotDot) operatorBounds :: remaining))
                               (Uncons Same))}
  parseRangeExpression nodeId
      ((B (TokSym SymDotDotEq) operatorBounds) :: remaining) (SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
        (operatorNodeId, afterOperatorNodeId) = reserveNodeId afterExpressionNodeId
        operator = surfaceAstNode
          (MkAstInfo operatorNodeId (sourceSpan operatorBounds)) RangeInclusive
     in case assert_total $
               parseBinaryExpression 0 afterOperatorNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (end, finalNodeId) finalTokens @{endSuffix} =>
            let expression = surfaceAstNode
                  (MkAstInfo expressionNodeId
                    (mergeSpans (sourceSpan operatorBounds) end.astInfo.span))
                  (ExprRange Nothing operator (Just end))
             in Succ0 (expression, finalNodeId) finalTokens
                  @{Data.List.Suffix.trans endSuffix
                      (the (Suffix True remaining
                              (B (TokSym SymDotDotEq) operatorBounds :: remaining))
                           (Uncons Same))}
  parseRangeExpression nodeId tokens acc =
    case parseBinaryExpression 0 nodeId tokens acc of
      Fail0 err => Fail0 err
      Succ0 (start, afterStartNodeId) afterStart @{startSuffix} =>
        case parseRangeExpressionRest start afterStartNodeId afterStart suffixAcc of
          Fail0 err => Fail0 err
          Succ0 result finalTokens @{rangeSuffix} =>
            Succ0 result finalTokens
              @{Data.List.Suffix.trans rangeSuffix startSuffix}

  ||| Parses an optional range operator and endpoint after an existing start expression.
  ||| The caller has already parsed the possible left endpoint. This helper consumes
  ||| `..` or `..=` and then decides whether the following token can terminate an
  ||| open range. Otherwise it parses the right endpoint at binary-expression
  ||| precedence. If no range operator is present, it returns the original expression
  ||| and token stream unchanged.
  ||| Tested by: `fn ranges() {1 + 2..3 * 4}`.
  parseRangeExpressionRest : SurfaceExpr -> Rule False SurfaceExpr
  parseRangeExpressionRest start nodeId
      ((B (TokSym SymDotDot) operatorBounds) :: remaining) (SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
        (operatorNodeId, afterOperatorNodeId) = reserveNodeId afterExpressionNodeId
        operator = surfaceAstNode
          (MkAstInfo operatorNodeId (sourceSpan operatorBounds)) RangeExclusive
     in case remaining of
          (B (TokSym SymSemi) _) :: _ =>
            finishOpenRange expressionNodeId afterOperatorNodeId operator
          (B (TokSym SymComma) _) :: _ =>
            finishOpenRange expressionNodeId afterOperatorNodeId operator
          (B (TokSym SymRParen) _) :: _ =>
            finishOpenRange expressionNodeId afterOperatorNodeId operator
          (B (TokSym SymRBracket) _) :: _ =>
            finishOpenRange expressionNodeId afterOperatorNodeId operator
          (B (TokSym SymRBrace) _) :: _ =>
            finishOpenRange expressionNodeId afterOperatorNodeId operator
          _ =>
            case assert_total $
                       parseBinaryExpression 0 afterOperatorNodeId remaining recur of
              Fail0 err => Fail0 err
              Succ0 (end, finalNodeId) finalTokens @{endSuffix} =>
                let expression = surfaceAstNode
                      (MkAstInfo expressionNodeId
                        (mergeSpans start.astInfo.span end.astInfo.span))
                      (ExprRange (Just start) operator (Just end))
                 in succF $ Succ0 (expression, finalNodeId) finalTokens @{endSuffix}
    where
      -- Builds an open-ended range after its operator has been consumed.
      -- Tested by: `fn ranges() {1..; ..}`.
      finishOpenRange :
           NodeId
        -> Nat
        -> SurfaceAstNode RangeOperator
        -> Res False Token
             (B (TokSym SymDotDot) operatorBounds :: remaining)
             CustomParseError (SurfaceExpr, Nat)
      finishOpenRange expressionNodeId afterOperatorNodeId operator =
        let expression = surfaceAstNode
              (MkAstInfo expressionNodeId
                (mergeSpans start.astInfo.span (sourceSpan operatorBounds)))
              (ExprRange (Just start) operator Nothing)
         in Succ0 (expression, afterOperatorNodeId) remaining
  parseRangeExpressionRest start nodeId
      ((B (TokSym SymDotDotEq) operatorBounds) :: remaining) (SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
        (operatorNodeId, afterOperatorNodeId) = reserveNodeId afterExpressionNodeId
        operator = surfaceAstNode
          (MkAstInfo operatorNodeId (sourceSpan operatorBounds)) RangeInclusive
     in case assert_total $
               parseBinaryExpression 0 afterOperatorNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (end, finalNodeId) finalTokens @{endSuffix} =>
            let expression = surfaceAstNode
                  (MkAstInfo expressionNodeId
                    (mergeSpans start.astInfo.span end.astInfo.span))
                  (ExprRange (Just start) operator (Just end))
             in succF $ Succ0 (expression, finalNodeId) finalTokens @{endSuffix}
  parseRangeExpressionRest start nodeId tokens _ =
    Succ0 (start, nodeId) tokens @{Same}

  ||| Parses a precedence-climbing binary expression at the requested minimum precedence.
  ||| It first parses the tighter cast/unary/postfix operand, then asks
  ||| `parseBinaryExpressionRest` to extend that operand. The minimum-precedence
  ||| parameter prevents a recursive right operand from consuming an operator that
  ||| belongs to its caller.
  ||| Tested by: `fn arithmetic() {1 + 2 * 3}`.
  parseBinaryExpression : Nat -> Rule True SurfaceExpr
  parseBinaryExpression minimumPrecedence nodeId tokens acc =
    case parseCastExpression nodeId tokens acc of
      Fail0 err => Fail0 err
      Succ0 (left, afterLeftNodeId) afterLeft @{leftSuffix} =>
        case parseBinaryExpressionRest minimumPrecedence left
               afterLeftNodeId afterLeft suffixAcc of
          Fail0 err => Fail0 err
          Succ0 result finalTokens @{restSuffix} =>
            Succ0 result finalTokens
              @{Data.List.Suffix.trans restSuffix leftSuffix}

  ||| Extends a parsed left operand with eligible binary operators and right operands.
  ||| For an operator with precedence `p`, the right side is parsed with minimum
  ||| precedence `p + 1`, which makes equal-precedence operators left-associative.
  ||| The combined node is then fed back into this helper at the caller's original
  ||| minimum. Unknown operators and operators below that minimum are intentionally
  ||| left unconsumed for an outer parser.
  ||| Tested by: `fn logic() {a & b ^ c | d && e || f}`.
  parseBinaryExpressionRest : Nat -> SurfaceExpr -> Rule False SurfaceExpr
  parseBinaryExpressionRest minimumPrecedence left nodeId
      ((B (TokSym symbol) operatorBounds) :: afterOperator) (SA recur) =
    case binaryOperator symbol of
      Nothing =>
        Succ0 (left, nodeId)
          (B (TokSym symbol) operatorBounds :: afterOperator) @{Same}
      Just (operatorValue, precedence) =>
        if precedence < minimumPrecedence
          then Succ0 (left, nodeId)
                 (B (TokSym symbol) operatorBounds :: afterOperator) @{Same}
          else if isComparisonOperator operatorValue &&
                  isUnparenthesizedComparison left
            then failWithCustomError
                   (ParseErrorWithMessage
                     "Comparison operators cannot be chained. Parenthesize one of the comparisons.")
                   operatorBounds
          else
            let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
                (operatorNodeId, afterOperatorNodeId) =
                  reserveNodeId afterExpressionNodeId
                operator = surfaceAstNode
                  (MkAstInfo operatorNodeId (sourceSpan operatorBounds)) operatorValue
             in case assert_total $
                       parseBinaryExpression (S precedence) afterOperatorNodeId
                         afterOperator recur of
                  Fail0 err => Fail0 err
                  Succ0 (right, afterRightNodeId) afterRight @{rightSuffix} =>
                    let expression = surfaceAstNode
                          (MkAstInfo expressionNodeId
                            (mergeSpans left.astInfo.span right.astInfo.span))
                          (ExprBinary operator left right)
                     in case assert_total $
                               parseBinaryExpressionRest minimumPrecedence expression
                                 afterRightNodeId afterRight suffixAcc of
                          Fail0 err => Fail0 err
                          Succ0 result finalTokens @{restSuffix} =>
                            succF $ Succ0 result finalTokens
                              @{Data.List.Suffix.trans restSuffix rightSuffix}
  parseBinaryExpressionRest _ left nodeId tokens _ =
    Succ0 (left, nodeId) tokens @{Same}

  ||| Parses a unary expression followed by zero or more `as type` casts.
  ||| Tested by: `fn casts() {x as i32; value as i32 as i64}`.
  parseCastExpression : Rule True SurfaceExpr
  parseCastExpression nodeId tokens acc =
    case parseUnaryExpression nodeId tokens acc of
      Fail0 err => Fail0 err
      Succ0 (operand, afterOperandNodeId) afterOperand @{operandSuffix} =>
        case parseCastExpressionRest operand afterOperandNodeId afterOperand suffixAcc of
          Fail0 err => Fail0 err
          Succ0 result finalTokens @{castSuffix} =>
            Succ0 result finalTokens
              @{Data.List.Suffix.trans castSuffix operandSuffix}

  ||| Repeatedly attaches `as type` casts to an existing operand.
  ||| Tested by: `fn casts() {value as i32 as i64}`.
  parseCastExpressionRest : SurfaceExpr -> Rule False SurfaceExpr
  parseCastExpressionRest operand nodeId
      ((B (TokKw KwAs) asBounds) :: afterAs) (SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
     in case parseType afterExpressionNodeId afterAs recur of
          Fail0 err => Fail0 err
          Succ0 (targetType, afterTypeNodeId) afterType @{typeSuffix} =>
            let expression = surfaceAstNode
                  (MkAstInfo expressionNodeId
                    (mergeSpans operand.astInfo.span targetType.astInfo.span))
                  (ExprCast operand targetType)
             in case assert_total $
                       parseCastExpressionRest expression afterTypeNodeId afterType suffixAcc of
                  Fail0 err => Fail0 err
                  Succ0 result finalTokens @{castSuffix} =>
                    succF $ Succ0 result finalTokens
                      @{Data.List.Suffix.trans castSuffix typeSuffix}
  parseCastExpressionRest operand nodeId tokens _ =
    Succ0 (operand, nodeId) tokens @{Same}

  ||| Parses prefix negation, logical not, shared borrow, mutable borrow, or a postfix expression.
  ||| Tested by: `fn unary() {-x; !x; &x; &mut x}`.
  parseUnaryExpression : Rule True SurfaceExpr
  parseUnaryExpression _ [] _ = Fail0 (B EOI NoBounds)
  parseUnaryExpression nodeId
      ((B (TokSym SymAmp) ampBounds) ::
       (B (TokKw KwMut) mutBounds) :: remaining) (SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
        (operatorNodeId, afterOperatorNodeId) = reserveNodeId afterExpressionNodeId
        operatorBounds = ampBounds <+> mutBounds
        operator = surfaceAstNode
          (MkAstInfo operatorNodeId (sourceSpan operatorBounds))
          (UnaryBorrow MutableBorrow)
     in case parseUnaryExpression afterOperatorNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (operand, finalNodeId) finalTokens @{operandSuffix} =>
            let expression = surfaceAstNode
                  (MkAstInfo expressionNodeId
                    (mergeSpans (sourceSpan operatorBounds) operand.astInfo.span))
                  (ExprUnary operator operand)
             in Succ0 (expression, finalNodeId) finalTokens
                  @{Data.List.Suffix.trans
                      (Data.List.Suffix.trans operandSuffix
                        (the (Suffix True remaining
                                (B (TokKw KwMut) mutBounds :: remaining))
                             (Uncons Same)))
                      (the (Suffix True
                              (B (TokKw KwMut) mutBounds :: remaining)
                              (B (TokSym SymAmp) ampBounds ::
                               B (TokKw KwMut) mutBounds :: remaining))
                           (Uncons Same))}
  parseUnaryExpression nodeId
      ((B (TokSym SymMinus) operatorBounds) :: remaining) (SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
        (operatorNodeId, afterOperatorNodeId) = reserveNodeId afterExpressionNodeId
        operator = surfaceAstNode
          (MkAstInfo operatorNodeId (sourceSpan operatorBounds)) UnaryNegate
     in case parseUnaryExpression afterOperatorNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (operand, finalNodeId) finalTokens @{operandSuffix} =>
            let expression = surfaceAstNode
                  (MkAstInfo expressionNodeId
                    (mergeSpans (sourceSpan operatorBounds) operand.astInfo.span))
                  (ExprUnary operator operand)
             in Succ0 (expression, finalNodeId) finalTokens
                  @{Data.List.Suffix.trans operandSuffix
                      (the (Suffix True remaining
                              (B (TokSym SymMinus) operatorBounds :: remaining))
                           (Uncons Same))}
  parseUnaryExpression nodeId
      ((B (TokSym SymBang) operatorBounds) :: remaining) (SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
        (operatorNodeId, afterOperatorNodeId) = reserveNodeId afterExpressionNodeId
        operator = surfaceAstNode
          (MkAstInfo operatorNodeId (sourceSpan operatorBounds)) UnaryLogicalNot
     in case parseUnaryExpression afterOperatorNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (operand, finalNodeId) finalTokens @{operandSuffix} =>
            let expression = surfaceAstNode
                  (MkAstInfo expressionNodeId
                    (mergeSpans (sourceSpan operatorBounds) operand.astInfo.span))
                  (ExprUnary operator operand)
             in Succ0 (expression, finalNodeId) finalTokens
                  @{Data.List.Suffix.trans operandSuffix
                      (the (Suffix True remaining
                              (B (TokSym SymBang) operatorBounds :: remaining))
                           (Uncons Same))}
  parseUnaryExpression nodeId
      ((B (TokSym SymAmp) operatorBounds) :: remaining) (SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
        (operatorNodeId, afterOperatorNodeId) = reserveNodeId afterExpressionNodeId
        operator = surfaceAstNode
          (MkAstInfo operatorNodeId (sourceSpan operatorBounds))
          (UnaryBorrow SharedBorrow)
     in case parseUnaryExpression afterOperatorNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (operand, finalNodeId) finalTokens @{operandSuffix} =>
            let expression = surfaceAstNode
                  (MkAstInfo expressionNodeId
                    (mergeSpans (sourceSpan operatorBounds) operand.astInfo.span))
                  (ExprUnary operator operand)
             in Succ0 (expression, finalNodeId) finalTokens
                  @{Data.List.Suffix.trans operandSuffix
                      (the (Suffix True remaining
                              (B (TokSym SymAmp) operatorBounds :: remaining))
                           (Uncons Same))}
  parseUnaryExpression nodeId tokens acc =
    case parsePrimaryExpression nodeId tokens acc of
      Fail0 err => Fail0 err
      Succ0 (primary, afterPrimaryNodeId) afterPrimary @{primarySuffix} =>
        case parsePostfixExpression primary afterPrimaryNodeId afterPrimary suffixAcc of
          Fail0 err => Fail0 err
          Succ0 result finalTokens @{postfixSuffix} =>
            Succ0 result finalTokens
              @{Data.List.Suffix.trans postfixSuffix primarySuffix}

  ||| Parses `ctrl(...)`, including its controls and optional basis clause.
  ||| Tested by: `fn f() {ctrl(&q0, &q1).on(bs"10").apply(H)(&q2)}`.
  parseControlExpression : Bounds -> Rule True SurfaceExpr
  parseControlExpression _ _ [] _ = Fail0 (B EOI NoBounds)
  parseControlExpression ctrlBounds nodeId
      ((B (TokSym SymLParen) openBounds) :: afterOpen) (SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
     in case parseCallArguments afterExpressionNodeId afterOpen recur of
          Fail0 err => Fail0 err
          Succ0 (MkCallArguments [] closeBounds, _) _ =>
            failWithCustomError
              (ParseErrorWithMessage
                "`ctrl` requires at least one control qubit.")
              (ctrlBounds <+> closeBounds)
          Succ0 (MkCallArguments (first :: rest) closeBounds, afterControlsNodeId)
                afterControls @{controlsSuffix} =>
            case parseControlAfterControls expressionNodeId ctrlBounds
                     (first ::: rest) Nothing afterControlsNodeId
                     afterControls suffixAcc of
              Fail0 err => Fail0 err
              Succ0 result finalTokens @{controlSuffix} =>
                succT $ Succ0 result finalTokens
                  @{Data.List.Suffix.trans controlSuffix controlsSuffix}
  parseControlExpression _ _ ((B token bounds) :: _) _ =
    Fail0 (B (Expected ["`(` after `ctrl`"] (show token)) bounds)

  ||| Finishes a control expression as either `.apply(callable)` or a controlled block.
  ||| At this point the controls, and possibly `.on(basis)`, have already been parsed.
  ||| `.apply(...)` parses exactly one callable inside its own parentheses and builds
  ||| an expression that can still receive ordinary postfix calls. A leading `{`
  ||| instead produces the block form. Keeping these two continuations here avoids
  ||| treating the control-list parentheses as normal call arguments.
  ||| Tested by: `fn f() {ctrl(&q0, &q1) {H(&q2);}}`.
  parseControlAfterControls :
       NodeId
    -> Bounds
    -> List1 SurfaceExpr
    -> Maybe (SurfaceAstNode String)
    -> Rule True SurfaceExpr
  parseControlAfterControls _ _ _ (Just _) _
      ((B (TokSym SymDot) _) ::
       (B (TokBuiltin BuiltinOn) onBounds) :: _) _ =
    failWithCustomError
      (ParseErrorWithMessage
        "A control expression can contain only one `.on(...)` clause.")
      onBounds
  parseControlAfterControls expressionNodeId ctrlBounds controls _
      nodeId
      ((B (TokSym SymDot) dotBounds) ::
       (B (TokBuiltin BuiltinOn) onBounds) ::
       (B (TokSym SymLParen) openBounds) ::
       (B (TokBasisStringLitRaw rawBasis) basisBounds) ::
       (B (TokSym SymRParen) closeBounds) :: remaining) acc =
    let (basisNodeId, nextNodeId) = reserveNodeId nodeId
        basis = surfaceAstNode
          (MkAstInfo basisNodeId (sourceSpan basisBounds)) rawBasis
     in case parseControlAfterControls expressionNodeId ctrlBounds controls
                 (Just basis) nextNodeId remaining suffixAcc of
          Fail0 err => Fail0 err
          Succ0 result finalTokens @{controlSuffix} =>
            Succ0 result finalTokens
              @{Data.List.Suffix.trans
                  (Data.List.Suffix.trans
                    (Data.List.Suffix.trans
                      (Data.List.Suffix.trans
                        (Data.List.Suffix.trans controlSuffix
                          (the (Suffix True remaining
                            (B (TokSym SymRParen) closeBounds :: remaining))
                            (Uncons Same)))
                        (the (Suffix True
                          (B (TokSym SymRParen) closeBounds :: remaining)
                          (B (TokBasisStringLitRaw rawBasis) basisBounds ::
                           B (TokSym SymRParen) closeBounds :: remaining))
                          (Uncons Same)))
                      (the (Suffix True
                        (B (TokBasisStringLitRaw rawBasis) basisBounds ::
                         B (TokSym SymRParen) closeBounds :: remaining)
                        (B (TokSym SymLParen) openBounds ::
                         B (TokBasisStringLitRaw rawBasis) basisBounds ::
                         B (TokSym SymRParen) closeBounds :: remaining))
                        (Uncons Same)))
                    (the (Suffix True
                      (B (TokSym SymLParen) openBounds ::
                       B (TokBasisStringLitRaw rawBasis) basisBounds ::
                       B (TokSym SymRParen) closeBounds :: remaining)
                      (B (TokBuiltin BuiltinOn) onBounds ::
                       B (TokSym SymLParen) openBounds ::
                       B (TokBasisStringLitRaw rawBasis) basisBounds ::
                       B (TokSym SymRParen) closeBounds :: remaining))
                      (Uncons Same)))
                  (the (Suffix True
                    (B (TokBuiltin BuiltinOn) onBounds ::
                     B (TokSym SymLParen) openBounds ::
                     B (TokBasisStringLitRaw rawBasis) basisBounds ::
                     B (TokSym SymRParen) closeBounds :: remaining)
                    (B (TokSym SymDot) dotBounds ::
                     B (TokBuiltin BuiltinOn) onBounds ::
                     B (TokSym SymLParen) openBounds ::
                     B (TokBasisStringLitRaw rawBasis) basisBounds ::
                     B (TokSym SymRParen) closeBounds :: remaining))
                    (Uncons Same))}
  parseControlAfterControls expressionNodeId ctrlBounds controls onBasis
      nodeId
      ((B (TokSym SymDot) dotBounds) ::
       (B (TokBuiltin BuiltinApply) applyBounds) ::
       (B (TokSym SymLParen) openBounds) :: afterOpen) (SA recur) =
    case assert_total $ parseExpression nodeId afterOpen recur of
      Fail0 err => Fail0 err
      Succ0 (callable, afterCallableNodeId) afterCallable @{callableSuffix} =>
        case afterCallable of
          (B (TokSym SymRParen) closeBounds) :: remaining =>
            let expression = surfaceAstNode
                  (MkAstInfo expressionNodeId
                    (sourceSpan (ctrlBounds <+> closeBounds)))
                  (ExprCtrl (ControlledCallable controls onBasis callable))
             in Succ0 (expression, afterCallableNodeId) remaining
                  @{Data.List.Suffix.trans
                      (Data.List.Suffix.trans
                        (Data.List.Suffix.trans
                          (Data.List.Suffix.trans
                            (the (Suffix True remaining
                              (B (TokSym SymRParen) closeBounds :: remaining))
                              (Uncons Same))
                            callableSuffix)
                          (the (Suffix True afterOpen
                            (B (TokSym SymLParen) openBounds :: afterOpen))
                            (Uncons Same)))
                        (the (Suffix True
                          (B (TokSym SymLParen) openBounds :: afterOpen)
                          (B (TokBuiltin BuiltinApply) applyBounds ::
                           B (TokSym SymLParen) openBounds :: afterOpen))
                          (Uncons Same)))
                      (the (Suffix True
                        (B (TokBuiltin BuiltinApply) applyBounds ::
                         B (TokSym SymLParen) openBounds :: afterOpen)
                        (B (TokSym SymDot) dotBounds ::
                         B (TokBuiltin BuiltinApply) applyBounds ::
                         B (TokSym SymLParen) openBounds :: afterOpen))
                        (Uncons Same))}
          (B unexpected unexpectedBounds) :: _ =>
            Fail0 (B (Expected [")"] (show unexpected)) unexpectedBounds)
          [] => Fail0 (B EOI NoBounds)
  parseControlAfterControls expressionNodeId ctrlBounds controls onBasis
      nodeId tokens@((B (TokSym SymLBrace) _) :: _) acc =
    case parseFunctionBody nodeId tokens acc of
      Fail0 err => Fail0 err
      Succ0 (body, finalNodeId) finalTokens @{bodySuffix} =>
        let expression = surfaceAstNode
              (MkAstInfo expressionNodeId
                (mergeSpans (sourceSpan ctrlBounds) body.astInfo.span))
              (ExprCtrl (ControlledBlock controls onBasis body))
         in Succ0 (expression, finalNodeId) finalTokens @{bodySuffix}
  parseControlAfterControls _ _ _ _ _ [] _ = Fail0 (B EOI NoBounds)
  parseControlAfterControls _ _ _ _ _
      ((B token bounds) :: _) _ =
    Fail0
      (B
        (Expected ["`.apply(...)` or a controlled block"] (show token))
        bounds)

  ||| Parses adjoint callable syntax `adjoint(f)` or an `adjoint { ... }` block.
  ||| Tested by: `fn f() {adjoint(f)(q1, q2, q3)}` and
  ||| `fn f() {adjoint {H(&q1); CT(&q1, &q2)}}`.
  parseAdjointExpression : Bounds -> Rule True SurfaceExpr
  parseAdjointExpression _ _ [] _ = Fail0 (B EOI NoBounds)
  parseAdjointExpression adjointBounds nodeId
      ((B (TokSym SymLParen) openBounds) ::
       (B (TokSym SymRParen) closeBounds) :: _) _ =
    failWithCustomError
      (ParseErrorWithMessage
        "`adjoint(...)` requires one callable expression.")
      (openBounds <+> closeBounds)
  parseAdjointExpression adjointBounds nodeId
      ((B (TokSym SymLParen) openBounds) :: afterOpen) (SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
     in case assert_total $
               parseExpression afterExpressionNodeId afterOpen recur of
          Fail0 err => Fail0 err
          Succ0 (callable, afterCallableNodeId)
                afterCallable @{callableSuffix} =>
            case afterCallable of
              (B (TokSym SymRParen) closeBounds) :: remaining =>
                let expression = surfaceAstNode
                      (MkAstInfo expressionNodeId
                        (sourceSpan (adjointBounds <+> closeBounds)))
                      (ExprAdjoint (AdjointOfCallable callable))
                 in succT $ Succ0 (expression, afterCallableNodeId) remaining
                      @{Data.List.Suffix.trans
                          (the (Suffix True remaining
                            (B (TokSym SymRParen) closeBounds :: remaining))
                            (Uncons Same))
                          callableSuffix}
              (B unexpected unexpectedBounds) :: _ =>
                Fail0 (B (Expected [")"] (show unexpected)) unexpectedBounds)
              [] => Fail0 (B EOI NoBounds)
  parseAdjointExpression adjointBounds nodeId
      ((B (TokSym SymLBrace) openBounds) :: remaining) acc@(SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
     in case parseFunctionBody afterExpressionNodeId
                 (B (TokSym SymLBrace) openBounds :: remaining) acc of
          Fail0 err => Fail0 err
          Succ0 (body, finalNodeId) finalTokens @{bodySuffix} =>
            let expression = surfaceAstNode
                  (MkAstInfo expressionNodeId
                    (mergeSpans (sourceSpan adjointBounds) body.astInfo.span))
                  (ExprAdjoint (AdjointBlock body))
             in Succ0 (expression, finalNodeId) finalTokens @{bodySuffix}
  parseAdjointExpression _ _ ((B token bounds) :: _) _ =
    Fail0
      (B
        (Expected ["`(` or `{` after `adjoint`"] (show token))
        bounds)

  ||| Dispatches literals, names, builtins, grouped values, collections, control flow,
  ||| quantum modifiers, and other primary expression forms.
  ||| This is where delimiter-sensitive ambiguities are resolved: `()` versus a
  ||| grouped expression versus a tuple, and an array literal versus `[value; count]`.
  ||| It creates the outer AST node before recursively parsing children, so node IDs
  ||| follow source-tree pre-order even when the child parser is mutually recursive.
  ||| Tested by: `fn booleans() {true; false}`.
  parsePrimaryExpression : Rule True SurfaceExpr
  parsePrimaryExpression _ [] _ = Fail0 (B EOI NoBounds)
  parsePrimaryExpression nodeId
      ((B (TokSym SymLParen) openBounds) ::
       (B (TokSym SymRParen) closeBounds) :: remaining) _ =
    Succ0 (makeLiteralExpression LiteralUnit (openBounds <+> closeBounds) nodeId)
      remaining
        @{Data.List.Suffix.trans
            (the (Suffix True remaining
                    (B (TokSym SymRParen) closeBounds :: remaining))
                 (Uncons Same))
            (the (Suffix True
                    (B (TokSym SymRParen) closeBounds :: remaining)
                    (B (TokSym SymLParen) openBounds ::
                     B (TokSym SymRParen) closeBounds :: remaining))
                 (Uncons Same))}
  parsePrimaryExpression nodeId
      ((B (TokSym SymLParen) openBounds) :: remaining) (SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
     in case assert_total $
               parseExpression afterExpressionNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (first, afterFirstNodeId) afterFirst @{firstSuffix} =>
            case afterFirst of
              (B (TokSym SymRParen) closeBounds) :: finalTokens =>
                let expression = surfaceAstNode
                      (MkAstInfo expressionNodeId
                        (sourceSpan (openBounds <+> closeBounds)))
                      (ExprParenthesized first)
                 in Succ0 (expression, afterFirstNodeId) finalTokens
                      @{Data.List.Suffix.trans
                          (Data.List.Suffix.trans
                            (the (Suffix True finalTokens
                                    (B (TokSym SymRParen) closeBounds :: finalTokens))
                                 (Uncons Same))
                            firstSuffix)
                          (the (Suffix True remaining
                                  (B (TokSym SymLParen) openBounds :: remaining))
                               (Uncons Same))}
              (B (TokSym SymComma) commaBounds) :: afterComma =>
                case
                           parseExpressionTupleTail afterFirstNodeId afterComma suffixAcc of
                  Fail0 err => Fail0 err
                  Succ0 (tail, finalNodeId) finalTokens @{tailSuffix} =>
                    let expression = surfaceAstNode
                          (MkAstInfo expressionNodeId
                            (sourceSpan (openBounds <+> tail.tupleCloseBounds)))
                          (ExprTuple (first ::: tail.tupleTailElements))
                     in Succ0 (expression, finalNodeId) finalTokens
                          @{Data.List.Suffix.trans
                              (Data.List.Suffix.trans
                                (Data.List.Suffix.trans tailSuffix
                                  (the (Suffix True afterComma
                                          (B (TokSym SymComma) commaBounds :: afterComma))
                                       (Uncons Same)))
                                firstSuffix)
                              (the (Suffix True remaining
                                      (B (TokSym SymLParen) openBounds :: remaining))
                                   (Uncons Same))}
              (B unexpected unexpectedBounds) :: _ =>
                Fail0 (B (Expected [",", ")"] (show unexpected)) unexpectedBounds)
              [] => Fail0 (B EOI NoBounds)
  parsePrimaryExpression nodeId
      ((B (TokSym SymLBracket) openBounds) ::
       (B (TokSym SymRBracket) closeBounds) :: remaining) _ =
    let (expressionNodeId, nextNodeId) = reserveNodeId nodeId
        expression = surfaceAstNode
          (MkAstInfo expressionNodeId (sourceSpan (openBounds <+> closeBounds)))
          (ExprArray [])
     in Succ0 (expression, nextNodeId) remaining
          @{Data.List.Suffix.trans
              (the (Suffix True remaining
                      (B (TokSym SymRBracket) closeBounds :: remaining))
                   (Uncons Same))
              (the (Suffix True
                      (B (TokSym SymRBracket) closeBounds :: remaining)
                      (B (TokSym SymLBracket) openBounds ::
                       B (TokSym SymRBracket) closeBounds :: remaining))
                   (Uncons Same))}
  parsePrimaryExpression nodeId
      ((B (TokSym SymLBracket) openBounds) :: remaining) (SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
     in case assert_total $
               parseExpression afterExpressionNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (first, afterFirstNodeId) afterFirst @{firstSuffix} =>
            case afterFirst of
              (B (TokSym SymRBracket) closeBounds) :: finalTokens =>
                let expression = surfaceAstNode
                      (MkAstInfo expressionNodeId
                        (sourceSpan (openBounds <+> closeBounds)))
                      (ExprArray [first])
                 in Succ0 (expression, afterFirstNodeId) finalTokens
                      @{Data.List.Suffix.trans
                          (Data.List.Suffix.trans
                            (the (Suffix True finalTokens
                                    (B (TokSym SymRBracket) closeBounds :: finalTokens))
                                 (Uncons Same))
                            firstSuffix)
                          (the (Suffix True remaining
                                  (B (TokSym SymLBracket) openBounds :: remaining))
                               (Uncons Same))}
              (B (TokSym SymComma) commaBounds) :: afterComma =>
                case
                           parseArrayElements afterFirstNodeId afterComma suffixAcc of
                  Fail0 err => Fail0 err
                  Succ0 (tail, finalNodeId) finalTokens @{tailSuffix} =>
                    let expression = surfaceAstNode
                          (MkAstInfo expressionNodeId
                            (sourceSpan (openBounds <+> tail.arrayCloseBounds)))
                          (ExprArray (first :: tail.arrayElementValues))
                     in Succ0 (expression, finalNodeId) finalTokens
                          @{Data.List.Suffix.trans
                              (Data.List.Suffix.trans
                                (Data.List.Suffix.trans tailSuffix
                                  (the (Suffix True afterComma
                                          (B (TokSym SymComma) commaBounds :: afterComma))
                                       (Uncons Same)))
                                firstSuffix)
                              (the (Suffix True remaining
                                      (B (TokSym SymLBracket) openBounds :: remaining))
                                   (Uncons Same))}
              (B (TokSym SymSemi) semiBounds) :: afterSemi =>
                case assert_total $ parseExpression afterFirstNodeId afterSemi suffixAcc of
                  Fail0 err => Fail0 err
                  Succ0 (count, finalNodeId) afterCount @{countSuffix} =>
                    case afterCount of
                      (B (TokSym SymRBracket) closeBounds) :: finalTokens =>
                        let expression = surfaceAstNode
                              (MkAstInfo expressionNodeId
                                (sourceSpan (openBounds <+> closeBounds)))
                              (ExprRepeatedArray first count)
                         in Succ0 (expression, finalNodeId) finalTokens
                              @{Data.List.Suffix.trans
                                  (Data.List.Suffix.trans
                                    (Data.List.Suffix.trans
                                      (the (Suffix True finalTokens
                                              (B (TokSym SymRBracket) closeBounds :: finalTokens))
                                           (Uncons Same))
                                      countSuffix)
                                    (the (Suffix True afterSemi
                                            (B (TokSym SymSemi) semiBounds :: afterSemi))
                                         (Uncons Same)))
                                  (Data.List.Suffix.trans firstSuffix
                                    (the (Suffix True remaining
                                            (B (TokSym SymLBracket) openBounds :: remaining))
                                         (Uncons Same)))}
                      (B unexpected unexpectedBounds) :: _ =>
                        Fail0 (B (Expected ["]"] (show unexpected)) unexpectedBounds)
                      [] => Fail0 (B EOI NoBounds)
              (B unexpected unexpectedBounds) :: _ =>
                Fail0 (B (Expected [",", ";", "]"] (show unexpected)) unexpectedBounds)
              [] => Fail0 (B EOI NoBounds)
  parsePrimaryExpression nodeId
      ((B (TokKw KwAdjoint) bounds) :: remaining) (SA recur) =
    succT $
      parseAdjointExpression bounds nodeId remaining recur
  parsePrimaryExpression nodeId tokens@((B (TokSym SymLBrace) _) :: _) acc =
    parseBlockExpression nodeId tokens acc
  parsePrimaryExpression nodeId tokens@((B (TokKw KwLoop) _) :: _) acc =
    parseLoopExpression nodeId tokens acc
  parsePrimaryExpression nodeId tokens@((B (TokKw KwWhile) _) :: _) acc =
    parseWhileExpression nodeId tokens acc
  parsePrimaryExpression nodeId tokens@((B (TokKw KwFor) _) :: _) acc =
    parseForExpression nodeId tokens acc
  parsePrimaryExpression nodeId tokens@((B (TokKw KwBreak) _) :: _) acc =
    parseBreakExpression nodeId tokens acc
  parsePrimaryExpression nodeId tokens@((B (TokKw KwContinue) _) :: _) acc =
    parseContinueExpression nodeId tokens acc
  parsePrimaryExpression nodeId tokens@((B (TokKw KwReturn) _) :: _) acc =
    parseReturnExpression nodeId tokens acc
  parsePrimaryExpression nodeId tokens@((B (TokKw KwIf) _) :: _) acc =
    parseIfExpression nodeId tokens acc
  parsePrimaryExpression nodeId
      ((B token bounds) :: remaining) acc@(SA recur) =
    case token of
      TokKw KwQif =>
        failWithCustomError
          (UnsupportedFeature "Quantum if expressions are not yet supported.") bounds
      TokKw KwMatch =>
        failWithCustomError
          (UnsupportedFeature "Match expressions are not yet supported.") bounds
      TokKw KwQmatch =>
        failWithCustomError
          (UnsupportedFeature "Quantum match expressions are not yet supported.") bounds
      TokKw KwSif =>
        failWithCustomError
          (UnsupportedFeature "State if expressions are not yet supported.") bounds
      TokKw KwSmatch =>
        failWithCustomError
          (UnsupportedFeature "State match expressions are not yet supported.") bounds
      TokKw KwSelf =>
        failWithCustomError
          (UnsupportedFeature "Self expressions are not yet supported.") bounds
      TokIdent nameText =>
        case remaining of
          braceTokens@((B (TokSym SymLBrace) braceBounds) :: afterBrace) =>
            if startsWithUppercase nameText
              then failWithCustomError
                     (UnsupportedFeature
                       "Struct literal expressions are not yet supported.") bounds
              else Succ0 (makeNameExpression nameText bounds nodeId)
                     (B (TokSym SymLBrace) braceBounds :: afterBrace)
                     @{the (Suffix True
                              (B (TokSym SymLBrace) braceBounds :: afterBrace)
                              (B (TokIdent nameText) bounds ::
                               B (TokSym SymLBrace) braceBounds :: afterBrace))
                           (Uncons Same)}
          (B (TokSym SymDoubleColon) _) :: _ =>
            failWithCustomError
              (UnsupportedFeature "Path expressions are not yet supported.") bounds
          _ => Succ0 (makeNameExpression nameText bounds nodeId) remaining
      TokBuiltin BuiltinCtrl =>
        succT $
          parseControlExpression bounds nodeId remaining recur
      TokBuiltin builtin =>
        Succ0 (makeBuiltinExpression builtin bounds nodeId) remaining
      TokIntLitRaw rawText =>
        Succ0 (makeLiteralExpression (LiteralIntegerRaw rawText) bounds nodeId) remaining
      TokFloatLitRaw rawText =>
        Succ0 (makeLiteralExpression (LiteralFloatRaw rawText) bounds nodeId) remaining
      TokBoolLit value =>
        Succ0 (makeLiteralExpression (LiteralBoolean value) bounds nodeId) remaining
      TokStringLitRaw rawText =>
        Succ0 (makeLiteralExpression (LiteralStringRaw rawText) bounds nodeId) remaining
      TokByteLitRaw _ =>
        failWithCustomError
          (UnsupportedFeature "Byte literals are not yet supported.") bounds
      TokByteStringLitRaw _ =>
        failWithCustomError
          (UnsupportedFeature "Byte string literals are not yet supported.") bounds
      TokBasisStringLitRaw rawText =>
        Succ0
          (makeLiteralExpression (LiteralBasisStringRaw rawText) bounds nodeId)
          remaining
      TokStateLit _ =>
        failWithCustomError
          (UnsupportedFeature "State literals are not yet supported.") bounds
      _ => Fail0 (B (Expected ["an expression"] (show token)) bounds)

  ||| Parses the remaining comma-separated values and closing `)` of a tuple expression.
  ||| Tested by: `fn tuples() {(1, true); (1, (2, 3),)}`.
  parseExpressionTupleTail : Rule True ExpressionTupleTail
  parseExpressionTupleTail _ [] _ = Fail0 (B EOI NoBounds)
  parseExpressionTupleTail nodeId
      ((B (TokSym SymRParen) closeBounds) :: remaining) _ =
    Succ0 (MkExpressionTupleTail [] closeBounds, nodeId) remaining
  parseExpressionTupleTail nodeId tokens acc@(SA recur) =
    case assert_total $ parseExpression nodeId tokens acc of
      Fail0 err => Fail0 err
      Succ0 (element, afterElementNodeId) afterElement @{elementSuffix} =>
        case afterElement of
          (B (TokSym SymRParen) closeBounds) :: finalTokens =>
            Succ0 (MkExpressionTupleTail [element] closeBounds, afterElementNodeId)
              finalTokens
                @{Data.List.Suffix.trans
                    (the (Suffix True finalTokens
                            (B (TokSym SymRParen) closeBounds :: finalTokens))
                         (Uncons Same))
                    elementSuffix}
          (B (TokSym SymComma) commaBounds) :: afterComma =>
            case assert_total $
                       parseExpressionTupleTail afterElementNodeId afterComma suffixAcc of
              Fail0 err => Fail0 err
              Succ0 (tail, finalNodeId) finalTokens @{tailSuffix} =>
                Succ0
                  (MkExpressionTupleTail
                    (element :: tail.tupleTailElements) tail.tupleCloseBounds,
                   finalNodeId)
                  finalTokens
                    @{Data.List.Suffix.trans
                        (Data.List.Suffix.trans tailSuffix
                          (the (Suffix True afterComma
                                  (B (TokSym SymComma) commaBounds :: afterComma))
                               (Uncons Same)))
                        elementSuffix}
          (B unexpected unexpectedBounds) :: _ =>
            Fail0 (B (Expected [",", ")"] (show unexpected)) unexpectedBounds)
          [] => Fail0 (B EOI NoBounds)

  ||| Repeatedly attaches calls, indexing, fields, tuple indices, and method calls.
  ||| The input expression is the already-parsed receiver. Every recognized postfix
  ||| form builds a new outer expression and recursively continues, producing a
  ||| maximal chain such as `values()[i].field.len()`. An unrecognized token is not
  ||| an error: it terminates the chain and is returned untouched to the caller.
  ||| Tested by: `fn postfix() {values()[i].field.len()}`.
  parsePostfixExpression : SurfaceExpr -> Rule False SurfaceExpr
  parsePostfixExpression callee nodeId
      ((B (TokSym SymLParen) openBounds) :: afterOpen) (SA recur) =
    let (callNodeId, afterCallNodeId) = reserveNodeId nodeId
     in case parseCallArguments afterCallNodeId afterOpen recur of
          Fail0 err => Fail0 err
          Succ0 (arguments, afterArgumentsNodeId) afterArguments @{argumentsSuffix} =>
            let call = surfaceAstNode
                  (MkAstInfo callNodeId
                    (mergeSpans callee.astInfo.span
                      (sourceSpan arguments.callCloseBounds)))
                  (ExprCall callee arguments.callArgumentValues)
             in case assert_total $
                       parsePostfixExpression call afterArgumentsNodeId afterArguments suffixAcc of
                  Fail0 err => Fail0 err
                  Succ0 result finalTokens @{postfixSuffix} =>
                    succF $ Succ0 result finalTokens
                      @{Data.List.Suffix.trans postfixSuffix argumentsSuffix}
  parsePostfixExpression indexed nodeId
      ((B (TokSym SymLBracket) openBounds) :: afterOpen) (SA recur) =
    let (indexNodeId, afterIndexNodeId) = reserveNodeId nodeId
     in case assert_total $ parseExpression afterIndexNodeId afterOpen recur of
          Fail0 err => Fail0 err
          Succ0 (index, afterIndexExpressionNodeId) afterIndex @{indexSuffix} =>
            case afterIndex of
              (B (TokSym SymRBracket) closeBounds) :: afterClose =>
                let expression = surfaceAstNode
                      (MkAstInfo indexNodeId
                        (mergeSpans indexed.astInfo.span (sourceSpan closeBounds)))
                      (ExprIndex indexed index)
                 in case assert_total $
                           parsePostfixExpression expression afterIndexExpressionNodeId
                             afterClose suffixAcc of
                      Fail0 err => Fail0 err
                      Succ0 result finalTokens @{postfixSuffix} =>
                        succF $ Succ0 result finalTokens
                          @{Data.List.Suffix.trans
                              (Data.List.Suffix.trans postfixSuffix
                                (the (Suffix True afterClose
                                        (B (TokSym SymRBracket) closeBounds :: afterClose))
                                     (Uncons Same)))
                              indexSuffix}
              (B unexpected unexpectedBounds) :: _ =>
                Fail0 (B (Expected ["]"] (show unexpected)) unexpectedBounds)
              [] => Fail0 (B EOI NoBounds)
  parsePostfixExpression receiver nodeId
      ((B (TokSym SymDot) dotBounds) ::
       (B (TokIdent methodText) methodBounds) ::
       (B (TokSym SymLParen) openBounds) :: afterOpen) (SA recur) =
    let (methodNodeId, afterMethodNodeId) = reserveNodeId nodeId
        (nameNodeId, afterNameNodeId) = reserveNodeId afterMethodNodeId
        methodName = surfaceAstNode (MkAstInfo nameNodeId (sourceSpan methodBounds))
                                    (MkNameNode methodText)
     in case parseCallArguments afterNameNodeId afterOpen recur of
          Fail0 err => Fail0 err
          Succ0 (arguments, afterArgumentsNodeId) afterArguments @{argumentsSuffix} =>
            let expression = surfaceAstNode
                  (MkAstInfo methodNodeId
                    (mergeSpans receiver.astInfo.span
                      (sourceSpan arguments.callCloseBounds)))
                  (ExprMethodCall receiver methodName arguments.callArgumentValues)
             in case assert_total $
                       parsePostfixExpression expression afterArgumentsNodeId
                         afterArguments suffixAcc of
                  Fail0 err => Fail0 err
                  Succ0 result finalTokens @{postfixSuffix} =>
                    weaken $ Succ0 result finalTokens
                      @{Data.List.Suffix.trans
                          (Data.List.Suffix.trans
                            (Data.List.Suffix.trans
                              (Data.List.Suffix.trans postfixSuffix argumentsSuffix)
                              (the (Suffix True afterOpen
                                      (B (TokSym SymLParen) openBounds :: afterOpen))
                                   (Uncons Same)))
                            (the (Suffix True
                                    (B (TokSym SymLParen) openBounds :: afterOpen)
                                    (B (TokIdent methodText) methodBounds ::
                                     B (TokSym SymLParen) openBounds :: afterOpen))
                                 (Uncons Same)))
                          (the (Suffix True
                                  (B (TokIdent methodText) methodBounds ::
                                   B (TokSym SymLParen) openBounds :: afterOpen)
                                  (B (TokSym SymDot) dotBounds ::
                                   B (TokIdent methodText) methodBounds ::
                                   B (TokSym SymLParen) openBounds :: afterOpen))
                               (Uncons Same))}
  parsePostfixExpression receiver nodeId
      ((B (TokSym SymDot) dotBounds) ::
       (B (TokIdent fieldText) fieldBounds) :: afterField) (SA recur) =
    let (fieldNodeId, afterFieldNodeId) = reserveNodeId nodeId
        (nameNodeId, afterNameNodeId) = reserveNodeId afterFieldNodeId
        fieldName = surfaceAstNode (MkAstInfo nameNodeId (sourceSpan fieldBounds))
                                   (MkNameNode fieldText)
        expression = surfaceAstNode
          (MkAstInfo fieldNodeId
            (mergeSpans receiver.astInfo.span (sourceSpan fieldBounds)))
          (ExprField receiver fieldName)
     in case assert_total $
               parsePostfixExpression expression afterNameNodeId afterField recur of
          Fail0 err => Fail0 err
          Succ0 result finalTokens @{postfixSuffix} =>
            weaken $ Succ0 result finalTokens
              @{Data.List.Suffix.trans
                  (Data.List.Suffix.trans postfixSuffix
                    (the (Suffix True afterField
                            (B (TokIdent fieldText) fieldBounds :: afterField))
                         (Uncons Same)))
                  (the (Suffix True
                          (B (TokIdent fieldText) fieldBounds :: afterField)
                          (B (TokSym SymDot) dotBounds ::
                           B (TokIdent fieldText) fieldBounds :: afterField))
                       (Uncons Same))}
  parsePostfixExpression receiver nodeId
      ((B (TokSym SymDot) dotBounds) ::
       (B (TokIntLitRaw indexRaw) indexBounds) :: afterIndex) (SA recur) =
    let (indexNodeId, afterIndexNodeId) = reserveNodeId nodeId
        expression = surfaceAstNode
          (MkAstInfo indexNodeId
            (mergeSpans receiver.astInfo.span (sourceSpan indexBounds)))
          (ExprTupleIndex receiver indexRaw)
     in case assert_total $
               parsePostfixExpression expression afterIndexNodeId afterIndex recur of
          Fail0 err => Fail0 err
          Succ0 result finalTokens @{postfixSuffix} =>
            weaken $ Succ0 result finalTokens
              @{Data.List.Suffix.trans
                  (Data.List.Suffix.trans postfixSuffix
                    (the (Suffix True afterIndex
                            (B (TokIntLitRaw indexRaw) indexBounds :: afterIndex))
                         (Uncons Same)))
                  (the (Suffix True
                          (B (TokIntLitRaw indexRaw) indexBounds :: afterIndex)
                          (B (TokSym SymDot) dotBounds ::
                           B (TokIntLitRaw indexRaw) indexBounds :: afterIndex))
                       (Uncons Same))}
  parsePostfixExpression callee nodeId tokens _ =
    Succ0 (callee, nodeId) tokens @{Same}

  ||| Parses comma-separated call arguments and their closing parenthesis.
  ||| Empty argument lists succeed on an immediate `)`. Otherwise each expression
  ||| must be followed by `,` or `)`; after a comma recursion also permits a trailing
  ||| comma. The closing bounds are returned with the values so the call node can
  ||| cover its full source range.
  ||| Tested by: `fn calls() {f(); f(x, y)}`.
  parseCallArguments : Rule True CallArguments
  parseCallArguments _ [] _ = Fail0 (B EOI NoBounds)
  parseCallArguments nodeId
      ((B (TokSym SymRParen) closeBounds) :: remaining) _ =
    Succ0 (MkCallArguments [] closeBounds, nodeId) remaining
  parseCallArguments nodeId tokens acc =
    case assert_total $ parseExpression nodeId tokens acc of
      Fail0 err => Fail0 err
      Succ0 (argument, afterArgumentNodeId) afterArgument @{argumentSuffix} =>
        case afterArgument of
          (B (TokSym SymRParen) closeBounds) :: finalTokens =>
            Succ0 (MkCallArguments [argument] closeBounds, afterArgumentNodeId)
              finalTokens
                @{Data.List.Suffix.trans
                    (the (Suffix True finalTokens
                            (B (TokSym SymRParen) closeBounds :: finalTokens))
                         (Uncons Same))
                    argumentSuffix}
          (B (TokSym SymComma) commaBounds) :: afterComma =>
            case assert_total $
                       parseCallArguments afterArgumentNodeId afterComma suffixAcc of
              Fail0 err => Fail0 err
              Succ0 (arguments, finalNodeId) finalTokens @{argumentsSuffix} =>
                Succ0
                  (MkCallArguments
                    (argument :: arguments.callArgumentValues)
                    arguments.callCloseBounds,
                   finalNodeId)
                  finalTokens
                    @{Data.List.Suffix.trans
                        (Data.List.Suffix.trans argumentsSuffix
                          (the (Suffix True afterComma
                                  (B (TokSym SymComma) commaBounds :: afterComma))
                               (Uncons Same)))
                        argumentSuffix}
          (B unexpected unexpectedBounds) :: _ =>
            Fail0 (B (Expected [",", ")"] (show unexpected)) unexpectedBounds)
          [] => Fail0 (B EOI NoBounds)

  ||| Parses comma-separated array elements and their closing bracket.
  ||| Its delimiter protocol mirrors `parseCallArguments`: an immediate `]` is an
  ||| empty array, commas recurse and allow a trailing comma, and the closing bounds
  ||| are retained for the array expression's span.
  ||| Tested by: `fn arrays() {[]; [1, 2, 3]; [1, 2, 3,]}`.
  parseArrayElements : Rule True ArrayElements
  parseArrayElements _ [] _ = Fail0 (B EOI NoBounds)
  parseArrayElements nodeId
      ((B (TokSym SymRBracket) closeBounds) :: remaining) _ =
    Succ0 (MkArrayElements [] closeBounds, nodeId) remaining
  parseArrayElements nodeId tokens acc =
    case assert_total $ parseExpression nodeId tokens acc of
      Fail0 err => Fail0 err
      Succ0 (element, afterElementNodeId) afterElement @{elementSuffix} =>
        case afterElement of
          (B (TokSym SymRBracket) closeBounds) :: finalTokens =>
            Succ0 (MkArrayElements [element] closeBounds, afterElementNodeId)
              finalTokens
                @{Data.List.Suffix.trans
                    (the (Suffix True finalTokens
                            (B (TokSym SymRBracket) closeBounds :: finalTokens))
                         (Uncons Same))
                    elementSuffix}
          (B (TokSym SymComma) commaBounds) :: afterComma =>
            case assert_total $
                       parseArrayElements afterElementNodeId afterComma suffixAcc of
              Fail0 err => Fail0 err
              Succ0 (tail, finalNodeId) finalTokens @{tailSuffix} =>
                Succ0
                  (MkArrayElements
                    (element :: tail.arrayElementValues) tail.arrayCloseBounds,
                   finalNodeId)
                  finalTokens
                    @{Data.List.Suffix.trans
                        (Data.List.Suffix.trans tailSuffix
                          (the (Suffix True afterComma
                                  (B (TokSym SymComma) commaBounds :: afterComma))
                               (Uncons Same)))
                        elementSuffix}
          (B unexpected unexpectedBounds) :: _ =>
            Fail0 (B (Expected [",", "]"] (show unexpected)) unexpectedBounds)
          [] => Fail0 (B EOI NoBounds)

  ||| Wraps a parsed braced block as an expression node.
  ||| Tested by: `fn block() {{1}}`.
  parseBlockExpression : Rule True SurfaceExpr
  parseBlockExpression nodeId tokens acc =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
     in case parseFunctionBody afterExpressionNodeId tokens acc of
          Fail0 err => Fail0 err
          Succ0 (block, finalNodeId) finalTokens @{blockSuffix} =>
            Succ0
              (surfaceAstNode (MkAstInfo expressionNodeId block.astInfo.span)
                (ExprBlock block),
               finalNodeId)
              finalTokens @{blockSuffix}

  ||| Parses an unconditional `loop` expression and its body.
  ||| Tested by:
  ||| `fn control() {loop {break}; while ready {continue}; for x in values {x}}`.
  parseLoopExpression : Rule True SurfaceExpr
  parseLoopExpression _ [] _ = Fail0 (B EOI NoBounds)
  parseLoopExpression nodeId
      ((B (TokKw KwLoop) loopBounds) :: remaining) (SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
     in case parseFunctionBody afterExpressionNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (body, finalNodeId) finalTokens @{bodySuffix} =>
            let expression = surfaceAstNode
                  (MkAstInfo expressionNodeId
                    (mergeSpans (sourceSpan loopBounds) body.astInfo.span))
                  (ExprLoop body)
             in Succ0 (expression, finalNodeId) finalTokens
                  @{Data.List.Suffix.trans bodySuffix
                      (the (Suffix True remaining
                              (B (TokKw KwLoop) loopBounds :: remaining))
                           (Uncons Same))}
  parseLoopExpression _ ((B unexpected unexpectedBounds) :: _) _ =
    Fail0 (B (Expected ["loop"] (show unexpected)) unexpectedBounds)

  ||| Parses a `while` condition and its braced body.
  ||| Tested by:
  ||| `fn control() {loop {break}; while ready {continue}; for x in values {x}}`.
  parseWhileExpression : Rule True SurfaceExpr
  parseWhileExpression _ [] _ = Fail0 (B EOI NoBounds)
  parseWhileExpression nodeId
      ((B (TokKw KwWhile) whileBounds) :: remaining) (SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
     in case assert_total $
               parseExpression afterExpressionNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (condition, afterConditionNodeId) afterCondition @{conditionSuffix} =>
            case
                       parseFunctionBody afterConditionNodeId afterCondition suffixAcc of
              Fail0 err => Fail0 err
              Succ0 (body, finalNodeId) finalTokens @{bodySuffix} =>
                let expression = surfaceAstNode
                      (MkAstInfo expressionNodeId
                        (mergeSpans (sourceSpan whileBounds) body.astInfo.span))
                      (ExprWhile condition body)
                 in Succ0 (expression, finalNodeId) finalTokens
                      @{Data.List.Suffix.trans
                          (Data.List.Suffix.trans bodySuffix conditionSuffix)
                          (the (Suffix True remaining
                                  (B (TokKw KwWhile) whileBounds :: remaining))
                               (Uncons Same))}
  parseWhileExpression _ ((B unexpected unexpectedBounds) :: _) _ =
    Fail0 (B (Expected ["while"] (show unexpected)) unexpectedBounds)

  ||| Parses `for name in expression` and its braced body.
  ||| Tested by:
  ||| `fn control() {loop {break}; while ready {continue}; for x in values {x}}`.
  parseForExpression : Rule True SurfaceExpr
  parseForExpression _ [] _ = Fail0 (B EOI NoBounds)
  parseForExpression nodeId
      ((B (TokKw KwFor) forBounds) ::
       (B (TokIdent binderText) binderBounds) ::
       (B (TokKw KwIn) inBounds) :: remaining) (SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
        (patternNodeId, afterPatternNodeId) = reserveNodeId afterExpressionNodeId
        (nameNodeId, afterNameNodeId) = reserveNodeId afterPatternNodeId
        name = surfaceAstNode (MkAstInfo nameNodeId (sourceSpan binderBounds))
                              (MkNameNode binderText)
        pattern = surfaceAstNode (MkAstInfo patternNodeId (sourceSpan binderBounds))
                                 (PatternName Nothing name)
     in case assert_total $ parseExpression afterNameNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (iterable, afterIterableNodeId) afterIterable @{iterableSuffix} =>
            case
                       parseFunctionBody afterIterableNodeId afterIterable suffixAcc of
              Fail0 err => Fail0 err
              Succ0 (body, finalNodeId) finalTokens @{bodySuffix} =>
                let expression = surfaceAstNode
                      (MkAstInfo expressionNodeId
                        (mergeSpans (sourceSpan forBounds) body.astInfo.span))
                      (ExprFor pattern iterable body)
                 in Succ0 (expression, finalNodeId) finalTokens
                      @{Data.List.Suffix.trans
                          (Data.List.Suffix.trans bodySuffix iterableSuffix)
                          (Data.List.Suffix.trans
                            (the (Suffix True remaining
                                    (B (TokKw KwIn) inBounds :: remaining))
                                 (Uncons Same))
                            (Data.List.Suffix.trans
                              (the (Suffix True
                                      (B (TokKw KwIn) inBounds :: remaining)
                                      (B (TokIdent binderText) binderBounds ::
                                       B (TokKw KwIn) inBounds :: remaining))
                                   (Uncons Same))
                              (the (Suffix True
                                      (B (TokIdent binderText) binderBounds ::
                                       B (TokKw KwIn) inBounds :: remaining)
                                      (B (TokKw KwFor) forBounds ::
                                       B (TokIdent binderText) binderBounds ::
                                       B (TokKw KwIn) inBounds :: remaining))
                                   (Uncons Same))))}
  parseForExpression _ ((B unexpected unexpectedBounds) :: _) _ =
    Fail0 (B (Expected ["for identifier in expression"] (show unexpected)) unexpectedBounds)

  ||| Parses `break` with an optional value.
  ||| Tested by: `fn exits() {break 1; continue; return; return value}`.
  parseBreakExpression : Rule True SurfaceExpr
  parseBreakExpression _ [] _ = Fail0 (B EOI NoBounds)
  parseBreakExpression nodeId
      ((B (TokKw KwBreak) breakBounds) :: remaining) (SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId in
    case remaining of
      (B (TokSym SymSemi) _) :: _ => finishWithoutValue expressionNodeId afterExpressionNodeId
      (B (TokSym SymRBrace) _) :: _ => finishWithoutValue expressionNodeId afterExpressionNodeId
      (B (TokSym SymComma) _) :: _ => finishWithoutValue expressionNodeId afterExpressionNodeId
      _ =>
        case assert_total $
                   parseExpression afterExpressionNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (value, finalNodeId) finalTokens @{valueSuffix} =>
            let expression = surfaceAstNode
                  (MkAstInfo expressionNodeId
                    (mergeSpans (sourceSpan breakBounds) value.astInfo.span))
                  (ExprBreak (Just value))
             in Succ0 (expression, finalNodeId) finalTokens
                  @{Data.List.Suffix.trans valueSuffix
                      (the (Suffix True remaining
                              (B (TokKw KwBreak) breakBounds :: remaining))
                           (Uncons Same))}
    where
      -- Builds a `break` node when the following token terminates the expression.
      -- Tested by:
      -- `fn control() {loop {break}; while ready {continue}; for x in values {x}}`.
      finishWithoutValue : NodeId -> Nat -> Res True Token
        (B (TokKw KwBreak) breakBounds :: remaining)
        CustomParseError (SurfaceExpr, Nat)
      finishWithoutValue expressionNodeId nextNodeId =
        Succ0
          (surfaceAstNode (MkAstInfo expressionNodeId (sourceSpan breakBounds))
            (ExprBreak Nothing), nextNodeId)
          remaining
  parseBreakExpression _ ((B unexpected unexpectedBounds) :: _) _ =
    Fail0 (B (Expected ["break"] (show unexpected)) unexpectedBounds)

  ||| Parses a value-less `continue` expression.
  ||| Tested by: `fn exits() {break 1; continue; return; return value}`.
  parseContinueExpression : Rule True SurfaceExpr
  parseContinueExpression _ [] _ = Fail0 (B EOI NoBounds)
  parseContinueExpression nodeId
      ((B (TokKw KwContinue) bounds) :: remaining) _ =
    let (expressionNodeId, nextNodeId) = reserveNodeId nodeId in
    Succ0
      (surfaceAstNode (MkAstInfo expressionNodeId (sourceSpan bounds)) ExprContinue,
       nextNodeId)
      remaining
  parseContinueExpression _ ((B unexpected unexpectedBounds) :: _) _ =
    Fail0 (B (Expected ["continue"] (show unexpected)) unexpectedBounds)

  ||| Parses `return` with an optional value.
  ||| Tested by: `fn exits() {return; return value}`.
  parseReturnExpression : Rule True SurfaceExpr
  parseReturnExpression _ [] _ = Fail0 (B EOI NoBounds)
  parseReturnExpression nodeId
      ((B (TokKw KwReturn) returnBounds) :: remaining) (SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId in
    case remaining of
      (B (TokSym SymSemi) _) :: _ => finishWithoutValue expressionNodeId afterExpressionNodeId
      (B (TokSym SymRBrace) _) :: _ => finishWithoutValue expressionNodeId afterExpressionNodeId
      (B (TokSym SymComma) _) :: _ => finishWithoutValue expressionNodeId afterExpressionNodeId
      _ =>
        case assert_total $
                   parseExpression afterExpressionNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (value, finalNodeId) finalTokens @{valueSuffix} =>
            let expression = surfaceAstNode
                  (MkAstInfo expressionNodeId
                    (mergeSpans (sourceSpan returnBounds) value.astInfo.span))
                  (ExprReturn (Just value))
             in Succ0 (expression, finalNodeId) finalTokens
                  @{Data.List.Suffix.trans valueSuffix
                      (the (Suffix True remaining
                              (B (TokKw KwReturn) returnBounds :: remaining))
                           (Uncons Same))}
    where
      -- Builds a `return` node when no return value follows.
      -- Tested by: `fn exits() {return; return value}`.
      finishWithoutValue : NodeId -> Nat -> Res True Token
        (B (TokKw KwReturn) returnBounds :: remaining)
        CustomParseError (SurfaceExpr, Nat)
      finishWithoutValue expressionNodeId nextNodeId =
        Succ0
          (surfaceAstNode (MkAstInfo expressionNodeId (sourceSpan returnBounds))
            (ExprReturn Nothing), nextNodeId)
          remaining
  parseReturnExpression _ ((B unexpected unexpectedBounds) :: _) _ =
    Fail0 (B (Expected ["return"] (show unexpected)) unexpectedBounds)

  ||| Parses classical `if`, chained `else if`, and optional `else` blocks.
  ||| Tested by: `fn choose() {if ready {1} else if retry {2} else {3}}`.
  parseIfExpression : Rule True SurfaceExpr
  parseIfExpression _ [] _ = Fail0 (B EOI NoBounds)
  parseIfExpression nodeId
      ((B (TokKw KwIf) ifBounds) :: remaining) (SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
        (ifNodeId, afterIfNodeId) = reserveNodeId afterExpressionNodeId
     in case assert_total $ parseExpression afterIfNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (condition, afterConditionNodeId) afterCondition @{conditionSuffix} =>
            case
                       parseFunctionBody afterConditionNodeId afterCondition suffixAcc of
              Fail0 err => Fail0 err
              Succ0 (thenBlock, afterThenNodeId) afterThen @{thenSuffix} =>
                case afterThen of
                  (B (TokKw KwElse) elseBounds) ::
                    afterElse@((B (TokSym SymLBrace) openElseBounds) :: elseTokens) =>
                      case
                                 parseFunctionBody afterThenNodeId afterElse suffixAcc of
                        Fail0 err => Fail0 err
                        Succ0 (elseBlock, finalNodeId) finalTokens @{elseSuffix} =>
                          let ifSpan = mergeSpans (sourceSpan ifBounds) elseBlock.astInfo.span
                              ifNode = MkClassicalIfNode condition thenBlock
                                (Just (ElseBlock elseBlock))
                              expression = surfaceAstNode
                                (MkAstInfo expressionNodeId ifSpan) (ExprIf ifNode)
                           in Succ0 (expression, finalNodeId) finalTokens
                                @{Data.List.Suffix.trans
                                    (Data.List.Suffix.trans
                                      (Data.List.Suffix.trans elseSuffix
                                        (the (Suffix True
                                                (B (TokSym SymLBrace) openElseBounds :: elseTokens)
                                                (B (TokKw KwElse) elseBounds ::
                                                 B (TokSym SymLBrace) openElseBounds :: elseTokens))
                                             (Uncons Same)))
                                      thenSuffix)
                                    (Data.List.Suffix.trans conditionSuffix
                                      (the (Suffix True remaining
                                              (B (TokKw KwIf) ifBounds :: remaining))
                                           (Uncons Same)))}
                  (B (TokKw KwElse) elseBounds) ::
                    afterElse@((B (TokKw KwIf) chainedIfBounds) :: chainedIfTokens) =>
                      case assert_total $
                                 parseIfExpression afterThenNodeId afterElse suffixAcc of
                        Fail0 err => Fail0 err
                        Succ0 (chainedExpression, finalNodeId) finalTokens @{elseSuffix} =>
                          case chainedExpression of
                            MkAstNode chainedInfo _ (ExprIf chainedIf) =>
                              let chainedNode = surfaceAstNode chainedInfo chainedIf
                                  ifSpan = mergeSpans (sourceSpan ifBounds)
                                    chainedExpression.astInfo.span
                                  ifNode = MkClassicalIfNode condition thenBlock
                                    (Just (ElseChainedIf chainedNode))
                                  expression = surfaceAstNode
                                    (MkAstInfo expressionNodeId ifSpan) (ExprIf ifNode)
                               in Succ0 (expression, finalNodeId) finalTokens
                                    @{Data.List.Suffix.trans
                                        (Data.List.Suffix.trans
                                          (Data.List.Suffix.trans elseSuffix
                                            (the (Suffix True
                                                    (B (TokKw KwIf) chainedIfBounds ::
                                                     chainedIfTokens)
                                                    (B (TokKw KwElse) elseBounds ::
                                                     B (TokKw KwIf) chainedIfBounds ::
                                                     chainedIfTokens))
                                                 (Uncons Same)))
                                          thenSuffix)
                                        (Data.List.Suffix.trans conditionSuffix
                                          (the (Suffix True remaining
                                                  (B (TokKw KwIf) ifBounds :: remaining))
                                               (Uncons Same)))}
                            _ =>
                              failWithCustomError
                                (ParseErrorWithMessage "Expected `if` after `else`.")
                                elseBounds
                  _ =>
                    let ifSpan = mergeSpans (sourceSpan ifBounds) thenBlock.astInfo.span
                        ifNode = MkClassicalIfNode condition thenBlock Nothing
                        expression = surfaceAstNode
                          (MkAstInfo expressionNodeId ifSpan) (ExprIf ifNode)
                     in Succ0 (expression, afterThenNodeId) afterThen
                          @{Data.List.Suffix.trans
                              (Data.List.Suffix.trans thenSuffix conditionSuffix)
                              (the (Suffix True remaining
                                      (B (TokKw KwIf) ifBounds :: remaining))
                                   (Uncons Same))}
  parseIfExpression _ ((B unexpected unexpectedBounds) :: _) _ =
    Fail0 (B (Expected ["if"] (show unexpected)) unexpectedBounds)

||| Builds a let initializer after its `=` or `:=` marker and parses the value.
||| Tested by: `fn compute() {let result := compute_value();}`.
parseLetInitializerValue :
     InitializerMarker
  -> Bounds
  -> Rule True LetInitializerNode
parseLetInitializerValue markerValue markerBounds nodeId tokens acc =
  let (markerNodeId, nextNodeId) = reserveNodeId nodeId
      marker = surfaceAstNode (MkAstInfo markerNodeId (sourceSpan markerBounds))
                              markerValue
   in case assert_total $ parseExpression nextNodeId tokens acc of
        Fail0 err => Fail0 err
        Succ0 (value, finalNodeId) finalTokens =>
          Succ0 (MkLetInitializerNode marker value, finalNodeId) finalTokens

||| Parses either an ordinary `=` or auto-uncompute `:=` let initializer.
||| Tested by: `fn compute() {let q: qubit := f(q);}`.
parseLetInitializer : Rule True LetInitializerNode
parseLetInitializer _ [] _ = Fail0 (B EOI NoBounds)
parseLetInitializer nodeId
    ((B (TokSym SymEq) bounds) :: remaining) (SA recur) =
  succT $ parseLetInitializerValue InitializerEquals bounds nodeId remaining recur
parseLetInitializer nodeId
    ((B (TokSym SymWalrusEq) bounds) :: remaining) (SA recur) =
  succT $
    parseLetInitializerValue InitializerAutoUncompute bounds nodeId remaining recur
parseLetInitializer _ ((B token bounds) :: _) _ =
  Fail0 (B (Expected ["=", ":="] (show token)) bounds)

||| Parses a `let` statement with qualifiers, pattern, optional type, and optional initializer.
||| The phases are deliberately ordered as they appear in source: storage qualifiers,
||| pattern, optional `: type`, then optional `=`/`:=` initializer. A semicolon is
||| always consumed by this function. At least a type or an initializer is required;
||| a typed binding may omit its initializer. Malformed punctuation after any
||| completed phase is reported at that token.
||| Tested by:
||| `fn mutable() { let mut x: i32 = 0; x = 5; let mut values = [0, 0]; values[0] = 10; }`.
parseLetStatement : Rule True SurfaceStatement
parseLetStatement _ [] _ = Fail0 (B EOI NoBounds)
parseLetStatement nodeId
    ((B (TokKw KwLet) letBounds) :: remaining) (SA recur) =
  let (statementNodeId, afterStatementNodeId) = reserveNodeId nodeId
   in case parseLetQualifiers [<] afterStatementNodeId remaining recur of
        Fail0 err => Fail0 err
        Succ0 (qualifiers, afterQualifiersNodeId) afterQualifiers
              @{qualifiersSuffix} =>
          case parsePattern afterQualifiersNodeId afterQualifiers suffixAcc of
            Fail0 err => Fail0 err
            Succ0 (pattern, afterPatternNodeId) afterPattern @{patternSuffix} =>
              case afterPattern of
                (B (TokSym SymColon) colonBounds) :: afterColon =>
                  case parseType afterPatternNodeId afterColon suffixAcc of
                    Fail0 err => Fail0 err
                    Succ0 (ty, afterTypeNodeId) afterType @{typeSuffix} =>
                      case afterType of
                        (B (TokSym SymSemi) semiBounds) :: finalTokens =>
                          let binding = MkLetBindingNode qualifiers pattern
                                (Just ty) Nothing
                              statement = surfaceAstNode
                                (MkAstInfo statementNodeId
                                  (sourceSpan (letBounds <+> semiBounds)))
                                (StatementLet binding)
                           in Succ0 (statement, afterTypeNodeId) finalTokens
                                @{Data.List.Suffix.trans
                                  (Data.List.Suffix.trans
                                    (the (Suffix True finalTokens
                                            (B (TokSym SymSemi) semiBounds :: finalTokens))
                                         (Uncons Same))
                                    (Data.List.Suffix.trans typeSuffix
                                      (the (Suffix True afterColon
                                              (B (TokSym SymColon) colonBounds :: afterColon))
                                           (Uncons Same))))
                                  (Data.List.Suffix.trans patternSuffix
                                    (Data.List.Suffix.trans qualifiersSuffix
                                      (the (Suffix True remaining
                                              (B (TokKw KwLet) letBounds :: remaining))
                                           (Uncons Same))))}
                        _ =>
                          case parseLetInitializer afterTypeNodeId afterType suffixAcc of
                            Fail0 err => Fail0 err
                            Succ0 (initializer, finalNodeId) afterInitializer
                                  @{initializerSuffix} =>
                              case afterInitializer of
                                (B (TokSym SymSemi) semiBounds) :: finalTokens =>
                                  let binding = MkLetBindingNode qualifiers pattern
                                        (Just ty) (Just initializer)
                                      statement = surfaceAstNode
                                        (MkAstInfo statementNodeId
                                          (sourceSpan (letBounds <+> semiBounds)))
                                        (StatementLet binding)
                                   in Succ0 (statement, finalNodeId) finalTokens
                                        @{Data.List.Suffix.trans
                                          (Data.List.Suffix.trans
                                            (the (Suffix True finalTokens
                                                    (B (TokSym SymSemi) semiBounds :: finalTokens))
                                                 (Uncons Same))
                                            (Data.List.Suffix.trans initializerSuffix
                                              (Data.List.Suffix.trans typeSuffix
                                                (the (Suffix True afterColon
                                                        (B (TokSym SymColon) colonBounds :: afterColon))
                                                     (Uncons Same)))))
                                          (Data.List.Suffix.trans patternSuffix
                                            (Data.List.Suffix.trans qualifiersSuffix
                                              (the (Suffix True remaining
                                                      (B (TokKw KwLet) letBounds :: remaining))
                                                   (Uncons Same))))}
                                (B unexpected bounds) :: _ =>
                                  Fail0 (B (Expected [";"] (show unexpected)) bounds)
                                [] => Fail0 (B EOI NoBounds)
                (B (TokSym symbol) markerBounds) :: afterMarker =>
                  if symbol == SymEq || symbol == SymWalrusEq
                    then
                  case parseLetInitializer afterPatternNodeId
                         (B (TokSym symbol) markerBounds :: afterMarker) suffixAcc of
                    Fail0 err => Fail0 err
                    Succ0 (initializer, finalNodeId) afterInitializer
                          @{initializerSuffix} =>
                      case afterInitializer of
                        (B (TokSym SymSemi) semiBounds) :: finalTokens =>
                          let binding = MkLetBindingNode qualifiers pattern Nothing
                                (Just initializer)
                              statement = surfaceAstNode
                                (MkAstInfo statementNodeId
                                  (sourceSpan (letBounds <+> semiBounds)))
                                (StatementLet binding)
                           in Succ0 (statement, finalNodeId) finalTokens
                                @{Data.List.Suffix.trans
                                  (Data.List.Suffix.trans
                                    (the (Suffix True finalTokens
                                            (B (TokSym SymSemi) semiBounds :: finalTokens))
                                         (Uncons Same))
                                    initializerSuffix)
                                  (Data.List.Suffix.trans patternSuffix
                                    (Data.List.Suffix.trans qualifiersSuffix
                                      (the (Suffix True remaining
                                              (B (TokKw KwLet) letBounds :: remaining))
                                           (Uncons Same))))}
                        (B unexpected bounds) :: _ =>
                          Fail0 (B (Expected [";"] (show unexpected)) bounds)
                        [] => Fail0 (B EOI NoBounds)
                    else
                      Fail0 (B (Expected [":", "=", ":="] (show (TokSym symbol)))
                        markerBounds)
                (B unexpected bounds) :: _ =>
                  Fail0 (B (Expected [":", "=", ":="] (show unexpected)) bounds)
                [] => Fail0 (B EOI NoBounds)
parseLetStatement _ ((B token bounds) :: _) _ =
  Fail0 (B (Expected ["let"] (show token)) bounds)
||| Converts a valid target expression plus an assignment operator and value into a statement.
||| Only names, field access, and indexing can become assignment targets. The target
||| is normalized into a dedicated assignment-target AST node before the right-hand
||| expression is parsed. This parser also consumes the required trailing semicolon,
||| so callers resume directly at the next block item.
||| Tested by: `fn assign() {a[i] = 1; p.x = 2;}`.
parseAssignmentStatement : SurfaceExpr -> Rule True SurfaceStatement
parseAssignmentStatement targetExpression _ [] _ = Fail0 (B EOI NoBounds)
parseAssignmentStatement targetExpression nodeId
    ((B (TokSym symbol) operatorBounds) :: afterOperator) (SA recur) =
  case (assignmentTargetFromExpression targetExpression, assignmentOperator symbol) of
    (Nothing, _) =>
      failWithCustomError
        (ParseErrorWithMessage "Expression is not a valid assignment target.")
        operatorBounds
    (_, Nothing) =>
      Fail0 (B (Expected ["an assignment operator"] (show (TokSym symbol)))
               operatorBounds)
    (Just targetValue, Just operator) =>
      let (statementNodeId, afterStatementNodeId) = reserveNodeId nodeId
          (targetNodeId, afterTargetNodeId) = reserveNodeId afterStatementNodeId
          (operatorNodeId, afterOperatorNodeId) = reserveNodeId afterTargetNodeId
          target = surfaceAstNode
            (MkAstInfo targetNodeId targetExpression.astInfo.span) targetValue
          locatedOperator = surfaceAstNode
            (MkAstInfo operatorNodeId (sourceSpan operatorBounds)) operator
       in case assert_total $
                 parseExpression afterOperatorNodeId afterOperator recur of
            Fail0 err => Fail0 err
            Succ0 (value, finalNodeId) afterValue @{valueSuffix} =>
              case afterValue of
                (B (TokSym SymSemi) semiBounds) :: finalTokens =>
                  let assignment = MkAssignmentNode target locatedOperator value
                      statement = surfaceAstNode
                        (MkAstInfo statementNodeId
                          (mergeSpans targetExpression.astInfo.span
                            (sourceSpan semiBounds)))
                        (StatementAssignment assignment)
                   in Succ0 (statement, finalNodeId) finalTokens
                        @{Data.List.Suffix.trans
                            (Data.List.Suffix.trans
                              (the (Suffix True finalTokens
                                      (B (TokSym SymSemi) semiBounds :: finalTokens))
                                   (Uncons Same))
                              valueSuffix)
                            (the (Suffix True afterOperator
                                    (B (TokSym symbol) operatorBounds :: afterOperator))
                                 (Uncons Same))}
                (B unexpected unexpectedBounds) :: _ =>
                  Fail0 (B (Expected [";"] (show unexpected)) unexpectedBounds)
                [] => Fail0 (B EOI NoBounds)
parseAssignmentStatement _ _ ((B unexpected unexpectedBounds) :: _) _ =
  Fail0 (B (Expected ["an assignment operator"] (show unexpected)) unexpectedBounds)

parseBlockContents _ _ _ _ [] _ = Fail0 (B EOI NoBounds)
parseBlockContents blockNodeId openBounds statements nodeId
    ((B token bounds) :: remaining) acc@(SA recur) =
    case token of
      TokSym SymRBrace =>
        let block = surfaceAstNode
              (MkAstInfo blockNodeId (sourceSpan (openBounds <+> bounds)))
              (MkBlockNode [] (statements <>> []) Nothing)
         in Succ0 (block, nodeId) remaining

      TokKw KwLet =>
        case parseLetStatement nodeId (B token bounds :: remaining) acc of
          Fail0 err => Fail0 err
          Succ0 (statement, nextNodeId) afterStatement =>
            succT $
              parseBlockContents blockNodeId openBounds
                (statements :< statement) nextNodeId afterStatement recur

      _ =>
        case assert_total $
                   parseStatementExpression nodeId
                     (B token bounds :: remaining) acc of
          Fail0 err => Fail0 err
          Succ0 (expression, afterExpressionNodeId) afterExpression
                @{expressionSuffix} =>
            case afterExpression of
              [] => Fail0 (B EOI NoBounds)

              (B (TokSym SymSemi) semiBounds) :: afterSemi =>
                let (statementNodeId, nextNodeId) =
                      reserveNodeId afterExpressionNodeId
                    statement = surfaceAstNode
                      (MkAstInfo statementNodeId
                        (mergeSpans expression.astInfo.span (sourceSpan semiBounds)))
                      (StatementSemiExpression expression)
                 in case assert_total $
                           parseBlockContents blockNodeId openBounds
                             (statements :< statement) nextNodeId afterSemi suffixAcc of
                      Fail0 err => Fail0 err
                      Succ0 result finalTokens @{blockSuffix} =>
                        Succ0 result finalTokens
                          @{Data.List.Suffix.trans blockSuffix $
                            Data.List.Suffix.trans
                              (the (Suffix True afterSemi
                                      (B (TokSym SymSemi) semiBounds :: afterSemi))
                                   (Uncons Same))
                              expressionSuffix}

              _ =>
                if isBlockLikeExpression expression
                  then
                    let (statementNodeId, nextNodeId) =
                          reserveNodeId afterExpressionNodeId
                        statement = surfaceAstNode
                          (MkAstInfo statementNodeId expression.astInfo.span)
                          (StatementExpression expression)
                     in case assert_total $
                               parseBlockContents blockNodeId openBounds
                                 (statements :< statement) nextNodeId
                                 afterExpression
                                 suffixAcc of
                          Fail0 err => Fail0 err
                          Succ0 result finalTokens @{blockSuffix} =>
                            Succ0 result finalTokens
                              @{Data.List.Suffix.trans blockSuffix expressionSuffix}
                  else
                    case afterExpression of
                      (B (TokSym SymRBrace) closeBounds) :: finalTokens =>
                        let block = surfaceAstNode
                              (MkAstInfo blockNodeId
                                (sourceSpan (openBounds <+> closeBounds)))
                              (MkBlockNode [] (statements <>> []) (Just expression))
                         in Succ0 (block, afterExpressionNodeId) finalTokens
                              @{Data.List.Suffix.trans
                                  (the (Suffix True finalTokens
                                          (B (TokSym SymRBrace) closeBounds ::
                                           finalTokens))
                                       (Uncons Same))
                                  expressionSuffix}

                      (B (TokSym symbol) operatorBounds) :: afterOperatorToken =>
                        case assignmentOperator symbol of
                          Just _ =>
                            case parseAssignmentStatement expression
                                   afterExpressionNodeId
                                   (B (TokSym symbol) operatorBounds ::
                                    afterOperatorToken)
                                   suffixAcc of
                              Fail0 err => Fail0 err
                              Succ0 (statement, nextNodeId) afterStatement
                                    @{assignmentSuffix} =>
                                case assert_total $
                                          parseBlockContents blockNodeId openBounds
                                            (statements :< statement) nextNodeId
                                            afterStatement suffixAcc of
                                  Fail0 err => Fail0 err
                                  Succ0 result finalTokens @{blockSuffix} =>
                                    Succ0 result finalTokens
                                      @{Data.List.Suffix.trans
                                          (Data.List.Suffix.trans blockSuffix
                                            assignmentSuffix)
                                          expressionSuffix}
                          Nothing =>
                            failWithCustomError (ParseErrorWithMessage
                              "Expected `;` or `}`, found instead: `\{interpolate (TokSym symbol)}`.")
                              operatorBounds

                      (B unexpected unexpectedBounds) :: _ =>
                        failWithCustomError (ParseErrorWithMessage
                          "Expected `;` or `}`, found instead: `\{interpolate unexpected}`.")
                          unexpectedBounds
                      [] => Fail0 (B EOI NoBounds)

parseFunctionBody _ [] _ = Fail0 (B EOI NoBounds)
parseFunctionBody nodeId ((B token bounds) :: remaining) acc@(SA recur) =
    case token of
      TokSym SymLBrace =>
        let (blockNodeId, nextNodeId) = reserveNodeId nodeId
         in succT $
              parseBlockContents blockNodeId bounds [<] nextNodeId remaining recur
      _ =>
        failWithCustomError (ParseErrorWithMessage
              "Expected a function body declaration starting with `{`, found instead: `\{interpolate token}`.") bounds

||| Parses a `#[name]` or `#[name("argument")]` function attribute.
||| Tested by: `#[qasm_gate]\nfn empty() -> () {}` and
||| `#[qasm_def("qasm_subroutine_name")]\npub general fn empty() -> () {}`.
parseAttribute : Rule True SurfaceAttribute
parseAttribute _ [] _ = Fail0 (B EOI NoBounds)
parseAttribute nodeId tokens _ =
  case tokens of
    [] => Fail0 (B EOI NoBounds)
    B (TokSym SymHash) hashBounds :: B (TokSym SymLBracket) _ ::
      B (TokIdent nameText) nameBounds :: B (TokSym SymRBracket) closeBounds :: remaining =>
        let (attributeNodeId, afterAttributeNodeId) = reserveNodeId nodeId
            (nameNodeId, nextNodeId) = reserveNodeId afterAttributeNodeId
            name = surfaceAstNode (MkAstInfo nameNodeId (sourceSpan nameBounds))
                                  (MkNameNode nameText)
            attribute = surfaceAstNode
              (MkAstInfo attributeNodeId (sourceSpan (hashBounds <+> closeBounds)))
              (MkAttributeNode name Nothing)
         in Succ0 (attribute, nextNodeId) remaining

    B (TokSym SymHash) hashBounds :: B (TokSym SymLBracket) _ ::
      B (TokIdent nameText) nameBounds :: B (TokSym SymLParen) _ ::
      B (TokStringLitRaw rawText) argumentBounds :: B (TokSym SymRParen) _ ::
      B (TokSym SymRBracket) closeBounds :: remaining =>
        let (attributeNodeId, afterAttributeNodeId) = reserveNodeId nodeId
            (nameNodeId, afterNameNodeId) = reserveNodeId afterAttributeNodeId
            (argumentNodeId, nextNodeId) = reserveNodeId afterNameNodeId
            name = surfaceAstNode (MkAstInfo nameNodeId (sourceSpan nameBounds))
                                  (MkNameNode nameText)
            argument = surfaceAstNode
              (MkAstInfo argumentNodeId (sourceSpan argumentBounds))
              (AttributeArgumentStringLit rawText)
            attribute = surfaceAstNode
              (MkAstInfo attributeNodeId (sourceSpan (hashBounds <+> closeBounds)))
              (MkAttributeNode name (Just [argument]))
         in Succ0 (attribute, nextNodeId) remaining

    B token bounds :: _ => failWithCustomError (ParseErrorWithMessage "Malformed attribute.") bounds

||| Parses a function declaration from its modifiers through parameters, contracts, and body.
||| Modifiers and attributes are supplied by the top-level dispatchers; this function
||| owns the `fn` token onward. It reserves the function item node before parsing its
||| children, threads the next free node ID through every optional phase, and composes
||| each phase's suffix proof in reverse return order. The final item span starts at
||| `declarationStart`, so preceding attributes, visibility, `const`, or effects are
||| included even though they were parsed by another function.
||| Tested by: `const fn square(x: i64) -> i64 { x * x }`.
parseFunDecl :
    (declarationStart : Bounds)
  -> (attributes : List SurfaceAttribute)
  -> (visibility : Maybe (SurfaceAstNode VisbilityQualifier))
  -> (isConst : Bool)
  -> (functionEffect : Maybe (SurfaceAstNode FunctionEffect))
  -> Rule True SurfaceItem
parseFunDecl declarationStart attributes visibility isConst functionEffect nodeId [] acc = Fail0 (B EOI NoBounds)
parseFunDecl declarationStart attributes visibility isConst functionEffect nodeId ((B token bounds) :: remaining) acc@(SA recur) =
    let (funNodeId, nextNodeId) = reserveNodeId nodeId
    in case token of
        TokKw KwFn =>
                case parseName "function name" nextNodeId remaining recur of
                    Fail0 err => Fail0 err
                    Succ0 (functionName, afterNameNodeId) afterName @{nameSuffix} =>
                            case parseFunctionParameters
                                    afterNameNodeId afterName suffixAcc of
                                Fail0 err => Fail0 err
                                Succ0 (functionParameters, afterParametersNodeId)
                                      afterParameters @{parametersSuffix} =>
                                        case parseOptionalReturnType
                                                afterParametersNodeId
                                                afterParameters
                                                suffixAcc of
                                            Fail0 err => Fail0 err
                                            Succ0 (returnType, afterReturnTypeNodeId)
                                                  afterReturnType @{returnTypeSuffix} =>
                                                    case parseOptionalSupportClause
                                                            afterReturnTypeNodeId
                                                            afterReturnType
                                                            suffixAcc of
                                                        Fail0 err => Fail0 err
                                                        Succ0 (supportClause, afterSupportNodeId)
                                                              afterSupport @{supportSuffix} =>
                                                                case parseContractClauses
                                                                        afterSupportNodeId
                                                                        afterSupport
                                                                        suffixAcc of
                                                                    Fail0 err =>Fail0 err
                                                                    Succ0 (contractClauses, afterContractsNodeId)
                                                                          afterContracts @{contractsSuffix} =>
                                                                            case parseFunctionBody
                                                                                    afterContractsNodeId
                                                                                    afterContracts
                                                                                    suffixAcc of
                                                                                Fail0 err => Fail0 err
                                                                                Succ0 (functionBody, finalNodeId)
                                                                                      finalTokens @{bodySuffix} =>
                                                                                    let declaration =
                                                                                            MkFunctionDeclarationNode
                                                                                                []                  -- docs
                                                                                                attributes
                                                                                                visibility
                                                                                                isConst
                                                                                                functionEffect
                                                                                                functionName
                                                                                                functionParameters
                                                                                                returnType
                                                                                                supportClause
                                                                                                contractClauses
                                                                                                functionBody
                                                                                        itemSpan =
                                                                                            mergeSpans
                                                                                                (sourceSpan declarationStart)
                                                                                                functionBody.astInfo.span
                                                                                        item =
                                                                                            surfaceAstNode
                                                                                                (MkAstInfo funNodeId itemSpan)
                                                                                                (ItemFunction declaration)
                                                                                     in Succ0
                                                                                            (item, finalNodeId)
                                                                                            finalTokens
                                                                                            @{Data.List.Suffix.trans bodySuffix $
                                                                                              Data.List.Suffix.trans contractsSuffix $
                                                                                              Data.List.Suffix.trans supportSuffix $
                                                                                              Data.List.Suffix.trans returnTypeSuffix $
                                                                                              Data.List.Suffix.trans parametersSuffix $
                                                                                              Data.List.Suffix.trans nameSuffix $
                                                                                              the
                                                                                                (Suffix True
                                                                                                  remaining
                                                                                                  (B (TokKw KwFn) bounds :: remaining))
                                                                                                (Uncons Same)}

        _ =>
            failWithCustomError (ParseErrorWithMessage
              "Expected `fun` keyword, found instead: `\{interpolate token}`.") bounds

||| Parses an effect modifier and then delegates to the function-declaration parser.
||| Tested by: `unitary fn empty() -> () {}`.
parseFunDeclWithEffect :
   (declarationStart : Bounds)
  -> (attributes : List SurfaceAttribute)
  -> (visibility : Maybe (SurfaceAstNode VisbilityQualifier))
  -> (effect : FunctionEffect)
  -> (effectBounds : Bounds)
  -> Rule True SurfaceItem
parseFunDeclWithEffect declarationStart attributes visibility effect effectBounds nodeId [] acc = Fail0 (B EOI NoBounds)
parseFunDeclWithEffect declarationStart attributes visibility effect effectBounds nodeId ((B token bounds) :: remaining) acc =
    let (funNodeId, nextNodeId) = reserveNodeId nodeId
    in case token of
        TokKw KwFn =>
            let effectNode =
                    surfaceAstNode
                        (MkAstInfo funNodeId (sourceSpan effectBounds))
                        effect
             in parseFunDecl declarationStart attributes visibility False (Just effectNode) nextNodeId (B token bounds :: remaining) acc

        _ =>
            failWithCustomError (ParseErrorWithMessage
              "Expected `fun` after `\{show effect}` effect modifier, found instead: `\{interpolate token}`.") bounds

||| Parses either a constant function or a typed constant value declaration.
||| Tested by: `const N: i64 = 4;`.
parseConstDecl :
    (declarationStart : Bounds)
  -> (attributes : List SurfaceAttribute)
  -> (visibility : Maybe (SurfaceAstNode VisbilityQualifier))
  -> Rule True SurfaceItem
parseConstDecl _ _ _ _ [] _ = Fail0 (B EOI NoBounds)
parseConstDecl declarationStart attributes visibility nodeId
    tokens@((B token tokenBounds) :: remaining) acc@(SA recur) =
  case token of
    TokKw KwFn =>
      parseFunDecl declarationStart attributes visibility True Nothing nodeId
        (B (TokKw KwFn) tokenBounds :: remaining) (SA recur)

    _ =>
      case attributes of
        _ :: _ =>
          failWithCustomError
            (ParseErrorWithMessage "Attributes on const declarations are not yet supported.")
            tokenBounds
        [] =>
          let (constNodeId, afterConstNodeId) = reserveNodeId nodeId
           in case parseName "constant name" afterConstNodeId
                     (B token tokenBounds :: remaining) (SA recur) of
                Fail0 err => Fail0 err
                Succ0 (constName, afterNameNodeId) afterName @{nameSuffix} =>
                  case afterName of
                    (B (TokSym SymColon) colonBounds) :: afterColon =>
                      case parseType afterNameNodeId afterColon suffixAcc of
                        Fail0 err => Fail0 err
                        Succ0 (constType, afterTypeNodeId) afterType @{typeSuffix} =>
                          case afterType of
                            (B (TokSym SymEq) equalsBounds) :: afterEquals =>
                              case parseExpression afterTypeNodeId afterEquals suffixAcc of
                                Fail0 err => Fail0 err
                                Succ0 (constValue, finalNodeId) afterValue
                                      @{valueSuffix} =>
                                  case afterValue of
                                    (B (TokSym SymSemi) semiBounds) :: finalTokens =>
                                      let declaration =
                                            MkConstDeclarationNode
                                              [] visibility constName constType constValue
                                          item = surfaceAstNode
                                            (MkAstInfo constNodeId
                                              (sourceSpan (declarationStart <+> semiBounds)))
                                            (ItemConst declaration)
                                       in Succ0 (item, finalNodeId) finalTokens
                                            @{Data.List.Suffix.trans
                                              (the (Suffix True finalTokens
                                                      (B (TokSym SymSemi) semiBounds :: finalTokens))
                                                   (Uncons Same))
                                              (Data.List.Suffix.trans valueSuffix
                                                (Data.List.Suffix.trans
                                                  (the (Suffix True afterEquals
                                                          (B (TokSym SymEq) equalsBounds :: afterEquals))
                                                       (Uncons Same))
                                                  (Data.List.Suffix.trans typeSuffix
                                                    (Data.List.Suffix.trans
                                                      (the (Suffix True afterColon
                                                              (B (TokSym SymColon) colonBounds :: afterColon))
                                                           (Uncons Same))
                                                      nameSuffix))))}
                                    (B unexpected bounds) :: _ =>
                                      failWithCustomError
                                        (ParseErrorWithMessage
                                          "Expected `;` after const declaration, found instead: `\{interpolate unexpected}`.")
                                        bounds
                                    [] => Fail0 (B EOI NoBounds)
                            (B unexpected bounds) :: _ =>
                              failWithCustomError
                                (ParseErrorWithMessage
                                  "Expected `=` in const declaration, found instead: `\{interpolate unexpected}`.")
                                bounds
                            [] => Fail0 (B EOI NoBounds)
                    (B unexpected bounds) :: _ =>
                      failWithCustomError
                        (ParseErrorWithMessage
                          "Expected `:` after constant name, found instead: `\{interpolate unexpected}`.")
                        bounds
                    [] => Fail0 (B EOI NoBounds)

||| Parses the declaration following `pub`, including effect-qualified and constant functions.
||| Tested by: `pub unitary fn empty() -> () {}`.
parsePubFunDecl : Bounds -> Bounds -> List SurfaceAttribute -> Rule True SurfaceItem
parsePubFunDecl declarationStart pubTokenBounds attributes nodeId [] acc = Fail0 (B EOI NoBounds)
parsePubFunDecl declarationStart pubTokenBounds attributes nodeId ((B token nexTokBounds) :: remaining) acc@(SA recur) =
    let (pubModifierNodeId, nextNodeId) = reserveNodeId nodeId
        pubModifierNode =
                    surfaceAstNode
                        (MkAstInfo pubModifierNodeId (sourceSpan pubTokenBounds))
                        VisibilityPublic
    in case token of
        TokKw KwFn =>
            parseFunDecl declarationStart attributes (Just pubModifierNode) False Nothing nextNodeId (B token nexTokBounds :: remaining) acc

        TokKw KwConst =>
            succT $ parseConstDecl declarationStart attributes
              (Just pubModifierNode) nextNodeId remaining recur

        TokKw keyword =>
          case unsupportedTopLevelItem keyword of
            Just err => failWithCustomError err nexTokBounds
            Nothing =>
              case functionEffectFromKeyword keyword of
                Just effect =>
                  succT $ parseFunDeclWithEffect declarationStart attributes
                    (Just pubModifierNode) effect nexTokBounds nextNodeId remaining recur
                Nothing =>
                  failWithCustomError (ParseErrorWithMessage
                    "Expected function declaration after `pub` visibility modifier, found instead: `\{interpolate token}`.") nexTokBounds

        _ =>
          failWithCustomError (ParseErrorWithMessage
            "Expected function declaration after `pub` visibility modifier, found instead: `\{interpolate token}`.") nexTokBounds

||| Collects consecutive attributes and applies them to the following function declaration.
||| Tested by:
||| `#[qasm_gate]\n#[qasm_def("qasm_subroutine_name")]\npub general fn empty() -> () {}`.
parseAttributedItem : Bounds -> SnocList SurfaceAttribute -> Rule True SurfaceItem
parseAttributedItem _ _ _ [] _ = Fail0 (B EOI NoBounds)
parseAttributedItem declarationStart attributes nodeId
    tokens@((B token bounds) :: remaining) acc@(SA recur) =
  case token of
    TokSym SymHash =>
      case parseAttribute nodeId (B token bounds :: remaining) acc of
        Fail0 err => Fail0 err
        Succ0 (attribute, nextNodeId) afterAttribute =>
          succT $
            parseAttributedItem declarationStart (attributes :< attribute)
              nextNodeId afterAttribute recur

    TokKw KwFn =>
      parseFunDecl declarationStart (attributes <>> []) Nothing False Nothing nodeId
        (B token bounds :: remaining) acc

    TokKw KwConst =>
      succT $ parseConstDecl declarationStart (attributes <>> []) Nothing
        nodeId remaining recur

    TokKw KwPub =>
      succT $ parsePubFunDecl declarationStart bounds (attributes <>> [])
        nodeId remaining recur

    TokKw keyword =>
      case functionEffectFromKeyword keyword of
        Just effect =>
          succT $ parseFunDeclWithEffect declarationStart (attributes <>> [])
            Nothing effect bounds nodeId remaining recur
        Nothing =>
          failWithCustomError (ParseErrorWithMessage
            "Expected function declaration after attribute, found instead: `\{interpolate token}`.") bounds

    _ =>
        failWithCustomError (ParseErrorWithMessage
          "Expected function declaration after attribute, found instead: `\{interpolate token}`.") bounds

||| Dispatches one top-level constant, function, public function, or attributed item.
||| Tested by: `const N: i64 = 4;` and `fn empty() {}`.
parseItem : Rule True SurfaceItem
parseItem nodeId [] acc = Fail0 (B EOI NoBounds)
parseItem nodeId ((B token tokBounds) :: remaining) acc@(SA recur) =
    case token of
    
        -- Start with items that are currently supported by the parser:

        TokKw KwImpl =>
            failWithCustomError (UnsupportedFeature "Impls blocks and structs are not yet supported.") tokBounds

        TokKw KwFn =>
            parseFunDecl tokBounds [] Nothing False Nothing nodeId (B token tokBounds :: remaining) acc

        TokKw KwConst =>
            succT $ parseConstDecl tokBounds [] Nothing nodeId remaining recur

        -- Visibility, documentation, attributes, and function effects that may precede the item keyword:

        TokKw KwPub =>
            succT $ parsePubFunDecl tokBounds tokBounds [] nodeId remaining recur

        TokOuterDoc _ =>
            failWithCustomError (UnsupportedFeature "Outer doc comments are not yet supported.") tokBounds

        TokInnerDoc _ =>
            failWithCustomError (UnsupportedFeature "Inner doc comments are not yet supported.") tokBounds

        TokSym SymHash =>
            parseAttributedItem tokBounds [<] nodeId (B token tokBounds :: remaining) acc

        TokKw keyword =>
            case unsupportedTopLevelItem keyword of
              Just err => failWithCustomError err tokBounds
              Nothing =>
                case functionEffectFromKeyword keyword of
                  Just effect =>
                    succT $ parseFunDeclWithEffect tokBounds [] Nothing effect
                      tokBounds nodeId remaining recur
                  Nothing =>
                    failWithCustomError (UnexpectedToken
                      ("Unexpected token: `" ++ interpolate token ++
                       "` at top level in source file. At module level only only function declarations are allowed for now."))
                      tokBounds

        _ =>
            -- Extend error message with new features when these become available: module declarations, const declarations, structs and/or impl blocks, enums, qenums and inline docs.
            failWithCustomError (UnexpectedToken ("Unexpected token: `" ++ interpolate token ++ "` at top level in source file. At module level only only function declarations are allowed for now.")) tokBounds

||| Parses top-level items until end of input while preserving source order.
||| Items accumulate in a `SnocList` and are converted once at EOF. A valid lexer
||| stream must contain exactly one final `TokEOF`; missing EOF or tokens following
||| EOF are rejected. Each iteration resumes with both the remaining suffix and the
||| next free node ID returned by `parseItem`.
||| Tested by: `const N: i64 = 4;\nfn arrays() { let c: [i32; N]; }`.
parseItems : SnocList SurfaceItem -> Rule False (List SurfaceItem)
parseItems items nextNodeId [] _ =
    Fail0 (B EOI NoBounds)  -- every valid token stream must contain TokEOF
parseItems items nextNodeId [B TokEOF _] (SA recur) =
    Succ0 (items <>> [], nextNodeId) []
parseItems _ _ ((B TokEOF _) :: (B token bounds) :: remaining) _ =
    Fail0 (B EOI bounds)
parseItems items nextNodeId tokens acc@(SA recur) =
    case parseItem nextNodeId tokens acc of
        Fail0 err =>
            Fail0 err

        Succ0 (item, followingNodeId) remaining =>
            succF $ parseItems (items :< item) followingNodeId remaining recur

||| Parses all items into a source-file AST associated with the supplied filename.
||| Tested by the top-module input `const N: i64 = 4;`.
parseModule : String -> Rule False SurfaceSourceFile
parseModule fileName firstItemNodeId tokens acc =
    case parseItems [<] firstItemNodeId tokens acc of
        Fail0 err =>
            Fail0 err

        Succ0 (items, nextNodeId) remaining =>
            Succ0
                ( surfaceAstNode
                    (sourceFileInfo fileName (MkNodeId 0) items)  -- source file node id is always 0
                    (MkSourceFileNode [] items)                   -- ignore inner doc comments for now
                , nextNodeId
                )
                remaining

---------------------------------------------------------------------------------------------------
-- Main entry point: parse file using the idris2-parser library's machinery from Text.Parse.Manual
---------------------------------------------------------------------------------------------------

||| Attaches a filename and converted source span to a parser error.
||| Tested by the malformed input `fn empty(); }`.
locatedParseError : String -> Bounds -> ParseError -> Located ParseError
locatedParseError fileName bounds parseError =
    MkLocated ({ file := fileName } (sourceSpan bounds)) parseError

||| Converts an unexpected bounded token into a located parse failure.
||| Tested by the top-level input `let i = 1;`.
unexpectedLocated : String -> Bounded Token -> Either (Located ParseError) a
unexpectedLocated fileName token =
    case the (Either (Bounded ParseError) a) (Text.ParseError.unexpected token) of
        Left (B err bounds) => Left (locatedParseError fileName bounds err)
        Right result => Right result

public export
||| Runs the module parser over a token stream and returns either a located error or source file.
||| Node ID zero is reserved for the source-file node, so item allocation begins at
||| one. Parser failures are enriched with the filename here. A nominally successful
||| parse is accepted only when it consumes the complete token stream; leftover
||| tokens are converted into an unexpected-token error as a defensive fallback.
||| Tested indirectly by every parser test, for example `fn empty() {}`.
parseFile : String -> List (Bounded Token) -> Either (Located ParseError) SurfaceSourceFile
parseFile fileName tokens =
    case parseModule fileName 1 tokens suffixAcc of   -- first item node id is 1 (0 is source file node id)
        Fail0 (B err bounds) =>
            Left (locatedParseError fileName bounds err)

        Succ0 (sourceFile, _) [] =>
            Right sourceFile

        Succ0 _ ((B token bounds) :: remaining) =>
            unexpectedLocated fileName (B token bounds)
