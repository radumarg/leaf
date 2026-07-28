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

||| Parses zero or more comma-separated elements through a required closing symbol.
||| An immediate closing symbol produces an empty list, and a comma immediately
||| before the closing symbol is accepted as a trailing comma.
parseCommaList :
     (closingSymbol : Symbol)
  -> (closingDescription : String)
  -> (parseElement : Rule True element)
  -> SnocList element
  -> Rule True (CommaList element)
parseCommaList _ _ _ _ _ [] _ = Fail0 (B EOI NoBounds)
parseCommaList closingSymbol closingDescription parseElement parsed nodeId
    ((B token tokenBounds) :: remaining) acc@(SA recur) =
  if token == TokSym closingSymbol
    then Succ0 (MkCommaList (parsed <>> []) tokenBounds, nodeId) remaining
    else parseNext
  where
    parseNext :
      Res True Token (B token tokenBounds :: remaining) CustomParseError
        (CommaList element, Nat)
    parseNext =
      case parseElement nodeId (B token tokenBounds :: remaining) acc of
        Fail0 err => Fail0 err
        Succ0 (elementValue, nextNodeId) afterElement =>
          case afterElement of
            [] => Fail0 (B EOI NoBounds)
            (B (TokSym symbol) symbolBounds) :: afterSymbol =>
              if symbol == closingSymbol
                then Succ0
                  (MkCommaList (parsed <>> [elementValue]) symbolBounds,
                   nextNodeId)
                  afterSymbol
                else if symbol == SymComma
                  then
                    succT $ assert_total $
                      parseCommaList closingSymbol closingDescription
                        parseElement (parsed :< elementValue)
                        nextNodeId afterSymbol suffixAcc
                  else
                    Fail0
                      (B (Expected [",", closingDescription]
                           (show (TokSym symbol)))
                         symbolBounds)
            (B unexpected unexpectedBounds) :: _ =>
              Fail0
                (B (Expected [",", closingDescription] (show unexpected))
                   unexpectedBounds)


||| Parses a complete expression, including ranges and every tighter-precedence form.
||| Tested by: `fn arithmetic() {1 + 2 * 3}`.
parseExpression : Rule True SurfaceExpr

-- Expression parsing has three smaller recursive components.  These declarations
-- expose the deliberately narrow boundaries between precedence parsing,
-- primary/postfix parsing, and block/control-flow parsing.
parseControlExpression : Bounds -> Rule True SurfaceExpr
parseAdjointExpression : Bounds -> Rule True SurfaceExpr
parsePrimaryExpression : Rule True SurfaceExpr
parsePostfixExpression : SurfaceExpr -> Rule False SurfaceExpr
parseBlockExpression : Rule True SurfaceExpr
parseLoopExpression : Rule True SurfaceExpr
parseWhileExpression : Rule True SurfaceExpr
parseForExpression : Rule True SurfaceExpr
parseBreakExpression : Rule True SurfaceExpr
parseContinueExpression : Rule True SurfaceExpr
parseReturnExpression : Rule True SurfaceExpr
parseIfExpression : Rule True SurfaceExpr

||| Parses an identifier into a named AST node with source bounds.
||| Tested by: `fn names() {value; result}`.
parseName : String -> Rule True SurfaceName
parseName _ _ [] acc = Fail0 (B EOI NoBounds)
parseName expectedNameDescription nodeId ((B token bounds) :: remaining) acc =
    case token of
        TokIdent name =>
            Succ0 (makeName name bounds nodeId) remaining

        _ =>
            Fail0 (B (Expected [ expectedNameDescription ] (show token)) bounds)

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
   in case succF $ parseTypePathTail nextNodeId remaining suffixAcc of
        Fail0 err => Fail0 err
        Succ0 (MkTypePathTail segments lastBounds, finalNodeId) finalTokens =>
          let finalBounds = case segments of
                [] => nameBounds
                _ => lastBounds
           in Succ0 (MkTypePathTail (segment :: segments) finalBounds, finalNodeId)
                finalTokens
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
        Succ0 (MkTypePathTail segments lastBounds, finalNodeId) finalTokens =>
          let pathBounds = case segments of
                [] => firstBounds
                _ => firstBounds <+> lastBounds
              path = surfaceAstNode
                (MkAstInfo pathNodeId (sourceSpan pathBounds))
                (MkPathNode firstSegment segments)
              ty = surfaceAstNode
                (MkAstInfo typeNodeId (sourceSpan pathBounds))
                (TyPath path)
           in Succ0 (ty, finalNodeId) finalTokens

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
                located = surfaceAstNode
                  (MkAstInfo qualifierNodeId (sourceSpan bounds)) qualifier
             in case addStorageQualifier emptyStorageQualifiers located of
                  Left message => failWithCustomError
                    (ParseErrorWithMessage message) bounds
                  Right qualifiers =>
                    succT $ parseQualifiedType typeNodeId qualifiers bounds
                      afterQualifierNodeId remaining recur
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

  ||| Parses shared and mutable reference types beginning with `&`.
  ||| Tested by: `fn borrow(person: &Person, mutable: &mut Person) {}`.
  parseReferenceType : NodeId -> Bounds -> Rule True SurfaceTy
  parseReferenceType typeNodeId ampBounds nodeId
      ((B (TokKw KwMut) mutBounds) :: remaining) (SA recur) =
    let (borrowNodeId, afterBorrowNodeId) = reserveNodeId nodeId
        borrowBounds = ampBounds <+> mutBounds
        borrow = surfaceAstNode
          (MkAstInfo borrowNodeId (sourceSpan borrowBounds)) MutableBorrow
     in case succT $ parseType afterBorrowNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (inner, finalNodeId) finalTokens =>
            Succ0
              (surfaceAstNode
                (MkAstInfo typeNodeId
                  (mergeSpans (sourceSpan ampBounds) inner.astInfo.span))
                (TyReference borrow inner),
               finalNodeId)
              finalTokens
  parseReferenceType typeNodeId ampBounds nodeId tokens acc =
    let (borrowNodeId, afterBorrowNodeId) = reserveNodeId nodeId
        borrow = surfaceAstNode
          (MkAstInfo borrowNodeId (sourceSpan ampBounds)) SharedBorrow
     in case parseType afterBorrowNodeId tokens acc of
          Fail0 err => Fail0 err
          Succ0 (inner, finalNodeId) finalTokens =>
            Succ0
              (surfaceAstNode
                (MkAstInfo typeNodeId
                  (mergeSpans (sourceSpan ampBounds) inner.astInfo.span))
                (TyReference borrow inner),
               finalNodeId)
              finalTokens

  ||| Collects consecutive quantum-storage qualifiers and parses their inner type.
  ||| Tested by: `fn qualified(q: affine qubit, pair: (scratch linear qubit, affine qubit)) {}`.
  parseMoreTypeQualifiers :
       NodeId
    -> StorageQualifiers
    -> Bounds
    -> Rule True SurfaceTy
  parseMoreTypeQualifiers typeNodeId qualifiers firstBounds nodeId
      ((B (TokKw keyword) bounds) :: remaining) (SA recur) =
    case storageQualifierFromKeyword keyword of
      Just qualifier =>
        let (qualifierNodeId, nextNodeId) = reserveNodeId nodeId
            located = surfaceAstNode
              (MkAstInfo qualifierNodeId (sourceSpan bounds)) qualifier
         in case addStorageQualifier qualifiers located of
              Left message =>
                failWithCustomError (ParseErrorWithMessage message) bounds
              Right updated =>
                succT $ parseMoreTypeQualifiers typeNodeId updated firstBounds
                  nextNodeId remaining recur
      Nothing =>
        case assert_total $
               parseType nodeId (B (TokKw keyword) bounds :: remaining) (SA recur) of
          Fail0 err => Fail0 err
          Succ0 (inner, finalNodeId) finalTokens =>
            case qualifiers.ordered <>> [] of
              first :: rest =>
                Succ0
                  (surfaceAstNode
                    (MkAstInfo typeNodeId
                      (mergeSpans (sourceSpan firstBounds) inner.astInfo.span))
                    (TyQualified (first ::: rest) inner),
                   finalNodeId)
                  finalTokens
              [] =>
                failWithCustomError
                  (ParseErrorWithMessage
                    "Internal parser error: qualified type has no qualifiers.")
                  firstBounds
  parseMoreTypeQualifiers typeNodeId qualifiers firstBounds nodeId tokens acc =
    case assert_total $ parseType nodeId tokens acc of
      Fail0 err => Fail0 err
      Succ0 (inner, finalNodeId) finalTokens =>
        case qualifiers.ordered <>> [] of
          first :: rest =>
            Succ0
              (surfaceAstNode
                (MkAstInfo typeNodeId
                  (mergeSpans (sourceSpan firstBounds) inner.astInfo.span))
                (TyQualified (first ::: rest) inner),
               finalNodeId)
              finalTokens
          [] =>
            failWithCustomError
              (ParseErrorWithMessage
                "Internal parser error: qualified type has no qualifiers.")
              firstBounds

  ||| Enters qualified-type parsing with the qualifier already consumed.
  ||| Tested by: `fn qualified(q: affine qubit) {}`.
  parseQualifiedType :
       NodeId
    -> StorageQualifiers
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
      Succ0 (name, afterNameNodeId) afterName =>
        case afterName of
          (B (TokSym SymColon) colonBounds :: afterColon) =>
            case succT $ parseType afterNameNodeId afterColon suffixAcc of
              Fail0 err => Fail0 err
              Succ0 (parameterType, finalNodeId) finalTokens =>
                Succ0
                  (surfaceAstNode
                    (MkAstInfo parameterNodeId
                      (mergeSpans name.astInfo.span parameterType.astInfo.span))
                    (MkFunctionTypeParameterNode name parameterType),
                   finalNodeId)
                  finalTokens
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
    -> Rule True (CommaList (SurfaceAstNode (FunctionTypeParameterNode SurfaceExpr)))
  parseFunctionTypeParameterList parsed nodeId tokens acc =
    assert_total $
      parseCommaList SymRParen ")" parseFunctionTypeParameter parsed nodeId tokens acc

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
    case succT $ parseFunctionTypeParameterList [<] nodeId remaining recur of
      Fail0 err => Fail0 err
      Succ0 (MkCommaList functionParams closeBounds, afterParamsNodeId)
            afterParams =>
        case afterParams of
          (B (TokSym SymArrow) arrowBounds :: afterArrow) =>
            case succT $ assert_total $ parseType afterParamsNodeId afterArrow suffixAcc of
              Fail0 err => Fail0 err
              Succ0 (returnType, finalNodeId) finalTokens =>
                Succ0
                  (surfaceAstNode
                    (MkAstInfo typeNodeId
                      (mergeSpans (sourceSpan startBounds) returnType.astInfo.span))
                    (TyFunction effect functionParams (Just returnType)),
                   finalNodeId)
                  finalTokens
          _ =>
            Succ0
              (surfaceAstNode
                (MkAstInfo typeNodeId
                  (sourceSpan (startBounds <+> closeBounds)))
                (TyFunction effect functionParams Nothing),
               afterParamsNodeId)
              afterParams
  parseFunctionType _ _ _ _ ((B token bounds) :: _) _ =
    Fail0 (B (Expected ["`(` after `fn`"] (show token)) bounds)

  ||| Parses slice types `[T]` and fixed-length array types `[T; expression]`.
  ||| Tested by: `fn arrays() { let b: [i32; 2 + 2]; }`.
  parseArrayType : NodeId -> Bounds -> Rule True SurfaceTy
  parseArrayType _ _ _ [] _ = Fail0 (B EOI NoBounds)
  parseArrayType arrayNodeId openBounds nodeId tokens acc =
    case parseType nodeId tokens acc of
      Fail0 err => Fail0 err
      Succ0 (elementType, afterElementNodeId) afterElement =>
        case afterElement of
          [] => Fail0 (B EOI NoBounds)
          (B (TokSym SymRBracket) closeBounds) :: finalTokens =>
            Succ0
              (surfaceAstNode
                (MkAstInfo arrayNodeId
                  (sourceSpan (openBounds <+> closeBounds)))
                (TySlice elementType),
               afterElementNodeId)
              finalTokens
          _ :: _ =>
            case succT $ (exact (TokSym SymSemi) *>
                  Text.Parse.Manual.acc
                    (assert_total $ parseExpression afterElementNodeId)) afterElement of
              Fail0 err => Fail0 err
              Succ0 (length, finalNodeId) afterLength =>
                case afterLength of
                  [] => Fail0 (B EOI NoBounds)
                  (B (TokSym SymRBracket) closeBounds) :: finalTokens =>
                    Succ0
                      (surfaceAstNode
                        (MkAstInfo arrayNodeId
                          (sourceSpan (openBounds <+> closeBounds)))
                        (TyArray elementType length),
                       finalNodeId)
                      finalTokens
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
    case parseType nodeId tokens acc of
      Fail0 err => Fail0 err
      Succ0 (firstType, afterFirstNodeId) afterFirst =>
        case afterFirst of
          [] => Fail0 (B EOI NoBounds)
          (B (TokSym SymRParen) closeBounds) :: remaining =>
            Succ0
              (surfaceAstNode
                (MkAstInfo typeNodeId (sourceSpan (openBounds <+> closeBounds)))
                (TyParenthesized firstType),
               afterFirstNodeId)
              remaining
          (B (TokSym SymComma) commaBounds) :: afterComma =>
            case succT $ parseTupleTail afterFirstNodeId afterComma suffixAcc of
              Fail0 err => Fail0 err
              Succ0 (tail, finalNodeId) finalTokens =>
                Succ0
                  (surfaceAstNode
                    (MkAstInfo typeNodeId
                      (sourceSpan (openBounds <+> tail.closeBounds)))
                    (TyTuple (firstType ::: tail.values)),
                   finalNodeId)
                  finalTokens
          (B unexpected unexpectedBounds) :: _ =>
            Fail0 (B (Expected [",", ")"] (show unexpected)) unexpectedBounds)

  ||| Parses the remaining comma-separated elements and closing `)` of a tuple type.
  ||| This helper starts after the tuple's first type and therefore also handles the
  ||| trailing-comma case. It consumes the closing delimiter and returns its bounds
  ||| separately, allowing `parseParenType` to distinguish grouping from tuple
  ||| syntax and to span the complete tuple.
  ||| Tested by: `fn qualified(pair: (scratch linear qubit, affine qubit)) {}`.
  parseTupleTail : Rule True (CommaList SurfaceTy)
  parseTupleTail nodeId tokens acc =
    assert_total $ parseCommaList SymRParen ")" parseType [<] nodeId tokens acc

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
        Succ0 (docs, afterDocsNodeId) afterDocs @{docsSuffix} :=
              parseParameterDocComments nextNodeId tokens acc
          | Fail0 err => Fail0 err
        Succ0 (mutability, afterMutabilityNodeId) afterMutability @{mutabilitySuffix} :=
              parseParameterMutability afterDocsNodeId afterDocs suffixAcc
          | Fail0 err => Fail0 err
        Succ0 (name, afterNameNodeId) afterName @{nameSuffix} :=
              parseName "parameter name" afterMutabilityNodeId afterMutability suffixAcc
          | Fail0 err => Fail0 err
        _ :: _ := afterName
          | [] => Fail0 (B EOI NoBounds)
        Succ0 (parameterType, finalNodeId) finalTokens @{typeSuffix} :=
              (exact (TokSym SymColon) *>
                    Text.Parse.Manual.acc (parseType afterNameNodeId)) afterName
          | Fail0 err => Fail0 err
        parameterSpan =
              mergeSpans
                (parameterStartSpan docs mutability name)
                parameterType.astInfo.span
        parameter =
              surfaceAstNode
                (MkAstInfo parameterNodeId parameterSpan)
                (NormalParameter docs mutability name parameterType)
     in Succ0 (parameter, finalNodeId) finalTokens
          @{trans typeSuffix $ trans nameSuffix $ trans mutabilitySuffix docsSuffix}

||| Parses comma-separated function parameters until the closing `)`.
||| Parsed parameters accumulate in a `SnocList`, then become an ordinary list only
||| at the delimiter. The helper consumes both commas and the closing parenthesis;
||| after a comma, failure to parse another parameter is an error rather than an
||| empty-list success.
||| Tested by: `fn add(i: i32, point: (i32, i32)) {}`.
parseFunctionParameterList :
    SnocList (SurfaceAstNode FunctionParameterNode) ->
    Rule True (List (SurfaceAstNode FunctionParameterNode))
parseFunctionParameterList parsed nodeId tokens acc =
  case parseCommaList SymRParen ")" parseFunctionParameter parsed
         nodeId tokens acc of
    Fail0 err => Fail0 err
    Succ0 (commaList, finalNodeId) finalTokens =>
      Succ0 (commaList.values, finalNodeId) finalTokens

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
            case succF $ parseType nodeId remaining recur of
                Fail0 err => Fail0 err
                Succ0 (returnType, nextNodeId) finalTokens =>
                    Succ0 (Just returnType, nextNodeId) finalTokens
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
     StorageQualifiers
  -> Rule False (List (SurfaceAstNode QuantumStorageQualifier))
parseLetQualifiers qualifiers nodeId
    ((B (TokKw keyword) qualifierBounds) :: remaining) (SA recur) =
  case storageQualifierFromKeyword keyword of
    Just qualifier => consume qualifier
    Nothing => Succ0 (qualifiers.ordered <>> [], nodeId)
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
       in case addStorageQualifier qualifiers located of
            Left message =>
              failWithCustomError (ParseErrorWithMessage message) qualifierBounds
            Right updated =>
              succF $
                parseLetQualifiers updated nextNodeId remaining recur
parseLetQualifiers qualifiers nodeId tokens _ =
  Succ0 (qualifiers.ordered <>> [], nodeId) tokens @{Same}

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
        (name, nextNodeId) = makeName text bounds afterPatternNodeId
     in Succ0
          (surfaceAstNode (MkAstInfo patternNodeId (sourceSpan bounds))
            (PatternName Nothing name),
           nextNodeId)
          remaining
  parsePattern nodeId
      ((B (TokKw KwMut) mutBounds) ::
       (B (TokIdent text) nameBounds) :: remaining) _ =
    let (patternNodeId, afterPatternNodeId) = reserveNodeId nodeId
        (name, nextNodeId) = makeName text nameBounds afterPatternNodeId
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
     in case succT $ parsePattern afterPatternNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (first, afterFirstNodeId) afterFirst =>
            case afterFirst of
              (B (TokSym SymRParen) closeBounds) :: finalTokens =>
                Succ0
                  (surfaceAstNode
                    (MkAstInfo patternNodeId
                      (sourceSpan (openBounds <+> closeBounds)))
                    (PatternParenthesized first),
                   afterFirstNodeId)
                  finalTokens
              (B (TokSym SymComma) commaBounds) :: afterComma =>
                case succT $
                           parseTuplePatternTail afterFirstNodeId afterComma suffixAcc of
                  Fail0 err => Fail0 err
                  Succ0 (tail, finalNodeId) finalTokens =>
                    Succ0
                      (surfaceAstNode
                        (MkAstInfo patternNodeId
                          (sourceSpan (openBounds <+> tail.closeBounds)))
                        (PatternTuple (first ::: tail.values)),
                       finalNodeId)
                      finalTokens
              (B unexpected bounds) :: _ =>
                Fail0 (B (Expected [",", ")"] (show unexpected)) bounds)
              [] => Fail0 (B EOI NoBounds)
  parsePattern nodeId
      ((B (TokSym SymLBracket) openBounds) :: remaining) (SA recur) =
    let (patternNodeId, afterPatternNodeId) = reserveNodeId nodeId
     in case succT $ parseArrayPatternElements afterPatternNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (elements, finalNodeId) finalTokens =>
            Succ0
              (surfaceAstNode
                (MkAstInfo patternNodeId
                  (sourceSpan (openBounds <+> elements.closeBounds)))
                (PatternArray elements.values),
               finalNodeId)
              finalTokens
  parsePattern _ ((B token bounds) :: _) _ =
    Fail0 (B (Expected ["a pattern"] (show token)) bounds)

  ||| Parses the remaining elements and closing `)` of a tuple pattern.
  ||| Tested by: `fn destructure() {let (a, b, c) = (1, 2, 3);}`.
  parseTuplePatternTail : Rule True (CommaList SurfacePattern)
  parseTuplePatternTail nodeId tokens acc =
    assert_total $ parseCommaList SymRParen ")" parsePattern [<] nodeId tokens acc

  ||| Parses the comma-separated elements and closing `]` of an array pattern.
  ||| Tested by: `fn measure() {let [b0, b1, b2]: [bit; 3] = measr(qs);}`.
  parseArrayPatternElements : Rule True (CommaList SurfacePattern)
  parseArrayPatternElements nodeId tokens acc =
    assert_total $ parseCommaList SymRBracket "]" parsePattern [<] nodeId tokens acc

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

||| Requires and parses a braced block used as a function body or block-like construct.
||| Tested by: `fn empty() {}`.
parseBracedBlock : Rule True SurfaceBlock

||| Falls through to primary and postfix parsing once no unary prefix
||| operator is present. Shared by the operator dispatch below and by the
||| final catch-all clause, so the fallback is written exactly once.
||| Tested by: `fn postfix() {values()[i].field.len()}`.
parsePrimaryAndPostfix : Rule True SurfaceExpr
parsePrimaryAndPostfix nodeId tokens acc =
  case assert_total $ parsePrimaryExpression nodeId tokens acc of
    Fail0 err => Fail0 err
    Succ0 (primary, afterPrimaryNodeId) afterPrimary =>
      succT $
                 parsePostfixExpression primary afterPrimaryNodeId
                   afterPrimary suffixAcc

mutual
  ||| Builds a unary-operator expression node around an already-classified
  ||| operator and recurses to parse its operand. Shared by every unary
  ||| prefix form so each `parseUnaryExpression` clause only has to say which
  ||| operator it matched and how many tokens that took.
  ||| Tested by: `fn unary() {-x; !x; &x; &mut x}`.
  parseUnaryOperand : Bounds -> UnaryOperator -> Rule True SurfaceExpr
  parseUnaryOperand operatorBounds operatorValue nodeId remaining recur =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
        (operatorNodeId, afterOperatorNodeId) = reserveNodeId afterExpressionNodeId
        operator = surfaceAstNode
          (MkAstInfo operatorNodeId (sourceSpan operatorBounds)) operatorValue
     in case parseUnaryExpression afterOperatorNodeId remaining recur of
          Fail0 err => Fail0 err
          Succ0 (operand, finalNodeId) finalTokens =>
            Succ0
              (surfaceAstNode
                (MkAstInfo expressionNodeId
                  (mergeSpans (sourceSpan operatorBounds) operand.astInfo.span))
                (ExprUnary operator operand),
               finalNodeId)
              finalTokens

  ||| Parses prefix negation, logical not, shared borrow, mutable borrow, or a postfix expression.
  ||| `&mut` is matched here directly since it is the only two-token prefix;
  ||| every single-token prefix operator is dispatched through `unaryOperator`.
  ||| Tested by: `fn unary() {-x; !x; &x; &mut x}`.
  parseUnaryExpression : Rule True SurfaceExpr
  parseUnaryExpression _ [] _ = Fail0 (B EOI NoBounds)
  parseUnaryExpression nodeId
      ((B (TokSym SymAmp) ampBounds) ::
       (B (TokKw KwMut) mutBounds) :: remaining) (SA recur) =
    succT $ parseUnaryOperand (ampBounds <+> mutBounds)
              (UnaryBorrow MutableBorrow) nodeId remaining recur
  parseUnaryExpression nodeId
      ((B (TokSym symbol) operatorBounds) :: remaining) (SA recur) =
    case unaryOperator symbol of
      Just operatorValue =>
        succT $ parseUnaryOperand operatorBounds operatorValue nodeId remaining recur
      Nothing =>
        parsePrimaryAndPostfix nodeId
          (B (TokSym symbol) operatorBounds :: remaining) (SA recur)
  parseUnaryExpression nodeId tokens acc =
    parsePrimaryAndPostfix nodeId tokens acc

||| Repeatedly attaches `as type` casts to an existing operand.
||| Tested by: `fn casts() {value as i32 as i64}`.
parseCastExpressionRest : SurfaceExpr -> Rule False SurfaceExpr
parseCastExpressionRest operand nodeId
    ((B (TokKw KwAs) asBounds) :: afterAs) (SA recur) =
  let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
   in case succT $ parseType afterExpressionNodeId afterAs recur of
        Fail0 err => Fail0 err
        Succ0 (targetType, afterTypeNodeId) afterType =>
          let expression = surfaceAstNode
                (MkAstInfo expressionNodeId
                  (mergeSpans operand.astInfo.span targetType.astInfo.span))
                (ExprCast operand targetType)
           in succF $ assert_total $
                     parseCastExpressionRest expression afterTypeNodeId afterType suffixAcc
parseCastExpressionRest operand nodeId tokens _ =
  Succ0 (operand, nodeId) tokens @{Same}

||| Parses a unary expression followed by zero or more `as type` casts.
||| Tested by: `fn casts() {x as i32; value as i32 as i64}`.
parseCastExpression : Rule True SurfaceExpr
parseCastExpression nodeId tokens acc =
  case parseUnaryExpression nodeId tokens acc of
    Fail0 err => Fail0 err
    Succ0 (operand, afterOperandNodeId) afterOperand =>
      succT $ parseCastExpressionRest operand afterOperandNodeId afterOperand suffixAcc

mutual
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
      Succ0 (left, afterLeftNodeId) afterLeft =>
        succT $ parseBinaryExpressionRest minimumPrecedence left
               afterLeftNodeId afterLeft suffixAcc

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
             in case succT $ assert_total $
                       parseBinaryExpression (S precedence) afterOperatorNodeId
                         afterOperator recur of
                  Fail0 err => Fail0 err
                  Succ0 (right, afterRightNodeId) afterRight =>
                    let expression = surfaceAstNode
                          (MkAstInfo expressionNodeId
                            (mergeSpans left.astInfo.span right.astInfo.span))
                          (ExprBinary operator left right)
                     in succF $ assert_total $
                               parseBinaryExpressionRest minimumPrecedence expression
                                 afterRightNodeId afterRight suffixAcc
  parseBinaryExpressionRest _ left nodeId tokens _ =
    Succ0 (left, nodeId) tokens @{Same}

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
   in if nextTokenSatisfies isOpenRangeTerminator remaining
        then finishOpenRange expressionNodeId afterOperatorNodeId operator
        else
          case succF $ parseBinaryExpression 0 afterOperatorNodeId remaining recur of
            Fail0 err => Fail0 err
            Succ0 (end, finalNodeId) finalTokens =>
              Succ0
                (surfaceAstNode
                  (MkAstInfo expressionNodeId
                    (mergeSpans start.astInfo.span end.astInfo.span))
                  (ExprRange (Just start) operator (Just end)),
                 finalNodeId)
                finalTokens
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
   in case succF $ parseBinaryExpression 0 afterOperatorNodeId remaining recur of
        Fail0 err => Fail0 err
        Succ0 (end, finalNodeId) finalTokens =>
          Succ0
            (surfaceAstNode
              (MkAstInfo expressionNodeId
                (mergeSpans start.astInfo.span end.astInfo.span))
              (ExprRange (Just start) operator (Just end)),
             finalNodeId)
            finalTokens
parseRangeExpressionRest start nodeId tokens _ =
  Succ0 (start, nodeId) tokens @{Same}

||| Parses a range expression or delegates to binary-expression parsing.
||| Tested by: `fn ranges() {1..5; 1..; ..5; ..=5; ..}`.
parseRangeExpression : Rule True SurfaceExpr
parseRangeExpression nodeId
    ((B (TokSym SymDotDot) operatorBounds) :: remaining) acc@(SA recur) =
  let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
      (operatorNodeId, afterOperatorNodeId) = reserveNodeId afterExpressionNodeId
      operator = surfaceAstNode
        (MkAstInfo operatorNodeId (sourceSpan operatorBounds)) RangeExclusive
   in if nextTokenSatisfies isOpenRangeTerminator remaining
        then
          Succ0
            (surfaceAstNode
              (MkAstInfo expressionNodeId (sourceSpan operatorBounds))
              (ExprRange Nothing operator Nothing),
             afterOperatorNodeId)
            remaining
        else
          case succT $ parseBinaryExpression 0 afterOperatorNodeId remaining recur of
            Fail0 err => Fail0 err
            Succ0 (end, finalNodeId) finalTokens =>
              Succ0
                (surfaceAstNode
                  (MkAstInfo expressionNodeId
                    (mergeSpans (sourceSpan operatorBounds) end.astInfo.span))
                  (ExprRange Nothing operator (Just end)),
                 finalNodeId)
                finalTokens
parseRangeExpression nodeId
    ((B (TokSym SymDotDotEq) operatorBounds) :: remaining) (SA recur) =
  let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
      (operatorNodeId, afterOperatorNodeId) = reserveNodeId afterExpressionNodeId
      operator = surfaceAstNode
        (MkAstInfo operatorNodeId (sourceSpan operatorBounds)) RangeInclusive
   in case succT $ parseBinaryExpression 0 afterOperatorNodeId remaining recur of
        Fail0 err => Fail0 err
        Succ0 (end, finalNodeId) finalTokens =>
          Succ0
            (surfaceAstNode
              (MkAstInfo expressionNodeId
                (mergeSpans (sourceSpan operatorBounds) end.astInfo.span))
              (ExprRange Nothing operator (Just end)),
             finalNodeId)
            finalTokens
parseRangeExpression nodeId tokens acc =
  case parseBinaryExpression 0 nodeId tokens acc of
    Fail0 err => Fail0 err
    Succ0 (start, afterStartNodeId) afterStart =>
      succT $ parseRangeExpressionRest start afterStartNodeId afterStart suffixAcc

parseExpression = parseRangeExpression

||| Continues an expression whose primary node has already been parsed.
||| Postfix operations bind first, followed by casts, binary operators, and
||| finally a possible range. Statement parsing uses this for callable `ctrl` and
||| `adjoint` forms only; their block forms deliberately bypass continuation.
parseExpressionContinuation : SurfaceExpr -> Rule False SurfaceExpr
parseExpressionContinuation primary nodeId tokens acc =
  let Succ0 (postfix, afterPostfixNodeId) afterPostfix @{postfixSuffix} :=
            parsePostfixExpression primary nodeId tokens acc
        | Fail0 err => Fail0 err
      Succ0 (cast, afterCastNodeId) afterCast @{castSuffix} :=
            parseCastExpressionRest postfix afterPostfixNodeId afterPostfix suffixAcc
        | Fail0 err => Fail0 err
      Succ0 (binary, afterBinaryNodeId) afterBinary @{binarySuffix} :=
            parseBinaryExpressionRest 0 cast afterCastNodeId afterCast suffixAcc
        | Fail0 err => Fail0 err
      Succ0 result finalTokens @{rangeSuffix} :=
            parseRangeExpressionRest binary afterBinaryNodeId afterBinary suffixAcc
        | Fail0 err => Fail0 err
   in Succ0 result finalTokens
        @{trans rangeSuffix $ trans binarySuffix $ trans castSuffix postfixSuffix}

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
  case succT $ parseControlExpression bounds nodeId remaining recur of
    Fail0 err => Fail0 err
    Succ0 (control, afterControlNodeId) afterControl =>
      if isBlockLikeExpression control
        then Succ0 (control, afterControlNodeId) afterControl
        else succT $ parseExpressionContinuation control afterControlNodeId
                 afterControl suffixAcc
parseStatementExpression nodeId
    ((B (TokKw KwAdjoint) bounds) :: remaining) (SA recur) =
  case succT $ parseAdjointExpression bounds nodeId remaining recur of
    Fail0 err => Fail0 err
    Succ0 (adjoint, afterAdjointNodeId) afterAdjoint =>
      if isBlockLikeExpression adjoint
        then Succ0 (adjoint, afterAdjointNodeId) afterAdjoint
        else succT $ parseExpressionContinuation adjoint afterAdjointNodeId
                 afterAdjoint suffixAcc
parseStatementExpression nodeId tokens acc =
  parseExpression nodeId tokens acc

||| Builds a supported literal primary after its token has been consumed.
parseLiteralExpression : Token -> Bounds -> Rule False SurfaceExpr
parseLiteralExpression token bounds nodeId tokens _ =
  case token of
    TokIntLitRaw rawText =>
      Succ0 (makeLiteralExpression (LiteralIntegerRaw rawText) bounds nodeId) tokens
    TokFloatLitRaw rawText =>
      Succ0 (makeLiteralExpression (LiteralFloatRaw rawText) bounds nodeId) tokens
    TokBoolLit value =>
      Succ0 (makeLiteralExpression (LiteralBoolean value) bounds nodeId) tokens
    TokStringLitRaw rawText =>
      Succ0 (makeLiteralExpression (LiteralStringRaw rawText) bounds nodeId) tokens
    TokBasisStringLitRaw rawText =>
      Succ0
        (makeLiteralExpression (LiteralBasisStringRaw rawText) bounds nodeId)
        tokens
    _ => Fail0 (B (Expected ["a literal expression"] (show token)) bounds)

||| Reports unsupported primary forms and the general primary-expression error.
parseUnsupportedPrimary : Token -> Bounds -> Rule True SurfaceExpr
parseUnsupportedPrimary token bounds _ tokens _ =
  case token of
    TokKw KwQif =>
      unsupported "Quantum if expressions are not yet supported."
    TokKw KwMatch =>
      unsupported "Match expressions are not yet supported."
    TokKw KwQmatch =>
      unsupported "Quantum match expressions are not yet supported."
    TokKw KwSif =>
      unsupported "State if expressions are not yet supported."
    TokKw KwSmatch =>
      unsupported "State match expressions are not yet supported."
    TokKw KwSelf =>
      unsupported "Self expressions are not yet supported."
    TokByteLitRaw _ =>
      unsupported "Byte literals are not yet supported."
    TokByteStringLitRaw _ =>
      unsupported "Byte string literals are not yet supported."
    TokStateLit _ =>
      unsupported "State literals are not yet supported."
    _ => Fail0 (B (Expected ["an expression"] (show token)) bounds)
  where
    unsupported : String -> Res True Token tokens CustomParseError (SurfaceExpr, Nat)
    unsupported message =
      failWithCustomError (UnsupportedFeature message) bounds

||| Parses comma-separated call arguments and their closing parenthesis.
||| Empty argument lists succeed on an immediate `)`. Otherwise each expression
||| must be followed by `,` or `)`; after a comma recursion also permits a trailing
||| comma. The closing bounds are returned with the values so the call node can
||| cover its full source range.
||| Tested by: `fn calls() {f(); f(x, y)}`.
parseCallArguments : Rule True (CommaList SurfaceExpr)
parseCallArguments nodeId tokens acc =
  assert_total $ parseCommaList SymRParen ")" parseExpression [<] nodeId tokens acc

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
   in succT $ parseControlAfterControls expressionNodeId ctrlBounds controls
               (Just basis) nextNodeId remaining suffixAcc
parseControlAfterControls expressionNodeId ctrlBounds controls onBasis
    nodeId
    ((B (TokSym SymDot) dotBounds) ::
     (B (TokBuiltin BuiltinApply) applyBounds) ::
     (B (TokSym SymLParen) openBounds) :: afterOpen) (SA recur) =
  case succT $ parseExpression nodeId afterOpen recur of
    Fail0 err => Fail0 err
    Succ0 (callable, afterCallableNodeId) afterCallable =>
      case afterCallable of
        (B (TokSym SymRParen) closeBounds) :: remaining =>
          Succ0
            (surfaceAstNode
              (MkAstInfo expressionNodeId
                (sourceSpan (ctrlBounds <+> closeBounds)))
              (ExprCtrl (ControlledCallable controls onBasis callable)),
             afterCallableNodeId)
            remaining
        (B unexpected unexpectedBounds) :: _ =>
          Fail0 (B (Expected [")"] (show unexpected)) unexpectedBounds)
        [] => Fail0 (B EOI NoBounds)
parseControlAfterControls expressionNodeId ctrlBounds controls onBasis
    nodeId tokens@((B (TokSym SymLBrace) _) :: _) acc =
  case parseBracedBlock nodeId tokens acc of
    Fail0 err => Fail0 err
    Succ0 (body, finalNodeId) finalTokens =>
      Succ0
        (surfaceAstNode
          (MkAstInfo expressionNodeId
            (mergeSpans (sourceSpan ctrlBounds) body.astInfo.span))
          (ExprCtrl (ControlledBlock controls onBasis body)),
         finalNodeId)
        finalTokens
parseControlAfterControls _ _ _ _ _ [] _ = Fail0 (B EOI NoBounds)
parseControlAfterControls _ _ _ _ _
    ((B token bounds) :: _) _ =
  Fail0
    (B
      (Expected ["`.apply(...)` or a controlled block"] (show token))
      bounds)

-- Parses `ctrl(...)`, including its controls and optional basis clause.
-- Tested by: `fn f() {ctrl(&q0, &q1).on(bs"10").apply(H)(&q2)}`.
parseControlExpression _ _ [] _ = Fail0 (B EOI NoBounds)
parseControlExpression ctrlBounds nodeId
    ((B (TokSym SymLParen) openBounds) :: afterOpen) (SA recur) =
  let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
   in case succT $ parseCallArguments afterExpressionNodeId afterOpen recur of
        Fail0 err => Fail0 err
        Succ0 (MkCommaList [] closeBounds, _) _ =>
          failWithCustomError
            (ParseErrorWithMessage
              "`ctrl` requires at least one control qubit.")
            (ctrlBounds <+> closeBounds)
        Succ0 (MkCommaList (first :: rest) closeBounds, afterControlsNodeId)
              afterControls =>
          succT $ parseControlAfterControls expressionNodeId ctrlBounds
                   (first ::: rest) Nothing afterControlsNodeId
                   afterControls suffixAcc
parseControlExpression _ _ ((B token bounds) :: _) _ =
  Fail0 (B (Expected ["`(` after `ctrl`"] (show token)) bounds)

-- Parses adjoint callable syntax `adjoint(f)` or an `adjoint { ... }` block.
-- Tested by: `fn f() {adjoint(f)(q1, q2, q3)}` and
-- `fn f() {adjoint {H(&q1); CT(&q1, &q2)}}`.
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
   in case succT $
             parseExpression afterExpressionNodeId afterOpen recur of
        Fail0 err => Fail0 err
        Succ0 (callable, afterCallableNodeId) afterCallable =>
          case afterCallable of
            (B (TokSym SymRParen) closeBounds) :: remaining =>
              Succ0
                (surfaceAstNode
                  (MkAstInfo expressionNodeId
                    (sourceSpan (adjointBounds <+> closeBounds)))
                  (ExprAdjoint (AdjointOfCallable callable)),
                 afterCallableNodeId)
                remaining
            (B unexpected unexpectedBounds) :: _ =>
              Fail0 (B (Expected [")"] (show unexpected)) unexpectedBounds)
            [] => Fail0 (B EOI NoBounds)
parseAdjointExpression adjointBounds nodeId
    ((B (TokSym SymLBrace) openBounds) :: remaining) acc@(SA recur) =
  let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
   in case parseBracedBlock afterExpressionNodeId
               (B (TokSym SymLBrace) openBounds :: remaining) acc of
        Fail0 err => Fail0 err
        Succ0 (body, finalNodeId) finalTokens =>
          Succ0
            (surfaceAstNode
              (MkAstInfo expressionNodeId
                (mergeSpans (sourceSpan adjointBounds) body.astInfo.span))
              (ExprAdjoint (AdjointBlock body)),
             finalNodeId)
            finalTokens
parseAdjointExpression _ _ ((B token bounds) :: _) _ =
  Fail0
    (B
      (Expected ["`(` or `{` after `adjoint`"] (show token))
      bounds)

||| Parses the remaining comma-separated values and closing `)` of a tuple expression.
||| Tested by: `fn tuples() {(1, true); (1, (2, 3),)}`.
parseExpressionTupleTail : Rule True (CommaList SurfaceExpr)
parseExpressionTupleTail nodeId tokens acc =
  parseCommaList SymRParen ")" parseExpression [<] nodeId tokens acc

||| Parses unit, parenthesized, and tuple expressions after their opening `(`.
parseParenOrTupleExpression : Bounds -> Rule True SurfaceExpr
parseParenOrTupleExpression _ _ [] _ = Fail0 (B EOI NoBounds)
parseParenOrTupleExpression openBounds nodeId
    ((B (TokSym SymRParen) closeBounds) :: remaining) _ =
  Succ0 (makeLiteralExpression LiteralUnit (openBounds <+> closeBounds) nodeId)
    remaining
parseParenOrTupleExpression openBounds nodeId tokens (SA recur) =
  let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
      Succ0 (first, afterFirstNodeId) afterFirst :=
            parseExpression afterExpressionNodeId tokens (SA recur)
        | Fail0 err => Fail0 err
   in case afterFirst of
        (B (TokSym SymRParen) closeBounds) :: finalTokens =>
          Succ0
            (surfaceAstNode
              (MkAstInfo expressionNodeId
                (sourceSpan (openBounds <+> closeBounds)))
              (ExprParenthesized first),
             afterFirstNodeId)
            finalTokens
        (B (TokSym SymComma) _) :: afterComma =>
          let Succ0 (tail, finalNodeId) finalTokens :=
                    succT $
                      parseExpressionTupleTail afterFirstNodeId afterComma suffixAcc
              | Fail0 err => Fail0 err
           in Succ0
                (surfaceAstNode
                  (MkAstInfo expressionNodeId
                    (sourceSpan (openBounds <+> tail.closeBounds)))
                  (ExprTuple (first ::: tail.values)),
                 finalNodeId)
                finalTokens
        (B unexpected unexpectedBounds) :: _ =>
          Fail0 (B (Expected [",", ")"] (show unexpected)) unexpectedBounds)
        [] => Fail0 (B EOI NoBounds)

||| Parses comma-separated array elements and their closing bracket.
||| Its delimiter protocol mirrors `parseCallArguments`: an immediate `]` is an
||| empty array, commas recurse and allow a trailing comma, and the closing bounds
||| are retained for the array expression's span.
||| Tested by: `fn arrays() {[]; [1, 2, 3]; [1, 2, 3,]}`.
parseArrayElements : Rule True (CommaList SurfaceExpr)
parseArrayElements nodeId tokens acc =
  parseCommaList SymRBracket "]" parseExpression [<] nodeId tokens acc

||| Parses array and repeated-array expressions after their opening `[`.
parseArrayExpression : Bounds -> Rule True SurfaceExpr
parseArrayExpression _ _ [] _ = Fail0 (B EOI NoBounds)
parseArrayExpression openBounds nodeId
    ((B (TokSym SymRBracket) closeBounds) :: remaining) _ =
  let (expressionNodeId, nextNodeId) = reserveNodeId nodeId
      expression = surfaceAstNode
        (MkAstInfo expressionNodeId (sourceSpan (openBounds <+> closeBounds)))
        (ExprArray [])
   in Succ0 (expression, nextNodeId) remaining
parseArrayExpression openBounds nodeId tokens (SA recur) =
  let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
      Succ0 (first, afterFirstNodeId) afterFirst :=
            parseExpression afterExpressionNodeId tokens (SA recur)
        | Fail0 err => Fail0 err
   in case afterFirst of
        (B (TokSym SymRBracket) closeBounds) :: finalTokens =>
          Succ0
            (surfaceAstNode
              (MkAstInfo expressionNodeId
                (sourceSpan (openBounds <+> closeBounds)))
              (ExprArray [first]),
             afterFirstNodeId)
            finalTokens
        (B (TokSym SymComma) _) :: afterComma =>
          let Succ0 (tail, finalNodeId) finalTokens :=
                    succT $ parseArrayElements afterFirstNodeId afterComma suffixAcc
              | Fail0 err => Fail0 err
           in Succ0
                (surfaceAstNode
                  (MkAstInfo expressionNodeId
                    (sourceSpan (openBounds <+> tail.closeBounds)))
                  (ExprArray (first :: tail.values)),
                 finalNodeId)
                finalTokens
        (B (TokSym SymSemi) _) :: afterSemi =>
          let Succ0 (count, finalNodeId) afterCount :=
                    succT $
                      parseExpression afterFirstNodeId afterSemi suffixAcc
              | Fail0 err => Fail0 err
              (B (TokSym SymRBracket) closeBounds) :: finalTokens := afterCount
                | (B unexpected unexpectedBounds) :: _ =>
                    Fail0 (B (Expected ["]"] (show unexpected)) unexpectedBounds)
                | [] => Fail0 (B EOI NoBounds)
           in Succ0
                (surfaceAstNode
                  (MkAstInfo expressionNodeId
                    (sourceSpan (openBounds <+> closeBounds)))
                  (ExprRepeatedArray first count),
                 finalNodeId)
                finalTokens
        (B unexpected unexpectedBounds) :: _ =>
          Fail0 (B (Expected [",", ";", "]"] (show unexpected)) unexpectedBounds)
        [] => Fail0 (B EOI NoBounds)

-- Dispatches literals, names, builtins, grouped values, collections, control flow,
-- quantum modifiers, and other primary expression forms.
-- This is where delimiter-sensitive ambiguities are resolved: `()` versus a
-- grouped expression versus a tuple, and an array literal versus `[value; count]`.
-- It creates the outer AST node before recursively parsing children, so node IDs
-- follow source-tree pre-order even when the child parser is mutually recursive.
-- Tested by: `fn booleans() {true; false}`.
parsePrimaryExpression _ [] _ = Fail0 (B EOI NoBounds)
parsePrimaryExpression nodeId
    ((B (TokSym SymLParen) openBounds) :: remaining) (SA recur) =
  succT $ parseParenOrTupleExpression openBounds nodeId remaining recur
parsePrimaryExpression nodeId
    ((B (TokSym SymLBracket) openBounds) :: remaining) (SA recur) =
  succT $ parseArrayExpression openBounds nodeId remaining recur
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
    TokIdent nameText =>
      case remaining of
        braceTokens@((B (TokSym SymLBrace) braceBounds) :: afterBrace) =>
          if startsWithUppercase nameText
            then failWithCustomError
                   (UnsupportedFeature
                     "Struct literal expressions are not yet supported.") bounds
            else Succ0 (makeNameExpression nameText bounds nodeId)
                   (B (TokSym SymLBrace) braceBounds :: afterBrace)
        (B (TokSym SymDoubleColon) _) :: _ =>
          failWithCustomError
            (UnsupportedFeature "Path expressions are not yet supported.") bounds
        _ => Succ0 (makeNameExpression nameText bounds nodeId) remaining
    TokBuiltin BuiltinCtrl =>
      succT $
        parseControlExpression bounds nodeId remaining recur
    TokBuiltin builtin =>
      Succ0 (makeBuiltinExpression builtin bounds nodeId) remaining
    TokIntLitRaw _ =>
      succT $ parseLiteralExpression token bounds nodeId remaining suffixAcc
    TokFloatLitRaw _ =>
      succT $ parseLiteralExpression token bounds nodeId remaining suffixAcc
    TokBoolLit _ =>
      succT $ parseLiteralExpression token bounds nodeId remaining suffixAcc
    TokStringLitRaw _ =>
      succT $ parseLiteralExpression token bounds nodeId remaining suffixAcc
    TokBasisStringLitRaw _ =>
      succT $ parseLiteralExpression token bounds nodeId remaining suffixAcc
    _ => succT $
           parseUnsupportedPrimary token bounds nodeId remaining suffixAcc

||| Resumes postfix parsing from a newly constructed receiver.
||| This is the single totality boundary for postfix-chain recursion; callers
||| remain responsible only for proving that their postfix form consumed input.
continuePostfix : SurfaceExpr -> Rule False SurfaceExpr
continuePostfix expression nodeId tokens acc =
  assert_total $ parsePostfixExpression expression nodeId tokens acc

-- Repeatedly attaches calls, indexing, fields, tuple indices, and method calls.
-- The input expression is the already-parsed receiver. Every recognized postfix
-- form builds a new outer expression and recursively continues, producing a
-- maximal chain such as `values()[i].field.len()`. An unrecognized token is not
-- an error: it terminates the chain and is returned untouched to the caller.
-- Tested by: `fn postfix() {values()[i].field.len()}`.
parsePostfixExpression callee nodeId
    ((B (TokSym SymLParen) openBounds) :: afterOpen) (SA recur) =
  let (callNodeId, afterCallNodeId) = reserveNodeId nodeId
   in case succT $ parseCallArguments afterCallNodeId afterOpen recur of
        Fail0 err => Fail0 err
        Succ0 (arguments, afterArgumentsNodeId) afterArguments =>
          let call = surfaceAstNode
                (MkAstInfo callNodeId
                  (mergeSpans callee.astInfo.span
                    (sourceSpan arguments.closeBounds)))
                (ExprCall callee arguments.values)
           in succF $
                continuePostfix call afterArgumentsNodeId afterArguments suffixAcc
parsePostfixExpression indexed nodeId
    ((B (TokSym SymLBracket) openBounds) :: afterOpen) (SA recur) =
  let (indexNodeId, afterIndexNodeId) = reserveNodeId nodeId
   in case succT $ assert_total $ parseExpression afterIndexNodeId afterOpen recur of
        Fail0 err => Fail0 err
        Succ0 (index, afterIndexExpressionNodeId) afterIndex =>
          case afterIndex of
            (B (TokSym SymRBracket) closeBounds) :: afterClose =>
              let expression = surfaceAstNode
                    (MkAstInfo indexNodeId
                      (mergeSpans indexed.astInfo.span (sourceSpan closeBounds)))
                    (ExprIndex indexed index)
               in succF $
                    continuePostfix expression afterIndexExpressionNodeId
                      afterClose suffixAcc
            (B unexpected unexpectedBounds) :: _ =>
              Fail0 (B (Expected ["]"] (show unexpected)) unexpectedBounds)
            [] => Fail0 (B EOI NoBounds)
parsePostfixExpression receiver nodeId
    ((B (TokSym SymDot) dotBounds) ::
     (B (TokIdent methodText) methodBounds) ::
     (B (TokSym SymLParen) openBounds) :: afterOpen) (SA recur) =
  let (methodNodeId, afterMethodNodeId) = reserveNodeId nodeId
      (methodName, afterNameNodeId) = makeName methodText methodBounds afterMethodNodeId
   in case succT $ parseCallArguments afterNameNodeId afterOpen recur of
        Fail0 err => Fail0 err
        Succ0 (arguments, afterArgumentsNodeId) afterArguments =>
          let expression = surfaceAstNode
                (MkAstInfo methodNodeId
                  (mergeSpans receiver.astInfo.span
                    (sourceSpan arguments.closeBounds)))
                (ExprMethodCall receiver methodName arguments.values)
           in succF $
                continuePostfix expression afterArgumentsNodeId
                  afterArguments suffixAcc
parsePostfixExpression receiver nodeId
    ((B (TokSym SymDot) dotBounds) ::
     (B (TokIdent fieldText) fieldBounds) :: afterField) (SA recur) =
  let (fieldNodeId, afterFieldNodeId) = reserveNodeId nodeId
      (fieldName, afterNameNodeId) = makeName fieldText fieldBounds afterFieldNodeId
      expression = surfaceAstNode
        (MkAstInfo fieldNodeId
          (mergeSpans receiver.astInfo.span (sourceSpan fieldBounds)))
        (ExprField receiver fieldName)
   in succF $ continuePostfix expression afterNameNodeId afterField recur
parsePostfixExpression receiver nodeId
    ((B (TokSym SymDot) dotBounds) ::
     (B (TokIntLitRaw indexRaw) indexBounds) :: afterIndex) (SA recur) =
  let (indexNodeId, afterIndexNodeId) = reserveNodeId nodeId
      expression = surfaceAstNode
        (MkAstInfo indexNodeId
          (mergeSpans receiver.astInfo.span (sourceSpan indexBounds)))
        (ExprTupleIndex receiver indexRaw)
   in succF $ continuePostfix expression afterIndexNodeId afterIndex recur
parsePostfixExpression callee nodeId tokens _ =
  Succ0 (callee, nodeId) tokens @{Same}

-- Parses a value-less `continue` expression.
-- Tested by: `fn exits() {break 1; continue; return; return value}`.
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

-- Wraps a parsed braced block as an expression node.
-- Tested by: `fn block() {{1}}`.
parseBlockExpression nodeId tokens acc =
  let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
   in case parseBracedBlock afterExpressionNodeId tokens acc of
        Fail0 err => Fail0 err
        Succ0 (block, finalNodeId) finalTokens =>
          Succ0
            (surfaceAstNode (MkAstInfo expressionNodeId block.astInfo.span)
              (ExprBlock block),
             finalNodeId)
            finalTokens

-- Parses an unconditional `loop` expression and its body.
-- Tested by:
-- `fn control() {loop {break}; while ready {continue}; for x in values {x}}`.
parseLoopExpression _ [] _ = Fail0 (B EOI NoBounds)
parseLoopExpression nodeId
    ((B (TokKw KwLoop) loopBounds) :: remaining) (SA recur) =
  let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
   in case succT $ parseBracedBlock afterExpressionNodeId remaining recur of
        Fail0 err => Fail0 err
        Succ0 (body, finalNodeId) finalTokens =>
          Succ0
            (surfaceAstNode
              (MkAstInfo expressionNodeId
                (mergeSpans (sourceSpan loopBounds) body.astInfo.span))
              (ExprLoop body),
             finalNodeId)
            finalTokens
parseLoopExpression _ ((B unexpected unexpectedBounds) :: _) _ =
  Fail0 (B (Expected ["loop"] (show unexpected)) unexpectedBounds)

-- Parses a `while` condition and its braced body.
-- Tested by:
-- `fn control() {loop {break}; while ready {continue}; for x in values {x}}`.
parseWhileExpression _ [] _ = Fail0 (B EOI NoBounds)
parseWhileExpression nodeId
    ((B (TokKw KwWhile) whileBounds) :: remaining) (SA recur) =
  let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
   in case succT $
                   parseExpression afterExpressionNodeId remaining recur of
        Fail0 err => Fail0 err
        Succ0 (condition, afterConditionNodeId) afterCondition =>
          case succT $ parseBracedBlock afterConditionNodeId afterCondition suffixAcc of
            Fail0 err => Fail0 err
            Succ0 (body, finalNodeId) finalTokens =>
              Succ0
                (surfaceAstNode
                  (MkAstInfo expressionNodeId
                    (mergeSpans (sourceSpan whileBounds) body.astInfo.span))
                  (ExprWhile condition body),
                 finalNodeId)
                finalTokens
parseWhileExpression _ ((B unexpected unexpectedBounds) :: _) _ =
  Fail0 (B (Expected ["while"] (show unexpected)) unexpectedBounds)

-- Parses `for name in expression` and its braced body.
-- Tested by:
-- `fn control() {loop {break}; while ready {continue}; for x in values {x}}`.
parseForExpression _ [] _ = Fail0 (B EOI NoBounds)
parseForExpression nodeId
    ((B (TokKw KwFor) forBounds) ::
     (B (TokIdent binderText) binderBounds) ::
     (B (TokKw KwIn) inBounds) :: remaining) (SA recur) =
  let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
      (patternNodeId, afterPatternNodeId) = reserveNodeId afterExpressionNodeId
      (name, afterNameNodeId) = makeName binderText binderBounds afterPatternNodeId
      pattern = surfaceAstNode (MkAstInfo patternNodeId (sourceSpan binderBounds))
                               (PatternName Nothing name)
   in case succT $ parseExpression afterNameNodeId remaining recur of
        Fail0 err => Fail0 err
        Succ0 (iterable, afterIterableNodeId) afterIterable =>
          case succT $
                     parseBracedBlock afterIterableNodeId afterIterable suffixAcc of
            Fail0 err => Fail0 err
            Succ0 (body, finalNodeId) finalTokens =>
              Succ0
                (surfaceAstNode
                  (MkAstInfo expressionNodeId
                    (mergeSpans (sourceSpan forBounds) body.astInfo.span))
                  (ExprFor pattern iterable body),
                 finalNodeId)
                finalTokens
parseForExpression _ ((B unexpected unexpectedBounds) :: _) _ =
  Fail0 (B (Expected ["for identifier in expression"] (show unexpected)) unexpectedBounds)

-- Parses `break` with an optional value.
-- Tested by: `fn exits() {break 1; continue; return; return value}`.
parseBreakExpression _ [] _ = Fail0 (B EOI NoBounds)
parseBreakExpression nodeId
    ((B (TokKw KwBreak) breakBounds) :: remaining) (SA recur) =
  let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId in
  if nextTokenSatisfies isOptionalValueTerminator remaining
    then finishWithoutValue expressionNodeId afterExpressionNodeId
    else
      case succT $
                 parseExpression afterExpressionNodeId remaining recur of
        Fail0 err => Fail0 err
        Succ0 (value, finalNodeId) finalTokens =>
          Succ0
            (surfaceAstNode
              (MkAstInfo expressionNodeId
                (mergeSpans (sourceSpan breakBounds) value.astInfo.span))
              (ExprBreak (Just value)),
             finalNodeId)
            finalTokens
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

-- Parses `return` with an optional value.
-- Tested by: `fn exits() {return; return value}`.
parseReturnExpression _ [] _ = Fail0 (B EOI NoBounds)
parseReturnExpression nodeId
    ((B (TokKw KwReturn) returnBounds) :: remaining) (SA recur) =
  let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId in
  if nextTokenSatisfies isOptionalValueTerminator remaining
    then finishWithoutValue expressionNodeId afterExpressionNodeId
    else
      case succT $
                 parseExpression afterExpressionNodeId remaining recur of
        Fail0 err => Fail0 err
        Succ0 (value, finalNodeId) finalTokens =>
          Succ0
            (surfaceAstNode
              (MkAstInfo expressionNodeId
                (mergeSpans (sourceSpan returnBounds) value.astInfo.span))
              (ExprReturn (Just value)),
             finalNodeId)
            finalTokens
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

-- Parses classical `if`, chained `else if`, and optional `else` blocks.
-- Tested by: `fn choose() {if ready {1} else if retry {2} else {3}}`.
parseIfExpression _ [] _ = Fail0 (B EOI NoBounds)
parseIfExpression nodeId
    ((B (TokKw KwIf) ifBounds) :: remaining) (SA recur) =
  let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
      (ifNodeId, afterIfNodeId) = reserveNodeId afterExpressionNodeId
      Succ0 (condition, afterConditionNodeId) afterCondition :=
            succT $ parseExpression afterIfNodeId remaining recur
        | Fail0 err => Fail0 err
      Succ0 (thenBlock, afterThenNodeId) afterThen :=
            succT $ parseBracedBlock afterConditionNodeId afterCondition suffixAcc
        | Fail0 err => Fail0 err
   in case afterThen of
        (B (TokKw KwElse) elseBounds) ::
          afterElse@((B (TokSym SymLBrace) openElseBounds) :: elseTokens) =>
            let Succ0 (elseBlock, finalNodeId) finalTokens :=
                      succT $ parseBracedBlock afterThenNodeId afterElse suffixAcc
                | Fail0 err => Fail0 err
                ifSpan = mergeSpans (sourceSpan ifBounds) elseBlock.astInfo.span
                ifNode = MkClassicalIfNode condition thenBlock (Just (ElseBlock elseBlock))
                expression = surfaceAstNode (MkAstInfo expressionNodeId ifSpan) (ExprIf ifNode)
             in Succ0 (expression, finalNodeId) finalTokens
        (B (TokKw KwElse) elseBounds) ::
          afterElse@((B (TokKw KwIf) chainedIfBounds) :: chainedIfTokens) =>
            let Succ0 (chainedExpression, finalNodeId) finalTokens :=
                      succT $ assert_total $
                                 parseIfExpression afterThenNodeId afterElse suffixAcc
                | Fail0 err => Fail0 err
             in case chainedExpression of
                  MkAstNode chainedInfo _ (ExprIf chainedIf) =>
                    let chainedNode = surfaceAstNode chainedInfo chainedIf
                        ifSpan = mergeSpans (sourceSpan ifBounds)
                          chainedExpression.astInfo.span
                        ifNode = MkClassicalIfNode condition thenBlock
                          (Just (ElseChainedIf chainedNode))
                        expression = surfaceAstNode
                          (MkAstInfo expressionNodeId ifSpan) (ExprIf ifNode)
                     in Succ0 (expression, finalNodeId) finalTokens
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
   in case parseExpression nextNodeId tokens acc of
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
      Succ0 (qualifiers, afterQualifiersNodeId) afterQualifiers :=
            succT $ parseLetQualifiers emptyStorageQualifiers afterStatementNodeId
              remaining recur
        | Fail0 err => Fail0 err
      Succ0 (pattern, afterPatternNodeId) afterPattern :=
            succT $ parsePattern afterQualifiersNodeId afterQualifiers suffixAcc
        | Fail0 err => Fail0 err
   in case afterPattern of
        (B (TokSym SymColon) colonBounds) :: afterColon =>
          let Succ0 (ty, afterTypeNodeId) afterType :=
                    succT $ parseType afterPatternNodeId afterColon suffixAcc
                | Fail0 err => Fail0 err
           in case afterType of
                (B (TokSym SymSemi) semiBounds) :: finalTokens =>
                  let binding = MkLetBindingNode qualifiers pattern (Just ty) Nothing
                   in Succ0
                        (surfaceAstNode
                          (MkAstInfo statementNodeId
                            (sourceSpan (letBounds <+> semiBounds)))
                          (StatementLet binding),
                         afterTypeNodeId)
                        finalTokens
                _ =>
                  let Succ0 (initializer, finalNodeId) afterInitializer :=
                            succT $ parseLetInitializer afterTypeNodeId afterType suffixAcc
                        | Fail0 err => Fail0 err
                      (B (TokSym SymSemi) semiBounds) :: finalTokens := afterInitializer
                        | (B unexpected bounds) :: _ =>
                            Fail0 (B (Expected [";"] (show unexpected)) bounds)
                        | [] => Fail0 (B EOI NoBounds)
                      binding = MkLetBindingNode qualifiers pattern (Just ty) (Just initializer)
                   in Succ0
                        (surfaceAstNode
                          (MkAstInfo statementNodeId
                            (sourceSpan (letBounds <+> semiBounds)))
                          (StatementLet binding),
                         finalNodeId)
                        finalTokens
        (B (TokSym symbol) markerBounds) :: afterMarker =>
          if symbol == SymEq || symbol == SymWalrusEq
            then
              let Succ0 (initializer, finalNodeId) afterInitializer :=
                        succT $ parseLetInitializer afterPatternNodeId
                          (B (TokSym symbol) markerBounds :: afterMarker) suffixAcc
                  | Fail0 err => Fail0 err
                  (B (TokSym SymSemi) semiBounds) :: finalTokens := afterInitializer
                    | (B unexpected bounds) :: _ =>
                        Fail0 (B (Expected [";"] (show unexpected)) bounds)
                    | [] => Fail0 (B EOI NoBounds)
                  binding = MkLetBindingNode qualifiers pattern Nothing (Just initializer)
               in Succ0
                    (surfaceAstNode
                      (MkAstInfo statementNodeId
                        (sourceSpan (letBounds <+> semiBounds)))
                      (StatementLet binding),
                     finalNodeId)
                    finalTokens
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
       in case succT $
                 parseExpression afterOperatorNodeId afterOperator recur of
            Fail0 err => Fail0 err
            Succ0 (value, finalNodeId) afterValue =>
              case afterValue of
                (B (TokSym SymSemi) semiBounds) :: finalTokens =>
                  let assignment = MkAssignmentNode target locatedOperator value
                   in Succ0
                        (surfaceAstNode
                          (MkAstInfo statementNodeId
                            (mergeSpans targetExpression.astInfo.span
                              (sourceSpan semiBounds)))
                          (StatementAssignment assignment),
                         finalNodeId)
                        finalTokens
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
          Succ0 (expression, afterExpressionNodeId) afterExpression =>
            case afterExpression of
              [] => Fail0 (B EOI NoBounds)

              (B (TokSym SymSemi) semiBounds) :: afterSemi =>
                let (statementNodeId, nextNodeId) =
                      reserveNodeId afterExpressionNodeId
                    statement = surfaceAstNode
                      (MkAstInfo statementNodeId
                        (mergeSpans expression.astInfo.span (sourceSpan semiBounds)))
                      (StatementSemiExpression expression)
                 in succT $ assert_total $
                           parseBlockContents blockNodeId openBounds
                             (statements :< statement) nextNodeId afterSemi suffixAcc

              _ =>
                if isBlockLikeExpression expression
                  then
                    let (statementNodeId, nextNodeId) =
                          reserveNodeId afterExpressionNodeId
                        statement = surfaceAstNode
                          (MkAstInfo statementNodeId expression.astInfo.span)
                          (StatementExpression expression)
                     in succT $ assert_total $
                               parseBlockContents blockNodeId openBounds
                                 (statements :< statement) nextNodeId
                                 afterExpression
                                 suffixAcc
                  else
                    case afterExpression of
                      (B (TokSym SymRBrace) closeBounds) :: finalTokens =>
                        Succ0
                          (surfaceAstNode
                            (MkAstInfo blockNodeId
                              (sourceSpan (openBounds <+> closeBounds)))
                            (MkBlockNode [] (statements <>> []) (Just expression)),
                           afterExpressionNodeId)
                          finalTokens

                      (B (TokSym symbol) operatorBounds) :: afterOperatorToken =>
                        case assignmentOperator symbol of
                          Just _ =>
                            case succT $ parseAssignmentStatement expression
                                   afterExpressionNodeId
                                   (B (TokSym symbol) operatorBounds ::
                                    afterOperatorToken)
                                   suffixAcc of
                              Fail0 err => Fail0 err
                              Succ0 (statement, nextNodeId) afterStatement =>
                                succT $ assert_total $
                                          parseBlockContents blockNodeId openBounds
                                            (statements :< statement) nextNodeId
                                            afterStatement suffixAcc
                          Nothing =>
                            failWithCustomError (ParseErrorWithMessage
                              "Expected `;` or `}`, found instead: `\{interpolate (TokSym symbol)}`.")
                              operatorBounds

                      (B unexpected unexpectedBounds) :: _ =>
                        failWithCustomError (ParseErrorWithMessage
                          "Expected `;` or `}`, found instead: `\{interpolate unexpected}`.")
                          unexpectedBounds
                      [] => Fail0 (B EOI NoBounds)

parseBracedBlock _ [] _ = Fail0 (B EOI NoBounds)
parseBracedBlock nodeId ((B token bounds) :: remaining) acc@(SA recur) =
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
            (name, nextNodeId) = makeName nameText nameBounds afterAttributeNodeId
            attribute = surfaceAstNode
              (MkAstInfo attributeNodeId (sourceSpan (hashBounds <+> closeBounds)))
              (MkAttributeNode name Nothing)
         in Succ0 (attribute, nextNodeId) remaining

    B (TokSym SymHash) hashBounds :: B (TokSym SymLBracket) _ ::
      B (TokIdent nameText) nameBounds :: B (TokSym SymLParen) _ ::
      B (TokStringLitRaw rawText) argumentBounds :: B (TokSym SymRParen) _ ::
      B (TokSym SymRBracket) closeBounds :: remaining =>
        let (attributeNodeId, afterAttributeNodeId) = reserveNodeId nodeId
            (name, afterNameNodeId) = makeName nameText nameBounds afterAttributeNodeId
            (argumentNodeId, nextNodeId) = reserveNodeId afterNameNodeId
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
    (itemNodeId : NodeId)
  -> (declarationStart : Bounds)
  -> (attributes : List SurfaceAttribute)
  -> (visibility : Maybe (SurfaceAstNode VisibilityQualifier))
  -> (constness : Maybe (SurfaceAstNode FunctionConstness))
  -> (functionEffect : Maybe (SurfaceAstNode FunctionEffect))
  -> Rule True SurfaceItem
parseFunDecl _ declarationStart attributes visibility constness functionEffect
    nodeId [] acc = Fail0 (B EOI NoBounds)
parseFunDecl itemNodeId declarationStart attributes visibility constness functionEffect
    nodeId ((B token bounds) :: remaining) acc@(SA recur) =
    case token of
        TokKw KwFn =>
            let Succ0 (functionName, afterNameNodeId) afterName :=
                      succT $ parseName "function name" nodeId remaining recur
                  | Fail0 err => Fail0 err
                Succ0 (functionParameters, afterParametersNodeId) afterParameters :=
                      succT $ parseFunctionParameters afterNameNodeId afterName suffixAcc
                  | Fail0 err => Fail0 err
                Succ0 (returnType, afterReturnTypeNodeId) afterReturnType :=
                      succT $ parseOptionalReturnType
                                afterParametersNodeId afterParameters suffixAcc
                  | Fail0 err => Fail0 err
                Succ0 (supportClause, afterSupportNodeId) afterSupport :=
                      succT $ parseOptionalSupportClause
                                afterReturnTypeNodeId afterReturnType suffixAcc
                  | Fail0 err => Fail0 err
                Succ0 (contractClauses, afterContractsNodeId) afterContracts :=
                      succT $ parseContractClauses
                                afterSupportNodeId afterSupport suffixAcc
                  | Fail0 err => Fail0 err
                Succ0 (functionBody, finalNodeId) finalTokens :=
                      succT $ parseBracedBlock
                                afterContractsNodeId afterContracts suffixAcc
                  | Fail0 err => Fail0 err
                declaration =
                      MkFunctionDeclarationNode
                        []                  -- docs
                        attributes
                        visibility
                        constness
                        functionEffect
                        functionName
                        functionParameters
                        returnType
                        supportClause
                        contractClauses
                        functionBody
                itemSpan =
                      mergeSpans (sourceSpan declarationStart) functionBody.astInfo.span
             in Succ0
                  (surfaceAstNode (MkAstInfo itemNodeId itemSpan) (ItemFunction declaration),
                   finalNodeId)
                  finalTokens

        _ =>
            failWithCustomError (ParseErrorWithMessage
              "Expected `fun` keyword, found instead: `\{interpolate token}`.") bounds

||| Parses a typed constant value declaration after its `const` prefix.
||| Tested by: `const N: i64 = 4;`.
parseConstDecl :
    (itemNodeId : NodeId)
  -> (declarationStart : Bounds)
  -> (attributes : List SurfaceAttribute)
  -> (visibility : Maybe (SurfaceAstNode VisibilityQualifier))
  -> Rule True SurfaceItem
parseConstDecl _ _ _ _ _ [] _ = Fail0 (B EOI NoBounds)
parseConstDecl itemNodeId declarationStart attributes visibility nodeId
    tokens@((B token tokenBounds) :: remaining) acc@(SA recur) =
  case token of
    TokKw KwFn =>
      Fail0 (B (Expected ["constant name"] (show token)) tokenBounds)

    _ =>
      case attributes of
        _ :: _ =>
          failWithCustomError
            (ParseErrorWithMessage "Attributes on const declarations are not yet supported.")
            tokenBounds
        [] =>
          let Succ0 (constName, afterNameNodeId) afterName :=
                    parseName "constant name" nodeId
                      (B token tokenBounds :: remaining) (SA recur)
                | Fail0 err => Fail0 err
              (B (TokSym SymColon) colonBounds) :: afterColon := afterName
                | (B unexpected bounds) :: _ =>
                    failWithCustomError
                      (ParseErrorWithMessage
                        "Expected `:` after constant name, found instead: `\{interpolate unexpected}`.")
                      bounds
                | [] => Fail0 (B EOI NoBounds)
              Succ0 (constType, afterTypeNodeId) afterType :=
                    succT $ parseType afterNameNodeId afterColon suffixAcc
                | Fail0 err => Fail0 err
              (B (TokSym SymEq) equalsBounds) :: afterEquals := afterType
                | (B unexpected bounds) :: _ =>
                    failWithCustomError
                      (ParseErrorWithMessage
                        "Expected `=` in const declaration, found instead: `\{interpolate unexpected}`.")
                      bounds
                | [] => Fail0 (B EOI NoBounds)
              Succ0 (constValue, finalNodeId) afterValue :=
                    succT $ parseExpression afterTypeNodeId afterEquals suffixAcc
                | Fail0 err => Fail0 err
              (B (TokSym SymSemi) semiBounds) :: finalTokens := afterValue
                | (B unexpected bounds) :: _ =>
                    failWithCustomError
                      (ParseErrorWithMessage
                        "Expected `;` after const declaration, found instead: `\{interpolate unexpected}`.")
                      bounds
                | [] => Fail0 (B EOI NoBounds)
              declaration = MkConstDeclarationNode [] visibility constName constType constValue
           in Succ0
                (surfaceAstNode
                  (MkAstInfo itemNodeId (sourceSpan (declarationStart <+> semiBounds)))
                  (ItemConst declaration),
                 finalNodeId)
                finalTokens

||| Reports the common top-level error after all recognized prefixes have been read.
unexpectedTopLevelItem :
     {isStrict : Bool}
  -> {tokens : List (Bounded Token)}
  -> Token
  -> Bounds
  -> Res isStrict Token tokens CustomParseError a
unexpectedTopLevelItem token bounds =
  failWithCustomError
    (UnexpectedToken
      ("Unexpected token: `" ++ interpolate token ++
       "` at top level in source file. At module level only only function declarations are allowed for now."))
    bounds

||| Preserves the diagnostic associated with the most specific prefix that commits
||| the parser to a function declaration.
invalidItemAfterPrefix :
     {isStrict : Bool}
  -> {tokens : List (Bounded Token)}
  -> ItemPrefix
  -> Token
  -> Bounds
  -> Res isStrict Token tokens CustomParseError a
invalidItemAfterPrefix itemPrefix token bounds =
  case itemPrefix.state of
    PrefixEffect _ locatedEffect =>
      failWithCustomError
        (ParseErrorWithMessage
          "Expected `fun` after `\{show locatedEffect.value}` effect modifier, found instead: `\{interpolate token}`.")
        bounds
    PrefixOrdinary visibility =>
      case visibility of
        Just _ =>
          failWithCustomError
            (ParseErrorWithMessage
              "Expected function declaration after `pub` visibility modifier, found instead: `\{interpolate token}`.")
            bounds
        Nothing =>
          case itemPrefix.attributes of
            [<] => unexpectedTopLevelItem token bounds
            _ :< _ =>
              failWithCustomError
                (ParseErrorWithMessage
                  "Expected function declaration after attribute, found instead: `\{interpolate token}`.")
                bounds
    PrefixConst _ _ =>
      failWithCustomError
        (ParseErrorWithMessage
          "Expected a function or constant declaration after `const`, found instead: `\{interpolate token}`.")
        bounds
    PrefixConstEffect _ _ locatedEffect =>
      failWithCustomError
        (ParseErrorWithMessage
          "Expected `fun` after `\{show locatedEffect.value}` effect modifier, found instead: `\{interpolate token}`.")
        bounds

||| Dispatches the item keyword after attributes, visibility, constness, and effect
||| have been accumulated. This is the only top-level declaration dispatcher.
parseItemAfterPrefix : ItemPrefix -> Rule True SurfaceItem
parseItemAfterPrefix _ _ [] _ = Fail0 (B EOI NoBounds)
parseItemAfterPrefix itemPrefix nodeId
    tokens@((B token bounds) :: remaining) acc@(SA recur) =
  case itemPrefix.state of
    PrefixConst visibility constBounds =>
      case token of
        TokKw KwFn =>
          let (constnessNodeId, nextNodeId) = reserveNodeId nodeId
              constness = surfaceAstNode
                (MkAstInfo constnessNodeId (sourceSpan constBounds)) ConstFunction
           in parseFunDecl itemPrefix.itemNodeId itemPrefix.declarationStart
                (itemPrefix.attributes <>> [])
                visibility (Just constness) Nothing nextNodeId
                (B (TokKw KwFn) bounds :: remaining) acc
        _ =>
          parseConstDecl itemPrefix.itemNodeId itemPrefix.declarationStart
            (itemPrefix.attributes <>> [])
            visibility nodeId (B token bounds :: remaining) acc

    PrefixOrdinary visibility =>
      case token of
        TokKw KwImpl =>
          failWithCustomError
            (UnsupportedFeature "Impls blocks and structs are not yet supported.")
            bounds
        TokKw KwFn =>
          parseFunDecl itemPrefix.itemNodeId itemPrefix.declarationStart
            (itemPrefix.attributes <>> [])
            visibility Nothing Nothing nodeId
            (B (TokKw KwFn) bounds :: remaining) acc
        TokKw keyword =>
          case unsupportedTopLevelItem keyword of
            Just err => failWithCustomError err bounds
            Nothing => invalidItemAfterPrefix itemPrefix token bounds
        _ => invalidItemAfterPrefix itemPrefix token bounds

    PrefixEffect visibility effect =>
      case token of
        TokKw KwImpl =>
          failWithCustomError
            (UnsupportedFeature "Impls blocks and structs are not yet supported.")
            bounds
        TokKw KwFn =>
          parseFunDecl itemPrefix.itemNodeId itemPrefix.declarationStart
            (itemPrefix.attributes <>> [])
            visibility Nothing (Just effect) nodeId
            (B (TokKw KwFn) bounds :: remaining) acc
        TokKw keyword =>
          case unsupportedTopLevelItem keyword of
            Just err => failWithCustomError err bounds
            Nothing => invalidItemAfterPrefix itemPrefix token bounds
        _ => invalidItemAfterPrefix itemPrefix token bounds

    PrefixConstEffect visibility constBounds effect =>
      case token of
        TokKw KwImpl =>
          failWithCustomError
            (UnsupportedFeature "Impls blocks and structs are not yet supported.")
            bounds
        TokKw KwFn =>
          let (constnessNodeId, nextNodeId) = reserveNodeId nodeId
              constness = surfaceAstNode
                (MkAstInfo constnessNodeId (sourceSpan constBounds)) ConstFunction
           in parseFunDecl itemPrefix.itemNodeId itemPrefix.declarationStart
                (itemPrefix.attributes <>> [])
                visibility (Just constness) (Just effect) nextNodeId
                (B (TokKw KwFn) bounds :: remaining) acc
        TokKw keyword =>
          case unsupportedTopLevelItem keyword of
            Just err => failWithCustomError err bounds
            Nothing => invalidItemAfterPrefix itemPrefix token bounds
        _ => invalidItemAfterPrefix itemPrefix token bounds

||| Collects the ordered prefix of a top-level declaration. Attributes must precede
||| visibility, which must precede constness or a function effect; duplicate or
||| misplaced modifiers fall through to the terminal dispatcher and are rejected.
parseItemPrefix : ItemPrefix -> Rule True SurfaceItem
parseItemPrefix _ _ [] _ = Fail0 (B EOI NoBounds)
parseItemPrefix itemPrefix nodeId
    tokens@((B token bounds) :: remaining) acc@(SA recur) =
  case token of
    TokOuterDoc _ =>
      failWithCustomError
        (UnsupportedFeature "Outer doc comments are not yet supported.") bounds

    TokInnerDoc _ =>
      failWithCustomError
        (UnsupportedFeature "Inner doc comments are not yet supported.") bounds

    TokSym SymHash =>
      case itemPrefix.state of
        PrefixOrdinary Nothing =>
          case parseAttribute nodeId
                 (B (TokSym SymHash) bounds :: remaining) acc of
            Fail0 err => Fail0 err
            Succ0 (attribute, nextNodeId) afterAttribute =>
              let nextPrefix = MkItemPrefix itemPrefix.itemNodeId
                    itemPrefix.declarationStart
                    (itemPrefix.attributes :< attribute)
                    (PrefixOrdinary Nothing)
               in succT $
                    parseItemPrefix nextPrefix nextNodeId afterAttribute recur
        _ => parseItemAfterPrefix itemPrefix nodeId
               (B (TokSym SymHash) bounds :: remaining) acc

    TokKw KwPub =>
      case itemPrefix.state of
        PrefixOrdinary Nothing =>
          let (visibilityNodeId, nextNodeId) = reserveNodeId nodeId
              locatedVisibility = surfaceAstNode
                (MkAstInfo visibilityNodeId (sourceSpan bounds)) VisibilityPublic
              nextPrefix = MkItemPrefix itemPrefix.itemNodeId
                itemPrefix.declarationStart itemPrefix.attributes
                (PrefixOrdinary (Just locatedVisibility))
           in succT $ parseItemPrefix nextPrefix nextNodeId remaining recur
        _ => parseItemAfterPrefix itemPrefix nodeId
               (B (TokKw KwPub) bounds :: remaining) acc

    TokKw KwConst =>
      case itemPrefix.state of
        PrefixOrdinary visibility =>
          let nextPrefix = MkItemPrefix itemPrefix.itemNodeId
                itemPrefix.declarationStart itemPrefix.attributes
                (PrefixConst visibility bounds)
           in succT $ parseItemPrefix nextPrefix nodeId remaining recur
        _ => parseItemAfterPrefix itemPrefix nodeId
               (B (TokKw KwConst) bounds :: remaining) acc

    TokKw keyword =>
      case (itemPrefix.state, functionEffectFromKeyword keyword) of
        (PrefixOrdinary visibility, Just effectValue) =>
          let (effectNodeId, nextNodeId) = reserveNodeId nodeId
              locatedEffect = surfaceAstNode
                (MkAstInfo effectNodeId (sourceSpan bounds)) effectValue
              nextPrefix = MkItemPrefix itemPrefix.itemNodeId
                itemPrefix.declarationStart itemPrefix.attributes
                (PrefixEffect visibility locatedEffect)
           in succT $ parseItemPrefix nextPrefix nextNodeId remaining recur
        (PrefixConst visibility constBounds, Just effectValue) =>
          let (effectNodeId, nextNodeId) = reserveNodeId nodeId
              locatedEffect = surfaceAstNode
                (MkAstInfo effectNodeId (sourceSpan bounds)) effectValue
              nextPrefix = MkItemPrefix itemPrefix.itemNodeId
                itemPrefix.declarationStart itemPrefix.attributes
                (PrefixConstEffect visibility constBounds locatedEffect)
           in succT $ parseItemPrefix nextPrefix nextNodeId remaining recur
        _ => parseItemAfterPrefix itemPrefix nodeId
               (B (TokKw keyword) bounds :: remaining) acc

    _ => parseItemAfterPrefix itemPrefix nodeId
           (B token bounds :: remaining) acc

||| Parses one top-level item by collecting its prefix and dispatching once.
||| Tested by: `const N: i64 = 4;`, `pub unitary fn empty() {}`, and
||| `#[qasm_gate] pub fn annotated() {}`.
parseItem : Rule True SurfaceItem
parseItem _ [] _ = Fail0 (B EOI NoBounds)
parseItem nodeId ((B firstToken startBounds) :: remaining) acc =
  let (itemNodeId, nextNodeId) = reserveNodeId nodeId
   in parseItemPrefix
        (MkItemPrefix itemNodeId startBounds [<] (PrefixOrdinary Nothing))
        nextNodeId (B firstToken startBounds :: remaining) acc

||| Parses top-level items until end of input while preserving source order.
||| Items accumulate in a `SnocList` and are converted once at EOF. A valid lexer
||| stream must contain exactly one final `TokEOF`; missing EOF or tokens following
||| EOF are rejected. Each iteration resumes with both the remaining suffix and the
||| next free node ID returned by `parseItem`.
||| Tested by: `const N: i64 = 4;\nfn arrays() { let c: [i32; N]; }`.
parseItems : SnocList SurfaceItem -> Rule True (List SurfaceItem)
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
            succT $ parseItems (items :< item) followingNodeId remaining recur

||| Parses all items into a source-file AST associated with the supplied filename.
parseModule : String -> Rule True SurfaceSourceFile
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

public export
||| Runs the module parser over a token stream and returns either a located error or source file.
||| Node ID zero is reserved for the source-file node, so item allocation begins at
||| one. Parser failures are enriched with the filename here. `parseItems` consumes
||| the complete token stream whenever it succeeds, so a successful module parse
||| with leftover tokens indicates an internal parser invariant violation.
parseFile : String -> List (Bounded Token) -> Either (Located ParseError) SurfaceSourceFile
parseFile fileName tokens =
    case parseModule fileName 1 tokens suffixAcc of   -- first item node id is 1 (0 is source file node id)
        Fail0 (B err bounds) =>
            Left (locatedParseError fileName bounds err)

        Succ0 (sourceFile, _) [] =>
            Right sourceFile

        Succ0 _ (_ :: _) =>
            assert_total $ idris_crash
                "parseFile: parseModule function succeeded without consuming the complete token stream."
