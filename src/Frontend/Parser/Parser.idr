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

record TupleTail where
    constructor MkTupleTail
    elementTypes : List SurfaceTy
    closingBounds : Bounds

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

parseIntegerExpression : String -> Rule True SurfaceExpr
parseIntegerExpression _ _ [] _ = Fail0 (B EOI NoBounds)
parseIntegerExpression expected nodeId ((B token bounds) :: remaining) _ =
    case token of
      TokIntLitRaw rawText =>
        let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
            (literalNodeId, nextNodeId) = reserveNodeId afterExpressionNodeId
            literal = surfaceAstNode (MkAstInfo literalNodeId (sourceSpan bounds))
                                     (LiteralIntegerRaw rawText)
            expression = surfaceAstNode (MkAstInfo expressionNodeId (sourceSpan bounds))
                                        (ExprLiteral literal)
         in Succ0 (expression, nextNodeId) remaining
      _ => Fail0 (B (Expected [expected] (show token)) bounds)

parseArrayLength : Rule True SurfaceExpr
parseArrayLength = parseIntegerExpression "array length"

mutual
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
      _ => Fail0 (B (Expected ["a type declaration"] (show token)) bounds)

  parseArrayType : NodeId -> Bounds -> Rule True SurfaceTy
  parseArrayType _ _ _ [] _ = Fail0 (B EOI NoBounds)
  parseArrayType arrayNodeId openBounds nodeId tokens acc =
    case assert_total $ parseType nodeId tokens acc of
      Fail0 err => Fail0 err
      Succ0 (elementType, afterElementNodeId) afterElement @{elementSuffix} =>
        case afterElement of
          [] => Fail0 (B EOI NoBounds)
          _ :: _ =>
            case (exact (TokSym SymSemi) *>
                  Text.Parse.Manual.acc (parseArrayLength afterElementNodeId)) afterElement of
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

parseParameterMutability : Rule False (Maybe (SurfaceAstNode Mutability))
parseParameterMutability nodeId [] _ =
    Succ0 (Nothing, nodeId) []
parseParameterMutability nodeId ((B token bounds) :: remaining) _ =
    case token of
        TokKw KwMut =>
            failWithCustomError
                (UnsupportedFeature "Mutable function arguments are not yet supported.")
                bounds
        _ =>
            Succ0 (Nothing, nodeId) (B token bounds :: remaining)

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

parseFunctionParameters : Rule True (List (SurfaceAstNode FunctionParameterNode))
parseFunctionParameters _ [] _ = Fail0 (B EOI NoBounds)
parseFunctionParameters nodeId tokens@(_ :: _) _ =
    (exact (TokSym SymLParen) *> acc (parseFunctionParameterList [<] nodeId)) tokens

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

parseBlockContents :
     NodeId
  -> Bounds
  -> SnocList SurfaceStatement
  -> Rule True SurfaceBlock

parseFunctionBody : Rule True SurfaceBlock

mutual
  parseExpression : Rule True SurfaceExpr
  parseExpression = parseRangeExpression

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
     in case assert_total $
               parseUnaryExpression afterOperatorNodeId remaining recur of
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
     in case assert_total $
               parseUnaryExpression afterOperatorNodeId remaining recur of
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
     in case assert_total $
               parseUnaryExpression afterOperatorNodeId remaining recur of
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
     in case assert_total $
               parseUnaryExpression afterOperatorNodeId remaining recur of
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
  parsePrimaryExpression _ ((B (TokKw KwAdjoint) bounds) :: _) _ =
    failWithCustomError
      (UnsupportedFeature "Adjoint expressions are not yet supported.") bounds
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
  parsePrimaryExpression nodeId ((B token bounds) :: remaining) _ =
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
        failWithCustomError
          (UnsupportedFeature "Control expressions are not yet supported.") bounds
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

  parsePostfixExpression : SurfaceExpr -> Rule False SurfaceExpr
  parsePostfixExpression callee nodeId
      ((B (TokSym SymLParen) openBounds) :: afterOpen) (SA recur) =
    let (callNodeId, afterCallNodeId) = reserveNodeId nodeId
     in case assert_total $ parseCallArguments afterCallNodeId afterOpen recur of
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
     in case assert_total $ parseCallArguments afterNameNodeId afterOpen recur of
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

  parseLoopExpression : Rule True SurfaceExpr
  parseLoopExpression _ [] _ = Fail0 (B EOI NoBounds)
  parseLoopExpression nodeId
      ((B (TokKw KwLoop) loopBounds) :: remaining) (SA recur) =
    let (expressionNodeId, afterExpressionNodeId) = reserveNodeId nodeId
     in case assert_total $ parseFunctionBody afterExpressionNodeId remaining recur of
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

parseLetInitializer : Rule True LetInitializerNode
parseLetInitializer _ [] _ = Fail0 (B EOI NoBounds)
parseLetInitializer nodeId ((B token bounds) :: remaining) acc@(SA recur) =
  case token of
    TokSym SymEq =>
      let (markerNodeId, nextNodeId) = reserveNodeId nodeId
          marker = surfaceAstNode (MkAstInfo markerNodeId (sourceSpan bounds))
                                  InitializerEquals
       in case assert_total $ parseExpression nextNodeId remaining recur of
            Fail0 err => Fail0 err
            Succ0 (value, finalNodeId) finalTokens @{valueSuffix} =>
              Succ0 (MkLetInitializerNode marker value, finalNodeId) finalTokens
                @{Data.List.Suffix.trans valueSuffix
                    (the (Suffix True remaining
                            (B (TokSym SymEq) bounds :: remaining))
                         (Uncons Same))}
    _ => Fail0 (B (Expected ["="] (show token)) bounds)

parseLetStatement : Rule True SurfaceStatement
parseLetStatement _ [] _ = Fail0 (B EOI NoBounds)
parseLetStatement nodeId ((B token letBounds) :: remaining) acc@(SA recur) =
  case token of
    TokKw KwLet =>
      case remaining of
        (B (TokKw KwMut) mutBounds) :: _ =>
          failWithCustomError
            (UnsupportedFeature "Mutable variables are not yet supported.") mutBounds
        _ =>
          let (statementNodeId, afterStatementNodeId) = reserveNodeId nodeId
              (patternNodeId, afterPatternNodeId) = reserveNodeId afterStatementNodeId
           in case parseName "variable name" afterPatternNodeId remaining recur of
                Fail0 err => Fail0 err
                Succ0 (name, afterNameNodeId) afterName @{nameSuffix} =>
                  case afterName of
                    (B (TokSym SymColon) colonBounds) :: afterColon =>
                      case parseType afterNameNodeId afterColon suffixAcc of
                        Fail0 err => Fail0 err
                        Succ0 (ty, afterTypeNodeId) afterType @{typeSuffix} =>
                          case
                                     parseLetInitializer afterTypeNodeId afterType suffixAcc of
                            Fail0 err => Fail0 err
                            Succ0 (initializer, finalNodeId) afterInitializer @{initializerSuffix} =>
                              case afterInitializer of
                                (B (TokSym SymSemi) semiBounds) :: finalTokens =>
                                  let pattern = surfaceAstNode
                                        (MkAstInfo patternNodeId name.astInfo.span)
                                        (PatternName Nothing name)
                                      binding = MkLetBindingNode [] pattern (Just ty)
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
                                                 (Uncons Same)) $
                                           Data.List.Suffix.trans initializerSuffix $
                                           Data.List.Suffix.trans typeSuffix $
                                           Data.List.Suffix.trans
                                            (the (Suffix True afterColon
                                                    (B (TokSym SymColon) colonBounds :: afterColon))
                                                 (Uncons Same))
                                            nameSuffix)
                                          (the (Suffix True remaining
                                                  (B (TokKw KwLet) letBounds :: remaining))
                                               (Uncons Same))}
                                (B unexpected unexpectedBounds) :: _ =>
                                  Fail0 (B (Expected [";"] (show unexpected)) unexpectedBounds)
                                [] => Fail0 (B EOI NoBounds)
                    (B (TokSym SymEq) eqBounds) :: afterEq =>
                      case
                                 parseLetInitializer afterNameNodeId afterName suffixAcc of
                        Fail0 err => Fail0 err
                        Succ0 (initializer, finalNodeId) afterInitializer
                              @{initializerSuffix} =>
                          case afterInitializer of
                            (B (TokSym SymSemi) semiBounds) :: finalTokens =>
                              let pattern = surfaceAstNode
                                    (MkAstInfo patternNodeId name.astInfo.span)
                                    (PatternName Nothing name)
                                  binding = MkLetBindingNode [] pattern Nothing
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
                                      (Data.List.Suffix.trans nameSuffix
                                        (the (Suffix True remaining
                                                (B (TokKw KwLet) letBounds :: remaining))
                                             (Uncons Same)))}
                            (B unexpected unexpectedBounds) :: _ =>
                              Fail0 (B (Expected [";"] (show unexpected)) unexpectedBounds)
                            [] => Fail0 (B EOI NoBounds)
                    (B unexpected unexpectedBounds) :: _ =>
                      Fail0 (B (Expected [":", "="] (show unexpected)) unexpectedBounds)
                    [] => Fail0 (B EOI NoBounds)
    _ => Fail0 (B (Expected ["let"] (show token)) letBounds)

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
                   parseExpression nodeId (B token bounds :: remaining) acc of
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

              (B (TokSym SymRBrace) closeBounds) :: finalTokens =>
                let block = surfaceAstNode
                      (MkAstInfo blockNodeId
                        (sourceSpan (openBounds <+> closeBounds)))
                      (MkBlockNode [] (statements <>> []) (Just expression))
                 in Succ0 (block, afterExpressionNodeId) finalTokens
                      @{Data.List.Suffix.trans
                          (the (Suffix True finalTokens
                                  (B (TokSym SymRBrace) closeBounds :: finalTokens))
                               (Uncons Same))
                          expressionSuffix}

              (B (TokSym symbol) operatorBounds) :: afterOperatorToken =>
                case assignmentOperator symbol of
                  Just _ =>
                    case parseAssignmentStatement expression
                           afterExpressionNodeId
                           (B (TokSym symbol) operatorBounds :: afterOperatorToken)
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

              afterBlockLike@((B unexpected unexpectedBounds) :: afterUnexpected) =>
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
                                 (B unexpected unexpectedBounds :: afterUnexpected)
                                 suffixAcc of
                          Fail0 err => Fail0 err
                          Succ0 result finalTokens @{blockSuffix} =>
                            Succ0 result finalTokens
                              @{Data.List.Suffix.trans blockSuffix expressionSuffix}
                  else
                    failWithCustomError (ParseErrorWithMessage
                      "Expected `;` or `}`, found instead: `\{interpolate unexpected}`.")
                      unexpectedBounds

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

parseFunDecl :
    (declarationStart : Bounds)
  -> (attributes : List SurfaceAttribute)
  -> (visibility : Maybe (SurfaceAstNode VisbilityQualifier))
  -> (functionEffect : Maybe (SurfaceAstNode FunctionEffect))
  -> Rule True SurfaceItem
parseFunDecl declarationStart attributes visibility functionEffect nodeId [] acc = Fail0 (B EOI NoBounds)
parseFunDecl declarationStart attributes visibility functionEffect nodeId ((B token bounds) :: remaining) acc@(SA recur) =
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
                                                                                                False               -- not const
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
             in parseFunDecl declarationStart attributes visibility (Just effectNode) nextNodeId (B token bounds :: remaining) acc

        _ =>
            failWithCustomError (ParseErrorWithMessage
              "Expected `fun` after `\{show effect}` effect modifier, found instead: `\{interpolate token}`.") bounds

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
            parseFunDecl declarationStart attributes (Just pubModifierNode) Nothing nextNodeId (B token nexTokBounds :: remaining) acc

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
      parseFunDecl declarationStart (attributes <>> []) Nothing Nothing nodeId
        (B token bounds :: remaining) acc

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

parseItem : Rule True SurfaceItem
parseItem nodeId [] acc = Fail0 (B EOI NoBounds)
parseItem nodeId ((B token tokBounds) :: remaining) acc@(SA recur) =
    case token of
    
        -- Start with items that are currently supported by the parser:

        TokKw KwImpl =>
            failWithCustomError (UnsupportedFeature "Impls blocks and structs are not yet supported.") tokBounds

        TokKw KwFn =>
            parseFunDecl tokBounds [] Nothing Nothing nodeId (B token tokBounds :: remaining) acc

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

locatedParseError : String -> Bounds -> ParseError -> Located ParseError
locatedParseError fileName bounds parseError =
    MkLocated ({ file := fileName } (sourceSpan bounds)) parseError

unexpectedLocated : String -> Bounded Token -> Either (Located ParseError) a
unexpectedLocated fileName token =
    case the (Either (Bounded ParseError) a) (Text.ParseError.unexpected token) of
        Left (B err bounds) => Left (locatedParseError fileName bounds err)
        Right result => Right result

public export
parseFile : String -> List (Bounded Token) -> Either (Located ParseError) SurfaceSourceFile
parseFile fileName tokens =
    case parseModule fileName 1 tokens suffixAcc of   -- first item node id is 1 (0 is source file node id)
        Fail0 (B err bounds) =>
            Left (locatedParseError fileName bounds err)

        Succ0 (sourceFile, _) [] =>
            Right sourceFile

        Succ0 _ ((B token bounds) :: remaining) =>
            unexpectedLocated fileName (B token bounds)
