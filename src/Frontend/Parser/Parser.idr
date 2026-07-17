module Frontend.Parser.Parser

import Text.Bounds
import Text.Parse.Manual
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
        Succ0 (surfaceAstNode (MkAstInfo typeNodeId (sourceSpan bounds))
                              (TyPrimitive primitiveName), nextNodeId) remaining
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
          (B (TokSym SymSemi) semiBounds) :: afterSemi =>
            case parseArrayLength afterElementNodeId afterSemi suffixAcc of
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
                                   (Uncons Same)) $
                            Data.List.Suffix.trans lengthSuffix $
                            Data.List.Suffix.trans
                              (the (Suffix True afterSemi
                                      (B (TokSym SymSemi) semiBounds :: afterSemi))
                                   (Uncons Same))
                              elementSuffix}
                  (B unexpected unexpectedBounds) :: _ =>
                    Fail0 (B (Expected ["]"] (show unexpected)) unexpectedBounds)
          (B unexpected unexpectedBounds) :: _ =>
            Fail0 (B (Expected [";"] (show unexpected)) unexpectedBounds)

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
            case assert_total $ parseTupleTail afterFirstNodeId afterComma suffixAcc of
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
                                (B token bounds) :: afterColon =>
                                    case token of
                                        TokSym SymColon =>
                                            case parseType
                                                    afterNameNodeId
                                                    afterColon
                                                    suffixAcc of
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
                                                              Data.List.Suffix.trans
                                                                (the
                                                                  (Suffix True
                                                                    afterColon
                                                                    (B (TokSym SymColon) bounds :: afterColon))
                                                                  (Uncons Same)) $
                                                              Data.List.Suffix.trans nameSuffix $
                                                              Data.List.Suffix.trans mutabilitySuffix $
                                                              docsSuffix}
                                        _ =>
                                            Fail0 (B (Expected [":"] (show token)) bounds)

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
parseFunctionParameters nodeId
    ((B token bounds) :: remaining) acc@(SA recur) =
    case token of
        TokSym SymLParen =>
            succT $ parseFunctionParameterList [<] nodeId remaining recur
        _ =>
            Fail0 (B (Expected ["("] (show token)) bounds)

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

-- TODO: parse other expressions, including array literals, function calls, and so on.
parseExpression : Rule True SurfaceExpr
parseExpression = parseIntegerExpression "integer literal"

parseLetInitializer : Rule True LetInitializerNode
parseLetInitializer _ [] _ = Fail0 (B EOI NoBounds)
parseLetInitializer nodeId ((B token bounds) :: remaining) acc@(SA recur) =
  case token of
    TokSym SymEq =>
      let (markerNodeId, nextNodeId) = reserveNodeId nodeId
          marker = surfaceAstNode (MkAstInfo markerNodeId (sourceSpan bounds))
                                  InitializerEquals
       in case parseExpression nextNodeId remaining recur of
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
                          case parseLetInitializer afterTypeNodeId afterType suffixAcc of
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
                    (B unexpected unexpectedBounds) :: _ =>
                      Fail0 (B (Expected [":"] (show unexpected)) unexpectedBounds)
                    [] => Fail0 (B EOI NoBounds)
    _ => Fail0 (B (Expected ["let"] (show token)) letBounds)

parseBlockContents :
     NodeId
  -> Bounds
  -> SnocList SurfaceStatement
  -> Rule True SurfaceBlock
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
            succT $ assert_total $
              parseBlockContents blockNodeId openBounds
                (statements :< statement) nextNodeId afterStatement recur

      _ =>
        case parseExpression nodeId (B token bounds :: remaining) acc of
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

              (B unexpected unexpectedBounds) :: _ =>
                Fail0 (B (Expected [";", "}"] (show unexpected)) unexpectedBounds)

parseFunctionBody : Rule True SurfaceBlock
parseFunctionBody _ [] _ = Fail0 (B EOI NoBounds)
parseFunctionBody nodeId ((B token bounds) :: remaining) acc@(SA recur) =
    case token of
      TokSym SymLBrace =>
        let (blockNodeId, nextNodeId) = reserveNodeId nodeId
         in succT $
              parseBlockContents blockNodeId bounds [<] nextNodeId remaining recur
      _ => Fail0 (B (Expected ["{"] (show token)) bounds)

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

    B token bounds :: _ =>
      Fail0 (B (Expected ["#[attribute] or #[attribute(\"name\")]"] (show token)) bounds)

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
            Fail0 (B (Expected ["fn"] (show token)) bounds)

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
            Fail0 (B (Expected ["fn"] (show token)) bounds)

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

        TokKw KwClassical =>
            succT $ parseFunDeclWithEffect declarationStart attributes (Just pubModifierNode) EffectClassical nexTokBounds nextNodeId remaining recur

        TokKw KwUnitary =>
            succT $ parseFunDeclWithEffect declarationStart attributes (Just pubModifierNode) EffectUnitary nexTokBounds nextNodeId remaining recur

        TokKw KwIsometry =>
            succT $ parseFunDeclWithEffect declarationStart attributes (Just pubModifierNode) EffectIsometry nexTokBounds nextNodeId remaining recur

        TokKw KwCoisometry =>
            succT $ parseFunDeclWithEffect declarationStart attributes (Just pubModifierNode) EffectCoisometry nexTokBounds nextNodeId remaining recur

        TokKw KwGeneral =>
            succT $ parseFunDeclWithEffect declarationStart attributes (Just pubModifierNode) EffectGeneral nexTokBounds nextNodeId remaining recur

        _ =>
            Fail0 (B (Expected ["fn", "classical", "unitary", "isometry", "coisometry", "general"] (show token)) nexTokBounds)

parseAttributedItem : Bounds -> SnocList SurfaceAttribute -> Rule True SurfaceItem
parseAttributedItem _ _ _ [] _ = Fail0 (B EOI NoBounds)
parseAttributedItem declarationStart attributes nodeId
    tokens@((B token bounds) :: remaining) acc@(SA recur) =
  case token of
    TokSym SymHash =>
      case parseAttribute nodeId (B token bounds :: remaining) acc of
        Fail0 err => Fail0 err
        Succ0 (attribute, nextNodeId) afterAttribute =>
          succT $ assert_total $
            parseAttributedItem declarationStart (attributes :< attribute)
              nextNodeId afterAttribute recur
    TokKw KwFn =>
      parseFunDecl declarationStart (attributes <>> []) Nothing Nothing nodeId
        (B token bounds :: remaining) acc
    TokKw KwPub =>
      succT $ parsePubFunDecl declarationStart bounds (attributes <>> [])
        nodeId remaining recur
    TokKw KwClassical =>
      succT $ parseFunDeclWithEffect declarationStart (attributes <>> [])
        Nothing EffectClassical bounds nodeId remaining recur
    TokKw KwUnitary =>
      succT $ parseFunDeclWithEffect declarationStart (attributes <>> [])
        Nothing EffectUnitary bounds nodeId remaining recur
    TokKw KwIsometry =>
      succT $ parseFunDeclWithEffect declarationStart (attributes <>> [])
        Nothing EffectIsometry bounds nodeId remaining recur
    TokKw KwCoisometry =>
      succT $ parseFunDeclWithEffect declarationStart (attributes <>> [])
        Nothing EffectCoisometry bounds nodeId remaining recur
    TokKw KwGeneral =>
      succT $ parseFunDeclWithEffect declarationStart (attributes <>> [])
        Nothing EffectGeneral bounds nodeId remaining recur
    _ => Fail0 (B (Expected ["function declaration after attribute"] (show token)) bounds)

parseItem : Rule True SurfaceItem
parseItem nodeId [] acc = Fail0 (B EOI NoBounds)
parseItem nodeId ((B token tokBounds) :: remaining) acc@(SA recur) =
    case token of
    
        -- Start with items that are currently supported by the parser:

        TokKw KwFn =>
            parseFunDecl tokBounds [] Nothing Nothing nodeId (B token tokBounds :: remaining) acc

        TokKw KwStruct =>
            failWithCustomError (UnsupportedFeature "Structs are not yet supported.") tokBounds

        TokKw KwEnum =>
            failWithCustomError (UnsupportedFeature "Enums are not yet supported.") tokBounds

        TokKw KwQenum =>
            failWithCustomError (UnsupportedFeature "Qenums are not yet supported.") tokBounds

        TokKw KwImpl =>
            failWithCustomError (UnsupportedFeature "Impls blocks for struct are not yet supported.") tokBounds

        TokKw KwUse =>
            failWithCustomError (UnsupportedFeature "Use statements are not yet supported.") tokBounds

        TokKw KwMod =>
            failWithCustomError (UnsupportedFeature "Modules are not yet supported.") tokBounds

        TokKw KwConst =>
            failWithCustomError (UnsupportedFeature "Const declarations or const functions are not yet supported.") tokBounds

        -- Visibility, documentation, attributes, and function effects that may precede the item keyword:

        TokKw KwPub =>
            succT $ parsePubFunDecl tokBounds tokBounds [] nodeId remaining recur

        TokOuterDoc _ =>
            failWithCustomError (UnsupportedFeature "Outer doc comments are not yet supported.") tokBounds

        TokSym SymHash =>
            parseAttributedItem tokBounds [<] nodeId (B token tokBounds :: remaining) acc

        TokKw KwClassical =>
            succT $ parseFunDeclWithEffect tokBounds [] Nothing EffectClassical tokBounds nodeId remaining recur

        TokKw KwUnitary =>
            succT $ parseFunDeclWithEffect tokBounds [] Nothing EffectUnitary tokBounds nodeId remaining recur

        TokKw KwIsometry =>
            succT $ parseFunDeclWithEffect tokBounds [] Nothing EffectIsometry tokBounds nodeId remaining recur

        TokKw KwCoisometry =>
            succT $ parseFunDeclWithEffect tokBounds [] Nothing EffectCoisometry tokBounds nodeId remaining recur

        TokKw KwGeneral =>
            succT $ parseFunDeclWithEffect tokBounds [] Nothing EffectGeneral tokBounds nodeId remaining recur

        _ =>
            -- Extend error message with new features when these become available: module declarations, const declarations, structs and/or impl blocks, enums, qenums and inline docs.
            failWithCustomError (UnexpectedToken ("Unexpected token: " ++ show token ++ " at top level in source file. At module level only only function declarations are allowed for now.")) tokBounds

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

parseModule : Rule False SurfaceSourceFile
parseModule firstItemNodeId tokens acc =
    case parseItems [<] firstItemNodeId tokens acc of
        Fail0 err =>
            Fail0 err

        Succ0 (items, nextNodeId) remaining =>
            Succ0
                ( surfaceAstNode
                    (sourceFileInfo (MkNodeId 0) items) -- source file node id is always 0
                    (MkSourceFileNode [] items)         -- ignore inner doc comments for now
                , nextNodeId
                )
                remaining

---------------------------------------------------------------------------------------------------
-- Main entry point: parse file using the idris2-parser library's machinery from Text.Parse.Manual
---------------------------------------------------------------------------------------------------

public export
parseFile : List (Bounded Token) -> Either (Bounded ParseError) SurfaceSourceFile
parseFile tokens =
    case parseModule 1 tokens suffixAcc of  -- first item node id is 1 (0 is source file node id)
        Fail0 err =>
            Left err

        Succ0 (sourceFile, _) [] =>
            Right sourceFile

        Succ0 _ (token :: _) =>
            unexpected token
