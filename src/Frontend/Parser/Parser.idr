module Frontend.Parser.Parser

import Text.Bounds
import Text.Parse.Manual

import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Source
import Frontend.Token
import Frontend.Syntax.AST
import Frontend.Syntax.Attribute
import Frontend.Syntax.Common
import Frontend.Syntax.Contract
import Frontend.Syntax.Doc
import Frontend.Syntax.Name
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

parseType : Rule True SurfaceTy
parseType = ?parse_type

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

parseFunctionBody : Rule True SurfaceBlock
parseFunctionBody = ?parse_function_body

parseFunDecl :
    (declarationStart : Bounds)
  -> (visibility : Maybe (SurfaceAstNode VisbilityQualifier))
  -> (functionEffect : Maybe (SurfaceAstNode FunctionEffect))
  -> Rule True SurfaceItem
parseFunDecl declarationStart visibility functionEffect nodeId [] acc = Fail0 (B EOI NoBounds)
parseFunDecl declarationStart visibility functionEffect nodeId ((B token bounds) :: remaining) acc@(SA recur) =
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
                                                                                                []                  -- attributes
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
  -> (visibility : Maybe (SurfaceAstNode VisbilityQualifier))
  -> (effect : FunctionEffect)
  -> (effectBounds : Bounds)
  -> Rule True SurfaceItem
parseFunDeclWithEffect declarationStart visibility effect effectBounds nodeId [] acc = Fail0 (B EOI NoBounds)
parseFunDeclWithEffect declarationStart visibility effect effectBounds nodeId ((B token bounds) :: remaining) acc =
    let (funNodeId, nextNodeId) = reserveNodeId nodeId
    in case token of
        TokKw KwFn =>
            let effectNode =
                    surfaceAstNode
                        (MkAstInfo funNodeId (sourceSpan effectBounds))
                        effect
             in parseFunDecl declarationStart visibility (Just effectNode) nextNodeId (B token bounds :: remaining) acc

        _ =>
            Fail0 (B (Expected ["fn"] (show token)) bounds)

parsePubFunDecl : Bounds -> Rule True SurfaceItem
parsePubFunDecl pubTokenBounds nodeId [] acc = Fail0 (B EOI NoBounds)
parsePubFunDecl pubTokenBounds nodeId ((B token nexTokBounds) :: remaining) acc@(SA recur) =
    let (pubModifierNodeId, nextNodeId) = reserveNodeId nodeId
        pubModifierNode =
                    surfaceAstNode
                        (MkAstInfo pubModifierNodeId (sourceSpan pubTokenBounds))
                        VisibilityPublic
    in case token of
        TokKw KwFn =>
            parseFunDecl pubTokenBounds (Just pubModifierNode) Nothing nextNodeId (B token nexTokBounds :: remaining) acc

        TokKw KwClassical =>
            succT $ parseFunDeclWithEffect pubTokenBounds (Just pubModifierNode) EffectClassical nexTokBounds nextNodeId remaining recur

        TokKw KwUnitary =>
            succT $ parseFunDeclWithEffect pubTokenBounds (Just pubModifierNode) EffectUnitary nexTokBounds nextNodeId remaining recur

        TokKw KwIsometry =>
            succT $ parseFunDeclWithEffect pubTokenBounds (Just pubModifierNode) EffectIsometry nexTokBounds nextNodeId remaining recur

        TokKw KwCoisometry =>
            succT $ parseFunDeclWithEffect pubTokenBounds (Just pubModifierNode) EffectCoisometry nexTokBounds nextNodeId remaining recur

        TokKw KwGeneral =>
            succT $ parseFunDeclWithEffect pubTokenBounds (Just pubModifierNode) EffectGeneral nexTokBounds nextNodeId remaining recur

        _ =>
            Fail0 (B (Expected ["fn", "classical", "unitary", "isometry", "coisometry", "general"] (show token)) nexTokBounds)

parseItem : Rule True SurfaceItem
parseItem nodeId [] acc = Fail0 (B EOI NoBounds)
parseItem nodeId ((B token tokBounds) :: remaining) acc@(SA recur) =
    case token of
        -- Start with items that are currently supported by the parser:

        TokKw KwFn =>
            parseFunDecl tokBounds Nothing Nothing nodeId (B token tokBounds :: remaining) acc

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
            succT $ parsePubFunDecl tokBounds nodeId remaining recur

        TokOuterDoc _ =>
            failWithCustomError (UnsupportedFeature "Outer doc comments are not yet supported.") tokBounds

        TokSym SymHash =>
            ?parse_attribute_item

        TokKw KwClassical =>
            succT $ parseFunDeclWithEffect tokBounds Nothing EffectClassical tokBounds nodeId remaining recur

        TokKw KwUnitary =>
            succT $ parseFunDeclWithEffect tokBounds Nothing EffectUnitary tokBounds nodeId remaining recur

        TokKw KwIsometry =>
            succT $ parseFunDeclWithEffect tokBounds Nothing EffectIsometry tokBounds nodeId remaining recur

        TokKw KwCoisometry =>
            succT $ parseFunDeclWithEffect tokBounds Nothing EffectCoisometry tokBounds nodeId remaining recur

        TokKw KwGeneral =>
            succT $ parseFunDeclWithEffect tokBounds Nothing EffectGeneral tokBounds nodeId remaining recur

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
