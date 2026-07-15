module Frontend.Parser.Parser

import Text.Bounds
import Text.Parse.Manual

import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Source
import Frontend.Token
import Frontend.Syntax.AST
import Frontend.Syntax.Common
import Frontend.Syntax.Doc
import Frontend.Parser.Error
import Frontend.Parser.Helper

%default total

0 Rule : Bool -> Type -> Type
Rule strict result =
     (nextNodeId : Nat)
  -> (tokens : List (Bounded Token))
  -> (0 acc : SuffixAcc tokens)
  -> Res strict Token tokens CustomParseError (result, Nat)

parseFunDecl :
     (functionEffect : Maybe (SurfaceAstNode FunctionEffect))
  -> Rule True SurfaceItem
parseFunDecl functionEffect nodeId [] acc = Fail0 (B EOI NoBounds)
parseFunDecl functionEffect nodeId ((B token bounds) :: remaining) acc =
    case token of
        TokKw KwFn =>
            ?parse_function_item

        _ =>
            Fail0 (B (Expected ["fn"] (show token)) bounds)

parseFunDeclWithEffect :
     (effect : FunctionEffect)
  -> (effectBounds : Bounds)
  -> Rule True SurfaceItem
parseFunDeclWithEffect effect effectBounds nodeId [] acc = Fail0 (B EOI NoBounds)
parseFunDeclWithEffect effect effectBounds nodeId ((B token bounds) :: remaining) acc =
    let nextNodeId = S nodeId
    in case token of
        TokKw KwFn =>
            let effectNode =
                    surfaceAstNode
                        (MkAstInfo (MkNodeId nodeId) (sourceSpan (effectBounds <+> bounds)))
                        effect
             in parseFunDecl (Just effectNode) nextNodeId (B token bounds :: remaining) acc

        _ =>
            Fail0 (B (Expected ["fn"] (show token)) bounds)

parseItem : Rule True SurfaceItem
parseItem nodeId [] acc = Fail0 (B EOI NoBounds)
parseItem nodeId ((B token bounds) :: remaining) acc@(SA recur) =
    case token of
        -- Start with items that are currently supported by the parser:

        TokKw KwFn =>
            parseFunDecl Nothing nodeId (B token bounds :: remaining) acc

        TokKw KwStruct =>
            failWithCustomError (UnsupportedFeature "Structs are not yet supported.") bounds

        TokKw KwEnum =>
            failWithCustomError (UnsupportedFeature "Enums are not yet supported.") bounds

        TokKw KwQenum =>
            failWithCustomError (UnsupportedFeature "Qenums are not yet supported.") bounds

        TokKw KwImpl =>
            failWithCustomError (UnsupportedFeature "Impls blocks for struct are not yet supported.") bounds

        TokKw KwUse =>
            failWithCustomError (UnsupportedFeature "Use statements are not yet supported.") bounds

        TokKw KwMod =>
            failWithCustomError (UnsupportedFeature "Modules are not yet supported.") bounds

        TokKw KwConst =>
            failWithCustomError (UnsupportedFeature "Const declarations or const functions are not yet supported.") bounds

        -- Visibility, documentation, attributes, and function effects that may precede the item keyword:

        TokKw KwPub =>
            failWithCustomError (UnsupportedFeature "Modules and public declarations are not yet supported.") bounds

        TokOuterDoc _ =>
            failWithCustomError (UnsupportedFeature "Outer doc comments are not yet supported.") bounds

        TokSym SymHash =>
            ?parse_attribute_item

        TokKw KwClassical =>
            succT $ parseFunDeclWithEffect EffectClassical bounds nodeId remaining recur

        TokKw KwUnitary =>
            succT $ parseFunDeclWithEffect EffectUnitary bounds nodeId remaining recur

        TokKw KwIsometry =>
            succT $ parseFunDeclWithEffect EffectIsometry bounds nodeId remaining recur

        TokKw KwCoisometry =>
            succT $ parseFunDeclWithEffect EffectCoisometry bounds nodeId remaining recur

        TokKw KwGeneral =>
            succT $ parseFunDeclWithEffect EffectGeneral bounds nodeId remaining recur

        _ =>
            failWithCustomError (UnsupportedFeature ("Unexpected token at top level: " ++ show token)) bounds

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
