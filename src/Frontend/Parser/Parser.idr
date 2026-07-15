module Frontend.Parser.Parser

import Text.Bounds
import Text.Parse.Manual

import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Source
import Frontend.Token
import Frontend.Syntax.AST
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

parseItem : Rule True SurfaceItem
parseItem nodeId [] acc = Fail0 (B EOI NoBounds)
parseItem nodeId tokens@((B token bounds) :: remaining) acc =
    let itemId = MkNodeId nodeId
        nextNodeId = S nodeId
     in case token of

            -- Start with items that are currently supported by the parser:

            TokKw KwFn =>
                ?parse_function_item

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

            TokKw KwUnitary =>
                ?parse_unitary_function_item

            TokKw KwClassical =>
                ?parse_classical_function_item

            TokKw KwGeneral =>
                ?parse_general_function_item

            _ =>
                failWithCustomError (UnsupportedFeature ("Unexpected token at top level: " ++ show token)) bounds

parseItems : SnocList SurfaceItem -> Rule False (List SurfaceItem)
parseItems items nextNodeId [] _ =
    Fail0 (B EOI NoBounds)  -- every valid token stream must contain TokEOF
parseItems items nextNodeId [B TokEOF _] (SA recur) =
    Succ0 (items <>> [], nextNodeId) []
parseItems _ _ ((B TokEOF _) :: (B token bounds) :: remaining) _ =
    failWithCustomError
        (UnexpectedEOFToken
            ("Token " ++ show token ++ " occurs after the end-of-input marker."))
        bounds
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
