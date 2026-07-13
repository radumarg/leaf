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
  -> Res strict Token tokens Void (result, Nat)

parseItem : Rule True SurfaceItem
parseItem nodeId [] acc =
    Fail0 (B EOI NoBounds)
parseItem nodeId tokens@((B token bounds) :: remaining) acc =
    let itemId = MkNodeId nodeId
        nextNodeId = S nodeId
     in case token of
            TokKw KwFn =>
                ?parse_function_item

            TokKw KwStruct =>
                ?parse_struct_item

            TokKw KwEnum =>
                ?parse_enum_item

            TokKw KwQenum =>
                ?parse_qenum_item

            TokKw KwImpl =>
                ?parse_impl_item

            -- This may begin either a const declaration or `const fn`.
            TokKw KwConst =>
                ?parse_const_item

            TokKw KwUse =>
                ?parse_use_item

            TokKw KwMod =>
                ?parse_module_item

            -- Visibility, documentation, attributes, and function effects
            -- precede the keyword that determines the ItemNode constructor.
            TokKw KwPub =>
                ?parse_public_item

            TokOuterDoc _ =>
                ?parse_documented_item

            TokSym _ =>
                ?parse_possibly_attributed_item

            TokKw KwUnitary =>
                ?parse_unitary_function_item

            TokKw KwClassical =>
                ?parse_classical_function_item

            TokKw KwGeneral =>
                ?parse_general_function_item

            _ =>
                ?parse_unexpected_item

parseItems : SnocList SurfaceItem -> Rule False (List SurfaceItem)
parseItems items nextNodeId [] _ =
    Fail0 (B EOI NoBounds)  -- every valid token stream must contain TokEOF
parseItems items nextNodeId
           ((B TokEOF _) :: remaining)
           (SA recur) =
    Succ0 (items <>> [], nextNodeId) remaining
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
