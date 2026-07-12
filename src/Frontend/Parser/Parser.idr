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

%default total

0 Rule : Bool -> Type -> Type
Rule strict result =
    (tokens : List (Bounded Token))
  -> (0 acc : SuffixAcc tokens)
  -> Res strict Token tokens Void result

parseItem : Rule True SurfaceItem
parseItem tokens acc = ?parseItem_rhs

parseItems : SnocList SurfaceItem -> Rule False (List SurfaceItem)
parseItems items [] _ =
    Succ0 (items <>> []) []
parseItems items tokens acc@(SA recur) =
    case parseItem tokens acc of
        Fail0 err =>
            Fail0 err

        Succ0 item remaining =>
            succF $ parseItems (items :< item) remaining recur

lastItemSpan : SurfaceItem -> List SurfaceItem -> SourceSpan
lastItemSpan item [] =
    item.astInfo.span
lastItemSpan _ (item :: rest) =
    lastItemSpan item rest

sourceFileInfo : List SurfaceItem -> AstInfo
sourceFileInfo [] =
    let start = MkSourcePos 1 1 0 in
        MkAstInfo (MkNodeId 0) (MkSourceSpan "" start start)
sourceFileInfo (first :: rest) =
    let firstSpan = first.astInfo.span
        lastSpan = lastItemSpan first rest
     in MkAstInfo (MkNodeId 0) (mergeSpans firstSpan lastSpan)

parseSourceFile : Rule False SurfaceSourceFile
parseSourceFile tokens acc =
    case parseItems [<] tokens acc of
        Fail0 err =>
            Fail0 err

        Succ0 items remaining =>
            Succ0
                (surfaceAstNode
                    (sourceFileInfo items)
                    (MkSourceFileNode [] items))
                remaining

--------------------------------------------------------------------------------
-- Main entry point: parseFile, using the idris2-parser library's machinery.
--------------------------------------------------------------------------------
public export
parseFile : List (Bounded Token) -> Either (Bounded ParseError) SurfaceSourceFile
parseFile tokens =
    case parseSourceFile tokens suffixAcc of
        Fail0 err =>
            Left err

        Succ0 sourceFile [] =>
            Right sourceFile

        Succ0 _ (token :: _) =>
            unexpected token
