module Frontend.Parser.Helper

import Text.Bounds
import Text.Parse.Manual

import Frontend.Source
import Frontend.Token
import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Parser.Error
import Frontend.Syntax.AST

%default total

lastItemSpan : SurfaceItem -> List SurfaceItem -> SourceSpan
lastItemSpan item [] =
    item.astInfo.span
lastItemSpan _ (item :: rest) =
    lastItemSpan item rest

public export
sourceFileInfo : NodeId -> List SurfaceItem -> AstInfo
sourceFileInfo nodeId [] =
    let start = MkSourcePos 1 1 0 in
        MkAstInfo nodeId (MkSourceSpan "" start start)
sourceFileInfo nodeId (first :: rest) =
    let firstSpan = first.astInfo.span
        lastSpan = lastItemSpan first rest
     in MkAstInfo nodeId (mergeSpans firstSpan lastSpan)

public export
failWithMessage : String -> Bounds -> Res strict Token tokens CustomParseError a
failWithMessage message bounds =
    Fail0 (B (Custom (UnsupportedFeature message)) bounds)

