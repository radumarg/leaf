module Frontend.Parser.Helper

import Text.Bounds
import Text.Parse.Manual

import Frontend.Source
import Frontend.Token
import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Parser.Error
import Frontend.Syntax.AST
import Frontend.Syntax.Common
import Frontend.Syntax.Doc
import Frontend.Syntax.Name

%default total

-- TODO: review
sourcePos : Position -> SourcePos
sourcePos (P line column) = MkSourcePos (S line) (S column) 0
-- TODO: review
public export
sourceSpan : Bounds -> SourceSpan
sourceSpan NoBounds =
    let start = MkSourcePos 1 1 0 in
        MkSourceSpan "" start start
sourceSpan (BS start end) =
    MkSourceSpan "" (sourcePos start) (sourcePos end)

lastItemSpan : SurfaceItem -> List SurfaceItem -> SourceSpan
lastItemSpan item [] =
    item.astInfo.span
lastItemSpan _ (item :: rest) =
    lastItemSpan item rest

public export
sourceFileInfo : String -> NodeId -> List SurfaceItem -> AstInfo
sourceFileInfo sourceFileName nodeId [] =
    let start = MkSourcePos 1 1 0 in
        MkAstInfo nodeId (MkSourceSpan sourceFileName start start)
sourceFileInfo _ nodeId (first :: rest) =
    let firstSpan = first.astInfo.span
        lastSpan = lastItemSpan first rest
     in MkAstInfo nodeId (mergeSpans firstSpan lastSpan)

public export
failWithCustomError : CustomParseError -> Bounds -> Res isStrict Token tokens CustomParseError a
failWithCustomError customParseError bounds = Fail0 (B (Custom customParseError) bounds)

public export
parameterStartSpan :
     List SurfaceDocComment
  -> Maybe (SurfaceAstNode Mutability)
  -> SurfaceName
  -> SourceSpan
parameterStartSpan (doc :: _) _ _ = doc.astInfo.span
parameterStartSpan [] (Just mutability) _ = mutability.astInfo.span
parameterStartSpan [] Nothing name = name.astInfo.span
