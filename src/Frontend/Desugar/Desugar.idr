module Frontend.Desugar.Desugar

import Frontend.Syntax.AST

%default total

desugarSurfaceSyntax : SurfaceSourceFile ->  SurfaceSourceFile
desugarSurfaceSyntax = ?desugar
