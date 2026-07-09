module Frontend.Parser.Parser

import Text.Bounds

import Frontend.Token
import Frontend.Syntax.AST
import Frontend.Parser.Error

%default total

--------------------------------------------------------------------------------
-- Main entry point: parseFile
--------------------------------------------------------------------------------
public export
parseFile : List (Bounded Token) -> Either (Bounded ParserError) SurfaceSourceFile
parseFile tokens = ?parseFile_rhs