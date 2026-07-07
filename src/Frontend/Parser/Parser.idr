module Frontend.Parser.Parser

import Text.Bounds

import Frontend.Token
import Frontend.Syntax.Module
import Frontend.Parser.Error

%default total

--------------------------------------------------------------------------------
-- Main entry point: parseFile
--------------------------------------------------------------------------------
public export
parseModule : List (Bounded Token) -> Either (Bounded ParseError) Module
parseModule tokens = ?parseModule_rhs