module Frontend.Lexer.Helpers

import Derive.Prelude
import Language.Reflection

import Text.Lex
import Text.Lex.Manual

import Frontend.Token
import Frontend.Lexer.Error

%default total
%language ElabReflection

-- Helpers.idr will contain the actual lexical machinery, grouped into clearly named sections.