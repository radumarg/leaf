module Frontend.Lexer.Helpers

import Derive.Prelude
import Language.Reflection

%default total
%language ElabReflection

-- this file shoould contain helpers for the lexer like:
-- isIdentStart : Char -> Bool
-- isIdentRest  : Char -> Bool
-- isDigit      : Char -> Bool
-- isWhitespace : Char -> Bool