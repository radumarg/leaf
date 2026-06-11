module Frontend.Lexer.Error

import Derive.Prelude
import Language.Reflection

%default total
%language ElabReflection

--------------------------------------------------------------------------------
-- LexerErr: lexical errors that can happen before parsing.
--------------------------------------------------------------------------------
public export
data LexerErr
  = LexUnexpectedChar Char
  | LexUnterminatedString
  | LexUnterminatedBlockComment
  | LexInvalidBitStringLiteral String
  | LexInvalidNumberLiteral String
  | LexFuelExhausted

%runElab derive "LexerErr" [Show, Eq]
