module Frontend.Lexer.Error

import Derive.Prelude
import Language.Reflection

%default total
%language ElabReflection

----------------------------------------------------
-- Lexical errors that can happen before parsing. --
----------------------------------------------------

-----------------------------------------
-- Leaf language specific lexical errors
-----------------------------------------
public export
data LeafLexerError
  = LexUnexpectedCharacter Char
  | LexUnterminatedStringLiteral
  | LexInvalidStringEscape String
  | LexUnterminatedBlockComment
  | LexUnterminatedBitStringLiteral
  | LexInvalidBitStringCharacter Char
  | LexInvalidNumberLiteral String
  | LexInvalidByteLiteral String
  | LexInvalidByteStringLiteral String
  | LexEmptyBitStringLiteral

-------------------------------
-- Public lexer error type
-------------------------------
public export
LexerErr : Type
LexerErr = InnerError LeafLexerError

%runElab derive "LexerErr" [Show, Eq]
%runElab derive "LeafLexerError" [Show, Eq]
