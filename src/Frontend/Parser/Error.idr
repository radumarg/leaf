module Frontend.Parser.Error

import Derive.Prelude
import Language.Reflection

%default total
%language ElabReflection

--------------------------------------------------------------------------------
-- ParserError
--
-- This is the public, user-facing Leaf parser error type.
--------------------------------------------------------------------------------
public export
data ParserError
  = MkParserError

%runElab derive "ParserError" [Show, Eq]

