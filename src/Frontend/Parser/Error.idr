module Frontend.Parser.Error

import Derive.Prelude
import Language.Reflection

%default total
%language ElabReflection

--------------------------------------------------------------------------------
-- ParseError
--
-- This is the public, user-facing Leaf parser error type.
--------------------------------------------------------------------------------
public export
data ParseError
  = MkParseError

%runElab derive "ParseError" [Show, Eq]

