module Frontend.Source

%default total

public export
record SourcePos where
  constructor MkSourcePos
  line   : Nat   -- 1-based
  column : Nat   -- 1-based
  offset : Nat   -- 0-based absolute character offset

public export
record SourceSpan where
  constructor MkSourceSpan
  file  : String
  start : SourcePos  -- position of first character in the token
  end   : SourcePos  -- position immediately after last character



