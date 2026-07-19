module Frontend.Source

%default total

--------------------------------------------------------------------------------
-- Source positions and spans
--------------------------------------------------------------------------------
-- The location vocabulary shared by the lexer, parser, AST, and diagnostics.
-- A SourcePos is a point; a SourceSpan is a half-open region [start, end)
-- within one file: `end` is the position immediately AFTER the last
-- character, so a span's length is end.offset - start.offset and two
-- adjacent tokens share a boundary position without overlapping.
--------------------------------------------------------------------------------

public export
record SourcePos where
  constructor MkSourcePos
  line   : Nat   -- 1-based
  column : Nat   -- 1-based
  offset : Nat   -- 0-based absolute character offset

public export
record SourceSpan where
  constructor MkSourceSpan  -- [start, end)
  file  : String
  start : SourcePos         -- position of first character in the token
  end   : SourcePos         -- position immediately after last character

public export
record Located a where
  constructor MkLocated
  span  : SourceSpan
  value : a


--------------------------------------------------------------------------------
-- Combining spans
--------------------------------------------------------------------------------
-- The span of a composite AST node is the merge of its first and last
-- constituent spans: `let q: qubit := f(q);` spans from the `let` token's
-- start to the `;` token's end.
-- Merging spans from different files and merging spans where the first span
-- starts after the second span ends is a logic error but will be detected at
-- runtime. The alternative is to add proofs to the type system, but that would
-- make the parser less readable.
--------------------------------------------------------------------------------

public export
mergeSpans : SourceSpan -> SourceSpan -> SourceSpan
mergeSpans firstSpan secondSpan =
  if firstSpan.file /= secondSpan.file
     then assert_total $ idris_crash "mergeSpans: cannot merge spans from different files"
     else if firstSpan.start.offset > secondSpan.end.offset
             then assert_total $ idris_crash "mergeSpans: first span starts after second span ends"
             else MkSourceSpan firstSpan.file firstSpan.start secondSpan.end
