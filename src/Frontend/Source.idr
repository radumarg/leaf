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
  constructor MkSourceSpan
  file  : String
  start : SourcePos  -- position of first character in the token
  end   : SourcePos  -- position immediately after last character

--------------------------------------------------------------------------------
-- Combining spans
--------------------------------------------------------------------------------
-- The span of a composite AST node is the merge of its first and last
-- constituent spans: `let q: qubit := f(q);` spans from the `let` token's
-- start to the `;` token's end. This is the parser's workhorse -- every
-- located node it builds from children gets its span this way.
--
-- PRECONDITIONS (not checked, by design):
--   * both spans come from the same file -- the file of the FIRST span is
--     kept. The parser only ever merges spans from the token stream it is
--     consuming, so a cross-file merge would be a compiler bug, not a user
--     error; checking it on every node would tax the common case to guard
--     an impossible one.
--   * the first span starts no later than the second ends. Merging in
--     source order is the parser's responsibility; mergeSpans does not
--     reorder or normalize.
--------------------------------------------------------------------------------

public export
mergeSpans : SourceSpan -> SourceSpan -> SourceSpan
mergeSpans firstSpan secondSpan =
  MkSourceSpan firstSpan.file firstSpan.start secondSpan.end