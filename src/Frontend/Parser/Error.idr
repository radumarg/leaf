module Frontend.Parser.Error

import Text.Parse.Manual
import public Text.ParseError

%default total

public export
data CustomParseError
    = UnsupportedFeature String
    | UnexpectedToken String

public export
0 ParseError : Type
ParseError = InnerError CustomParseError

public export
renderParseError : ParseError -> String
renderParseError (Custom (UnsupportedFeature message)) = message
renderParseError (Custom (UnexpectedToken message)) = message
renderParseError EOI = "Parse error: unexpected end of input"
renderParseError (Expected expected actual) =
  "Parse error: expected " ++ show expected ++ ", but got " ++ actual
renderParseError (ExpectedChar charClass) = "Parse error: expected " ++ interpolate charClass
renderParseError ExpectedEOI = "Parse error: expected end of input"
renderParseError (InvalidControl char) = "Parse error: invalid control character: " ++ show char
renderParseError InvalidEscape = "Parse error: invalid escape sequence"
renderParseError (OutOfBounds value) = "Parse error: value out of bounds: " ++ value
renderParseError (Unclosed value) = "Parse error: unclosed " ++ value
renderParseError (Unknown value) = "Parse error: unknown or invalid token: " ++ value
renderParseError (InvalidByte value) = "Parse error: unexpected or invalid byte: " ++ show value
