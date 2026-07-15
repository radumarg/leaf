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
renderParseError EOI = "Unexpected end of input"
renderParseError (Expected expected actual) =
  "Expected " ++ show expected ++ ", but got " ++ actual
renderParseError (ExpectedChar charClass) = "Expected " ++ interpolate charClass
renderParseError ExpectedEOI = "Expected end of input"
renderParseError (InvalidControl char) = "Invalid control character: " ++ show char
renderParseError InvalidEscape = "Invalid escape sequence"
renderParseError (OutOfBounds value) = "Value out of bounds: " ++ value
renderParseError (Unclosed value) = "Unclosed " ++ value
renderParseError (Unknown value) = "Unknown or invalid token: " ++ value
renderParseError (InvalidByte value) = "Unexpected or invalid byte: " ++ show value
