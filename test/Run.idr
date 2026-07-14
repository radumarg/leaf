module Run

import Lexer.LexerTest
import Parser.BasicParseTest

main : IO ()
main = do
  runLexerTests
  runBasicParseTests
  putStrLn "All tests completed."
