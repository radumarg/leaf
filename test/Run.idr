module Run

import Lexer.LexerTest

main : IO ()
main = do
  runLexerTests
  putStrLn "All tests completed."
