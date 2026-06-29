module Test.Run

import Test.Lexer.LexerTest

main : IO ()
main = do
  runLexerTests
  putStrLn "All tests completed."
