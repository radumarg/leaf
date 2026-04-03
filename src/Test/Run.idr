module Test.Run

import Test.TypeParseTests

main : IO ()
main = do
  runTypeParseTest
  putStrLn "All tests completed."