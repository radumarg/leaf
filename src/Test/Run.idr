module Test.Run

import Test.ArraysParseTests
import Test.BitQubitParseTests
import Test.QuantumGatesParserTests
import Test.TypeParseTests
import Test.TypeQualifierParserTests


main : IO ()
main = do
  runArraysParseTests
  runBitQubitParseTests
  runQuantumGatesParseTests
  runTypeParseTests
  runTypeQualifierParseTests
  putStrLn "All tests completed."