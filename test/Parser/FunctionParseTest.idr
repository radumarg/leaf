module Parser.FunctionParseTest

import Test.Simple

import Parser.Helper

%default total

export
runFunctionParseTests : IO ()
runFunctionParseTests = runTests $ Test.do

  test "empty function" $
    parseAndPrettyPrint "fn empty() {}" `shouldBe` Just "fn empty() { }"
