module Test.TypeParseTests

import Test.TestHelper

export
runTypeParseTests : IO ()
runTypeParseTests = do

  -- Test type declarations
  runParseOkTest "src/Test/Fixtures/Good/Types/i8TypeDeclaration.lf"
                  "let i : i8 = -1;"
                  "let i : i8 = (-1);"
  runParseOkTest "src/Test/Fixtures/Good/Types/i16TypeDeclaration.lf"
                  "let i : i16 = -1;"
                  "let i : i16 = (-1);"
  runParseOkTest "src/Test/Fixtures/Good/Types/i32TypeDeclaration.lf"
                  "let i : i32 = 1;"
                  "let i : i32 = 1;"
  runParseOkTest "src/Test/Fixtures/Good/Types/i64TypeDeclaration.lf"
                  "let i : i64 = 1;"
                  "let i : i64 = 1;"
  runParseOkTest "src/Test/Fixtures/Good/Types/i128TypeDeclaration.lf"
                  "let i : i128 = -1;"
                  "let i : i128 = (-1);"
  runParseOkTest "src/Test/Fixtures/Good/Types/u8TypeDeclaration.lf"
                  "let u : u8 = 1;"
                  "let u : u8 = 1;"
  runParseOkTest "src/Test/Fixtures/Good/Types/u16TypeDeclaration.lf"
                  "let u : u16 = 1;"
                  "let u : u16 = 1;"
  runParseOkTest "src/Test/Fixtures/Good/Types/u32TypeDeclaration.lf"
                  "let u : u32 = 1;"
                  "let u : u32 = 1;"
  runParseOkTest "src/Test/Fixtures/Good/Types/u64TypeDeclaration.lf"
                  "let u : u64 = 1;"
                  "let u : u64 = 1;"
  runParseOkTest "src/Test/Fixtures/Good/Types/u128TypeDeclaration.lf"
                  "let u : u128 = 1;"
                  "let u : u128 = 1;"
  runParseOkTest "src/Test/Fixtures/Good/Types/f32TypeDeclaration.lf"
                  "let d : f32 = 1.234567;"
                  "let d : f32 = 1.234567;"
  runParseOkTest "src/Test/Fixtures/Good/Types/f64TypeDeclaration.lf"
                  "let d : f64 = -1.2345678901234567;"
                  "let d : f64 = (-1.2345678901234567);"
  runParseOkTest "src/Test/Fixtures/Good/Types/bitTypeDeclaration.lf"
                  "let i : bit = 1;"
                  "let i : bit = 1;"
  runParseOkTest "src/Test/Fixtures/Good/Types/qubitTypeDeclaration.lf"
                  "let q : qubit = qalloc();"
                  "let q : qubit = qalloc();"
  runParseOkTest "src/Test/Fixtures/Good/Types/boolTypeDeclaration.lf"
                  "let b : bool = true;"
                  "let b : bool = true;"
  runParseOkTest "src/Test/Fixtures/Good/Types/angle32TypeDeclaration.lf"
                  "let theta : angle32 = 1.234567;"
                  "let theta : angle32 = 1.234567;"
  runParseOkTest "src/Test/Fixtures/Good/Types/angle64TypeDeclaration.lf"
                  "let theta : angle64 = 1.2345678901234567;"
                  "let theta : angle64 = 1.2345678901234567;"
  runParseOkTest "src/Test/Fixtures/Good/Types/unitTypeDeclaration.lf"
                  "let unit : () = ();"
                  "let unit : () = ();"
  runParseOkTest "src/Test/Fixtures/Good/Types/paramTypeDeclaration.lf"
                  "let theta : param = Param(\"theta\");"
                  "let theta : param = Param(\"theta\");"

  -- Test inferred types
  runParseOkTest "src/Test/Fixtures/Good/Types/boolInferredType.lf"
                  "let b = true;"
                  "let b = true;"
  runParseOkTest "src/Test/Fixtures/Good/Types/i32InferredType.lf"
                  "let i = -7;"
                  "let i = (-7);"
  runParseOkTest "src/Test/Fixtures/Good/Types/f64InferredType.lf"
                  "let f = -1000.0;"
                  "let f = (-1000.0);"
  runParseOkTest "src/Test/Fixtures/Good/Types/qubitInferredType.lf"
                  "let q = qalloc();"
                  "let q = qalloc();"
  runParseOkTest "src/Test/Fixtures/Good/Types/unitInferredType.lf"
                  "let () = ();"
                  "let () = ();"

   -- Test type declarations corner case
  --runParseOkTest "src/Test/Fixtures/Good/Types/redundantSemicolumnsTypeDeclaration.lf" "let i : i32 = -99;" "let i : i32 = (-99);"
  runParseOkTest "src/Test/Fixtures/Good/Types/noSpacesParamTypeDeclaration.lf"
                  "let theta : param = Param(\"phi\");"
                  "let theta : param = Param(\"phi\");"
  runParseOkTest "src/Test/Fixtures/Good/Types/extraWhitespaceQubitTypeDeclaration.lf"
                  "let q : qubit = qalloc();"
                  "let q : qubit = qalloc();"
  runParseOkTest "src/Test/Fixtures/Good/Types/negativeZeroF64TypeDeclaration.lf"
                  "let d : f64 = -0.0;"
                  "let d : f64 = (-0.0);"
  runParseOkTest "src/Test/Fixtures/Good/Types/boolFalseTypeDeclaration.lf"
                  "let b : bool = false;"
                  "let b : bool = false;"
  runParseOkTest "src/Test/Fixtures/Good/Types/bitZeroTypeDeclaration.lf"
                  "let i : bit = 0;"
                  "let i : bit = 0;"

  -- Test invalid type declarations
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/missingTypeAfterColon.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/missingColonBeforeType.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/missingInitializerAfterEquals.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/missingEqualsBeforeInitializer.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/doubleColonInTypeDeclaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/unexpectedIntegerAfterInitializer.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/unexpectedFloatAfterParamInitializer.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/unexpectedSecondParamAfterInitializer.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/unexpectedFloatAfterInferredInitializer.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/unexpectedCommaSeparatedQallocInInitializer.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/unexpectedCommaSeparatedBoolInitializer.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/missingBindingNameInTypedI32Declaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/missingEqualsInTypedI32Declaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/unexpectedSecondIntegerInTypedI32Declaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/unexpectedCommaSeparatedIntegerInTypedI32Declaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/doubleColonWithSpacesInTypeDeclaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/missingClosingParenInTypedI32Declaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/unexpectedClosingParenInTypedI32Declaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/unexpectedSecondBoolLiteralInTypedDeclaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/unexpectedLeadingCommaInTypedBoolDeclaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/unexpectedSecondQallocInTypedQubitDeclaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/unexpectedTrailingCommaAfterQallocInTypedQubitDeclaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/missingClosingParenInTypedParamDeclaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/missingArgumentInTypedParamDeclaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/unexpectedTrailingCommaInTypedParamDeclaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/unexpectedExtraClosingParenInTypedUnitDeclaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/missingInitializerInTypedUnitDeclaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/missingInitializerInInferredDeclaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/missingBindingNameInInferredDeclaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/unexpectedSecondQallocInInferredDeclaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/unexpectedSecondBoolLiteralInInferredDeclaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/unexpectedSecondParamInInferredDeclaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/unexpectedSecondUnitLiteralInInferredDeclaration.lf"
  runParseShouldFailTest
                  "src/Test/Fixtures/Bad/Types/unexpectedLeadingCommaInInferredDeclaration.lf"

  --runParseOkTest "src/Test/Fixtures/Good/Types/i8TypeDeclaration.lf" "let i : i8 = 1;" "let i : i8 = 1;" {debug = True}