module Test.TypeParseTests

import Test.TestsHelper

export
runTypeParseTests : IO ()
runTypeParseTests = do

  -- Test type declarations
  runParseOkTest "Fixtures/Good/Types/i8TypeDeclaration.lf"
                  "let i : i8 = -1;"
                  "let i : i8 = (-1);"
  runParseOkTest "Fixtures/Good/Types/i16TypeDeclaration.lf"
                  "let i : i16 = -1;"
                  "let i : i16 = (-1);"
  runParseOkTest "Fixtures/Good/Types/i32TypeDeclaration.lf"
                  "let i : i32 = 1;"
                  "let i : i32 = 1;"
  runParseOkTest "Fixtures/Good/Types/i64TypeDeclaration.lf"
                  "let i : i64 = 1;"
                  "let i : i64 = 1;"
  runParseOkTest "Fixtures/Good/Types/i128TypeDeclaration.lf"
                  "let i : i128 = -1;"
                  "let i : i128 = (-1);"
  runParseOkTest "Fixtures/Good/Types/u8TypeDeclaration.lf"
                  "let u : u8 = 1;"
                  "let u : u8 = 1;"
  runParseOkTest "Fixtures/Good/Types/u16TypeDeclaration.lf"
                  "let u : u16 = 1;"
                  "let u : u16 = 1;"
  runParseOkTest "Fixtures/Good/Types/u32TypeDeclaration.lf"
                  "let u : u32 = 1;"
                  "let u : u32 = 1;"
  runParseOkTest "Fixtures/Good/Types/u64TypeDeclaration.lf"
                  "let u : u64 = 1;"
                  "let u : u64 = 1;"
  runParseOkTest "Fixtures/Good/Types/u128TypeDeclaration.lf"
                  "let u : u128 = 1;"
                  "let u : u128 = 1;"
  runParseOkTest "Fixtures/Good/Types/f32TypeDeclaration.lf"
                  "let d : f32 = 1.234567;"
                  "let d : f32 = 1.234567;"
  runParseOkTest "Fixtures/Good/Types/f64TypeDeclaration.lf"
                  "let d : f64 = -1.2345678901234567;"
                  "let d : f64 = (-1.2345678901234567);"
  runParseOkTest "Fixtures/Good/Types/bitTypeDeclaration.lf"
                  "let i : bit = 1;"
                  "let i : bit = 1;"
  runParseOkTest "Fixtures/Good/Types/qubitTypeDeclaration.lf"
                  "let q : qubit = qalloc();"
                  "let q : qubit = qalloc();"
  runParseOkTest "Fixtures/Good/Types/boolTypeDeclaration.lf"
                  "let b : bool = true;"
                  "let b : bool = true;"
  runParseOkTest "Fixtures/Good/Types/angle32TypeDeclaration.lf"
                  "let theta : angle32 = 1.234567;"
                  "let theta : angle32 = 1.234567;"
  runParseOkTest "Fixtures/Good/Types/angle64TypeDeclaration.lf"
                  "let theta : angle64 = 1.2345678901234567;"
                  "let theta : angle64 = 1.2345678901234567;"
  runParseOkTest "Fixtures/Good/Types/unitTypeDeclaration.lf"
                  "let unit : () = ();"
                  "let unit : () = ();"
  runParseOkTest "Fixtures/Good/Types/paramTypeDeclaration.lf"
                  "let theta : param = Param(\"theta\");"
                  "let theta : param = Param(\"theta\");"

  -- Test inferred types
  runParseOkTest "Fixtures/Good/Types/boolInferredType.lf"
                  "let b = true;"
                  "let b = true;"
  runParseOkTest "Fixtures/Good/Types/i32InferredType.lf"
                  "let i = -7;"
                  "let i = (-7);"
  runParseOkTest "Fixtures/Good/Types/f64InferredType.lf"
                  "let f = -1000.0;"
                  "let f = (-1000.0);"
  runParseOkTest "Fixtures/Good/Types/qubitInferredType.lf"
                  "let q = qalloc();"
                  "let q = qalloc();"
  runParseOkTest "Fixtures/Good/Types/unitInferredType.lf"
                  "let () = ();"
                  "let () = ();"

   -- Test type declarations corner case
  --runParseOkTest "Fixtures/Good/Types/redundantSemicolumnsTypeDeclaration.lf" "let i : i32 = -99;" "let i : i32 = (-99);"
  runParseOkTest "Fixtures/Good/Types/noSpacesParamTypeDeclaration.lf"
                  "let theta : param = Param(\"phi\");"
                  "let theta : param = Param(\"phi\");"
  runParseOkTest "Fixtures/Good/Types/extraWhitespaceQubitTypeDeclaration.lf"
                  "let q : qubit = qalloc();"
                  "let q : qubit = qalloc();"
  runParseOkTest "Fixtures/Good/Types/negativeZeroF64TypeDeclaration.lf"
                  "let d : f64 = -0.0;"
                  "let d : f64 = (-0.0);"
  runParseOkTest "Fixtures/Good/Types/boolFalseTypeDeclaration.lf"
                  "let b : bool = false;"
                  "let b : bool = false;"
  runParseOkTest "Fixtures/Good/Types/bitZeroTypeDeclaration.lf"
                  "let i : bit = 0;"
                  "let i : bit = 0;"

  -- Test invalid type declarations
  runParseShouldFailTest
                  "Fixtures/Bad/Types/missingTypeAfterColon.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/missingColonBeforeType.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/missingInitializerAfterEquals.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/missingEqualsBeforeInitializer.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/doubleColonInTypeDeclaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/unexpectedIntegerAfterInitializer.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/unexpectedFloatAfterParamInitializer.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/unexpectedSecondParamAfterInitializer.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/unexpectedFloatAfterInferredInitializer.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/unexpectedCommaSeparatedQallocInInitializer.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/unexpectedCommaSeparatedBoolInitializer.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/missingBindingNameInTypedI32Declaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/missingEqualsInTypedI32Declaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/unexpectedSecondIntegerInTypedI32Declaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/unexpectedCommaSeparatedIntegerInTypedI32Declaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/doubleColonWithSpacesInTypeDeclaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/missingClosingParenInTypedI32Declaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/unexpectedClosingParenInTypedI32Declaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/unexpectedSecondBoolLiteralInTypedDeclaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/unexpectedLeadingCommaInTypedBoolDeclaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/unexpectedSecondQallocInTypedQubitDeclaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/unexpectedTrailingCommaAfterQallocInTypedQubitDeclaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/missingClosingParenInTypedParamDeclaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/missingArgumentInTypedParamDeclaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/unexpectedTrailingCommaInTypedParamDeclaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/unexpectedExtraClosingParenInTypedUnitDeclaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/missingInitializerInTypedUnitDeclaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/missingInitializerInInferredDeclaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/missingBindingNameInInferredDeclaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/unexpectedSecondQallocInInferredDeclaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/unexpectedSecondBoolLiteralInInferredDeclaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/unexpectedSecondParamInInferredDeclaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/unexpectedSecondUnitLiteralInInferredDeclaration.lf"
  runParseShouldFailTest
                  "Fixtures/Bad/Types/unexpectedLeadingCommaInInferredDeclaration.lf"

  --runParseOkTest "Fixtures/Good/Types/i8TypeDeclaration.lf" "let i : i8 = 1;" "let i : i8 = 1;" {debug = True}