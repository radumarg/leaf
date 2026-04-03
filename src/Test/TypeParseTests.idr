module Test.TypeParseTests

import Test.TestHelper

export
runTypeParseTest : IO ()
runTypeParseTest = do

  -- Test type declarations
  runParseOkTest "src/Test/Fixtures/Good/Types/i8TypeDeclaration.lf" "let i : i8 = -1;" "let i : i8 = (-1);"
  runParseOkTest "src/Test/Fixtures/Good/Types/i16TypeDeclaration.lf" "let i : i16 = -1;" "let i : i16 = (-1);"
  runParseOkTest "src/Test/Fixtures/Good/Types/i32TypeDeclaration.lf" "let i : i32 = 1;" "let i : i32 = 1;"
  runParseOkTest "src/Test/Fixtures/Good/Types/i64TypeDeclaration.lf" "let i : i64 = 1;" "let i : i64 = 1;"
  runParseOkTest "src/Test/Fixtures/Good/Types/i128TypeDeclaration.lf" "let i : i128 = -1;" "let i : i128 = (-1);"
  runParseOkTest "src/Test/Fixtures/Good/Types/u8TypeDeclaration.lf" "let u : u8 = 1;" "let u : u8 = 1;"
  runParseOkTest "src/Test/Fixtures/Good/Types/u16TypeDeclaration.lf" "let u : u16 = 1;" "let u : u16 = 1;"
  runParseOkTest "src/Test/Fixtures/Good/Types/u32TypeDeclaration.lf" "let u : u32 = 1;" "let u : u32 = 1;"
  runParseOkTest "src/Test/Fixtures/Good/Types/u64TypeDeclaration.lf" "let u : u64 = 1;" "let u : u64 = 1;"
  runParseOkTest "src/Test/Fixtures/Good/Types/u128TypeDeclaration.lf" "let u : u128 = 1;" "let u : u128 = 1;"
  runParseOkTest "src/Test/Fixtures/Good/Types/f32TypeDeclaration.lf" "let d : f32 = 1.234567;" "let d : f32 = 1.234567;"
  runParseOkTest "src/Test/Fixtures/Good/Types/f64TypeDeclaration.lf" "let d : f64 = -1.2345678901234567;" "let d : f64 = (-1.2345678901234567);"
  runParseOkTest "src/Test/Fixtures/Good/Types/bitTypeDeclaration.lf" "let i : bit = 1;" "let i : bit = 1;"
  runParseOkTest "src/Test/Fixtures/Good/Types/qubitTypeDeclaration.lf" "let q : qubit = qalloc();" "let q : qubit = qalloc();"
  runParseOkTest "src/Test/Fixtures/Good/Types/boolTypeDeclaration.lf" "let b : bool = true;" "let b : bool = true;"
  runParseOkTest "src/Test/Fixtures/Good/Types/angle32TypeDeclaration.lf" "let theta : angle32 = 1.234567;" "let theta : angle32 = 1.234567;"
  runParseOkTest "src/Test/Fixtures/Good/Types/angle64TypeDeclaration.lf" "let theta : angle64 = 1.2345678901234567;" "let theta : angle64 = 1.2345678901234567;"
  runParseOkTest "src/Test/Fixtures/Good/Types/unitTypeDeclaration.lf" "let unit : () = ();" "let unit : () = ();"
  runParseOkTest "src/Test/Fixtures/Good/Types/paramTypeDeclaration.lf" "let theta : Param = param(\"theta\");" "let theta : Param = param(\"theta\");"

  -- Test inferred types
  runParseOkTest "src/Test/Fixtures/Good/Types/i32InferredType.lf" "let i = -7;" "let i = (-7);"
  runParseOkTest "src/Test/Fixtures/Good/Types/f64InferredType.lf" "let f = -1000.0;" "let f = (-1000.0);"
  runParseOkTest "src/Test/Fixtures/Good/Types/qubitInferredType.lf" "let q = qalloc();" "let q = qalloc();"
  runParseOkTest "src/Test/Fixtures/Good/Types/unitInferredType.lf" "let () = ();" "let () = ();"

   -- Test type declarations corner case
  --runParseOkTest "src/Test/Fixtures/Good/Types/redundantSemicolumnsTypeDeclaration.lf" "let i : i32 = -99;" "let i : i32 = (-99);"
  runParseOkTest "src/Test/Fixtures/Good/Types/noSpacesParamTypeDeclaration.lf" "let theta : Param = param(\"phi\");" "let theta : Param = param(\"phi\");"
  runParseOkTest "src/Test/Fixtures/Good/Types/extraWhitespaceQubitTypeDeclaration.lf" "let q : qubit = qalloc();" "let q : qubit = qalloc();"
  runParseOkTest "src/Test/Fixtures/Good/Types/negativeZeroF64TypeDeclaration.lf" "let d : f64 = -0.0;" "let d : f64 = (-0.0);"
  runParseOkTest "src/Test/Fixtures/Good/Types/boolFalseTypeDeclaration.lf" "let b : bool = false;" "let b : bool = false;"
  runParseOkTest "src/Test/Fixtures/Good/Types/bitZeroTypeDeclaration.lf" "let i : bit = 0;" "let i : bit = 0;"

  -- Test invalid type declarations
  runParseShouldFailTest "src/Test/Fixtures/Bad/Types/missingTypeAfterColon.lf" 
  runParseShouldFailTest "src/Test/Fixtures/Bad/Types/missingColonBeforeType.lf"
  runParseShouldFailTest "src/Test/Fixtures/Bad/Types/missingInitializerAfterEquals.lf"
  runParseShouldFailTest "src/Test/Fixtures/Bad/Types/missingEqualsBeforeInitializer.lf"
  runParseShouldFailTest "src/Test/Fixtures/Bad/Types/doubleColonInTypeDeclaration.lf"

  --runParseOkTest "src/Test/Fixtures/Good/Types/i8TypeDeclaration.lf" "let i : i8 = 1;" "let i : i8 = 1;" {debug = True}