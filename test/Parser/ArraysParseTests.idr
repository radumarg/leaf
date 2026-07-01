module Test.ArraysParseTests

import Test.TestsHelper

export
runArraysParseTests : IO ()
runArraysParseTests = do

    -- Test array type declarations
    runParseOkTest "Fixtures/Good/Arrays/boolArrayDeclaration.lf" 
                    "let bools : [bool; 4] = [true, false, true, false];" 
                    "let bools : [bool; 4] = [true, false, true, false];"
    runParseOkTest "Fixtures/Good/Arrays/i32ArrayDeclaration.lf"
                    "let ints : [i32; 3] = [1, 2, 3];"
                    "let ints : [i32; 3] = [1, 2, 3];"
    runParseOkTest "Fixtures/Good/Arrays/u64ArrayDeclaration.lf"
                    "let us : [u64; 2] = [10, 20];"
                    "let us : [u64; 2] = [10, 20];"
    runParseOkTest "Fixtures/Good/Arrays/f64ArrayDeclaration.lf"
                    "let fs : [f64; 3] = [1.0, 2.0, 3.0];"
                    "let fs : [f64; 3] = [1.0, 2.0, 3.0];"
    runParseOkTest "Fixtures/Good/Arrays/bitArrayDeclaration.lf"
                    "let bits : [bit; 3] = [1, 0, 1];"
                    "let bits : [bit; 3] = [1, 0, 1];"
    runParseOkTest "Fixtures/Good/Arrays/i16RepeatArrayDeclaration.lf"
                    "let zeros : [i16; 8] = [0; 8];"
                    "let zeros : [i16; 8] = [0; 8];"
    runParseOkTest "Fixtures/Good/Arrays/boolRepeatArrayDeclaration.lf"
                    "let flags : [bool; 3] = [true; 3];"
                    "let flags : [bool; 3] = [true; 3];"
    runParseOkTest "Fixtures/Good/Arrays/qubitArrayDeclaration.lf"
                    "let qubits : [qubit; 2] = qalloc(2);"
                    "let qubits : [qubit; 2] = qalloc(2);"
    runParseOkTest "Fixtures/Good/Arrays/angle64ArrayDeclaration.lf"
                    "let angles : [angle64; 2] = [3.14, 1.57];"
                    "let angles : [angle64; 2] = [3.14, 1.57];"
    runParseOkTest "Fixtures/Good/Arrays/unitArrayDeclaration.lf"
                    "let units : [(); 5] = [(), (), (), (), ()];"
                    "let units : [(); 5] = [(), (), (), (), ()];"
    runParseOkTest "Fixtures/Good/Arrays/paramArrayDeclaration.lf"
                    "let params : [param; 2] = [Param(\"theta\"), Param(\"phi\")];"
                    "let params : [param; 2] = [Param(\"theta\"), Param(\"phi\")];"

    -- Test inferred type array declarations
    runParseOkTest "Fixtures/Good/Arrays/boolInferredArrayDeclaration.lf"
                    "let bools = [true, false, true, false];"
                    "let bools = [true, false, true, false];"
    runParseOkTest "Fixtures/Good/Arrays/i32InferredArrayDeclaration.lf"
                    "let ints = [1, 2, 3];"
                    "let ints = [1, 2, 3];"
    runParseOkTest "Fixtures/Good/Arrays/i32RepeatInferredArrayDeclaration.lf"
                    "let zeros = [0; 3];"
                    "let zeros = [0; 3];"
    runParseOkTest "Fixtures/Good/Arrays/f64InferredArrayDeclaration.lf"
                    "let fs = [1.0, 2.0, 3.0];"
                    "let fs = [1.0, 2.0, 3.0];"
    runParseOkTest "Fixtures/Good/Arrays/qubitInferredArrayDeclaration.lf"
                    "let qubits = qalloc(2);"
                    "let qubits = qalloc(2);"
    runParseOkTest "Fixtures/Good/Arrays/unitInferredArrayDeclaration.lf"
                    "let units = [(), (), (), (), ()];"
                    "let units = [(), (), (), (), ()];"

    -- Test type declarations corner cases
    runParseOkTest "Fixtures/Good/Types/noSpacesBoolArrayTypeDeclaration.lf"
                    "let bools : [bool; 4] = [true, false, true, false];"
                    "let bools : [bool; 4] = [true, false, true, false];"
    runParseOkTest "Fixtures/Good/Types/extraWhitespaceI32ArrayTypeDeclaration.lf"
                    "let ints : [i32; 3] = [1, 2, 3];"
                    "let ints : [i32; 3] = [1, 2, 3];"
    runParseOkTest "Fixtures/Good/Types/noSpacesQubitArrayTypeDeclaration.lf"
                    "let qubits : [qubit; 2] = qalloc(2);"
                    "let qubits : [qubit; 2] = qalloc(2);"
    runParseOkTest "Fixtures/Good/Types/noSpacesParamArrayTypeDeclaration.lf"
                    "let params : [param; 2] = [Param(\"theta\"), Param(\"phi\")];"
                    "let params : [param; 2] = [Param(\"theta\"), Param(\"phi\")];"
    runParseOkTest "Fixtures/Good/Types/noSpacesI16RepeatArrayTypeDeclaration.lf"
                    "let zeros : [i16; 8] = [0; 8];"
                    "let zeros : [i16; 8] = [0; 8];"
    runParseOkTest "Fixtures/Good/Arrays/zeroLengthI64ArrayDeclaration.lf"
                    "let ints : [i64; 0] = [];"
                    "let ints : [i64; 0] = [];"
    runParseOkTest "Fixtures/Good/Arrays/zeroLengthU16ArrayDeclaration.lf"
                    "let ints : [u16; 0] = [];"
                    "let ints : [u16; 0] = [];"
    runParseOkTest "Fixtures/Good/Arrays/zeroLengthBitArrayDeclaration.lf"
                    "let bits : [bit; 0] = [];"
                    "let bits : [bit; 0] = [];"
    runParseOkTest "Fixtures/Good/Arrays/zeroLengthUnitArrayDeclaration.lf"
                    "let units : [(); 0] = [];"
                    "let units : [(); 0] = [];"
    runParseOkTest "Fixtures/Good/Arrays/zeroLengthQubitArrayDeclaration.lf"
                    "let qubits : [qubit; 0] = [];"
                    "let qubits : [qubit; 0] = [];"
    runParseOkTest "Fixtures/Good/Arrays/negativeI128ArrayDeclaration.lf"
                    "let ints : [i128; 3] = -[1, 2, 3];"
                    "let ints : [i128; 3] = (-[1, 2, 3]);"
    runParseOkTest "Fixtures/Good/Arrays/negativeI64RepeatArrayDeclaration.lf"
                    "let zeros : [i64; 4] = -[0; 4];"
                    "let zeros : [i64; 4] = (-[0; 4]);"
    runParseOkTest "Fixtures/Good/Arrays/negativeParamArrayDeclaration.lf"
                    "let params : [param; 1] = -[Param(\"theta\")];"
                    "let params : [param; 1] = (-[Param(\"theta\")]);"
    runParseOkTest "Fixtures/Good/Arrays/negativeQAllocArrayDeclaration.lf"
                    "let qubits : [qubit; 3] = -qalloc(3);"
                    "let qubits : [qubit; 3] = (-qalloc(3));"
    runParseOkTest "Fixtures/Good/Arrays/negativeBitArrayFourDeclaration.lf"
                    "let bits : [bit; 4] = -[0, 0, 1, 1];"
                    "let bits : [bit; 4] = (-[0, 0, 1, 1]);"
    runParseOkTest "Fixtures/Good/Types/extraWhitespaceBoolRepeatArrayTypeDeclaration.lf"
                    "let flags : [bool; 3] = [true; 3];"
                    "let flags : [bool; 3] = [true; 3];"
    runParseOkTest "Fixtures/Good/Types/noSpacesBoolRepeatArrayTypeDeclaration.lf"
                    "let flags : [bool; 3] = [true; 3];"
                    "let flags : [bool; 3] = [true; 3];"

    -- Test invalid array declarations
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/missingElementTypeInArrayDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/missingArrayLengthInDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/missingClosingBracketInArrayTypeDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/missingCommaInArrayLiteralDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/missingInitializerInArrayDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/missingRepeatCountInArrayDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/missingSemicolonInRepeatArrayDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/nonNumericRepeatCountInArrayDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/unexpectedScalarAfterArrayInitializer.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/unexpectedScalarAfterArrayLiteralInitializer.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/malformedParamArrayDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/malformedQubitArrayDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/malformedUnitArrayDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/unexpectedFloatAfterInferredArrayInitializer.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/missingClosingBracketAfterArrayLiteral.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/missingMiddleElementInArrayLiteral.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/missingCommaBetweenFirstTwoArrayElements.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/missingRepeatedValueBeforeSemicolon.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/unexpectedLiteralAfterRepeatCount.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/missingSemicolonInArrayTypeDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/unexpectedLiteralAfterEmptyArrayInitializer.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/unexpectedLiteralAfterMeasrBitArrayInitializer.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/Arrays/unexpectedTrailingCommaAfterQallocInitializer.lf"