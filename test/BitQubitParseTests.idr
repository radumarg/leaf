module Test.BitQubitParseTests

import Test.TestsHelper

export
runBitQubitParseTests : IO ()
runBitQubitParseTests = do

    -- Test bit/qubit type declarations
    runParseOkTest "Fixtures/Good/BitQubit/qubitDeclaration1.lf" 
                    "let q : qubit = qalloc();" 
                    "let q : qubit = qalloc();"
    runParseOkTest "Fixtures/Good/BitQubit/qubitDeclaration2.lf" 
                    "let singleQubit : qubit = qalloc(1);" 
                    "let singleQubit : qubit = qalloc(1);"
    runParseOkTest "Fixtures/Good/BitQubit/qubitArrayDeclaration.lf" 
                    "let qubits : [qubit; 3] = qalloc(3);" 
                    "let qubits : [qubit; 3] = qalloc(3);"
    runParseOkTest "Fixtures/Good/BitQubit/bitMeasrDeclaration.lf" 
                    "let singleBit : bit = measr(q);" 
                    "let singleBit : bit = measr(q);"
    runParseOkTest "Fixtures/Good/BitQubit/bitIntDeclaration.lf" 
                    "let singleBit : bit = 0;" 
                    "let singleBit : bit = 0;"
    runParseOkTest "Fixtures/Good/BitQubit/bitArrayDeclaration.lf" 
                    "let bs : [bit; 8] = measr(qs);" 
                    "let bs : [bit; 8] = measr(qs);"

    -- Test inferred bit/qubit type declarations
    runParseOkTest "Fixtures/Good/BitQubit/inferredQubitDeclaration.lf"
                    "let singleQubit = qalloc(1);"
                    "let singleQubit = qalloc(1);"
    runParseOkTest "Fixtures/Good/BitQubit/inferredQubitArrayDeclaration.lf" 
                    "let qubits = qalloc(2);" 
                    "let qubits = qalloc(2);"
    runParseOkTest "Fixtures/Good/BitQubit/inferredQubitArrayExprDeclaration.lf"
                    "let qubits = qalloc(n + 1);"
                    "let qubits = qalloc(n + 1);"
    runParseOkTest "Fixtures/Good/BitQubit/bitInferredDeclaration.lf" 
                    "let bs = measr(qs);" 
                    "let bs = measr(qs);"
                    
    -- Test bit/qubit declarations corner cases
    runParseOkTest "Fixtures/Good/BitQubit/noSpacesQubitDeclaration.lf"
                    "let q : qubit = qalloc();"
                    "let q : qubit = qalloc();"
    runParseOkTest "Fixtures/Good/BitQubit/extraWhitespaceQubitDeclaration.lf"
                    "let q : qubit = qalloc();"
                    "let q : qubit = qalloc();"
    runParseOkTest "Fixtures/Good/BitQubit/noSpacesQubitArrayDeclaration.lf"
                    "let qubits : [qubit; 3] = qalloc(3);"
                    "let qubits : [qubit; 3] = qalloc(3);"
    runParseOkTest "Fixtures/Good/BitQubit/noSpacesBitDeclaration.lf"
                    "let singleBit : bit = 0;"
                    "let singleBit : bit = 0;"
    runParseOkTest "Fixtures/Good/BitQubit/extraWhitespaceBitDeclaration.lf"
                    "let singleBit : bit = 0;"
                    "let singleBit : bit = 0;"
    runParseOkTest "Fixtures/Good/BitQubit/noSpacesBitArrayDeclaration.lf"
                    "let bs : [bit; 8] = measr(qs);"
                    "let bs : [bit; 8] = measr(qs);"

    -- Test invalid bit/qubit declarations
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/missingInitializerInQubitDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/missingEqualsInQubitDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/unexpectedSecondQallocInQubitDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/missingBindingNameInQubitDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/missingInitializerInBitDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/missingEqualsInBitDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/unexpectedSecondBitLiteralInDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/missingBindingNameInBitDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/missingEqualsInMeasuredBitDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/unexpectedSecondMeasrInBitDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/unexpectedSecondMeasrInBitArrayDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/missingBindingNameWithCommaSeparatedBitLiterals.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/unexpectedCommaSeparatedLiteralAfterMeasrBitDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/missingClosingParenInQallocDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/missingArgumentInQallocDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/unexpectedSecondArgumentInQallocDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/unexpectedClosingParenAfterQallocDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/missingClosingParenInMeasrBitDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/unexpectedLiteralAfterMeasrBitDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/unexpectedCommaAfterMeasrBitDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/missingClosingParenInMeasrBitArrayDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/unexpectedLiteralAfterMeasrBitArrayDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/missingBindingNameInTypedQubitDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/missingColonBeforeQubitType.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/missingTypeAfterColonInQubitDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/missingInitializerInNamedBitDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/unexpectedSecondBitLiteralInNamedDeclaration.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/BitQubit/unexpectedLeadingCommaInBitDeclaration.lf"