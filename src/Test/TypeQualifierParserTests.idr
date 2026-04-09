module Test.TypeQualifierParserTests

import Test.TestHelper

export
runTypeQualifierParseTests : IO ()
runTypeQualifierParseTests = do

    -- Test type qualifier declarations
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/linearQubit.lf"
                "let linear q: qubit = qalloc();"
                "let linear q: qubit = qalloc();"
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/linearQubits.lf"
                "let linear qs: [qubit; 2] = qalloc(2);"
                "let linear qs: [qubit; 2] = qalloc(2);"
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/affineQubit.lf"
                "let affine q: qubit = qalloc();"
                "let affine q: qubit = qalloc();"
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/affineQubits.lf"
                "let affine qs: [qubit; 2] = qalloc(2);"
                "let affine qs: [qubit; 2] = qalloc(2);"
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/scratchQubit.lf"
                "let scratch q: qubit = qalloc();"
                "let scratch q: qubit = qalloc();"
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/scratchQubits.lf"
                "let scratch qs: [qubit; 2] = qalloc(2);"
                "let scratch qs: [qubit; 2] = qalloc(2);"
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/scratchLinearQubit.lf"
                "let scratch linear q: qubit = qalloc();"
                "let scratch linear q: qubit = qalloc();"
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/scratchLinearQubits.lf"
                "let scratch linear qs: [qubit; 2] = qalloc(2);"
                "let scratch linear qs: [qubit; 2] = qalloc(2);"
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/scratchAffineQubit.lf"
                "let scratch affine q: qubit = qalloc();"
                "let scratch affine q: qubit = qalloc();"
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/scratchAffineQubits.lf"
                "let scratch affine qs: [qubit; 2] = qalloc(2);"
                "let scratch affine qs: [qubit; 2] = qalloc(2);"

    -- Test type qualifier declarations with inferred types
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/linearInferredQubit.lf"
                "let linear q = qalloc();"
                "let linear q = qalloc();"
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/linearInferredQubits.lf"
                "let linear qs = qalloc(2);"
                "let linear qs = qalloc(2);"
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/affineInferredQubit.lf"
                "let affine q = qalloc();"
                "let affine q = qalloc();"
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/affineInferredQubits.lf"
                "let affine qs = qalloc(2);"
                "let affine qs = qalloc(2);"
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/scratchInferredQubit.lf"
                "let scratch q = qalloc();"
                "let scratch q = qalloc();"
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/scratchInferredQubits.lf"
                "let scratch qs = qalloc(2);"
                "let scratch qs = qalloc(2);"
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/scratchInferredLinearQubit.lf"
                "let scratch linear q = qalloc();"
                "let scratch linear q = qalloc();"
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/scratchInferredLinearQubits.lf"
                "let scratch linear qs = qalloc(2);"
                "let scratch linear qs = qalloc(2);"
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/scratchInferredAffineQubit.lf"
                "let scratch affine q = qalloc();"
                "let scratch affine q = qalloc();"
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/scratchInferredAffineQubits.lf"
                "let scratch affine qs = qalloc(2);"
                "let scratch affine qs = qalloc(2);"

    -- Test type qualifier declarations corner cases
    runParseOkTest "src/Test/Fixtures/Good/TypeQualifier/todo"
                    "todo"
                    "todo"

    -- Test invalid type qualifier declarations
    runParseShouldFailTest
                    "src/Test/Fixtures/Bad/TypeQualifier/todo"
