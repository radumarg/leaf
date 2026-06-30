module Test.TypeQualifierParserTests

import Test.TestsHelper

export
runTypeQualifierParseTests : IO ()
runTypeQualifierParseTests = do

    -- Test type qualifier declarations
    runParseOkTest "Fixtures/Good/TypeQualifier/linearQubit.lf"
                "let linear q: qubit = qalloc();"
                "let linear q: qubit = qalloc();"
    runParseOkTest "Fixtures/Good/TypeQualifier/linearQubits.lf"
                "let linear qs: [qubit; 2] = qalloc(2);"
                "let linear qs: [qubit; 2] = qalloc(2);"
    runParseOkTest "Fixtures/Good/TypeQualifier/affineQubit.lf"
                "let affine q: qubit = qalloc();"
                "let affine q: qubit = qalloc();"
    runParseOkTest "Fixtures/Good/TypeQualifier/affineQubits.lf"
                "let affine qs: [qubit; 2] = qalloc(2);"
                "let affine qs: [qubit; 2] = qalloc(2);"
    runParseOkTest "Fixtures/Good/TypeQualifier/scratchQubit.lf"
                "let scratch q: qubit = qalloc();"
                "let scratch q: qubit = qalloc();"
    runParseOkTest "Fixtures/Good/TypeQualifier/scratchQubits.lf"
                "let scratch qs: [qubit; 2] = qalloc(2);"
                "let scratch qs: [qubit; 2] = qalloc(2);"
    runParseOkTest "Fixtures/Good/TypeQualifier/scratchLinearQubit.lf"
                "let scratch linear q: qubit = qalloc();"
                "let scratch linear q: qubit = qalloc();"
    runParseOkTest "Fixtures/Good/TypeQualifier/scratchLinearQubits.lf"
                "let scratch linear qs: [qubit; 2] = qalloc(2);"
                "let scratch linear qs: [qubit; 2] = qalloc(2);"
    runParseOkTest "Fixtures/Good/TypeQualifier/scratchAffineQubit.lf"
                "let scratch affine q: qubit = qalloc();"
                "let scratch affine q: qubit = qalloc();"
    runParseOkTest "Fixtures/Good/TypeQualifier/affineScratchQubit.lf"
                "let affine scratch q: qubit = qalloc();"
                "let affine scratch q: qubit = qalloc();"
    runParseOkTest "Fixtures/Good/TypeQualifier/affineScratchQubits.lf"
                "let affine scratch qs: [qubit; 2] = qalloc(2);"
                "let affine scratch qs: [qubit; 2] = qalloc(2);"
    runParseOkTest "Fixtures/Good/TypeQualifier/linearScratchQubit.lf"
                "let linear scratch q: qubit = qalloc();"
                "let linear scratch q: qubit = qalloc();"
    runParseOkTest "Fixtures/Good/TypeQualifier/linearScratchQubits.lf"
                "let linear scratch qs: [qubit; 2] = qalloc(2);"
                "let linear scratch qs: [qubit; 2] = qalloc(2);"
    runParseOkTest "Fixtures/Good/TypeQualifier/scratchAffineQubits.lf"
                "let scratch affine qs: [qubit; 2] = qalloc(2);"
                "let scratch affine qs: [qubit; 2] = qalloc(2);"

    -- Test type qualifier declarations with inferred types
    runParseOkTest "Fixtures/Good/TypeQualifier/linearInferredQubit.lf"
                "let linear q = qalloc();"
                "let linear q = qalloc();"
    runParseOkTest "Fixtures/Good/TypeQualifier/linearInferredQubits.lf"
                "let linear qs = qalloc(2);"
                "let linear qs = qalloc(2);"
    runParseOkTest "Fixtures/Good/TypeQualifier/affineInferredQubit.lf"
                "let affine q = qalloc();"
                "let affine q = qalloc();"
    runParseOkTest "Fixtures/Good/TypeQualifier/affineInferredQubits.lf"
                "let affine qs = qalloc(2);"
                "let affine qs = qalloc(2);"
    runParseOkTest "Fixtures/Good/TypeQualifier/scratchInferredQubit.lf"
                "let scratch q = qalloc();"
                "let scratch q = qalloc();"
    runParseOkTest "Fixtures/Good/TypeQualifier/scratchInferredQubits.lf"
                "let scratch qs = qalloc(2);"
                "let scratch qs = qalloc(2);"
    runParseOkTest "Fixtures/Good/TypeQualifier/scratchInferredLinearQubit.lf"
                "let scratch linear q = qalloc();"
                "let scratch linear q = qalloc();"
    runParseOkTest "Fixtures/Good/TypeQualifier/scratchInferredLinearQubits.lf"
                "let scratch linear qs = qalloc(2);"
                "let scratch linear qs = qalloc(2);"
    runParseOkTest "Fixtures/Good/TypeQualifier/scratchInferredAffineQubit.lf"
                "let scratch affine q = qalloc();"
                "let scratch affine q = qalloc();"
    runParseOkTest "Fixtures/Good/TypeQualifier/affineScratchInferredQubit.lf"
                "let affine scratch q = qalloc();"
                "let affine scratch q = qalloc();"
    runParseOkTest "Fixtures/Good/TypeQualifier/linearScratchInferredQubit.lf"
                "let linear scratch q = qalloc();"
                "let linear scratch q = qalloc();"
    runParseOkTest "Fixtures/Good/TypeQualifier/linearScratchInferredQubits.lf"
                "let linear scratch qs = qalloc(2);"
                "let linear scratch qs = qalloc(2);"
    runParseOkTest "Fixtures/Good/TypeQualifier/scratchInferredAffineQubits.lf"
                "let scratch affine qs = qalloc(2);"
                "let scratch affine qs = qalloc(2);"

    -- Test type qualifier declarations corner cases
    runParseOkTest "Fixtures/Good/TypeQualifier/linearSpaceBeforeColonQubit.lf"
                "let linear q: qubit = qalloc();"
                "let linear q: qubit = qalloc();"
    runParseOkTest "Fixtures/Good/TypeQualifier/scratchCompactTypedQubit.lf"
                "let scratch q: qubit = qalloc();"
                "let scratch q: qubit = qalloc();"
    runParseOkTest "Fixtures/Good/TypeQualifier/scratchExtraWhitespaceTypedQubit.lf"
                "let scratch q: qubit = qalloc();"
                "let scratch q: qubit = qalloc();"
    runParseOkTest "Fixtures/Good/TypeQualifier/scratchBindingSplitLine.lf"
                "let scratch q: qubit = qalloc();"
                "let scratch q: qubit = qalloc();"
    runParseOkTest "Fixtures/Good/TypeQualifier/linearScratchTypeOnNextLine.lf"
                "let linear scratch q: qubit = qalloc();"
                "let linear scratch q: qubit = qalloc();"
    runParseOkTest "Fixtures/Good/TypeQualifier/affineScratchArraySplitLine.lf"
                "let affine scratch qs: [qubit; 2] = qalloc(2);"
                "let affine scratch qs: [qubit; 2] = qalloc(2);"
    runParseOkTest "Fixtures/Good/TypeQualifier/scratchLinearStackedLines.lf"
                "let scratch linear q: qubit = qalloc();"
                "let scratch linear q: qubit = qalloc();"
    runParseOkTest "Fixtures/Good/TypeQualifier/scratchSingleQubitArray.lf"
                "let scratch q: [qubit; 1] = qalloc(1);"
                "let scratch q: [qubit; 1] = qalloc(1);"

    -- Test invalid type qualifier declarations
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/linearAffineInferredQubit.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/linearAffineInferredQubits.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/doubleScratchQubit.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/doubleLinearQubit.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/doubleAffineQubit.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/scratchLinearAffineQubit.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/missingBindingNameAfterLinear.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/missingBindingNameAfterAffineScratch.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/missingBindingNameAfterScratchLinear.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/missingInitializerAfterQualifiedTypedBinding.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/missingEqualsBeforeQallocInAffineBinding.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/missingTypeAfterColonInLinearBinding.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/missingArrayLengthInScratchTypedBinding.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/missingArrayLengthInLinearScratchTypedBinding.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/missingClosingBracketInAffineArrayBinding.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/missingClosingParenInScratchQalloc.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/missingClosingParenInLinearScratchQalloc.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/missingSemicolonInAffineScratchBinding.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/missingTypeAfterColonInScratchBinding.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/unexpectedScratchAfterBindingName.lf"
    runParseShouldFailTest
                    "Fixtures/Bad/TypeQualifier/trailingCommaInScratchQalloc.lf"
    runParseShouldFailTest 
                    "Fixtures/Bad/TypeQualifier/scratchInferredQallocNoParens.lf"

