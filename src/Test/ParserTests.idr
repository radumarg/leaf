module Test.ParserTests

import Frontend.AST
import Frontend.ExprPrettyPrinter
import Frontend.Lexer
import Frontend.Parser
import Frontend.Token
import System.File
import Text.Bounds
import Tester

runTest : String -> String -> String -> {default False debug : Bool} -> IO ()
runTest fileName laxParseExpression strictParseExpression {debug} = do
  fileResult <- readFile fileName
  case fileResult of
    Left fileErr => putStrLn $ "Failed to read " ++ fileName ++ ": " ++ show fileErr
    Right testProgram =>
      case lexProgram testProgram of
        Left err => putStrLn $ "Lexer error: " ++ show err
        Right tokens =>
          case parseProgramAll tokens of
            Left err => putStrLn $ "Parse error: " ++ show err
            Right program => do
              let laxParseResult = showProgramLax program
              let strictParseResult = showProgramStrict program
              case debug of
                True => do
                  putStrLn $ "Parsed program: " ++ show program
                  putStrLn $ "Lax parsed expression: " ++ laxParseResult 
                  putStrLn $ "Strict parsed expression: " ++ strictParseResult
                False => do
                  result <- runEitherT $ do
                    assertEq laxParseExpression laxParseResult
                  case result of
                    Left err => putStrLn ("Lax parsing: " ++ err ++ " in " ++ fileName)
                    Right () => pure ()
                  result <- runEitherT $ do
                    assertEq strictParseExpression strictParseResult
                  case result of
                    Left err => putStrLn ("Strict parsing: " ++ err ++ " in " ++ fileName)
                    Right () => pure ()

main : IO ()
main = do
  -- Test type declarations
  runTest "src/Test/Good/Types/i8TypeDeclaration.lf" "let i : i8 = -1;" "let i : i8 = (-1);"
  runTest "src/Test/Good/Types/i16TypeDeclaration.lf" "let i : i16 = -1;" "let i : i16 = (-1);"
  runTest "src/Test/Good/Types/i32TypeDeclaration.lf" "let i : i32 = 1;" "let i : i32 = 1;"
  runTest "src/Test/Good/Types/i64TypeDeclaration.lf" "let i : i64 = 1;" "let i : i64 = 1;"
  runTest "src/Test/Good/Types/i128TypeDeclaration.lf" "let i : i128 = -1;" "let i : i128 = (-1);"
  runTest "src/Test/Good/Types/u8TypeDeclaration.lf" "let u : u8 = 1;" "let u : u8 = 1;"
  runTest "src/Test/Good/Types/u16TypeDeclaration.lf" "let u : u16 = 1;" "let u : u16 = 1;"
  runTest "src/Test/Good/Types/u32TypeDeclaration.lf" "let u : u32 = 1;" "let u : u32 = 1;"
  runTest "src/Test/Good/Types/u64TypeDeclaration.lf" "let u : u64 = 1;" "let u : u64 = 1;"
  runTest "src/Test/Good/Types/u128TypeDeclaration.lf" "let u : u128 = 1;" "let u : u128 = 1;"
  runTest "src/Test/Good/Types/f32TypeDeclaration.lf" "let d : f32 = 1.234567;" "let d : f32 = 1.234567;"
  runTest "src/Test/Good/Types/f64TypeDeclaration.lf" "let d : f64 = -1.2345678901234567;" "let d : f64 = (-1.2345678901234567);"
  runTest "src/Test/Good/Types/bitTypeDeclaration.lf" "let i : bit = 1;" "let i : bit = 1;"
  runTest "src/Test/Good/Types/qubitTypeDeclaration.lf" "let q : qubit = qalloc();" "let q : qubit = qalloc();"
  runTest "src/Test/Good/Types/boolTypeDeclaration.lf" "let b : bool = true;" "let b : bool = true;"
  runTest "src/Test/Good/Types/angle32TypeDeclaration.lf" "let theta : angle32 = 1.234567;" "let theta : angle32 = 1.234567;"
  runTest "src/Test/Good/Types/angle64TypeDeclaration.lf" "let theta : angle64 = 1.2345678901234567;" "let theta : angle64 = 1.2345678901234567;"
  runTest "src/Test/Good/Types/unitTypeDeclaration.lf" "let unit : () = ();" "let unit : () = ();"
  runTest "src/Test/Good/Types/paramTypeDeclaration.lf" "let theta : Param = param(\"theta\");" "let theta : Param = param(\"theta\");"
  -- Test inferred types
  runTest "src/Test/Good/Types/i32InferredType.lf" "let i = -7;" "let i = (-7);"
  runTest "src/Test/Good/Types/f64InferredType.lf" "let f = -1000.0;" "let f = (-1000.0);"
  runTest "src/Test/Good/Types/qubitInferredType.lf" "let q = qalloc();" "let q = qalloc();"
  runTest "src/Test/Good/Types/unitInferredType.lf" "let () = ();" "let () = ();"
  -- Test type declarations corner case
  --runTest "src/Test/Good/Types/redundantSemicolumnsTypeDeclaration.lf" "let i : i32 = -99;" "let i : i32 = (-99);"

  --runTest "src/Test/Good/Types/i8TypeDeclaration.lf" "let i : i8 = 1;" "let i : i8 = 1;" {debug = True}
