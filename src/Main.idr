module Main

--import Frontend.AST
--import Frontend.ExprPrettyPrinter
--import Frontend.Lexer
--import Frontend.Parser
import Frontend.Token
import System.File
import Text.Bounds

main : IO ()
main = do
  putStrLn "Hello from Idris2!"
  -- fileResult <- readFile "program.rs"
  -- case fileResult of
  --   Left fileErr => putStrLn $ "Failed to read program.rs: " ++ show fileErr
  --   Right sampleProgram =>
  --     case lexProgram sampleProgram of
  --       Left err => putStrLn $ "Lexer error: " ++ show err
  --       Right tokens => do
  --         putStrLn $ "Tokens: " ++ show tokens
  --         case parseProgramAll tokens of
  --           Left err => putStrLn $ "Parse error: " ++ show err
  --           Right program => putStrLn $ "Parsed program: " ++ show program
