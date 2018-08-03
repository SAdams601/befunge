module Main where
import Data.Either
import Lib

main :: IO ()
main = do
  let fp = "test/data/helloWorld.bf"
  fc <- readFile fp
  let eProg = parse fp fc
  case eProg of
    Left err -> do
      putStrLn "Parsing failed with error: "
      print err
    Right prog -> do
      putStrLn $ "The parsed program is " ++ show (maxCols prog) ++ " wide"
      putStrLn $ "The parsed program is " ++ show (maxRows prog) ++ " tall"
      putStrLn "Parsed program: "
      putStrLn $ prettyPrint prog


getProg :: IO BefungeProgram
getProg = do
    let fp = "test/data/helloWorld.bf"
    fc <- readFile fp
    let eProg = parse fp fc
    return $ (head . rights) [eProg]
