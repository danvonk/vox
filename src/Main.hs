module Main where

import System.Environment
import Text.Printf

import Parser

run :: String -> IO ()
run s = case parseString s of
  Right a -> print a
  Left _ -> putStrLn "Parse Error"

report :: Int -> String -> String -> IO ()
report = printf "[line %d] Error %s: %s"

err :: Int -> String -> IO ()
err line = report line ""

runFile :: String -> IO ()
runFile f = parseFile f >>= (\x -> case x of
                                Right a -> print a
                                Left _ -> putStrLn "Error parsing file."
                            )
runPrompt :: IO ()
runPrompt = do
  l <- getLine
  if null l
    then return ()
    else
    putStr "vox> " >>
    run l >>
    runPrompt

main :: IO ()
main = do
  as <- getArgs
  case length as of
    0 -> runPrompt
    1 -> runFile (head as)
    _ -> putStrLn "Usage: vox [script]"
