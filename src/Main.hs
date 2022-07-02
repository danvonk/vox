module Main where

import System.Environment
import Text.Printf

import Parser
import AST

run f = undefined

report :: Int -> String -> String -> IO ()
report = printf "[line %d] Error %s: %s"

err :: Int -> String -> IO ()
err line = report line ""

runFile :: String -> IO ()
runFile path = do
  f <- readFile path
  run f

runPrompt :: IO ()
runPrompt = do
  l <- getLine
  if null l
    then return ()
    else
    putStrLn ">" >>
    run l >>
    runPrompt

main :: IO ()
main = do
  as <- getArgs
  case length as of
    0 -> runPrompt
    1 -> runFile (head as)
    _ -> putStrLn "Usage: jlox [script]"

