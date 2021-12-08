module Main

import Collie
import Build
import Util


mainCommand : Command "sirdi"
mainCommand = MkCommand
         { description = "A simple package manager for Idris2"
         , subcommands = [ "build" ::= basic "Builds the current project" none
                         , "run"   ::= basic "Runs the executable" none
                         , "new"   ::= basic "Creates a new project" none ]
         , modifiers = []
         , arguments = none }


main : IO ()
main = do
  Right cmdParse <- mainCommand.parseArgs | Left err => putStrLn "Error: \{err}"
  case fst (lookup cmdParse) of
       "run"   => putStrLn "Running executable..."
       "build" => do
          putStrLn "Building project..."
          ignore $ runM build
       "new"   => putStrLn "Creating new project..."
       _       => putStrLn "Invalid command"
