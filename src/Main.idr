module Main

import Collie
import Build
import Util


mainCommand : Command "sirdi"
mainCommand = MkCommand
         { description = "A simple package manager for Idris2"
         , subcommands = [ "build" ::= basic "Builds the current project" none
                         , "run"   ::= basic "Runs the executable" none
                         , "new"   ::= basic "Creates a new project" (lotsOf filePath) ]
         , modifiers = []
         , arguments = none }


main' : Main.mainCommand ~~> IO ()
main' = [
    \_ => putStrLn "Invalid command",
    "run" ::= [ const $ ignore $ runM run ],
    "build" ::= [ const $ ignore $ runM build ],
    "new" ::= [ \args => case args.arguments of { Just [ s ] => ignore $ runM (new s); _ => putStrLn "Bad args to new command" } ] ]

main : IO ()
main = Main.mainCommand .handleWith main'
