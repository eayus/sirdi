module Main

import Build
import Util
import System


usage : String
usage = """
Usage: sirdi <command> <arg...>
Commands:
    new <project_name>   Creates a template Idris2 project.
    build       Builds the project and its dependencies.
    run         Runs the executable (if a main has been specified in the config).
    clean       Removes all build files.
    dep-tree    Prints a dependency tree.
    prune       Deletes build files for old dependencies that are no longer used.
"""

processArgs : List String -> M ()
processArgs ["run"] = run Nothing
processArgs ["run", subPkgName] = run (Just subPkgName)

processArgs ["build"] = build Nothing
processArgs ["build", subPkgName] = build (Just subPkgName)

processArgs ["clean"] = clean

processArgs ["dep-tree"] = depTree Nothing
processArgs ["dep-tree", subPkgName] = depTree (Just subPkgName)

processArgs ["new", fp] = new fp

processArgs ["prune"] = prune

processArgs _ = putStrLn usage


main : IO ()
main = do
    args <- getArgs
    case args of
         [] => pure ()
         (_ :: args') => ignore $ runM $ processArgs args'
