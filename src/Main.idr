module Main

import Build
import Util
import System


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

processArgs _ = putStrLn "Invalid arguments"


main : IO ()
main = do
    args <- getArgs
    case args of
         [] => pure ()
         (_ :: args') => ignore $ runM $ processArgs args'
