module Build

import Config
import System
import System.Directory
import Control.Monad.State


fetchDeps : Config -> IO (List (Location, String))
fetchDeps = execStateT [] . fetchDeps'
    where
        mutual
            fetchDeps' : Config -> StateT (List (Location, String)) IO ()
            fetchDeps' config = traverse_ fetchDep config.deps

            --| This shouldn't infitely loop, however it doesn't explicitly error
            --| if a project depends on itself

            fetchDep : Dependency -> StateT (List (Location, String)) IO ()
            fetchDep (MkDep loc@(Link link)) =
              if not (loc `elem` map fst !get) then
                  (do
                      let name = "dep\{show (length !get)}"
                      success <- system "git clone \{link} tmp/\{name}"
                      modify ((Link link, name) ::)

                      depConfig <- lift $ parseConfig name
                      fetchDeps' depConfig)
                   else putStrLn "Skipping \{link}, already downloaded..."


export
build : IO ()
build = do
    config <- parseConfig "./"
    _ <- createDir "tmp" -- TODO: Handle errors
    allDeps <- fetchDeps config
    pure ()
