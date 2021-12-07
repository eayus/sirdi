module Build

import Config
import System
import System.Directory
import Control.Monad.State
import Data.List
import Data.Maybe
import Ipkg


fetchTo : Location -> String -> IO ()
fetchTo (Link link) dest = ignore $ system "git clone \{link} \{dest}"
fetchTo (Local source) dest = ignore $ system "cp -r \{source} \{dest}"


fetchDeps : String -> IO (List (Location, String))
fetchDeps = execStateT [] . fetchDeps'
    where
        mutual
            fetchDeps' : String -> StateT (List (Location, String)) IO ()
            fetchDeps' projectDir = do
                config <- lift $ parseConfig projectDir
                traverse_ fetchDep config.deps

            -- This shouldn't infitely loop, however it doesn't explicitly error
            -- if a project depends on itself

            fetchDep : Location -> StateT (List (Location, String)) IO ()
            fetchDep loc =
              if not (loc `elem` map fst !get) then
                  (do
                      let name = "dep\{show (length !get)}"
                      lift $ fetchTo loc "tmp/sources/\{name}"
                      modify ((loc, name) ::)

                      fetchDeps' "tmp/sources/\{name}")
                    else putStrLn "Skipping \{show loc}, already downloaded..."


createBuildDirs : IO ()
createBuildDirs = do
    ignore $ createDir "tmp"
    ignore $ createDir "tmp/sources"  -- Store the raw git clones
    ignore $ createDir "tmp/deps"     -- Contains the built dependencies


forceLookup : Eq a => a -> List (a, b) -> b
forceLookup x xs = assert_total $ case lookup x xs of Just y => y


buildProject : String -> List (Location, String) -> IO ()
buildProject dir locs = do
    config <- parseConfig dir
    traverse_ (\loc => buildDep loc locs) config.deps

    let depNames = map (\loc => forceLookup loc locs) config.deps
    let ipkg = MkIpkg { name = "pname", depends = depNames, modules = config.modules }

    writeIpkg ipkg "\{dir}/pname.ipkg"

    ignore $ system "IDRIS2_PACKAGE_PATH=tmp/deps idris2 --build \{dir}/pname.ipkg"
        where
            buildDep : Location -> List (Location, String) -> IO ()
            buildDep loc locs = do
                let name = forceLookup loc locs
                buildProject "tmp/sources/\{name}" locs

                ignore $ createDir "tmp/deps/\{name}"
                ignore $ system "cp -r tmp/sources/\{name}/build/ttc/* tmp/deps/\{name}/"


export
build : IO ()
build = do
    createBuildDirs
    locs <- fetchDeps "."
    buildProject "." locs
