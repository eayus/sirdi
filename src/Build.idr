module Build

import Config
import System
import System.Directory
import Control.Monad.State
import Data.List
import Data.Maybe
import Ipkg
import Util


fetchTo : Location -> String -> IO ()
fetchTo (Link link) dest = ignore $ system "git clone \{link} \{dest}"
fetchTo (Local source) dest = ignore $ system "cp -r \{source} \{dest}"


{-
fetchDeps : String -> IO (List (Location, String))
fetchDeps = execStateT [] . fetchDeps'
    where
        mutual
            fetchDeps' : String -> StateT (List (Location, String)) IO ()
            fetchDeps' projectDir = do
                config <- lift $ readConfig projectDir
                traverse_ fetchDep config.deps

            -- This shouldn't infitely loop, however it doesn't explicitly error
            -- if a project depends on itself

            fetchDep : Location -> StateT (List (Location, String)) IO ()
            fetchDep loc =
              if not (loc `elem` map fst !get) then
                  (do
                      let name = "dep\{show (length !get)}"
                      lift $ fetchTo loc ".build/sources/\{name}"
                      modify ((loc, name) ::)

                      fetchDeps' ".build/sources/\{name}")
                    else putStrLn "Skipping \{show loc}, already downloaded..."
                    -}


createBuildDirs : M ()
createBuildDirs = do
    ignore $ mIO $ createDir ".build"
    ignore $ mIO $ createDir ".build/sources"  -- Store the raw git clones
    ignore $ mIO $ createDir ".build/deps"     -- Contains the built dependencies


forceLookup : Eq a => a -> List (a, b) -> b
forceLookup x xs = assert_total $ case lookup x xs of Just y => y

{-
buildProject : String -> List (Location, String) -> IO ()
buildProject dir locs = do
    config <- readConfig dir
    traverse_ (\loc => buildDep loc locs) config.deps

    let depNames = map (\loc => forceLookup loc locs) config.deps
    let ipkg = MkIpkg { name = "pname", depends = depNames, modules = config.modules }

    writeIpkg ipkg "\{dir}/pname.ipkg"

    ignore $ system "IDRIS2_PACKAGE_PATH=.build/deps idris2 --build \{dir}/pname.ipkg"
        where
            buildDep : Location -> List (Location, String) -> IO ()
            buildDep loc locs = do
                let name = forceLookup loc locs
                buildProject ".build/sources/\{name}" locs

                ignore $ createDir ".build/deps/\{name}"
                ignore $ system "cp -r .build/sources/\{name}/build/ttc/* .build/deps/\{name}/"
                -}

doBuild : String -> M ()
doBuild name = do
    let dir = ".build/sources/\{name}"
    config <- readConfig dir

    let ipkg = MkIpkg { name = name, depends = [], modules = config.modules }

    writeIpkg ipkg "\{dir}/\{name}.ipkg"
    ignore $ mIO $ system "idris2 --build \{dir}/\{name}.ipkg"


export
build : M ()
build = do
    createBuildDirs

    ignore $ mIO $ createDir ".build/sources/main"
    ignore $ mIO $ system "cp ./sirdi.dhall .build/sources/main"
    ignore $ mIO $ system "cp -r ./src .build/sources/main"

    doBuild "main"


    --locs <- fetchDeps "."
    --buildProject ".build/deps/dep0" locs
