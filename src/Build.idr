module Build

import Config
import System
import System.Directory
import Control.Monad.State
import Data.List
import Data.Maybe
import Ipkg
import Util


fetchTo : Location -> String -> M ()
fetchTo (Link link) dest = mIO $ ignore $ system "git clone \{link} \{dest}"
fetchTo (Local source) dest = mIO $ ignore $ system "cp -r \{source} \{dest}"


createBuildDirs : M ()
createBuildDirs = do
    ignore $ mIO $ createDir ".build"
    ignore $ mIO $ createDir ".build/sources"  -- Store the raw git clones
    ignore $ mIO $ createDir ".build/deps"     -- Contains the built dependencies


forceLookup : Eq a => a -> List (a, b) -> b
forceLookup x xs = assert_total $ case lookup x xs of Just y => y


installDep : String -> M ()
installDep name = do
    ignore $ mIO $ createDir ".build/deps/\{name}"
    ignore $ mIO $ system "cp -r .build/sources/\{name}/build/ttc/* .build/deps/\{name}/"


doBuild : String -> List (Location, String) -> M ()
doBuild name depLocs = do
    let dir = ".build/sources/\{name}"
    config <- readConfig dir

    let depNames = map (\x => forceLookup x depLocs) config.deps

    traverse_ (\depName => doBuild depName depLocs >> installDep depName) depNames

    let ipkg = MkIpkg { name = name, depends = depNames, modules = config.modules, main = config.main, exec = "main" <$ config.main }

    writeIpkg ipkg "\{dir}/\{name}.ipkg"

    let setpath = "IDRIS2_PACKAGE_PATH=$(realpath ./.build/deps)"
    ignore $ mIO $ system "\{setpath} idris2 --build \{dir}/\{name}.ipkg"


fetchDeps : String -> M (List (Location, String))
fetchDeps = execStateT [] . fetchDeps'
    where
        mutual
            fetchDeps' : String -> StateT (List (Location, String)) M ()
            fetchDeps' name = do
                config <- lift $ readConfig ".build/sources/\{name}"
                traverse_ fetchDep config.deps


            fetchDep : Location -> StateT (List (Location, String)) M ()
            fetchDep loc =
                if loc `elem` map fst !get
                    then lift $ mIO $ putStrLn "Skipping fetching \{show loc}, already encountered"
                    else do
                        let name = "dep\{show (length !get)}"
                        lift $ fetchTo loc ".build/sources/\{name}"
                        modify ((loc, name) ::)
                        fetchDeps' name


export
build : M ()
build = do
    createBuildDirs

    ignore $ mIO $ createDir ".build/sources/main"
    ignore $ mIO $ system "cp ./sirdi.json .build/sources/main"
    ignore $ mIO $ system "cp -r ./src .build/sources/main"

    depLocs <- fetchDeps "main"

    doBuild "main" depLocs


export
run : M ()
run = ignore $ mIO $ system ".build/sources/main/build/exec/main"
