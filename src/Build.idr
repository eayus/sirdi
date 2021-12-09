module Build

import Config
import System
import System.Directory
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


installDep : String -> M ()
installDep name = do
    ignore $ mIO $ createDir ".build/deps/\{name}"
    ignore $ mIO $ system "cp -r .build/sources/\{name}/build/ttc/* .build/deps/\{name}/"


doBuild : String -> M ()
doBuild name = do
    mIO $ putStrLn "Building \{name}"
    let dir = ".build/sources/\{name}"
    config <- readConfig dir

    let depNames = map hashLoc config.deps

    traverse_ doBuildDep depNames

    let ipkg = MkIpkg {
        name = name,
        depends = depNames,
        modules = config.modules,
        main = config.main,
        exec = "main" <$ config.main,
        passthru = config.passthru
    }

    writeIpkg ipkg "\{dir}/\{name}.ipkg"

    let setpath = "IDRIS2_PACKAGE_PATH=$(realpath ./.build/deps)"
    ignore $ mIO $ system "\{setpath} idris2 --build \{dir}/\{name}.ipkg"
        where
            doBuildDep : String -> M ()
            doBuildDep depName = do
                n <- mIO $ system "[ -d '.build/deps/\{depName}' ]"

                when (n /= 0) (doBuild depName >> installDep depName)
                


-- Eventually this should return a depdency tree!
-- Then we can feed that tree into doBuild, so that it doesn't have to
-- re-read config files. In addition, we can define a new command to
-- print the dep tree.
fetchDeps : String -> M ()
fetchDeps name = do
    config <- readConfig ".build/sources/\{name}"

    traverse_ fetchDep config.deps

    where
        fetchDep : Location -> M ()
        fetchDep loc = do
            let depName = hashLoc loc
            n <- mIO $ system "[ -d '.build/sources/\{depName}' ]"
            when (n /= 0) (fetchTo loc ".build/sources/\{depName}")

            fetchDeps depName


export
build : M ()
build = do
    createBuildDirs

    ignore $ mIO $ createDir ".build/sources/main"
    ignore $ mIO $ system "cp ./sirdi.json .build/sources/main"
    ignore $ mIO $ system "cp -r ./src .build/sources/main"

    fetchDeps "main"
    doBuild "main"

    -- Since interactive editors are not yet compatible with sirdi, we must copy
    -- the "build/", ".deps" and "ipkg" back to the project root. This is annoying and
    -- can hopefully be removed eventually.
    ignore $ mIO $ system "cp -r .build/sources/main/build ./"
    ignore $ mIO $ system "cp -r .build/sources/main/main.ipkg ./"
    ignore $ mIO $ system "cp -r .build/deps ./depends"


export
run : M ()
run = ignore $ mIO $ system ".build/sources/main/build/exec/main"


export
new : String -> M ()
new name = do
    ignore $ mIO $ createDir "\{name}"
    ignore $ mIO $ createDir "\{name}/src"

    ignore $ mIO $ writeFile "\{name}/sirdi.json" jsonFile
    ignore $ mIO $ writeFile "\{name}/src/Main.idr" idrFile
        where
            idrFile : String
            idrFile = """
            module Main

            main : IO ()
            main = putStrLn "Hello from Idris2!"
            """

            jsonFile : String
            jsonFile = """
            { "deps": [ ], "modules": [ "Main" ], "main": "Main" }

            """
