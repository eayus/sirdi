module Build

import Config
import System
import System.Directory
import Ipkg
import Util
import DepTree


fetchTo : Source -> String -> M ()
fetchTo (Git link) dest = mSystem "git clone \{link} \{dest}" "Failed to clone \{link}"
fetchTo (Local source) dest = mSystem "cp -r \{source} \{dest}" "Failed to copy \{source}"


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

    let depNames = map depID config.deps

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
    mSystem "\{setpath} idris2 --build \{dir}/\{name}.ipkg" "Failed to build \{name}"
        where
            doBuildDep : String -> M ()
            doBuildDep depName = do
                n <- mIO $ system "[ -d '.build/deps/\{depName}' ]"

                when (n /= 0) (doBuild depName >> installDep depName)
                


fetchDeps : String -> M ()
fetchDeps name = do
    config <- readConfig ".build/sources/\{name}"

    traverse_ fetchDep config.deps

    where
        fetchDep : Dependency -> M ()
        fetchDep dep = do
            let depName = depID dep
            n <- mIO $ system "[ -d '.build/sources/\{depName}' ]"
            when (n /= 0) (fetchTo dep.source ".build/sources/\{depName}")

            fetchDeps depName


buildDepTree : (dir : String) -> (source : Source) -> M DepTree
buildDepTree dir source = do
    config <- readConfig dir
    subtrees <- traverse (\dep => buildDepTree ".build/sources/\{depID dep}" dep.source) config.deps
    pure $ Node (MkDep config.pkgName source) subtrees


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
depTree : M ()
depTree = do
    build
    tree <- buildDepTree "." (Local "." )
    mIO $ print tree


export
run : M ()
run = do
    -- We read config files a lot. Perhaps we should add a caching system to the
    -- monad so that config files are kept in memory once they've been read once.
    config <- readConfig "."

    case config.main of
         Just _ => do
            build
            ignore $ mIO $ system ".build/sources/main/build/exec/main"
         Nothing => mIO $ putStrLn "Cannot run. No 'main' specified in sirdi configuration file."


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

export
clean : M ()
clean = do
    config <- readConfig "."

    ignore $ mIO $ system "rm -rf ./depends"
    ignore $ mIO $ system "rm -rf ./build"
    ignore $ mIO $ system "rm -rf ./main.ipkg"
    ignore $ mIO $ system "rm -rf ./.build"

    mIO $ putStrLn "Cleaned up"
