module Build

import Config
import System
import System.Directory
import Ipkg
import Util
import DepTree


-- TODO: Rearrange the functions in this file so they are at least grouped sensibly.


fetchTo : Source -> String -> M ()
fetchTo (Git link) dest = mSystem "git clone \{link} \{dest}" "Failed to clone \{link}"
fetchTo (Local source) dest = do
    ignore $ createDir "\{dest}"
    mSystem "cp \{source}/sirdi.json \{dest}/sirdi.json" "Failed to copy \{source}/sirdi.json"
    mSystem "cp -r \{source}/src \{dest}/src" "Failed to copy \{source}/src"
fetchTo Legacy _ = pure ()


createBuildDirs : M ()
createBuildDirs = do
    ignore $ createDir ".build"
    ignore $ createDir ".build/sources"  -- Store the raw git clones
    ignore $ createDir ".build/deps"     -- Contains the built dependencies


installDep : String -> M ()
installDep name = do
    ignore $ createDir ".build/deps/\{name}"
    ignore $ system "cp -r .build/sources/\{name}/build/ttc/* .build/deps/\{name}/"


buildPackage : Package -> M ()
buildPackage dep = unless (isLegacy dep) $ do
    putStrLn "Building \{dep.name}"

    let dir = ".build/sources/\{pkgID dep}"

    multiConfig <- readConfig dir
    config <- findSubConfig dep.name multiConfig

    traverse_ buildPackage config.deps

    n <- system "[ -d '.build/deps/\{pkgID dep}' ]"
    when (n /= 0) (do

        let ipkg = MkIpkg {
            name = dep.name,
            depends = map pkgID config.deps,
            modules = config.modules,
            main = config.main,
            exec = "main" <$ config.main,
            passthru = config.passthru
        }

        writeIpkg ipkg "\{dir}/\{dep.name}.ipkg"

        let setpath = "IDRIS2_PACKAGE_PATH=$(realpath ./.build/deps)"
        mSystem "\{setpath} idris2 --build \{dir}/\{dep.name}.ipkg" "Failed to build \{dep.name}"

        installDep (pkgID dep)
        )


-- TODO: perhaps rename "Package" to "Package"
fetchPackage : Package -> M ()
fetchPackage dep = unless (isLegacy dep) $ do
    -- Calculate where the dependency should be fetched to.
    let dir = ".build/sources/\{pkgID dep}"

    -- If we haven't already fetched it, fetch it.
    n <- system "[ -d '\{dir}' ]"
    when (n /= 0) (fetchTo dep.source dir)

    -- Read the dependencies config.
    multiConfig <- readConfig dir
    config <- findSubConfig dep.name multiConfig

    -- Recursively fetch the dependencies of this dependency.
    traverse_ fetchPackage config.deps


makeDepTree : Package -> M DepTree
makeDepTree dep = case isLegacy dep of
    True => pure $ Node dep []
    False => do
        let dir = ".build/sources/\{pkgID dep}"

        multiConfig <- readConfig dir
        config <- findSubConfig dep.name multiConfig

        children <- traverse makeDepTree config.deps

        pure $ Node dep children


export
build : Maybe String -> M ()
build subPkgName = do
    multiConfig <- readConfig "."
    config <- getSubConfig subPkgName multiConfig

    createBuildDirs

    let mainDep = MkPkg config.pkgName (Local ".")
    let mainID = pkgID mainDep

    fetchPackage mainDep
    buildPackage mainDep

    -- Since interactive editors are not yet compatible with sirdi, we must copy
    -- the "build/", ".deps" and "ipkg" back to the project root. This is annoying and
    -- can hopefully be removed eventually.
    ignore $ system "cp -r .build/sources/\{mainID}/build ./"
    ignore $ system "cp -r .build/sources/\{mainID}/\{mainDep.name}.ipkg ./"
    ignore $ system "cp -r .build/deps ./depends"


export
depTree : Maybe String -> M ()
depTree subPkgName = do
    multiConfig <- readConfig "."
    config <- getSubConfig subPkgName multiConfig

    let mainDep = MkPkg config.pkgName (Local ".")

    build subPkgName
    tree <- makeDepTree mainDep
    print tree


export
run : Maybe String -> M ()
run subPkgName = do
    -- We read config files a lot. Perhaps we should add a caching system to the
    -- monad so that config files are kept in memory once they've been read once.
    multiConfig <- readConfig "."
    config <- getSubConfig subPkgName multiConfig

    let mainDep = MkPkg config.pkgName (Local ".")

    case config.main of
         Just _ => do
            build subPkgName
            ignore $ system ".build/sources/\{pkgID mainDep}/build/exec/main"
         Nothing => putStrLn "Cannot run. No 'main' specified in sirdi configuration file."


export
new : String -> M ()
new name = do
    ignore $ createDir "\{name}"
    ignore $ createDir "\{name}/src"

    ignore $ writeFile "\{name}/sirdi.json" jsonFile
    ignore $ writeFile "\{name}/src/Main.idr" idrFile
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
    _ <- readConfig "." -- To ensure that we are in a sirdi directory.

    ignore $ system "rm -rf ./depends"
    ignore $ system "rm -rf ./build"
    ignore $ system "rm -rf ./*.ipkg"
    ignore $ system "rm -rf ./.build"

    putStrLn "Cleaned up"
