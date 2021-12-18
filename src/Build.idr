module Build

import Config
import System
import System.Directory
import System.File
import Ipkg
import Util
import DepTree
import Data.List


-- TODO: Rearrange the functions in this file so they are at least grouped sensibly.


createBuildDirs : M ()
createBuildDirs = do
    ignore $ createDir ".build"
    ignore $ createDir ".build/sources"  -- Store the raw git clones
    ignore $ createDir ".build/deps"     -- Contains the built dependencies


||| The directory where sirdi stores the source of a package
(.sourceDir) : Package -> String
(.sourceDir) p = ".build/sources/\{pkgID p}"

||| The directory where built dependency files are stored
(.installDir) : Package -> String
(.installDir) p = ".build/deps/\{pkgID p}"

installDep : Package -> M ()
installDep p = do
    ignore $ createDir p.installDir
    ignore $ system "cp -r \{p.sourceDir}/build/ttc/* \{p.installDir}/"


makeDepTree : Package -> M DepTree
makeDepTree dep = case isLegacy dep of
    True => pure $ Node dep []
    False => do
        multiConfig <- readConfig dep.sourceDir
        config <- findSubConfig dep.name multiConfig
        children <- traverse makeDepTree config.deps

        pure $ Node dep children


buildPackage : Package -> M ()
buildPackage dep = unless (isLegacy dep) $ do
    putStrLn "Building \{dep.name}"

    multiConfig <- readConfig dep.sourceDir
    config <- findSubConfig dep.name multiConfig

    -- Get a DepTree for each dependency
    deps <- traverse makeDepTree config.deps
    -- get list of unique dependencies
    let deps = nubOn pkgID . treeToList =<< deps

    traverse_ buildPackage deps

    unless !(exists dep.installDir) (do

        let fname = "\{dep.sourceDir}/\{dep.name}.ipkg"
        let ipkg = MkIpkg {
            name = dep.name,
            depends = map pkgID deps,
            modules = config.modules,
            main = config.main,
            exec = "main" <$ config.main,
            passthru = config.passthru
        }

        writeIpkg ipkg fname

        let setpath = "IDRIS2_PACKAGE_PATH=$(realpath ./.build/deps)"
        mSystem "\{setpath} idris2 --build \{fname}" "Failed to build \{dep.name}"

        installDep dep
        )


fetchPackage : Package -> M ()
fetchPackage dep = unless (isLegacy dep) $ do
    -- Calculate where the dependency should be fetched to.
    let dir = dep.sourceDir

    -- If we haven't already fetched it, fetch it.
    unless !(exists dir) $ fetchTo dep.source dir

    -- Read the dependencies config.
    multiConfig <- readConfig dir
    config <- findSubConfig dep.name multiConfig

    -- Recursively fetch the dependencies of this dependency.
    traverse_ fetchPackage config.deps
      where
        fetchTo : Source -> String -> M ()
        fetchTo (Git link) dest = mSystem "git clone \{link} \{dest}" "Failed to clone \{link}"
        fetchTo (Local source) dest = do
            ignore $ createDir "\{dest}"
            mSystem "cp \{source}/sirdi.json \{dest}/sirdi.json" "Failed to copy \{source}/sirdi.json"
            mSystem "cp -r \{source}/src \{dest}/src" "Failed to copy \{source}/src"
        fetchTo Legacy _ = pure ()


export
build : Maybe String -> M ()
build subPkgName = do
    multiConfig <- readConfig "."
    config <- getSubConfig subPkgName multiConfig

    createBuildDirs

    let mainDep = MkPkg config.pkgName (Local ".")

    ensureRebuild mainDep
    fetchPackage mainDep
    buildPackage mainDep

    -- Since interactive editors are not yet compatible with sirdi, we must copy
    -- the "build/", ".deps" and "ipkg" back to the project root. This is annoying and
    -- can hopefully be removed eventually.
    ignore $ system "cp -r \{mainDep.sourceDir}/build ./"
    ignore $ system "cp -r \{mainDep.sourceDir}/\{mainDep.name}.ipkg ./"
    ignore $ system "cp -r .build/deps ./depends"
    where
        ensureRebuild : Package -> M ()
        ensureRebuild p = do
            ignore $ system "rm -rf \{p.sourceDir}"
            ignore $ system "rm -rf \{p.installDir}"


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
            ignore $ system "\{mainDep.sourceDir}/build/exec/main"
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
            [{ "deps": [ ], "modules": [ "Main" ], "main": "Main", "name": "mypkg" }]

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


export
prune : M ()
prune = do
    multiConfig <- readConfig "."
    let pkgs = map (\cfg => MkPkg cfg.pkgName (Local ".")) multiConfig

    trees <- traverse makeDepTree pkgs
    let deps = concatMap treeToList trees

    let validDirs = map pkgID deps

    res <- listDir ".build/sources"

    case res of
         Left err => print err
         Right dirs => do
            let badDirs = filter (\x => not $ x `elem` validDirs) dirs

            let srcDirs = map (\x => ".build/sources/\{x}") badDirs
            let depDirs = map (\x => ".build/deps/\{x}") badDirs

            traverse_ (\s => system "rm -rf \{s}") srcDirs
            traverse_ (\s => system "rm -rf \{s}") depDirs
