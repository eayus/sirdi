module Build

import Config
import System
import System.Directory
import System.File
import Ipkg
import Util
import DepTree
import Data.List


||| The directory where sirdi stores the source of a package
(.sourceDir) : Identifier pin -> String
(.sourceDir) ident = ".build/sources/\{pkgID ident}"

||| The directory where built dependency files are stored
(.installDir) : Identifier pin -> String
(.installDir) ident = ".build/deps/\{pkgID ident}"

installDep : Identifier pin -> M ()
installDep ident = do
    ignore $ createDir ident.installDir
    ignore $ system "cp -r \{ident.sourceDir}/build/ttc/* \{ident.installDir}/"


Package : Type
Package = (Identifier Unspecified, Config)
-- name . fst == pkgName . snd


createBuildDirs : M ()
createBuildDirs = do
    ignore $ createDir ".build"
    ignore $ createDir ".build/sources"  -- Store the raw git clones
    ignore $ createDir ".build/deps"     -- Contains the built dependencies


||| Loads a tree with configs of all dependencies, fetching them if necessary
configTree : Identifier Unspecified -> M (Tree Package)
configTree ident = case isLegacy ident of
    True => pure $ Node (ident, emptyConfig ident) []
    False => do
        unless !(exists ident.sourceDir) $ fetch ident
        multiConfig <- readConfig ident.sourceDir
        config <- findSubConfig ident.name multiConfig
        children <- traverse configTree config.deps
        pure $ Node (ident, config) children
    where
        fetch : Identifier pin -> M ()
        fetch ident = case source ident of
            Git link _ => mSystem "git clone \{link} \{ident.sourceDir}"
                                "Failed to clone \{link}"
            Local path => do ignore $ createDir "\{ident.sourceDir}"
                             mSystem "cp \{path}/sirdi.json \{ident.sourceDir}/sirdi.json"
                                "Failed to copy \{path}/sirdi.json"
                             mSystem "cp -r \{path}/src \{ident.sourceDir}/src"
                                "Failed to copy \{path}/src"
            Legacy => pure ()


||| Builds a package
|||
||| Returns a list of idris packages to add to "depends" in order to properly
||| depend on this package.
buildTree : Tree Package -> M (List String)
buildTree (Node (ident, config) deps) = case (isLegacy ident) of
    True => pure [ config.pkgName ]
    False => do
        -- Build all dependencies first
        depNames <- nub . join <$> traverse buildTree deps

        putStrLn "Building \{ident.name}"
        unless !(exists ident.installDir) $ do
            let fname = "\{ident.sourceDir}/\{ident.name}.ipkg"
            let ipkg = MkIpkg {
                name = ident.name,
                depends = depNames,
                modules = config.modules,
                main = config.main,
                exec = "main" <$ config.main,
                passthru = config.passthru
            }

            writeIpkg ipkg fname

            let setpath = "IDRIS2_PACKAGE_PATH=$(realpath ./.build/deps)"
            mSystem "\{setpath} idris2 --build \{fname}" "Failed to build \{ident.name}"

            installDep ident

        pure $ pkgID ident :: depNames


makeDepTree : Identifier Unspecified -> M DepTree
makeDepTree ident = map @{Compose} fst $ configTree ident


getMain : Maybe String -> M Package
getMain subPkgName = do
    multiConfig <- readConfig "."
    config <- getSubConfig subPkgName multiConfig
    let mainIdent = MkPkg config.pkgName (Local ".")
    pure (mainIdent, config)


export
build : Maybe String -> M ()
build subPkgName = do
    (ident, _) <- getMain subPkgName
    ensureRebuild ident

    createBuildDirs
    cfgs <- configTree ident -- loads main cfg again?
    ignore $ buildTree cfgs

    -- Since interactive editors are not yet compatible with sirdi, we must copy
    -- the "build/", ".deps" and "ipkg" back to the project root. This is annoying and
    -- can hopefully be removed eventually.
    ignore $ system "cp -r \{ident.sourceDir}/build ./"
    ignore $ system "cp -r \{ident.sourceDir}/\{ident.name}.ipkg ./"
    ignore $ system "cp -r .build/deps ./depends"
    where
        ensureRebuild : Identifier pin -> M ()
        ensureRebuild ident = do
            ignore $ system "rm -rf \{ident.sourceDir}"
            ignore $ system "rm -rf \{ident.installDir}"


export
depTree : Maybe String -> M ()
depTree subPkgName = do
    (ident, _) <- getMain subPkgName
    tree <- makeDepTree ident
    print tree


export
run : Maybe String -> M ()
run subPkgName = do
    (ident, config) <- getMain subPkgName
    case config.main of
         Just _ => do
            createBuildDirs
            cfgs <- configTree ident -- loads main cfg again?
            ignore $ buildTree cfgs

            ignore $ system "\{ident.sourceDir}/build/exec/main"
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
    let pkgs = map (\cfg => MkPkg {sk = Unspecified} cfg.pkgName (Local ".")) multiConfig

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
