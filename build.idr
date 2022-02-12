import System
import System.Directory
import System.Path
import Control.ANSI

depends : List (String, String, Maybe String) -- package, repo, commit
depends =
    [ ("hashable", "https://github.com/Z-snails/Idris2-hashable.git", Nothing)
    , ("toml", "https://github.com/cuddlefishie/toml-idr", Nothing)
    , ("sap", "https://github.com/eayus/sap.git", Nothing)
    ]

libDir, libDepends : String
libDir = "sources/sirdi_lib"
libDepends = libDir </> "depends"

cliDir, cliDepends : String
cliDir = "sources/sirdi_cli"
cliDepends = cliDir </> "depends"

-- print the given error in red
printError : String -> IO ()
printError msg = do printLn (colored Red msg); exitFailure

-- try to create a directory, exiting on failure
tryCreateDir : String -> IO ()
tryCreateDir dir = case !(createDir dir) of
    Right _ => pure ()
    Left FileExists => pure ()
    Left err => printError "error creating directory \"\{dir}\": \{show err}"

-- check a given path is a directory
isDir : String -> IO Bool
isDir dir = do
    Right dirh <- openDir dir
        | Left err => pure False
    closeDir dirh
    pure True

-- run an action in a specific directory
inDir : String -> IO a -> IO (Maybe a)
inDir dir act = do
    Just currentDir <- currentDir
        | Nothing => pure Nothing
    True <- changeDir dir
        | False => pure Nothing
    res <- act
    ignore $ changeDir currentDir
    pure $ Just res

-- clone a url from git
cloneDep : String -> String -> IO ()
cloneDep pkg url =
    unless !(exists $ "build" </> pkg)
        $ ignore $ system ["git", "clone", url, "build" </> pkg]

-- fetch a dependency from git
fetchDep : (String, String, Maybe String) -> IO ()
fetchDep (pkg, url, commit) = do
    cloneDep pkg url
    whenJust commit
        $ \commit => ignore $ inDir ("build" </> pkg)
            $ ignore $ system ["git", "checkout", "--detach", commit]

-- build a dependency
buildDep : (String, String, Maybe String) -> IO ()
buildDep (pkg, url, _) = do
    Just 0 <- inDir ("build" </> pkg) $ do
        putStrLn "building \{pkg}"
        system ["idris2", "--build", pkg <.> "ipkg"]
        | Nothing => printError "failed to clone git repository: \{url}"
        | Just code => printError "failed to build package, code: \{show code}"
    pure ()

-- copy all .ttc files from source to dest
copyAll : (source : String) -> (dest : String) -> IO ()
copyAll buildDir depDir = do
    Right ents <- listDir buildDir
        | Left err => printError $ show err
    for_ ents $ \ent => do
        let path = buildDir </> ent
        if !(isDir path)
            then do
                let depDir = depDir </> ent
                tryCreateDir depDir
                copyAll path depDir
            else do
                let Just ext = extension ent
                    | Nothing => pure ()
                when (ext == "ttc")
                    $ ignore $ copyFile path (depDir </> ent)

-- copy the ttc files for a given dependency to the destination depends dir
copyTTC : (depends : String) -> (String, String, Maybe String) -> IO ()
copyTTC depends (pkg, _, _) = do
    let depDir = depends </> (pkg ++ "-0")
        buildDir = "build" </> pkg </> "build" </> "ttc"
    tryCreateDir depDir
    copyAll buildDir depDir

buildDepends : IO ()
buildDepends = do
    traverse_ fetchDep depends
    traverse_ buildDep depends
    tryCreateDir "depends"
    traverse_ (copyTTC "depends") depends

buildSirdiLib : IO ()
buildSirdiLib = do
    -- copy depends to sources/sirdi_lib/depends
    tryCreateDir libDepends
    traverse_ (copyTTC libDepends) depends

    -- build sirdi_lib
    0 <- system ["idris2", "--build", "sources" </> "sirdi_lib" </> "sirdi_lib.ipkg"]
        | code => printError "failed to build sirdi_lib: \{show code}"
    pure ()

buildSirdiCli : IO ()
buildSirdiCli = do
    -- copy depends to sources/sirdi_cli/depends
    tryCreateDir cliDepends
    traverse_ (copyTTC cliDepends) depends

    -- copy sirdi_lib to sources/sirdi_cli/depends
    tryCreateDir (cliDepends </> "sirdi_lib-0")
    copyAll (libDir </> "build" </> "ttc") (cliDepends </> "sirdi_lib-0")

    -- build sirdi_cli
    0 <- system ["idris2", "--build", "sources" </> "sirdi_cli" </> "sirdi_cli.ipkg"]
        | code => printError "failed to build sirdi_cli: \{show code}"

    pure ()
