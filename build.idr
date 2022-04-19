import System
import System.Directory
import System.Path
import Control.ANSI
import Data.String

depends : List (String, String, Maybe String) -- package, repo, commit
depends = [("hashable", "https://github.com/Z-snails/Idris2-hashable.git", Just "d6fec8c878057909b67f3d4da334155de4f37907")]

printError : String -> IO ()
printError = printLn . colored Red

isDir : String -> IO Bool
isDir dir = do
    Right dirh <- openDir dir
        | Left err => pure False
    closeDir dirh
    pure True

inDir : String -> IO a -> IO (Maybe a)
inDir dir act = do
    Just currentDir <- currentDir
        | Nothing => pure Nothing
    True <- changeDir dir
        | False => pure Nothing
    res <- act
    ignore $ changeDir currentDir
    pure $ Just res

cloneDep : String -> String -> IO ()
cloneDep pkg url =
    unless !(exists $ "build" </> pkg)
        $ ignore $ system $ unwords ["git", "clone", url, "build" </> pkg]

fetchDep : (String, String, Maybe String) -> IO ()
fetchDep (pkg, url, commit) = do
    cloneDep pkg url
    whenJust commit
        $ \commit => ignore $ inDir ("build" </> pkg)
            $ ignore $ system $ unwords ["git", "checkout", "--detach", commit]

buildDep : (String, String, Maybe String) -> IO ()
buildDep (pkg, url, _) = do
    Just 0 <- inDir ("build" </> pkg) $ do
        putStrLn "building \{pkg}"
        system $ unwords ["idris2", "--build", pkg <.> "ipkg"]
        | Nothing => printError "failed to clone git repository: \{url}"
        | Just code => printError "failed to build package, code: \{show code}"
    pure ()

copyTTC : (String, String, Maybe String) -> IO ()
copyTTC (pkg, _, _) = do
    let depDir = "depends" </> (pkg ++ "-0")
        buildDir = "build" </> pkg </> "build" </> "ttc"
    ignore $ createDir depDir
    copyAll depDir buildDir
  where
    copyAll : String -> String -> IO ()
    copyAll depDir buildDir = do
        Right ents <- listDir buildDir
            | Left err => printError $ show err
        for_ ents $ \ent => do
            let path = buildDir </> ent
            if !(isDir path)
                then do
                    let depDir = depDir </> ent
                    res <- createDir depDir
                    case res of
                        Right () => pure ()
                        Left FileExists => pure ()
                        Left err => printError $ show err
                    copyAll depDir path
                else do
                    let Just ext = extension ent
                        | Nothing => pure ()
                    when (ext == "ttc")
                        $ ignore $ copyFile path (depDir </> ent)

build : IO ()
build = do
    ignore $ traverse fetchDep depends
    ignore $ traverse buildDep depends
    ignore $ createDir "depends"
    ignore $ traverse copyTTC depends
