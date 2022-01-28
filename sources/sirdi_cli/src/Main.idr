module Main

import Sap
import Sirdi
import Core.Core
import System.Path
import Language.TOML


Show InitError where
    show NoConfigFile = "No config file in current directory found!"


Show ConfigError where
    show (TOMLError x) = "TOML Error: \{show x}"
    show (ValidateError x) = "Validation error: \{x}"


Show (FetchError ident) where
    show (BadGitRepo badURL) = "Bad git repo \{badURL}"
    show (BadGitCommit badCommit) = "Bad git commit \{badCommit}"
    show (BadGitPath badPath) = "Bad git path \{show badPath}"
    show (BadLocalPath badPath) = "Bad local path \{show badPath}"
    show NoConfigFile = "No config file"
    show (BadConfig x) = "Bad config \{show x}"


Show RecBuildError where
    show (FetchErr ident x) = "Fetch error:\n\{show x}"
    show (BuildErr (CompileError x)) = "Compilation error\n\{show x}"


Show NewProjectError where
    show (CreateDirError x) = "Create dir error: \{x}"


myBuild : IOEither String ()
myBuild = do
    x <- mapErr show $ init
    ignore $ mapErr show $ recBuild Self


myBuild' : IO ()
myBuild' = runIOE myBuild putStrLn pure


myRun' : IOEither String ()
myRun' = do
    initialised <- mapErr show $ init
    ignore $ mapErr show $ buildAndRun Self


myRun : IO ()
myRun = runIOE myRun' putStrLn pure


cmd : Command (IO ())
cmd = Cmd {
    name = "sirdi",
    desc = "Idris2 Package manager CLI",
    rhs = SubCmds [
        Cmd {
            name = "build",
            desc = "Builds the current project",
            rhs = Basic [] [] (\[], [] => myBuild')
        },
        Cmd {
            name = "run",
            desc = "Builds and runs the current project",
            rhs = Basic [] [] (\[], [] => myRun)
        },
        Cmd {
            name = "new",
            desc = "Creates a new project",
            rhs = Basic ["name" # String] [] (\[name], [] => runIOE (new name) print pure)
        }
    ]
}


partial
main : IO ()
main = runCommand cmd
