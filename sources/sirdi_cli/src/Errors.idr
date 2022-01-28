module Errors

import Sirdi
import System.Path
import Language.TOML
import Core.Core


public export
Show InitError where
    show NoConfigFile = "No config file in current directory found!"


public export
Show ConfigError where
    show (TOMLError x) = "TOML Error: \{show x}"
    show (ValidateError x) = "Validation error: \{x}"


public export
Show (FetchError ident) where
    show (BadGitRepo badURL) = "Bad git repo \{badURL}"
    show (BadGitCommit badCommit) = "Bad git commit \{badCommit}"
    show (BadGitPath badPath) = "Bad git path \{show badPath}"
    show (BadLocalPath badPath) = "Bad local path \{show badPath}"
    show NoConfigFile = "No config file"
    show (BadConfig x) = "Bad config \{show x}"


public export
Show RecBuildError where
    show (FetchErr ident x) = "Fetch error:\n\{show x}"
    show (BuildErr (CompileError x)) = "Compilation error\n\{show x}"


public export
Show NewProjectError where
    show (CreateDirError x) = "Create dir error: \{x}"

