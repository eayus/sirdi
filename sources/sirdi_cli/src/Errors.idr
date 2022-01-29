module Errors

import Sirdi
import System.Path
import Language.TOML
import Language.TOML.Processing
import Core.Core


public export
Show InitError where
    show NoConfigFile = "No config file in current directory found!"


public export
Show ValueTy where
    show TString = "String"
    show TInteger = "Integer"
    show TFloat = "Float"
    show TBoolean = "Boolean"
    show (TEnum xs) = "Enum as \{show xs}"
    show (TArray x) = "Array of (\{show x})"
    show (TTable x) = "Table"


mutual
    public export
    Show TableError where
        show (FieldError x y) = "In the definition of the field \{show x}...\n\{show y}"
        show (ExpectedField x) = "Required field \{show x} not supplied"
        show (UnexpectedFields xs) = "Unexpected fields \{show xs}"


    public export
    Show ValueError where
        show (ExpectedType x) = "Expected a value of type \{show x}"
        show (InsideArray x) = "Inside the array...\n\{show x}"
        show (InsideTable x) = "Inside the table...\n\{show x}"
        show (BadEnumVal x) = "\{show x} is not a valid value for the enum"


public export
Show ConfigError where
    show (TOMLError x) = "TOML Error: \{show x}"
    show (ValidateError x) = "Validation error: \{show x}"


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

