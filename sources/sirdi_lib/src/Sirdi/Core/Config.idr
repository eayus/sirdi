module Sirdi.Core.Config

import Sirdi.Core
import Util.IOEither
import Language.TOML
import System.Path


public export
data ConfigError : Type where
    TOMLError : TOML.Error -> ConfigError
    ValidateError : String -> ConfigError


processPath : Value -> Either String Path
processPath (VString x) = pure $ parse x -- TODO: On next Idris version, parse to either
processPath _ = Left "'path' should be a string"


processGitDependency : Table -> Either String Identifier
processGitDependency x with (lookup "url" x, lookup "commit" x, lookup "path" x)
  _ | (Nothing, _, _) = Left "'url' is a required field of a git dependency"
  _ | (_, Nothing, _) = Left "'commit' is a required field of a git dependency"
  _ | (_, _, Nothing) = Left "'path' is a required field of a git dependency"
  _ | (Just url, Just commit, Just path) = pure $ Git !(processURL url) !(processCommit commit) !(processPath path)
    where
        processURL : Value -> Either String String
        processURL (VString x) = pure x
        processURL _ = Left "'url' should be a string"

        processCommit : Value -> Either String String
        processCommit (VString x) = pure x
        processCommit _ = Left "'commit' should be a string"

        
processLocalDependency : Table -> Either String Identifier
processLocalDependency x with (lookup "path" x)
  _ | Nothing = Left "'path' is a required field of a local dependency"
  _ | Just path = Local <$> processPath path


processDependency : Value -> Either String Identifier
processDependency (VTable x) with (lookup "type" x)
  _ | Just (VString "git") = processGitDependency x
  _ | Just (VString "local") = processLocalDependency x
  _ | Just (VString type) = Left "Invalid dependency type '\{type}'"
  _ | Just _ = Left "Dependency 'type' must be a string"
  _ | Nothing = Left "Dependencies must specify a 'type'"
processDependency _ = Left "Individual dependencies should be specified as a table"


processDependencies : Value -> Either String (List Identifier)
processDependencies (VArray x) = traverse processDependency x
processDependencies _ = Left "Dependencies should be specified as an array"


processMain : Maybe Value -> Either String (Maybe String)
processMain Nothing = pure Nothing
processMain (Just (VString x)) = pure $ Just x
processMain (Just _) = Left "'main' should be specified as a string"


processTOML : Table -> Either String Description
processTOML x with (lookup "dependencies" x)
  _ | Nothing = Left "'dependencies' is a required top level field"
  _ | Just deps = pure $ MkDescription {
          main = !(processMain $ lookup "main" x),
          dependencies = !(processDependencies deps) }


export
parseDesc : String -> Either ConfigError Description
parseDesc s = do
    tbl <- bimap TOMLError id $ parseTOML s
    bimap ValidateError id $ processTOML tbl
