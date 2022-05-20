module Sirdi.Core.Config

import Sirdi.Core

import Data.List.Elem
import Language.TOML
import Language.TOML.Processing


public export
record Config where
    constructor MkConfig
    depends : List Identifier
    main    : Maybe String


public export
data ValidationError
    = InvalidPath String


public export
data ConfigError
    = TOMLError TOML.Error
    | FormatError TableError
    | ValidateError ValidationError


LocalOpts : TableTy
LocalOpts = MkFieldTy { name = "path", optional = False, ty = TString } `And` Emp


DepOpts : TableTy
DepOpts = MkFieldTy { name = "type", optional = False, ty = TEnum ["local"] }
   `Ext` \case
       ("local" ** Here) => LocalOpts
       (x ** There p) impossible


ConfigOpts : TableTy
ConfigOpts = MkFieldTy { name = "depends", optional = False, ty = TArray (TTable DepOpts) }
       `And` MkFieldTy { name = "main", optional = True, ty = TString }
       `And` Emp


toLocal : TableOf LocalOpts -> Either ValidationError Identifier
toLocal [path] =
    case tryParse path of
         Just path' => pure $ Local path'
         Nothing    => Left $ InvalidPath path


toIdent : ValueOf (TTable DepOpts) -> Either ValidationError Identifier
toIdent ((MkDPair "local" Here) :: opts) = toLocal opts
toIdent _ = assert_total $ idris_crash "Impossible case toIdent"


toConfig : TableOf ConfigOpts -> Either ValidationError Config
toConfig [depends, main] = do
    depends' <- traverse toIdent depends
    pure $ MkConfig {
        main = main,
        depends = depends'
    }


export
parseConfig : String -> Either ConfigError Config
parseConfig s = do
    toml <- mapFst TOMLError $ parseTOML s
    tbl <- mapFst FormatError $ processTable ConfigOpts toml
    mapFst ValidateError $ toConfig tbl
