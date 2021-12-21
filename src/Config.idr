module Config

import System
import System.File.ReadWrite
import Language.JSON
import Data.List
import Util
import Data.Hashable
import public Package.Source
import public Package.Identifier

public export
record Config where
    constructor MkConfig
    pkgName : String         -- Use a more precise type for this that captures valid package names
    deps : List (Identifier MaybePinned)
    modules : List String -- Need a better type for module names maybe?
    main : Maybe String
    passthru : List (String, String)


export
emptyConfig : Identifier a -> Config
emptyConfig pkg = MkConfig pkg.name [] [] Nothing []


public export
MultiConfig : Type
MultiConfig = List Config


P : Type -> Type
P = Either String


lookup' : String -> List (String, JSON) -> P JSON
lookup' x xs = case lookup x xs of
                    Just y => pure y
                    Nothing => Left "Expected to find key \{show x} in \{show xs}"


pConfig : String -> P MultiConfig
pConfig s = case parse s of
                 Just x => parseMultiConfig x
                 Nothing => Left "Failed to parse JSON from: \{s}"
      where
            getStr : JSON -> P String
            getStr (JString s) = pure s
            getStr x = Left "Expected a string, instead got: \{show x}"

            parseName : JSON -> P String
            parseName json = do
                s <- getStr json
                if length s > 0 && all isAlpha (unpack s)
                   then pure s
                   else Left "Invalid package name \{s}"

            parseGit : JSON -> P (URL, Maybe CommitHash)
            parseGit (JObject [("url", url), ("commit", ch)]) = pure (!(getStr url), Just !(getStr ch))
            parseGit (JObject [("url", url)]) = pure (!(getStr url), Nothing)
            parseGit x = Left "Expected git dependency data, instead got \{show x}"

            parseSource : (String, JSON) -> P (Source MaybePinned)
            parseSource ("git", x) = uncurry Git <$> parseGit x
            parseSource ("local", x) = (\url => Local url) <$> getStr x
            parseSource ("legacy", x) = pure Legacy
            parseSource x = Left
                $ "Expected one of { 'git': ... }, { 'local': ... }, or { 'legacy': ... }\n"
                ++ "instead got \{show x}"

            parseDep : JSON -> P (Identifier MaybePinned)
            parseDep (JObject [("name", name), source]) = MkPkg <$> getStr name <*> parseSource source
            parseDep x = Left "Invalid dependency \{show x}"

            parseMain : JSON -> P String
            parseMain = getStr

            parseDeps : JSON -> P (List (Identifier MaybePinned))
            parseDeps (JArray deps) = sequence (map parseDep deps)
            parseDeps x = Left "Expected a list of dependencies, instead got \{show x}"

            parseMods : JSON -> P (List String)
            parseMods (JArray mods) = sequence (map getStr mods)
            parseMods x = Left "Expected a list of modules, instead got \{show x}"

            parsePassthru : JSON -> Maybe (List (String, String))
            parsePassthru (JObject p) = traverse go p
                where
                  go : (String, JSON) -> Maybe (String, String)
                  go (name, JString s) = Just (name, s)
                  go _ = Nothing
            parsePassthru _ = Nothing


            parseConfig : JSON -> P Config
            parseConfig (JObject obj) = do
                pkgName <- lookup' "name" obj >>= parseName
                deps <- lookup' "deps" obj >>= parseDeps
                mods <- lookup' "modules" obj >>= parseMods

                -- This looks strange but the crucial part is that if the "main" key
                -- is present, then we should fail if it can't parse the corresponding
                -- JSON. Equally, if the key isn't present, then that should be fine too.
                let main = parseMain <$> lookup "main" obj
                main <- sequence main

                let pthru = catMaybes . sequence $
                    (lookup "passthru" obj >>= parsePassthru)

                pure $ MkConfig pkgName deps mods main pthru
            parseConfig x = Left "Expected config object, instead got \{show x}"

            parseMultiConfig : JSON -> P MultiConfig
            parseMultiConfig (JArray configs) = traverse parseConfig configs
            parseMultiConfig x = Left "Expected an array of configs, instead got \{show x}"

export
readConfig : (dir : String) -> M MultiConfig
readConfig dir = do
    let filepath = "\{dir}/sirdi.json"

    Right contents <- readFile filepath | Left err => mErr "Can't find file \{filepath}"

    case pConfig contents of
         Right config => pure config
         Left err => mErr "Failed to parse JSON file \{filepath}. Error:\n\{err}"


export
findSubConfig : String -> MultiConfig -> M Config
findSubConfig name multi =
    case find (\cfg => cfg.pkgName == name) multi of
         Just c => pure c
         Nothing => mErr "Cannot find definition for package \{name} in config"


export
getSubConfig : Maybe String -> MultiConfig -> M Config
getSubConfig (Just name) multi = findSubConfig name multi
getSubConfig Nothing [ x ] = pure x
getSubConfig Nothing [] = mErr "Empty configuration"
getSubConfig Nothing _ = mErr "Need to specify which subconfig to build"
