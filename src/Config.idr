module Config

import System.File.ReadWrite
import Language.JSON
import Data.List
import Data.Maybe
import Util


public export
data Location
    = Link String
    | Local String


-- Ideally we do a proper hashing algorithm. This is obviously flawed.
-- Also fails if a location only contains non-alpha chars
export
hashLoc : Location -> String
hashLoc (Link s) = pack $ filter isAlpha $ unpack s
hashLoc (Local s) = pack $ filter isAlpha $ unpack s



public export
record Config where
    constructor MkConfig
    deps : List Location
    modules : List String -- Need a better type for module names maybe?
    main : Maybe String
    passthru : List (String, String)


parseConfig : String -> Maybe Config
parseConfig s = case parse s of
                     Just (JObject obj) => do
                        deps <- lookup "deps" obj >>= parseDeps
                        mods <- lookup "modules" obj >>= parseMods

                        -- Surely there is a better way of writing this lol
                        let main = parseMain <$> lookup "main" obj
                        let pthru = fromMaybe [] (lookup "passthru" obj >>= parsePassthru)
                        main <- sequence main

                        Just $ MkConfig deps mods main pthru
                     _ => Nothing
      where
            getStr : JSON -> Maybe String
            getStr (JString s) = Just s
            getStr _ = Nothing

            parseLoc : JSON -> Maybe Location
            parseLoc (JObject [("link", x)]) = Link <$> getStr x
            parseLoc (JObject [("local", x)]) = Local <$> getStr x
            parseLoc _ = Nothing

            parseMain : JSON -> Maybe String
            parseMain = getStr

            parseDeps : JSON -> Maybe (List Location)
            parseDeps (JArray deps) = sequence (map parseLoc deps)
            parseDeps _ = Nothing

            parseMods : JSON -> Maybe (List String)
            parseMods (JArray mods) = sequence (map getStr mods)
            parseMods _ = Nothing

            parsePassthru : JSON -> Maybe (List (String, String))
            parsePassthru (JObject p) = traverse go p
                where
                  go : (String, JSON) -> Maybe (String, String)
                  go (name, JString s) = Just (name, s)
                  go _ = Nothing
            parsePassthru _ = Nothing

export
readConfig : (dir : String) -> M Config
readConfig dir = do
    let filepath = "\{dir}/sirdi.json"

    Right contents <- mIO (readFile filepath) | Left err => mErr "Can't find file \{filepath}"

    case parseConfig contents of
         Just config => pure config
         Nothing => mErr "Failed to parse JSON."


public export
Eq Location where
    (Link x) == (Link y) = x == y
    (Local x) == (Local y) = x == y
    _ == _ = False


public export
Show Location where
    show (Link s) = "Link \{s}"
    show (Local s) = "Local \{s}"
