module Sirdi.Core.Ipkg

import Data.List
import Data.String
import System
import System.File.ReadWrite
import System.Path


public export
record Ipkg where
    constructor MkIpkg
    iname : String
    idepends : List String
    imodules : List String
    imain : Maybe String
    iexec : Maybe String


Show Ipkg where
    show p = let depends = if p.idepends == [] then Nothing else Just "depends = \{concat $ intersperse ", " p.idepends}"
                 mains = p.imain <&> \s => "main = \{s}"
                 exec = p.iexec <&> \s => "executable = \{s}"
      in """
         package \{p.iname}
         sourcedir = "src"
         modules = \{concat $ intersperse ", " p.imodules}
         """
         ++ "\n" ++ (unlines $ catMaybes [depends, mains, exec])


public export
writeIpkg : HasIO io => Ipkg -> String -> io (Either FileError ())
writeIpkg ipkg dest = writeFile dest $ show ipkg


removeIdrSuffix : String -> String
removeIdrSuffix = pack . go . unpack
    where
        go : List Char -> List Char
        go = reverse . drop 4 . reverse


replaceSlash : String -> String
replaceSlash = pack . go . unpack
    where
        replace : Char -> Char
        replace '/' = '.'
        replace c   = c

        go : List Char -> List Char
        go = map replace


export
findModules : HasIO io => Path -> io (List String)
findModules path = do
    (findOutput, 0) <- run "find \{show path} -type f -name \"*.idr\" -exec realpath --relative-to \{show path} {} \\\;"
        | (output, _) => die "Failed to execute 'find' to find modules: \{show output}"

    let modulePaths = lines findOutput

    let modules = map (replaceSlash . removeIdrSuffix) modulePaths

    pure modules
