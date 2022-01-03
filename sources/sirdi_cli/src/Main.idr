module Main

import Sap
import Sirdi
import Core.Core


Show InitError where
    show NoConfigFile = "No config file in current directory found!"


Show RecBuildError where
    show (FetchErr ident x) = "Fetch error for something.."
    show (BuildErr (CompileError x)) = "Compilation error\n\{show x}"


myBuild : IOEither String ()
myBuild = do
    x <- mapErr show $ init
    ignore $ mapErr show $ recBuild Self


myBuild' : IO ()
myBuild' = runIOE myBuild putStrLn pure


cmd : Command (IO ())
cmd = Cmd {
    name = "sirdi",
    desc = "Idris2 Package manager CLI",
    rhs = Basic [] [] (\opts => myBuild')
}


partial
main : IO ()
main = runCommand cmd
