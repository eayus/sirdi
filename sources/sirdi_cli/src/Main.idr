module Main

import Sap
import Sirdi
import Errors


onBuild : IO ()
onBuild = runIOE_ putStrLn $ do
    initialised <- mapErr show $ init
    ignore $ mapErr show $ recBuild Self


onRun : IO ()
onRun = runIOE_ putStrLn $ do
    initialised <- mapErr show $ init
    ignore $ mapErr show $ buildAndRun Self
    -- TODO: propogate the program's exit code


onNew : String -> IO ()
onNew = runIOE_ print . new


cmd : Command (IO ())
cmd = Cmd {
    name = "sirdi",
    desc = "Idris2 Package manager CLI",
    rhs = SubCmds [
        Cmd {
            name = "build",
            desc = "Builds the current project",
            rhs = Basic [] [] (\[], [] => onBuild)
        },
        Cmd {
            name = "run",
            desc = "Builds and runs the current project",
            rhs = Basic [] [] (\[], [] => onRun)
        },
        Cmd {
            name = "new",
            desc = "Creates a new project",
            rhs = Basic ["name" # String] [] (\[name], [] => onNew name)
        }
    ]
}


partial
main : IO ()
main = runCommand cmd
