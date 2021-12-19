module Nia

import Examp
import Language.JSON

main : IO ()
main = do
    example
    print $ parse "I'm invalid JSON!"
    putStrLn "Hello I'm Nia!"
