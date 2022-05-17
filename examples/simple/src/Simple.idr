module Simple


export
sayHello : IO ()
sayHello = putStrLn "Hello from Sirdi 2!"


main : IO ()
main = sayHello
