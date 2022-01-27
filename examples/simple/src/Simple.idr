module Simple


export
sayHello : IO ()
sayHello = putStrLn "Hello from Sirdi!"


main : IO ()
main = sayHello
