This example involves a library, `examp`, and a main application, `nia`.

`nia/sirdi.json` defines a local depedency on `examp`. `nia/src/Nia.idr` defines the main function, which calls a function in the `examp` library.

To build this project, `cd` into the `nia` directory and run
`sirdi build`

Sirdi will generate a hidden `.build` folder where it collates the depdencies and builds everything.

After building, run the program using
`sirdi run`
