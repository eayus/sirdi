# sirdi
A simple package manager for Idris 2.

## Usage

### Commands

`sirdi new <project_name>` - Creates a template Idris2 project.

`sirdi build` - Builds the project in the current directory.

`sirdi run` - Runs the executable for the project in the current directory.

`sirdi clean` - Removes all build files for the project in the current directory.

`sirdi dep-tree` - Prints a dependency tree for the project in the current directory.

### Configuration

Configuration takes places in the `sirdi.json` file. Check out the `examples/` directory for example uses.


## Design

The main design goal of `sirdi` is simplicity. It is intended as a convenient wrapper around the existing `ipkg` system and `git`.
