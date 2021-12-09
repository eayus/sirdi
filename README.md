# sirdi
A simple package manager for Idris 2.

## Usage

### Commands

| Sub Command | Effect |
| --- | --- |
| `new <project_name>` | Creates a template Idris2 project. |
| `build` | Builds the project and its dependencies. |
| `run` | Runs the executable (if a `main` has been specified in the config). |
| `clean` | Removes all build files. |
| `dep-tree` | Prints a dependency tree. |

### Configuration

Configuration takes places in the `sirdi.json` file. Check out the `examples/` directory for example uses.


## Design

The main design goal of `sirdi` is simplicity. It is intended as a convenient wrapper around the existing `ipkg` system and `git`.
