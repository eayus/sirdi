# sirdi
A simple package manager for Idris 2.

## Contributing

See the issues for work that needs to be done. PRs that fix existing issues should be a priority.

If you want to make some large changes, then it might be worth opening an issue or draft PR. This will ensure that multiple people don't duplicate their work by independently working to solve the same problem.

## Usage

### Commands

| Sub Command | Effect |
| --- | --- |
| `new <project_name>` | Creates a template Idris2 project. |
| `build` | Builds the project and its dependencies. |
| `run` | Runs the executable (if a `main` has been specified in the config). |
| `clean` | Removes all build files. |
| `dep-tree` | Prints a dependency tree. |
| `prune` | Deletes build files for old dependencies that are no longer used. |

### Configuration

Configuration takes places in the `sirdi.json` file. Check out the `examples/` directory for example uses.


## Design

The main design goal of `sirdi` is simplicity. It is intended as a convenient wrapper around the existing `ipkg` system and `git`.
