<p align="center">
  <a href="http://www.adjoint.io"><img src="assets/uplink.png" width="250"/></a>
</p>

FCL Compiler
============

Setup
-----

TODO

Quickstart
----------

TODO

Command Line Interface
----------------------

Use the FCL executable to compile, format, lint, extract the graph of a script.

```
$ fcl --help
```

```
Usage: fcl (COMMAND | COMMAND | COMMAND | COMMAND | COMMAND)

Available options:
  -h,--help                Show this help text

Available commands:
  compile                  Compile and typecheck a script.
  format                   Format a script
  lint                     Lint a script.
  graph                    Extract graph from a script.
  transitions              Infer the transition declarations of a script.
```

### Examples:

- Typecheck and dump method types

```
fcl compile examples/minimal.s
```

