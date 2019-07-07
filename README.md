<p align="center">
<a href="https://www.adjoint.io">
  <img width="250" src="./.assets/fcl.png" alt="Adjoint Logo" />
</a>
</p>

FCL
===

FCL is the Adjoint financial modeling language. Adjoint provides domain modeling
tool for building safe, secure, and transparent workflows for financial products
and digital process automation.

Adjoint offers solutions to allow enterprises to build auditable and
synchronized business processes that scale across large vendor and consortium
networks, enabling the next generation of international commerce.

* [Adjoint Inc.](https://www.adjoint.io)
* [FCL Documentation](https://www.adjoint.io/docs/workflows.html)
* [Uplink Documentation](https://www.adjoint.io/docs/index.html)

Quickstart
----------

To install FCL from source install the standard
[stack](https://docs.haskellstack.org/en/stable/README/) tooling.

```
$ git clone git@github.com:adjoint-io/fcl.git
$ stack install
```

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

We provide several examples programs which model financial workflows, data
reconciliation, pre and post trade attestation, and structured product lifecycle
management:

* [Hello World 1](examples/minimal.s)
* [Hello World 2](examples/simple.s)
* [Swap](examples/swap.s)
* [Zero Coupon Bond](examples/zcb.s)
* [Novations](examples/novation.s)
* [Gas Forward](examples/gas-forward.s)
* [Structured Product](examples/product.s)
* [Concurrent Workflows](examples/concurrent.s)
* [Amendments](examples/amendment.s)
* [Graph Example](examples/graph.s)
* [Loan](examples/loan_contract.s)
* [Private Storage](examples/locals.s)
* [Notary Attestation](examples/notary.s)
* [Role Access Controls & Datetime Preconditions](examples/preconditions.s)
* [Noop Contract](examples/single.s)

Example compiler usage:

- Typecheck and print method names and type signatures

```
$ fcl compile examples/minimal.s
```

- Create a visual representation of an FCL workflow

```
$ fcl graph examples/concurrent.s
```

This will output the corresponding `dot` and `svg` files.

<p>
  <img src="assets/concurrent.svg" width="250"/>
</p>


Make sure you have `graphviz` installed before running `fcl graph`. In Ubuntu:
```
$ sudo apt install graphviz
```

### License

Copyright (c) 2016-2019 Adjoint Inc.

FCL is released under an Apache License.
