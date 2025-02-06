# Qunity

[![ci](https://github.com/mikhailmints/qunity/actions/workflows/ci.yml/badge.svg?event=push)](https://github.com/mikhailmints/qunity/actions/workflows/ci.yml)
[![docs](https://img.shields.io/badge/docs-odoc-blue)](https://mikhailmints.github.io/qunity/qunity/Qunity/index.html)

This repository contains the interpreter and compiler for the Qunity quantum programming language, based on the paper by Voichick et al. (https://arxiv.org/abs/2204.12384), and the typechecker implementation in Coq (https://gitlab.umiacs.umd.edu/finn/qunity).


## Running the Code

To run the tests:
```bash
./run-tests
```

To run a single program using the simulator:
```bash
./qunity-run <filename>
```

To run all the example Qunity programs located in the examples folder:
```bash
./run-all-examples
```

To start an interactive Qunity REPL[^1]:
```bash
./qunity-interact
```

To compile a single Qunity file into OpenQASM 3:
```bash
./qunity-compile <in_filename> [-o <out_filename>] [--nobuild] [--annotate] [--analyze] [--debug] [--noopt]
```
If no output filename is specified, by default it goes in the `qasm_out` directory. If `--nobuild` is used, `dune build` is not run before compiling. If `--annotate` is used, the output file will have comments that indicate the roles of the qubits in each of the gates in the low-level circuit. If `--analyze` is used, a circuit diagram will be generated, and the circuit will be simulated using Qiskit. If `--debug` is used, verbose debugging information will be printed out during compilation. If `--noopt` is used, the post-processing optimization step is skipped.

To compile all the example Qunity programs:
```bash
./compile-all-examples [--annotate] [--analyze] [--debug] [--noopt]
```

## Acknowledgements

This project is done as part of Caltech's SURF program, supervised by Robert Rand at UChicago. It is supported by the Samuel P. and Frances Krown SURF Fellowship.

[^1]:
    To avoid some input/prompt issues, it is recommended to add the following to your `~/.inputrc`:
    ```inputrc
    $include /etc/inputrc
    # To prevent prompt from disappearing:
    set enable-bracketed-paste off
    # Optional - to prevent parentheses blinking:
    set blink-matching-paren off
    ```
