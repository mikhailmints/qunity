This is a prototype simulator and compiler for the Qunity quantum programming language, based on https://arxiv.org/abs/2204.12384.

To run the tests:
```bash
dune test
```

To run a single program using the simulator, for example:
```bash
./qunity-run <filename>
```

To run all the example Qunity programs located in the examples folder:
```bash
./run-all-examples
```

To start an interactive Qunity REPL:
```bash
./qunity-interact
```

To avoid some input/prompt issues, it is recommended to add the following to your `~/.inputrc`:
```inputrc
$include /etc/inputrc
# To prevent prompt from disappearing:
set enable-bracketed-paste off
# Optional - to prevent parentheses blinking:
set blink-matching-paren off
```

To compile a single Qunity file into Qiskit and OpenQASM 3, for example:
```bash
./qunity-compile <in_filename> [-o <out_filename>] [--nobuild] [--annotate] [--analyze]
```
If no output filename is specified, by default it goes in the `qasm_out` directory. If `--nobuild` is used, `dune build` is not run before compiling. If `--annotate` is used, the output file will have comments that indicate the roles of the qubits in each of the gates in the low-level circuit. If `--analyze` is used, a circuit diagram will be generated, and the circuit will be simulated using Qiskit.

To compile all the example Qunity programs:
```bash
./compile-all-examples [--annotate] [--analyze]
```
