This is a prototype simulator and compiler for the Qunity quantum programming language, based on https://arxiv.org/abs/2204.12384.

To run the tests:
```bash
dune test
```

To run a single program using the simulator, for example:
```bash
./qunity-run examples/qft.qunity
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
./qunity-compile examples/qft.qunity qiskit_out/qft.py
```
will create the files qiskit_out/qft.py, qiskit_out/qft.qasm, and a circuit diagram qiskit_out/qft.png

To compile all the example Qunity programs:
```bash
./compile-all-examples
```
