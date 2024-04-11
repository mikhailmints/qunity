This is a prototype simulator/interpreter for the Qunity quantum programming language, based on https://arxiv.org/abs/2204.12384.

To run the tests:
```bash
dune test
```

To run the example Qunity programs located in the examples folder:
```bash
./run-all-examples
```

To run a single program, for example:
```bash
./qunity-run examples/qft.qunity
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
# Optional: to prevent parentheses blinking
set blink-matching-paren off
```
