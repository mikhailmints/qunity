This is the artifact for the paper "Compositional Quantum Control Flow with Efficient Compilation in Qunity". This artifact contains the code for the Qunity compiler and interpreter, as well as examples of Qunity code and scripts to run tests and benchmarks.

# File Structure

The main parts of the code for the Qunity parser, preprocessor, typechecker, compiler, and interpreter are found in `lib`. The `bin` directory contains driver files for running the compiler and interpreter. The `examples` directory contains many examples of Qunity code, which are used for testing. The `qunitylib` directory contains the Qunity standard library, `stdlib.qunity`. The `test` directory contains files used for testing. The Bash and Python scripts in the outer directory can be used to more conveniently run the code and analyze, draw, and simulate the resulting circuits with Qiskit.

# Instructions for Reproducing Table 3

To reproduce the benchmark results from Table 3, you can simply run:
```bash
./benchmarks
```
This script should take approximately 10 minutes to run. It will output the qubit and gate counts for the unoptimized and optimized Qunity compiler, as well as the reference Qiskit implementation when available. Note that the optimized Qunity results for Grover with list sum oracle should be 11 qubits and 13,470 gates instead of the 19 qubits and 215,104 listed in Table 3 - this was due to the fact that the implementation used in the original evaluation had a trivially fixable issue that prevented some optimization from occurring. This will be corrected in the revised version of the paper. Also, note that the evaluation of the unoptimized Grover's algorithm example should time out as the generated circuit is too large - the number of qubits can be confirmed by looking at the generated `qasm_out/grover_unoptimized.qasm` file. The order finding example will not have a Qiskit implementation and is not supported by the unoptimized compiler, and the list sum oracle example is also not supported by the unoptimized compiler.

# Running the Tests

To run the tests, use the following:
```bash
./run-tests
```
This should take approximately 1 minute to run. This script runs some basic unit tests, and also performs the differential unit testing mentioned in Section 7. The results of this can be seen in the tests labeled `compile_file_correctness`. These run the Qunity compiler on the listed files and simulate the resulting circuit, comparing the result to the output of the Qunity interpreter. For files that output circuits too large to simulate classically, the compiler is run without comparing the results, just to check that no exceptions are raised in the compilation process - these tests are labeled `compile_file_no_error`. All tests should say "passed".

# Running the Qunity Interpreter

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
You can enter Qunity expressions or create definitions in the interpreter. Inputs should be terminated by a double semicolon: `;;`. Here is an example of a REPL session:
```
./qunity-interact
<qunity> $0;;
Expression type: Bit
Isometry: true
Pure semantics:
|0>

Mixed semantics:
|0><0|

Possible measurement outcomes:
Probability 1.000000: $0

<qunity> def @share : Bit -> Bit * Bit := lambda x -> (x, x) end;;
<qunity> @share($plus);;
Expression type: Bit * Bit
Isometry: true
Pure semantics:
 0.707+0.000i 
 0.000+0.000i 
 0.000+0.000i 
 0.707+0.000i 

Mixed semantics:
 0.500+0.000i  0.000+0.000i  0.000+0.000i  0.500+0.000i 
 0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i 
 0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i 
 0.500+0.000i  0.000+0.000i  0.000+0.000i  0.500+0.000i 

Possible measurement outcomes:
Probability 0.500000: ($0, $0)
Probability 0.500000: ($1, $1)

<qunity> $ListEmpty{2, Bit} |> @list_append_const{2, Bit, $0} |> @list_append_const{2, Bit, $plus};;
Expression type: List{2, Bit}
Isometry: false
Pure semantics:
None

Mixed semantics:
 0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i 
 0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i 
 0.000+0.000i  0.000+0.000i  0.500+0.000i  0.500+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i 
 0.000+0.000i  0.000+0.000i  0.500+0.000i  0.500+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i 
 0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i 
 0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i 
 0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i  0.000+0.000i 

Possible measurement outcomes:
Probability 0.500000: @ListCons{2, Bit}(($0, @ListCons{1, Bit}(($0, $ListEmpty{0, Bit}))))
Probability 0.500000: @ListCons{2, Bit}(($0, @ListCons{1, Bit}(($1, $ListEmpty{0, Bit}))))
```
The interpreter will output the type of the provided expression, whether or not it is considered an isometry by the typechecker, the computed semantics, and the possible measurement outcomes with their probabilities. The operator/superoperator semantics are represented in matrix form, with pure semantics written in bra-ket notation if the matrix is internally represented as sparse.

# Running the Qunity Compiler

To compile a single Qunity file into OpenQASM 3:
```bash
./qunity-compile <in_filename> [-o <out_filename>] [--analyze]
```
If no output filename is specified, by default it goes in the `qasm_out` directory. If `--analyze` is used, a circuit diagram will be generated, and the circuit will be simulated using Qiskit.

For instance:
```
./qunity-compile examples/grover.qunity --analyze
Starting compiler                     
Preprocessing
Typechecking
Compiling to QASM
Postprocessing
...
Outputting to file
Compilation done
qasm_out/grover.qasm

Starting analysis script
Importing libraries
Analyzing file qasm_out/grover.qasm
Loading circuit
Drawing circuit
Diagram in diagrams/circuits/grover.svg
Transpiling circuit
Qubits: 7
Depth: 614
Gates: 760
Simulating circuit
Results in diagrams/sim_results/grover_sim_results.svg
```
The corresponding files in the `diagrams` directory should then display the generated circuit diagram and the simulation results histogram.

To compile all the example Qunity programs:
```bash
./compile-all-examples [--analyze]
```
Note that while running `./compile-all-examples` should only take about 20 seconds, running the analysis script to generate all diagrams and perform all simulations can take approximately 20 minutes.

[^1]:
    To avoid some input/prompt issues, it is recommended to add the following to your `~/.inputrc`:
    ```inputrc
    $include /etc/inputrc
    # To prevent prompt from disappearing:
    set enable-bracketed-paste off
    # Optional - to prevent parentheses blinking:
    set blink-matching-paren off
    ```
