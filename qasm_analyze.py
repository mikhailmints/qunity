print("Starting analysis script")
print("Importing libraries")

import sys
import os
import timeout_decorator
from qiskit import qasm3, transpile
from qiskit_aer import Aer
from qiskit.visualization import plot_histogram
from qiskit.circuit import IfElseOp


RED = "\033[0;31m"
GREEN = "\033[0;32m"
YELLOW = "\033[0;33m"
NC = "\033[0m"

DRAW_TIMEOUT = 10
SIMULATE_TIMEOUT = 60
SIM_SHOTS = 10000


def format_label(x):
    x = x[::-1]
    parts = x.split(" ")
    assert len(parts) == 2
    if parts[1] == "0" * len(parts[1]):
        if parts[0] == "":
            return "null"
        else:
            return parts[0]
    else:
        return "error"


@timeout_decorator.timeout(DRAW_TIMEOUT, use_signals=False)
def draw_circuit(circuit, basename):
    print("Drawing circuit")
    out_filename = "diagrams/circuits/" + basename + ".png"
    circuit.draw(
        "mpl",
        filename=out_filename,
        cregbundle=False,
        fold=-1,
    )
    print(f"Diagram in {out_filename}")


@timeout_decorator.timeout(SIMULATE_TIMEOUT, use_signals=False)
def simulate_circuit(circuit, basename):
    print("Simulating circuit")
    reg_counts = {reg.name: len(reg) for reg in circuit.cregs}
    if len(reg_counts) == 0 or max(reg_counts.values()) == 0:
        print("Circuit has no measurements")
        counts = {"null": SIM_SHOTS}
    else:
        backend = Aer.get_backend("qasm_simulator")
        if "if_else" not in backend.target:
            backend.target.add_instruction(IfElseOp, name="if_else")
        counts = (
            backend.run(
                transpile(circuit, backend, seed_transpiler=0),
                seed_simulator=0,
                shots=SIM_SHOTS,
            )
            .result()
            .get_counts()
        )
        counts_list = [(format_label(x), y) for x, y in counts.items()]
        counts = dict()
        for x, y in counts_list:
            if x not in counts:
                counts[x] = 0
            counts[x] += y
    out_filename = "diagrams/sim_results/" + basename + "_sim_results.png"
    plot_histogram(counts, filename=out_filename)
    print(f"Results in {out_filename}")


def analyze_file(qasm_filename):
    print(f"Analyzing file {qasm_filename}")
    basename = os.path.splitext(os.path.basename(qasm_filename))[0]

    print("Loading circuit")
    circuit = qasm3.load(qasm_filename)

    draw_circuit(circuit, basename)

    simulate_circuit(circuit, basename)


path = sys.argv[1]

if os.path.isdir(path):
    print()
    for filename in os.listdir(path):
        try:
            analyze_file(os.path.join(path, filename))
        except Exception as e:
            print(f"{RED}Error: {e}{NC}")
        print()
else:
    try:
        analyze_file(path)
    except Exception as e:
        print(f"{RED}Error: {e}{NC}")
