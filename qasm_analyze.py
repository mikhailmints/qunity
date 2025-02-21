print("Starting analysis script")
print("Importing libraries")

import sys
import os
import timeout_decorator
from qiskit import qasm3, transpile
from qiskit_aer import Aer
from qiskit.visualization import plot_histogram
import matplotlib
matplotlib.use("svg")
import matplotlib.pyplot as plt

RED = "\033[0;31m"
GREEN = "\033[0;32m"
YELLOW = "\033[0;33m"
NC = "\033[0m"

LOAD_TIMEOUT = 40
DRAW_TIMEOUT = 40
SIMULATE_TIMEOUT = 300
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
    out_filename = "diagrams/circuits/" + basename + ".svg"
    try:
        circuit.draw(
            "mpl",
            filename=out_filename,
            fold=100,
        )
    except Exception:
        circuit.draw("mpl", scale=0.2, filename=out_filename)
    print(f"Diagram in {out_filename}")


@timeout_decorator.timeout(SIMULATE_TIMEOUT, use_signals=False)
def simulate_circuit(circuit, basename):
    reg_counts = {reg.name: len(reg) for reg in circuit.cregs}
    if len(reg_counts) == 0 or max(reg_counts.values()) == 0:
        counts = {"null": SIM_SHOTS}
    else:
        backend = Aer.get_backend("qasm_simulator")
        print("Transpiling circuit")
        circuit = transpile(
            circuit,
            basis_gates=(backend.operation_names + ["if_else"]),
            optimization_level=3,
            seed_transpiler=0,
        )
        print("Depth:", circuit.depth())
        print("Gates:", sum(circuit.count_ops().values()))
        job = backend.run(
            circuit,
            seed_simulator=0,
            shots=SIM_SHOTS,
        )
        print("Simulating circuit")
        result = job.result()
        counts_raw = result.get_counts()
        counts_list = [(format_label(x), y) for x, y in counts_raw.items()]
        counts = dict()
        for x, y in counts_list:
            if x not in counts:
                counts[x] = 0
            counts[x] += y
    out_filename = "diagrams/sim_results/" + basename + "_sim_results.svg"
    fig, ax = plt.subplots()
    plot_histogram(counts, ax=ax)
    fig.savefig(out_filename)
    plt.close(fig)
    print(f"Results in {out_filename}")


def analyze_file(qasm_filename):
    print(f"Analyzing file {qasm_filename}")
    basename = os.path.splitext(os.path.basename(qasm_filename))[0]

    print("Loading circuit")
    circuit = timeout_decorator.timeout(LOAD_TIMEOUT)(qasm3.load)(qasm_filename)

    draw_circuit(circuit, basename)

    simulate_circuit(circuit, basename)


path = sys.argv[1]

if os.path.isdir(path):
    print()
    for filename in os.listdir(path):
        if filename.endswith(".qasm"):
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
