print("Starting analysis script")
print("Importing libraries")

import sys
import os
import timeout_decorator
from qiskit import qasm3, transpile
from qiskit_aer import Aer
from qiskit.visualization import plot_histogram
from qiskit.providers.basic_provider import BasicProvider
from qiskit.circuit import IfElseOp


DRAW_TIMEOUT = 10
SIMULATE_TIMEOUT = 10


def format_label(x):
    x = x[::-1]
    parts = x.split(" ")
    if len(parts) == 1:
        return parts[0]
    else:
        if parts[1] == "0" * len(parts[1]):
            return parts[0]
        else:
            return x


@timeout_decorator.timeout(DRAW_TIMEOUT, use_signals=False)
def draw_circuit(circuit, basename):
    print("Drawing circuit")
    circuit.draw(
        "mpl",
        filename=("diagrams/circuits/" + basename + ".png"),
        cregbundle=False,
        fold=-1,
    )


@timeout_decorator.timeout(SIMULATE_TIMEOUT, use_signals=False)
def simulate_circuit(circuit, basename):
    print("Simulating circuit")
    backend = Aer.get_backend("qasm_simulator")
    if "if_else" not in backend.target:
        backend.target.add_instruction(IfElseOp, name="if_else")
    counts = (
        backend.run(
            transpile(circuit, backend, seed_transpiler=0),
            seed_simulator=0,
            shots=10000,
        )
        .result()
        .get_counts()
    )
    counts = {format_label(x): y for x, y in counts.items()}
    plot_histogram(
        counts, filename=("diagrams/sim_results/" + basename + "_sim_results.png")
    )


qasm_filename = sys.argv[1]
basename = os.path.splitext(os.path.basename(qasm_filename))[0]

print("Loading circuit")
circuit = qasm3.load(qasm_filename)

draw_circuit(circuit, basename)

simulate_circuit(circuit, basename)
