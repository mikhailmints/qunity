print("Starting analysis script")
print("Importing libraries")

import sys
import os
from qiskit import qasm3, transpile
from qiskit_aer import Aer
from qiskit.visualization import plot_histogram

qasm_filename = sys.argv[1]

print("Loading circuit")
basename = os.path.splitext(os.path.basename(qasm_filename))[0]

circuit = qasm3.load(qasm_filename)

print("Drawing circuit")
circuit.draw(
    "mpl",
    filename=("diagrams/circuits/" + basename + ".png"),
    cregbundle=False,
    fold=-1,
)

print("Simulating circuit")
simulator = Aer.get_backend("qasm_simulator")
counts = simulator.run(transpile(circuit, simulator), shots=10000).result().get_counts()
counts = {x[::-1]: y for x, y in counts.items()}
plot_histogram(
    counts, filename=("diagrams/sim_results/" + basename + "_sim_results.png")
)
