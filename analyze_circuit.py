from qiskit import transpile
from qiskit_aer import Aer
from qiskit.visualization import plot_histogram
import matplotlib
import matplotlib.pyplot as plt

matplotlib.use("svg")


def format_label(x):
    x = x[::-1]
    parts = x.split(" ")
    if len(parts) == 1:
        return parts[0]
    assert len(parts) == 2
    if parts[1] == "0" * len(parts[1]):
        if parts[0] == "":
            return "null"
        else:
            return parts[0]
    else:
        return "error"


def draw_circuit(circuit, basename, img_format="png"):
    print("Drawing circuit")
    out_filename = "diagrams/circuits/" + basename + "." + img_format
    circuit.draw(
        "mpl",
        filename=out_filename,
        fold=100,
    )
    plt.close()
    print(f"Diagram in {out_filename}")


def transpile_circuit(circuit):
    backend = Aer.get_backend("qasm_simulator")
    print("Transpiling circuit")
    circuit = transpile(
        circuit,
        basis_gates=["u3", "cx"],
        optimization_level=3,
        seed_transpiler=0,
    )
    print("Qubits:", circuit.num_qubits)
    print("Depth:", circuit.depth())
    print("Gates:", sum(circuit.count_ops().values()))
    return backend, circuit


def simulate_circuit(circuit, basename, sim_shots=10000, img_format="png"):
    reg_counts = {reg.name: len(reg) for reg in circuit.cregs}
    if len(reg_counts) == 0 or max(reg_counts.values()) == 0:
        counts = {"null": sim_shots}
    else:
        backend, circuit = transpile_circuit(circuit)
        job = backend.run(
            circuit,
            seed_simulator=0,
            shots=sim_shots,
        )
        print("Simulating circuit")
        result = job.result()
        counts_raw = result.get_counts()
        counts_list = [(format_label(x), y) for x, y in counts_raw.items()]
        n_out_qubits = len(circuit.cregs[0])
        counts = {}
        if n_out_qubits <= 6:
            counts = {
                (lambda x: "0" * (n_out_qubits - len(x)) + x)(bin(n)[2:]): 0
                for n in range(2**n_out_qubits)
            }
        for x, y in counts_list:
            if x not in counts:
                counts[x] = 0
            counts[x] += y
    out_filename = "diagrams/sim_results/" + basename + f"_sim_results.{img_format}"
    fig, ax = plt.subplots()
    plot_histogram(counts, ax=ax)
    fig.savefig(out_filename)
    plt.close(fig)
    print(f"Results in {out_filename}")
