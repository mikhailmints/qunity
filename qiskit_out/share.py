import numpy as np
import os
from qiskit import QuantumRegister, ClassicalRegister, QuantumCircuit, qasm3, transpile
from qiskit.circuit.library import XGate, HGate, U3Gate, Reset, SwapGate
from qiskit_aer import Aer
from qiskit.visualization import plot_histogram


def gate_sequence(gate0, l0, gate1, l1, label):
    l = sorted(list(set(l0 + l1)))
    nqubits = len(l)
    rewiring = {i: j for i, j in zip(l, range(nqubits))}
    circ = QuantumCircuit(nqubits)
    circ.append(gate0, [rewiring[i] for i in l0])
    circ.append(gate1, [rewiring[i] for i in l1])
    circ.name = label
    try:
        return circ.to_gate()
    except:
        return circ.to_instruction()


def build_circuit(n_qubits, out_reg, flag_reg, gate, gate_indices, decompose_names):
    print("Building circuit")
    qr = QuantumRegister(n_qubits, "q")
    cr_out = ClassicalRegister(len(out_reg), "out")
    cr_error = ClassicalRegister(len(flag_reg), "err")
    circuit = QuantumCircuit(qr, cr_out, cr_error)

    if gate is not None:
        circuit.append(gate, gate_indices)
        circuit = circuit.decompose(
            reps=len(decompose_names), gates_to_decompose=decompose_names
        )

    circuit.measure(out_reg, cr_out)
    circuit.measure(flag_reg, cr_error)

    base_filename = os.path.splitext(__file__)[0]

    print("Drawing circuit")
    circuit.draw("mpl", filename=(base_filename + ".png"), cregbundle=False, fold=-1)

    print("Outputting QASM")
    qasm3.dump(circuit, open(base_filename + ".qasm", "w"))

    print("Simulating circuit")
    simulator = Aer.get_backend("qasm_simulator")
    counts = (
        simulator.run(transpile(circuit, simulator), shots=10000).result().get_counts()
    )
    counts = {x[::-1]: y for x, y in counts.items()}
    plot_histogram(counts, filename=(base_filename + "_sim_results.png"))


# Auto-generated code goes here:
build_circuit(5, [0, 1, 3, 2, 4], [], gate_sequence(HGate(), [0], gate_sequence(XGate().control(num_ctrl_qubits=1, ctrl_state="1"), [0, 1], gate_sequence(gate_sequence(XGate().control(num_ctrl_qubits=1, ctrl_state="1"), [1, 2], gate_sequence(gate_sequence(XGate().control(num_ctrl_qubits=1, ctrl_state="1"), [1, 3], QuantumCircuit(1, global_phase=(np.pi) / (2), name="gphase_0").to_gate(), [1], "_0"), [1, 3], HGate(), [3], "_1"), [1, 3], "_2"), [1, 2, 3], gate_sequence(gate_sequence(XGate().control(num_ctrl_qubits=1, ctrl_state="1"), [2, 4], XGate(), [2], "_3"), [2, 4], QuantumCircuit(1, global_phase=(np.pi) / (4), name="gphase_4").to_gate(), [4], "_4"), [2, 4], "_5"), [1, 2, 3, 4], "_6"), [0, 1, 2, 3, 4], "_7"), [0, 1, 2, 3, 4], ["_0", "_1", "_2", "_3", "_4", "_5", "_6", "_7", "gphase_0", "gphase_4"])
