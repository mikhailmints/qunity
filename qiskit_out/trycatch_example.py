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
build_circuit(6, [1], [3], gate_sequence(gate_sequence(gate_sequence(gate_sequence(gate_sequence(gate_sequence(HGate(), [2], XGate(), [3], "_0"), [2, 3], gate_sequence(gate_sequence(XGate(), [0], gate_sequence(SwapGate(), [3, 1], gate_sequence(XGate().control(num_ctrl_qubits=1, ctrl_state="0"), [2, 0], SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [2, 3, 1], "_1"), [0, 1, 2, 3], "_2"), [0, 1, 2, 3], "_3"), [0, 1, 2, 3], gate_sequence(Reset(), [1], Reset(), [2], "_4"), [1, 2], "_5"), [0, 1, 2, 3], "_6"), [0, 1, 2, 3], gate_sequence(gate_sequence(XGate(), [4], HGate(), [4], "_7"), [4], Reset(), [2], "_8"), [2, 4], "_9"), [0, 1, 2, 3, 4], gate_sequence(SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [0, 1, 3], SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [0, 1, 4], "_10"), [0, 1, 3, 4], "_11"), [0, 1, 2, 3, 4], gate_sequence(gate_sequence(gate_sequence(gate_sequence(SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [0, 4, 5], SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [0, 1, 4], "_12"), [0, 1, 4, 5], SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [0, 3, 1], "_13"), [0, 1, 3, 4, 5], XGate().control(num_ctrl_qubits=1, ctrl_state="1"), [3, 0], "_14"), [0, 1, 3, 4, 5], gate_sequence(SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [3, 0, 1], gate_sequence(SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [3, 1, 4], SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [3, 4, 5], "_15"), [1, 3, 4, 5], "_16"), [0, 1, 3, 4, 5], "_17"), [0, 1, 3, 4, 5], "_18"), [0, 1, 2, 3, 4, 5], gate_sequence(Reset(), [0], gate_sequence(Reset(), [4], Reset(), [5], "_19"), [4, 5], "_20"), [0, 4, 5], "_21"), [0, 1, 2, 3, 4, 5], ["_0", "_1", "_10", "_11", "_12", "_13", "_14", "_15", "_16", "_17", "_18", "_19", "_2", "_20", "_21", "_3", "_4", "_5", "_6", "_7", "_8", "_9"])
