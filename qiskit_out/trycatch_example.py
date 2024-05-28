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
    circuit.draw("mpl", filename=(base_filename + ".png"), cregbundle=False)

    print("Outputting QASM")
    qasm3.dump(circuit, open(base_filename + ".qasm", "w"))

    print("Simulating circuit")
    simulator = Aer.get_backend("qasm_simulator")
    counts = simulator.run(transpile(circuit, simulator)).result().get_counts()
    counts = {x[::-1]: y for x, y in counts.items()}
    plot_histogram(counts, filename=(base_filename + "_sim_results.png"))


# Auto-generated code goes here:
build_circuit(10, [3], [9, 2], gate_sequence(HGate(), [0], gate_sequence(gate_sequence(gate_sequence(gate_sequence(XGate(), [1], gate_sequence(XGate(), [2], gate_sequence(gate_sequence(XGate().control(num_ctrl_qubits=1, ctrl_state="0"), [0, 2], SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [0, 1, 3], "_0"), [0, 1, 2, 3], gate_sequence(Reset(), [0], Reset(), [3], "_1"), [0, 3], "_2"), [0, 1, 2, 3], "_3"), [0, 1, 2, 3], "_4"), [0, 1, 2, 3], gate_sequence(gate_sequence(XGate(), [0], HGate(), [0], "_5"), [0], gate_sequence(XGate(), [3], Reset(), [4], "_6"), [3, 4], "_7"), [0, 3, 4], "_8"), [0, 1, 2, 3, 4], gate_sequence(gate_sequence(gate_sequence(gate_sequence(gate_sequence(gate_sequence(SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [2, 8, 9], SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [2, 0, 8], "_9"), [0, 2, 8, 9], SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [2, 3, 0], "_10"), [0, 2, 3, 8, 9], SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [2, 1, 3], "_11"), [0, 1, 2, 3, 8, 9], SwapGate(), [2, 1], "_12"), [0, 1, 2, 3, 8, 9], XGate().control(num_ctrl_qubits=1, ctrl_state="1"), [2, 1], "_13"), [0, 1, 2, 3, 8, 9], gate_sequence(SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [2, 1, 3], gate_sequence(SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [2, 3, 0], gate_sequence(SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [2, 0, 8], SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [2, 8, 9], "_14"), [0, 2, 8, 9], "_15"), [0, 2, 3, 8, 9], "_16"), [0, 1, 2, 3, 8, 9], "_17"), [0, 1, 2, 3, 8, 9], "_18"), [0, 1, 2, 3, 4, 8, 9], gate_sequence(Reset(), [1], gate_sequence(Reset(), [0], Reset(), [8], "_19"), [0, 8], "_20"), [0, 1, 8], "_21"), [0, 1, 2, 3, 4, 8, 9], "_22"), [0, 1, 2, 3, 4, 8, 9], ["_0", "_1", "_10", "_11", "_12", "_13", "_14", "_15", "_16", "_17", "_18", "_19", "_2", "_20", "_21", "_22", "_3", "_4", "_5", "_6", "_7", "_8", "_9"])
