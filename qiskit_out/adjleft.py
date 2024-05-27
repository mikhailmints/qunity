import numpy as np
import os
from qiskit import QuantumRegister, ClassicalRegister, QuantumCircuit, qasm3
from qiskit.circuit.library import XGate, HGate, U3Gate, Reset, SwapGate


def gate_sequence(gate0, l0, gate1, l1, label, nqubits):
    circ = QuantumCircuit(nqubits)
    circ.append(gate0, l0)
    circ.append(gate1, l1)
    return circ.to_instruction(label=label)


def build_circuit(n_qubits, out_reg, flag_reg, gate, gate_indices, decompose_names):
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

    circuit.draw("mpl", filename=(base_filename + ".png"), cregbundle=False)
    qasm3.dump(circuit, open(base_filename + ".qasm", "w"))


# Auto-generated code goes here:
build_circuit(1, [], [0], XGate(), [0], [])
