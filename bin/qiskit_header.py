import numpy as np
from qiskit import QuantumRegister, ClassicalRegister, QuantumCircuit, qasm3
from qiskit.circuit.library import XGate, HGate, U3Gate, Reset

def gate_sequence(gate0, l0, gate1, l1, label, nqubits):
    circ = QuantumCircuit(nqubits)
    circ.append(gate0, l0)
    circ.append(gate1, l1)
    return circ.to_gate(label=label)
