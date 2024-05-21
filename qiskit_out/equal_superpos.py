import numpy as np
from qiskit import QuantumRegister, ClassicalRegister, QuantumCircuit, qasm3
from qiskit.circuit.library import XGate, HGate, U3Gate, Reset

def gate_sequence(gate0, l0, gate1, l1, label, nqubits):
    circ = QuantumCircuit(nqubits)
    circ.append(gate0, l0)
    circ.append(gate1, l1)
    return circ.to_gate(label=label)


cr = ClassicalRegister(6)
circuit = QuantumCircuit(QuantumRegister(6), cr)

circuit.append(gate_sequence(HGate(), [0], gate_sequence(HGate(), [1], gate_sequence(HGate(), [2], gate_sequence(HGate(), [3], gate_sequence(HGate(), [4], HGate(), [5], "_0", 6), [0, 1, 2, 3, 4, 5], "_1", 6), [0, 1, 2, 3, 4, 5], "_2", 6), [0, 1, 2, 3, 4, 5], "_3", 6), [0, 1, 2, 3, 4, 5], "_4", 6), [0, 1, 2, 3, 4, 5])

circuit = circuit.decompose(reps=5, gates_to_decompose=["_0", "_1", "_2", "_3", "_4"])

circuit.measure([0, 1, 2, 3, 4, 5], cr)

circuit.draw("mpl", filename=__file__.replace(".py", ".png"))

qasm3.dump(circuit, open(__file__.replace(".py", ".qasm"), "w"))
