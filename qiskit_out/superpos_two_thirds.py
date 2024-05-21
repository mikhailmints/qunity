import numpy as np
from qiskit import QuantumRegister, ClassicalRegister, QuantumCircuit, qasm3
from qiskit.circuit.library import XGate, HGate, U3Gate, Reset

def gate_sequence(gate0, l0, gate1, l1, label, nqubits):
    circ = QuantumCircuit(nqubits)
    circ.append(gate0, l0)
    circ.append(gate1, l1)
    return circ.to_gate(label=label)


cr = ClassicalRegister(1)
circuit = QuantumCircuit(QuantumRegister(1), cr)

circuit.append(U3Gate((2) * (np.arccos(np.sqrt((1) / (3)))), 0, 0), [0])

circuit = circuit.decompose(reps=0, gates_to_decompose=[])

circuit.measure([0], cr)

circuit.draw("mpl", filename=__file__.replace(".py", ".png"))

qasm3.dump(circuit, open(__file__.replace(".py", ".qasm"), "w"))
