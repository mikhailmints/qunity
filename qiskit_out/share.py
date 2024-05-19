import numpy as np
from qiskit import QuantumRegister, ClassicalRegister, QuantumCircuit, qasm3
from qiskit.circuit.library import XGate, HGate, U3Gate

def gate_sequence(gate0, l0, gate1, l1, label, nqubits):
    circ = QuantumCircuit(nqubits)
    circ.append(gate0, l0)
    circ.append(gate1, l1)
    return circ.to_gate(label=label)


cr = ClassicalRegister(5)
circuit = QuantumCircuit(QuantumRegister(5), cr)

circuit.append(gate_sequence(HGate(), [0], gate_sequence(XGate().control(num_ctrl_qubits=1, ctrl_state="1"), [0, 1], gate_sequence(gate_sequence(XGate().control(num_ctrl_qubits=1, ctrl_state="1"), [1, 2], gate_sequence(gate_sequence(XGate().control(num_ctrl_qubits=1, ctrl_state="1"), [1, 3], QuantumCircuit(1, global_phase=(np.pi) / (2), name="gphase(1.57)").to_gate(), [1], "$0", 5), [0, 1, 2, 3, 4], HGate(), [3], "$1", 5), [0, 1, 2, 3, 4], "$2", 5), [0, 1, 2, 3, 4], gate_sequence(XGate().control(num_ctrl_qubits=1, ctrl_state="1"), [2, 4], XGate(), [2], "$3", 5), [0, 1, 2, 3, 4], "$4", 5), [0, 1, 2, 3, 4], "$5", 5), [0, 1, 2, 3, 4], "$6", 5), [0, 1, 2, 3, 4])

circuit = circuit.decompose(reps=7, gates_to_decompose=["$0", "$1", "$2", "$3", "$4", "$5", "$6"])

circuit.measure([0, 1, 3, 2, 4], cr)

circuit.draw("mpl", filename=__file__.replace(".py", ".png"))

qasm3.dump(circuit, open(__file__.replace(".py", ".qasm"), "w"))
