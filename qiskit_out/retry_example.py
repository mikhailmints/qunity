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
build_circuit(15, [1], [3], gate_sequence(gate_sequence(gate_sequence(gate_sequence(gate_sequence(HGate(), [2], gate_sequence(gate_sequence(XGate(), [0], gate_sequence(SwapGate(), [3, 1], gate_sequence(XGate().control(num_ctrl_qubits=1, ctrl_state="0"), [2, 0], SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [2, 3, 1], "_0"), [0, 1, 2, 3], "_1"), [0, 1, 2, 3], "_2"), [0, 1, 2, 3], gate_sequence(Reset(), [1], Reset(), [2], "_3"), [1, 2], "_4"), [0, 1, 2, 3], "_5"), [0, 1, 2, 3], gate_sequence(gate_sequence(gate_sequence(gate_sequence(gate_sequence(gate_sequence(HGate(), [6], gate_sequence(gate_sequence(XGate(), [4], gate_sequence(SwapGate(), [7, 5], gate_sequence(XGate().control(num_ctrl_qubits=1, ctrl_state="0"), [6, 4], SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [6, 7, 5], "_6"), [4, 5, 6, 7], "_7"), [4, 5, 6, 7], "_8"), [4, 5, 6, 7], gate_sequence(Reset(), [5], Reset(), [6], "_9"), [5, 6], "_10"), [4, 5, 6, 7], "_11"), [4, 5, 6, 7], gate_sequence(gate_sequence(gate_sequence(gate_sequence(gate_sequence(gate_sequence(HGate(), [10], gate_sequence(gate_sequence(XGate(), [8], gate_sequence(SwapGate(), [11, 9], gate_sequence(XGate().control(num_ctrl_qubits=1, ctrl_state="0"), [10, 8], SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [10, 11, 9], "_12"), [8, 9, 10, 11], "_13"), [8, 9, 10, 11], "_14"), [8, 9, 10, 11], gate_sequence(Reset(), [9], Reset(), [10], "_15"), [9, 10], "_16"), [8, 9, 10, 11], "_17"), [8, 9, 10, 11], gate_sequence(HGate(), [12], gate_sequence(gate_sequence(XGate(), [9], gate_sequence(SwapGate(), [13, 10], gate_sequence(XGate().control(num_ctrl_qubits=1, ctrl_state="0"), [12, 9], SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [12, 13, 10], "_18"), [9, 10, 12, 13], "_19"), [9, 10, 12, 13], "_20"), [9, 10, 12, 13], gate_sequence(Reset(), [10], Reset(), [12], "_21"), [10, 12], "_22"), [9, 10, 12, 13], "_23"), [9, 10, 12, 13], "_24"), [8, 9, 10, 11, 12, 13], gate_sequence(SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [8, 9, 11], SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [8, 9, 13], "_25"), [8, 9, 11, 13], "_26"), [8, 9, 10, 11, 12, 13], gate_sequence(gate_sequence(gate_sequence(gate_sequence(SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [8, 13, 14], SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [8, 9, 13], "_27"), [8, 9, 13, 14], SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [8, 11, 9], "_28"), [8, 9, 11, 13, 14], XGate().control(num_ctrl_qubits=1, ctrl_state="1"), [11, 8], "_29"), [8, 9, 11, 13, 14], gate_sequence(SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [11, 8, 9], gate_sequence(SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [11, 9, 13], SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [11, 13, 14], "_30"), [9, 11, 13, 14], "_31"), [8, 9, 11, 13, 14], "_32"), [8, 9, 11, 13, 14], "_33"), [8, 9, 10, 11, 12, 13, 14], gate_sequence(Reset(), [8], gate_sequence(Reset(), [13], Reset(), [14], "_34"), [13, 14], "_35"), [8, 13, 14], "_36"), [8, 9, 10, 11, 12, 13, 14], gate_sequence(gate_sequence(XGate(), [5], gate_sequence(SwapGate(), [9, 6], gate_sequence(XGate().control(num_ctrl_qubits=1, ctrl_state="0"), [11, 5], SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [11, 9, 6], "_37"), [5, 6, 9, 11], "_38"), [5, 6, 9, 11], "_39"), [5, 6, 9, 11], gate_sequence(Reset(), [6], gate_sequence(Reset(), [8], gate_sequence(Reset(), [9], gate_sequence(Reset(), [10], gate_sequence(Reset(), [11], gate_sequence(Reset(), [12], gate_sequence(Reset(), [13], Reset(), [14], "_40"), [13, 14], "_41"), [12, 13, 14], "_42"), [11, 12, 13, 14], "_43"), [10, 11, 12, 13, 14], "_44"), [9, 10, 11, 12, 13, 14], "_45"), [8, 9, 10, 11, 12, 13, 14], "_46"), [6, 8, 9, 10, 11, 12, 13, 14], "_47"), [5, 6, 8, 9, 10, 11, 12, 13, 14], "_48"), [5, 6, 8, 9, 10, 11, 12, 13, 14], "_49"), [4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], gate_sequence(SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [4, 5, 7], SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [4, 5, 9], "_50"), [4, 5, 7, 9], "_51"), [4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], gate_sequence(gate_sequence(gate_sequence(gate_sequence(SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [4, 9, 11], SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [4, 5, 9], "_52"), [4, 5, 9, 11], SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [4, 7, 5], "_53"), [4, 5, 7, 9, 11], XGate().control(num_ctrl_qubits=1, ctrl_state="1"), [7, 4], "_54"), [4, 5, 7, 9, 11], gate_sequence(SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [7, 4, 5], gate_sequence(SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [7, 5, 9], SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [7, 9, 11], "_55"), [5, 7, 9, 11], "_56"), [4, 5, 7, 9, 11], "_57"), [4, 5, 7, 9, 11], "_58"), [4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], gate_sequence(Reset(), [4], gate_sequence(Reset(), [9], Reset(), [11], "_59"), [9, 11], "_60"), [4, 9, 11], "_61"), [4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], gate_sequence(gate_sequence(XGate(), [1], gate_sequence(SwapGate(), [5, 2], gate_sequence(XGate().control(num_ctrl_qubits=1, ctrl_state="0"), [7, 1], SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [7, 5, 2], "_62"), [1, 2, 5, 7], "_63"), [1, 2, 5, 7], "_64"), [1, 2, 5, 7], gate_sequence(Reset(), [2], gate_sequence(Reset(), [4], gate_sequence(Reset(), [5], gate_sequence(Reset(), [6], gate_sequence(Reset(), [7], gate_sequence(Reset(), [8], gate_sequence(Reset(), [9], gate_sequence(Reset(), [10], gate_sequence(Reset(), [11], gate_sequence(Reset(), [12], gate_sequence(Reset(), [13], Reset(), [14], "_65"), [13, 14], "_66"), [12, 13, 14], "_67"), [11, 12, 13, 14], "_68"), [10, 11, 12, 13, 14], "_69"), [9, 10, 11, 12, 13, 14], "_70"), [8, 9, 10, 11, 12, 13, 14], "_71"), [7, 8, 9, 10, 11, 12, 13, 14], "_72"), [6, 7, 8, 9, 10, 11, 12, 13, 14], "_73"), [5, 6, 7, 8, 9, 10, 11, 12, 13, 14], "_74"), [4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], "_75"), [2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], "_76"), [1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], "_77"), [1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], "_78"), [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], gate_sequence(SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [0, 1, 3], SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [0, 1, 5], "_79"), [0, 1, 3, 5], "_80"), [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], gate_sequence(gate_sequence(gate_sequence(gate_sequence(SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [0, 5, 7], SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [0, 1, 5], "_81"), [0, 1, 5, 7], SwapGate().control(num_ctrl_qubits=1, ctrl_state="0"), [0, 3, 1], "_82"), [0, 1, 3, 5, 7], XGate().control(num_ctrl_qubits=1, ctrl_state="1"), [3, 0], "_83"), [0, 1, 3, 5, 7], gate_sequence(SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [3, 0, 1], gate_sequence(SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [3, 1, 5], SwapGate().control(num_ctrl_qubits=1, ctrl_state="1"), [3, 5, 7], "_84"), [1, 3, 5, 7], "_85"), [0, 1, 3, 5, 7], "_86"), [0, 1, 3, 5, 7], "_87"), [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], gate_sequence(Reset(), [0], gate_sequence(Reset(), [5], Reset(), [7], "_88"), [5, 7], "_89"), [0, 5, 7], "_90"), [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], ["_0", "_1", "_10", "_11", "_12", "_13", "_14", "_15", "_16", "_17", "_18", "_19", "_2", "_20", "_21", "_22", "_23", "_24", "_25", "_26", "_27", "_28", "_29", "_3", "_30", "_31", "_32", "_33", "_34", "_35", "_36", "_37", "_38", "_39", "_4", "_40", "_41", "_42", "_43", "_44", "_45", "_46", "_47", "_48", "_49", "_5", "_50", "_51", "_52", "_53", "_54", "_55", "_56", "_57", "_58", "_59", "_6", "_60", "_61", "_62", "_63", "_64", "_65", "_66", "_67", "_68", "_69", "_7", "_70", "_71", "_72", "_73", "_74", "_75", "_76", "_77", "_78", "_79", "_8", "_80", "_81", "_82", "_83", "_84", "_85", "_86", "_87", "_88", "_89", "_9", "_90"])
