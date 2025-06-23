import sys
import math
from qiskit import transpile, QuantumCircuit
from qiskit.circuit.library import (
    QFT,
    CDKMRippleCarryAdder,
    PhaseEstimation,
    GlobalPhaseGate,
)


def transpile_check(circuit):
    print("Transpiling circuit")
    circuit = transpile(
        circuit,
        basis_gates=("u3", "cx"),
        optimization_level=3,
        seed_transpiler=0,
    )
    print("Qubits:", circuit.num_qubits)
    print("Depth:", circuit.depth())
    print("Gates:", sum(circuit.count_ops().values()))
    return circuit


testcase = sys.argv[1]

if testcase == "multi_and":
    circ_and5 = QuantumCircuit(5, 5)
    circ_and5.h(range(5))
    circ_and5.mcp(math.pi, [0, 1, 2, 3], [4])
    circ_and5.measure(range(5), range(5))
    transpile_check(circ_and5)
elif testcase == "fourier_transform":
    circ_qft = QuantumCircuit(5, 5)
    circ_qft.h([1, 2])
    circ_qft.append(QFT(5), [0, 1, 2, 3, 4])
    circ_qft.measure([0, 1, 2, 3, 4], [4, 3, 2, 1, 0])
    transpile_check(circ_qft)
elif testcase == "phase_estimation":
    pe_circ = QuantumCircuit(5, 5)
    pe_circ.h(range(5))
    pe_circ.append(PhaseEstimation(5, GlobalPhaseGate(1 / 3)), range(5))
    pe_circ.measure(range(5), range(5))
    transpile_check(pe_circ)
elif testcase == "order_finding":
    print("No Qiskit reference implementation")
elif testcase == "adder_reversible":
    circ_add = QuantumCircuit(11, 10)
    circ_add.h(range(11))
    circ_add.append(CDKMRippleCarryAdder(5, kind="fixed"), range(11))
    circ_add.measure(range(10), range(10))
    transpile_check(circ_add)
elif testcase == "grover":
    circ_grover = QuantumCircuit(6, 5)
    circ_grover.x(5)
    circ_grover.h(range(6))
    circ_grover.mcx([0, 1, 2, 3, 4], 5)
    circ_grover.h(range(5))
    circ_grover.mcx([0, 1, 2, 3, 4], 5, ctrl_state="01100")
    circ_grover.h(range(5))
    circ_grover.measure(range(5), range(5))
    transpile_check(circ_grover)
elif testcase == "grover_with_lists":
    circ_grover_list = QuantumCircuit(5, 4)
    circ_grover_list.x(4)
    circ_grover_list.h(4)
    circ_grover_list.ry(2 * math.acos(math.sqrt(1 / (2**3 - 1))), 0)
    circ_grover_list.ch(0, 1)
    circ_grover_list.cry(2 * math.acos(math.sqrt(1 / (2**2 - 1))), 0, 2)
    circ_grover_list.ch(2, 3)
    circ_grover_list.cx(1, 4)
    circ_grover_list.cx(3, 4)
    circ_grover_list.ch(2, 3)
    circ_grover_list.cry(-2 * math.acos(math.sqrt(1 / (2**2 - 1))), 0, 2)
    circ_grover_list.ch(0, 1)
    circ_grover_list.ry(-2 * math.acos(math.sqrt(1 / (2**3 - 1))), 0)
    circ_grover_list.mcx([0, 1, 2, 3], 4, ctrl_state="0000")
    circ_grover_list.ry(2 * math.acos(math.sqrt(1 / (2**3 - 1))), 0)
    circ_grover_list.ch(0, 1)
    circ_grover_list.cry(2 * math.acos(math.sqrt(1 / (2**2 - 1))), 0, 2)
    circ_grover_list.ch(2, 3)
    circ_grover_list.measure([0, 1, 2, 3], [0, 1, 2, 3])
    transpile_check(circ_grover_list)
else:
    print(f'Unknown testcase "{testcase}"')
