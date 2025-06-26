import sys
import math
from analyze_circuit import draw_circuit, simulate_circuit
from qiskit import QuantumCircuit
from qiskit.circuit.library import (
    QFT,
    CDKMRippleCarryAdder,
    PhaseEstimation,
    GlobalPhaseGate,
)


testcase = sys.argv[1]
basename = testcase + "_qiskit"

circuit = None

if testcase == "multi_and":
    circuit = QuantumCircuit(5, 5)
    circuit.h(range(5))
    circuit.mcp(math.pi, [0, 1, 2, 3], [4])
    circuit.measure(range(5), range(5))
elif testcase == "fourier_transform":
    circuit = QuantumCircuit(5, 5)
    circuit.h([1, 2])
    circuit.append(QFT(5), [0, 1, 2, 3, 4])
    circuit.measure([0, 1, 2, 3, 4], [4, 3, 2, 1, 0])
elif testcase == "phase_estimation":
    circuit = QuantumCircuit(5, 5)
    circuit.append(PhaseEstimation(5, GlobalPhaseGate(2 * math.pi / 3)), range(5))
    circuit.measure(range(5), range(5))
elif testcase == "order_finding":
    print("No Qiskit reference implementation")
elif testcase == "adder_reversible":
    circuit = QuantumCircuit(11, 10)
    circuit.h(range(10))
    circuit.append(CDKMRippleCarryAdder(5, kind="fixed"), range(11))
    circuit.measure(range(10), range(10))
elif testcase == "grover":
    circuit = QuantumCircuit(6, 5)
    circuit.x(5)
    circuit.h(range(6))
    circuit.mcx([0, 1, 2, 3, 4], 5)
    circuit.h(range(5))
    circuit.mcx([0, 1, 2, 3, 4], 5, ctrl_state="01100")
    circuit.h(range(5))
    circuit.measure(range(5), range(5))
elif testcase == "grover_with_lists":
    circuit = QuantumCircuit(5, 4)
    circuit.x(4)
    circuit.h(4)
    circuit.ry(2 * math.acos(math.sqrt(1 / (2**3 - 1))), 0)
    circuit.ch(0, 1)
    circuit.cry(2 * math.acos(math.sqrt(1 / (2**2 - 1))), 0, 2)
    circuit.ch(2, 3)
    circuit.cx(1, 4)
    circuit.cx(3, 4)
    circuit.ch(2, 3)
    circuit.cry(-2 * math.acos(math.sqrt(1 / (2**2 - 1))), 0, 2)
    circuit.ch(0, 1)
    circuit.ry(-2 * math.acos(math.sqrt(1 / (2**3 - 1))), 0)
    circuit.mcx([0, 1, 2, 3], 4, ctrl_state="0000")
    circuit.ry(2 * math.acos(math.sqrt(1 / (2**3 - 1))), 0)
    circuit.ch(0, 1)
    circuit.cry(2 * math.acos(math.sqrt(1 / (2**2 - 1))), 0, 2)
    circuit.ch(2, 3)
    circuit.measure([0, 1, 2, 3], [0, 1, 2, 3])
else:
    print(f'Unknown testcase "{testcase}"')

if circuit is not None:
    draw_circuit(circuit, basename)
    simulate_circuit(circuit, basename)
