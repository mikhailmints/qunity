print("Starting analysis script")
print("Importing libraries")

import os
from argparse import ArgumentParser
from qiskit import qasm3
import timeout_decorator
from analyze_circuit import draw_circuit, transpile_circuit, simulate_circuit

RED = "\033[0;31m"
GREEN = "\033[0;32m"
YELLOW = "\033[0;33m"
NC = "\033[0m"

argparser = ArgumentParser()
argparser.add_argument("path")
argparser.add_argument("--draw", action="store_true")
argparser.add_argument("--simulate", action="store_true")
argparser.add_argument("--load_timeout", type=int, default=40)
argparser.add_argument("--draw_timeout", type=int, default=40)
argparser.add_argument("--simulate_timeout", type=int, default=200)
argparser.add_argument("--sim_shots", type=int, default=10000)
argparser.add_argument("--img_format", default="png", choices=["png", "jpeg", "svg"])

args = argparser.parse_args()

LOAD_TIMEOUT = args.load_timeout
DRAW_TIMEOUT = args.draw_timeout
SIMULATE_TIMEOUT = args.simulate_timeout
SIM_SHOTS = args.sim_shots
IMG_FORMAT = args.img_format


def analyze_file(qasm_filename, draw, simulate):
    print(f"Analyzing file {qasm_filename}")
    basename = os.path.splitext(os.path.basename(qasm_filename))[0]

    print("Loading circuit")
    circuit = timeout_decorator.timeout(LOAD_TIMEOUT)(qasm3.load)(qasm_filename)

    if draw:
        timeout_decorator.timeout(DRAW_TIMEOUT)(draw_circuit)(
            circuit, basename, img_format=IMG_FORMAT
        )
    if simulate:
        timeout_decorator.timeout(SIMULATE_TIMEOUT)(simulate_circuit)(
            circuit, basename, sim_shots=SIM_SHOTS, img_format=IMG_FORMAT
        )
    else:
        transpile_circuit(circuit)


path = args.path

if os.path.isdir(path):
    print()
    for filename in os.listdir(path):
        if filename.endswith(".qasm"):
            simulate = args.simulate
            if filename in [
                "adder_in_match.qasm",
                "bogosort.qasm",
                "list_length.qasm",
            ]:
                simulate = False
            try:
                analyze_file(os.path.join(path, filename), args.draw, simulate)
            except Exception as e:
                print(f"{RED}Error: {e}{NC}")
            print()
else:
    try:
        analyze_file(path, args.draw, args.simulate)
    except Exception as e:
        print(f"{RED}Error: {e}{NC}")
