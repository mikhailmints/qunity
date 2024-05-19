OPENQASM 3.0;
include "stdgates.inc";
bit[1] c0;
qubit[1] q0;
c0[0] = measure q0[0];
