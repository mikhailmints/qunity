OPENQASM 3.0;
include "stdgates.inc";
qubit[2] q;
bit[2] out;
bit[0] err;
U(1.91063323624901859610, 0.00000000000000000000, 0.00000000000000000000) q[0];
ctrl @ h q[0], q[1];
out[0] = measure q[0];
out[1] = measure q[1];
