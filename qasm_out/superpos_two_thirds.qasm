OPENQASM 3.0;
include "stdgates.inc";
qubit[1] q;
bit[1] out;
bit[0] err;
U(1.91063323624901859610, 0.00000000000000000000, 0.00000000000000000000) q[0];
out[0] = measure q[0];
