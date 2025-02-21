OPENQASM 3.0;
include "stdgates.inc";
qubit[2] q;
bit[2] out;
bit[0] err;
h q[0];
ctrl @ x q[0], q[1];
out[0] = measure q[0];
out[1] = measure q[1];
