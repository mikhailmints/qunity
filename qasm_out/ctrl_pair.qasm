OPENQASM 3.0;
include "stdgates.inc";
qubit[3] q;
bit[3] out;
bit[0] err;
x q[1];
negctrl @ ctrl @ x q[0], q[1], q[2];
out[0] = measure q[0];
out[1] = measure q[1];
out[2] = measure q[2];
