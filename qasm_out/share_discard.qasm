OPENQASM 3.0;
include "stdgates.inc";
qubit[2] q;
bit[1] out;
bit[0] err;
ctrl @ x q[0], q[1];
reset q[1];
out[0] = measure q[0];
