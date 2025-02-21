OPENQASM 3.0;
include "stdgates.inc";
qubit[3] q;
bit[1] out;
bit[2] err;
h q[0];
ctrl @ x q[0], q[1];
err[0] = measure q[1];
reset q[1];
ctrl @ x q[0], q[2];
err[1] = measure q[2];
reset q[2];
reset q[0];
out[0] = measure q[1];
