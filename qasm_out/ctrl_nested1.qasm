OPENQASM 3.0;
include "stdgates.inc";
qubit[4] q;
bit[2] out;
bit[5] err;
h q[0];
h q[1];
ctrl @ x q[0], q[2];
ctrl @ x q[1], q[3];
err[0] = measure q[2];
reset q[2];
ctrl @ x q[3], q[2];
err[1] = measure q[2];
reset q[2];
ctrl @ x q[3], q[2];
err[2] = measure q[2];
reset q[2];
ctrl @ x q[1], q[3];
err[3] = measure q[3];
reset q[3];
ctrl @ x q[0], q[2];
err[4] = measure q[2];
reset q[2];
out[0] = measure q[0];
out[1] = measure q[1];
