OPENQASM 3.0;
include "stdgates.inc";
qubit[2] q;
bit[1] out;
bit[1] err;
x q[0];
err[0] = measure q[1];
reset q[1];
out[0] = measure q[0];
