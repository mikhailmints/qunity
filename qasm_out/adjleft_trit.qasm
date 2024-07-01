OPENQASM 3.0;
include "stdgates.inc";
qubit[2] q;
bit[0] out;
bit[2] err;
x q[0];
x q[1];
err[0] = measure q[0];
err[1] = measure q[1];
