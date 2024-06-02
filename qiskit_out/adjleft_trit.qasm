OPENQASM 3.0;
include "stdgates.inc";
bit[0] out;
bit[2] err;
qubit[2] q;
x q[0];
x q[1];
err[0] = measure q[0];
err[1] = measure q[1];
