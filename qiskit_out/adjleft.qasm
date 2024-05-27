OPENQASM 3.0;
include "stdgates.inc";
bit[0] out;
bit[1] err;
qubit[1] q;
x q[0];
err[0] = measure q[0];
