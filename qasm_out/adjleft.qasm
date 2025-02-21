OPENQASM 3.0;
include "stdgates.inc";
qubit[1] q;
bit[0] out;
bit[1] err;
x q[0];
err[0] = measure q[0];
reset q[0];
