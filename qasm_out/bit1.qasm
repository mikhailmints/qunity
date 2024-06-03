OPENQASM 3.0;
include "stdgates.inc";
qubit[1] q;
bit[1] out;
bit[0] err;
// right prep [0]
x q[0];
// right out [0]
out[0] = measure q[0];
