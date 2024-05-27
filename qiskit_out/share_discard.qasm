OPENQASM 3.0;
include "stdgates.inc";
bit[1] out;
bit[0] err;
qubit[2] q;
cx q[0], q[1];
reset q[1];
out[0] = measure q[0];
