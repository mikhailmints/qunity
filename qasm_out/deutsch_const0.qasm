OPENQASM 3.0;
include "stdgates.inc";
qubit[2] q;
bit[1] out;
bit[0] err;
z q[1];
out[0] = measure q[0];
