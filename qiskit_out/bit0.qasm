OPENQASM 3.0;
include "stdgates.inc";
bit[1] out;
bit[0] err;
qubit[1] q;
out[0] = measure q[0];
