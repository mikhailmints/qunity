OPENQASM 3.0;
include "stdgates.inc";
bit[1] out;
bit[0] err;
qubit[1] q;
u3(1.9106332362490186, 0, 0) q[0];
out[0] = measure q[0];
