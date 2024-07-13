OPENQASM 3.0;
include "stdgates.inc";
qubit[1] q;
bit[1] out;
bit[0] err;
U((2) * (0.955317), 0, 0) q[0];
out[0] = measure q[0];
