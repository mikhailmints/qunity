OPENQASM 3.0;
include "stdgates.inc";
bit[1] c0;
qubit[1] q0;
u3(1.9106332362490186, 0, 0) q0[0];
c0[0] = measure q0[0];
