OPENQASM 3.0;
include "stdgates.inc";
qubit[1] q;
bit[0] out;
bit[1] err;
// right prep [0]
x q[0];
// right out [0]
// adjoint in [0]
// left out [0]
// left prep [0]
// adjoint flag [0]
err[0] = measure q[0];
