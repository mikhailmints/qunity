OPENQASM 3.0;
include "stdgates.inc";
qubit[2] q;
bit[0] out;
bit[2] err;
// right prep [0]
x q[0];
// right out [0]
// right in [0]
// right prep [1]
x q[1];
// right out [1, 0]
// adjoint in [1, 0]
// left out [1, 0]
// left prep [1, 0]
// adjoint flag [1, 0]
err[0] = measure q[0];
err[1] = measure q[1];
