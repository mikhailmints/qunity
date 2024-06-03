OPENQASM 3.0;
include "stdgates.inc";
qubit[2] q;
bit[1] out;
bit[1] err;
// right prep [0]
x q[0];
// right out [0]
// right in [0]
// right prep [1]
x q[1];
// right out [1, 0]
// adjoint in [1, 0]
// right out [1, 0]
x q[1];
// right prep [1]
// right in [0]
// adjoint out [0]
// adjoint flag [1]
out[0] = measure q[0];
err[0] = measure q[1];
