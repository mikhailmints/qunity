OPENQASM 3.0;
include "stdgates.inc";
qubit[2] q;
bit[1] out;
bit[0] err;
// left prep [0]
// left out [0]
// adjoint in [0]
// adjoint out [0]
// context_partition in [0]
// context_partition out [0]
// left prep [1]
// left out [1]
// discard in [0]
reset q[0];
// discard garb [0]
out[0] = measure q[1];
