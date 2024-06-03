OPENQASM 3.0;
include "stdgates.inc";
qubit[5] q;
bit[5] out;
bit[0] err;
// left prep [0]
// left out [0]
// u3 in [0]
h q[0];
// u3 out [0]
// adjoint in [0]
// adjoint out [0]
// context_partition in [0]
// context_partition out [0]
// share in [0]
// share prep [1]
ctrl @ x q[0], q[1];
// share out [0, 1]
// adjoint in [0]
// context_partition out [0]
// context_partition in [0]
// adjoint out [0]
// adjoint in [1]
// context_partition out [1]
// context_partition in [1]
// adjoint out [1]
// context_partition in [1]
// context_partition out [1]
// share in [1]
// share prep [2]
ctrl @ x q[1], q[2];
// share out [1, 2]
// adjoint in [1]
// context_partition out [1]
// context_partition in [1]
// adjoint out [1]
// adjoint in [2]
// context_partition out [2]
// context_partition in [2]
// adjoint out [2]
// context_partition in [1]
// context_partition out [1]
// share in [1]
// share prep [3]
ctrl @ x q[1], q[3];
// share out [1, 3]
// adjoint in [1]
// context_partition out [1]
// context_partition in [1]
// adjoint out [1]
// adjoint in [3]
// context_partition out [3]
// context_partition in [3]
// adjoint out [3]
// gphase in [1]
gphase(1.570796);
// gphase out [1]
// u3 in [3]
h q[3];
// u3 out [3]
// pair in [1, 3]
// pair out [1, 3]
// context_partition in [2]
// context_partition out [2]
// share in [2]
// share prep [4]
ctrl @ x q[2], q[4];
// share out [2, 4]
// adjoint in [2]
// context_partition out [2]
// context_partition in [2]
// adjoint out [2]
// adjoint in [4]
// context_partition out [4]
// context_partition in [4]
// adjoint out [4]
// u3 in [2]
x q[2];
// u3 out [2]
// gphase in [4]
gphase(0.785398);
// gphase out [4]
// pair in [2, 4]
// pair out [2, 4]
// pair in [1, 3, 2, 4]
// pair out [1, 3, 2, 4]
// pair in [0, 1, 3, 2, 4]
// pair out [0, 1, 3, 2, 4]
out[0] = measure q[0];
out[1] = measure q[1];
out[2] = measure q[3];
out[3] = measure q[2];
out[4] = measure q[4];
