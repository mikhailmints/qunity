OPENQASM 3.0;
include "stdgates.inc";
qubit[6] q;
bit[6] out;
bit[0] err;
// left prep [0]
// left out [0]
// u3 in [0]
h q[0];
// u3 out [0]
// left prep [1]
// left out [1]
// u3 in [1]
h q[1];
// u3 out [1]
// left prep [2]
// left out [2]
// u3 in [2]
h q[2];
// u3 out [2]
// left prep [3]
// left out [3]
// u3 in [3]
h q[3];
// u3 out [3]
// left prep [4]
// left out [4]
// u3 in [4]
h q[4];
// u3 out [4]
// left prep [5]
// left out [5]
// u3 in [5]
h q[5];
// u3 out [5]
// pair in [5]
// pair out [5]
// pair in [4, 5]
// pair out [4, 5]
// pair in [3, 4, 5]
// pair out [3, 4, 5]
// pair in [2, 3, 4, 5]
// pair out [2, 3, 4, 5]
// pair in [1, 2, 3, 4, 5]
// pair out [1, 2, 3, 4, 5]
// pair in [0, 1, 2, 3, 4, 5]
// pair out [0, 1, 2, 3, 4, 5]
out[0] = measure q[0];
out[1] = measure q[1];
out[2] = measure q[2];
out[3] = measure q[3];
out[4] = measure q[4];
out[5] = measure q[5];
