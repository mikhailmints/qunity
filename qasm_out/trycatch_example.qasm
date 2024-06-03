OPENQASM 3.0;
include "stdgates.inc";
qubit[6] q;
bit[1] out;
bit[1] err;
// mixed_err prep [0, 1, 2, 3]
// left prep [2]
// left out [2]
// u3 in [2]
h q[2];
// u3 out [2]
// adjoint in [2]
// left out [2]
// left prep [2]
// adjoint flag [2]
// right prep [3]
x q[3];
// right out [3]
x q[0];
swap q[3], q[1];
negctrl @ x q[2], q[0];
negctrl @ swap q[2], q[3], q[1];
reset q[1];
reset q[2];
// mixed_err out [0, 3]
// mixed_err garb [1, 2]
// mixed_err prep [1, 2, 4]
// right prep [4]
x q[4];
// right out [4]
// u3 in [4]
h q[4];
// u3 out [4]
reset q[2];
// mixed_err out [1, 4]
// mixed_err garb [2]
// distr_right in [0, 3, 1, 4]
ctrl @ swap q[0], q[1], q[3];
ctrl @ swap q[0], q[1], q[4];
// distr_right out [0, 3, 1, 4]
// adjoint in [0, 3, 1, 4]
// adjoint prep [5]
// assoc flag [5]
// assoc out [0, 3, 1, 4]
negctrl @ swap q[0], q[4], q[5];
negctrl @ swap q[0], q[1], q[4];
negctrl @ swap q[0], q[3], q[1];
ctrl @ x q[3], q[0];
ctrl @ swap q[3], q[0], q[1];
ctrl @ swap q[3], q[1], q[4];
ctrl @ swap q[3], q[4], q[5];
// assoc in [3, 0, 1, 4, 5]
// adjoint out [3, 0, 1, 4, 5]
// adjoint in [3, 0, 1, 4, 5]
// left out [3, 0, 1, 4, 5]
// left prep [3]
// left in [0, 1, 4, 5]
// adjoint out [0, 1, 4, 5]
// adjoint flag [3]
// adjoint in [0, 1, 4, 5]
// distr_left out [0, 1, 4, 5]
// distr_left in [1, 0, 4, 5]
// adjoint out [1, 0, 4, 5]
// discard in [0, 4, 5]
reset q[0];
reset q[4];
reset q[5];
// discard garb [0, 4, 5]
out[0] = measure q[1];
err[0] = measure q[3];
