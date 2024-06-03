OPENQASM 3.0;
include "stdgates.inc";
qubit[15] q;
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
// left prep [3]
// left out [3]
x q[0];
swap q[3], q[1];
negctrl @ x q[2], q[0];
negctrl @ swap q[2], q[3], q[1];
reset q[1];
reset q[2];
// mixed_err out [0, 3]
// mixed_err garb [1, 2]
// mixed_err prep [1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]
// mixed_err prep [4, 5, 6, 7]
// left prep [6]
// left out [6]
// u3 in [6]
h q[6];
// u3 out [6]
// adjoint in [6]
// left out [6]
// left prep [6]
// adjoint flag [6]
// left prep [7]
// left out [7]
x q[4];
swap q[7], q[5];
negctrl @ x q[6], q[4];
negctrl @ swap q[6], q[7], q[5];
reset q[5];
reset q[6];
// mixed_err out [4, 7]
// mixed_err garb [5, 6]
// mixed_err prep [5, 6, 8, 9, 10, 11, 12, 13, 14]
// mixed_err prep [8, 9, 10, 11]
// left prep [10]
// left out [10]
// u3 in [10]
h q[10];
// u3 out [10]
// adjoint in [10]
// left out [10]
// left prep [10]
// adjoint flag [10]
// left prep [11]
// left out [11]
x q[8];
swap q[11], q[9];
negctrl @ x q[10], q[8];
negctrl @ swap q[10], q[11], q[9];
reset q[9];
reset q[10];
// mixed_err out [8, 11]
// mixed_err garb [9, 10]
// mixed_err prep [9, 10, 12, 13]
// left prep [12]
// left out [12]
// u3 in [12]
h q[12];
// u3 out [12]
// adjoint in [12]
// left out [12]
// left prep [12]
// adjoint flag [12]
// left prep [13]
// left out [13]
x q[9];
swap q[13], q[10];
negctrl @ x q[12], q[9];
negctrl @ swap q[12], q[13], q[10];
reset q[10];
reset q[12];
// mixed_err out [9, 13]
// mixed_err garb [10, 12]
// distr_right in [8, 11, 9, 13]
ctrl @ swap q[8], q[9], q[11];
ctrl @ swap q[8], q[9], q[13];
// distr_right out [8, 11, 9, 13]
// adjoint in [8, 11, 9, 13]
// adjoint prep [14]
// assoc flag [14]
// assoc out [8, 11, 9, 13]
negctrl @ swap q[8], q[13], q[14];
negctrl @ swap q[8], q[9], q[13];
negctrl @ swap q[8], q[11], q[9];
ctrl @ x q[11], q[8];
ctrl @ swap q[11], q[8], q[9];
ctrl @ swap q[11], q[9], q[13];
ctrl @ swap q[11], q[13], q[14];
// assoc in [11, 8, 9, 13, 14]
// adjoint out [11, 8, 9, 13, 14]
// adjoint in [11, 8, 9, 13, 14]
// left out [11, 8, 9, 13, 14]
// left prep [11]
// left in [8, 9, 13, 14]
// adjoint out [8, 9, 13, 14]
// adjoint flag [11]
// adjoint in [8, 9, 13, 14]
// distr_left out [8, 9, 13, 14]
// distr_left in [9, 8, 13, 14]
// adjoint out [9, 8, 13, 14]
// discard in [8, 13, 14]
reset q[8];
reset q[13];
reset q[14];
// discard garb [8, 13, 14]
x q[5];
swap q[9], q[6];
negctrl @ x q[11], q[5];
negctrl @ swap q[11], q[9], q[6];
reset q[6];
reset q[8];
reset q[9];
reset q[10];
reset q[11];
reset q[12];
reset q[13];
reset q[14];
// mixed_err out [5, 9]
// mixed_err garb [6, 8, 9, 10, 11, 12, 13, 14]
// distr_right in [4, 7, 5, 9]
ctrl @ swap q[4], q[5], q[7];
ctrl @ swap q[4], q[5], q[9];
// distr_right out [4, 7, 5, 9]
// adjoint in [4, 7, 5, 9]
// adjoint prep [11]
// assoc flag [11]
// assoc out [4, 7, 5, 9]
negctrl @ swap q[4], q[9], q[11];
negctrl @ swap q[4], q[5], q[9];
negctrl @ swap q[4], q[7], q[5];
ctrl @ x q[7], q[4];
ctrl @ swap q[7], q[4], q[5];
ctrl @ swap q[7], q[5], q[9];
ctrl @ swap q[7], q[9], q[11];
// assoc in [7, 4, 5, 9, 11]
// adjoint out [7, 4, 5, 9, 11]
// adjoint in [7, 4, 5, 9, 11]
// left out [7, 4, 5, 9, 11]
// left prep [7]
// left in [4, 5, 9, 11]
// adjoint out [4, 5, 9, 11]
// adjoint flag [7]
// adjoint in [4, 5, 9, 11]
// distr_left out [4, 5, 9, 11]
// distr_left in [5, 4, 9, 11]
// adjoint out [5, 4, 9, 11]
// discard in [4, 9, 11]
reset q[4];
reset q[9];
reset q[11];
// discard garb [4, 9, 11]
x q[1];
swap q[5], q[2];
negctrl @ x q[7], q[1];
negctrl @ swap q[7], q[5], q[2];
reset q[2];
reset q[4];
reset q[5];
reset q[6];
reset q[7];
reset q[8];
reset q[9];
reset q[10];
reset q[11];
reset q[12];
reset q[13];
reset q[14];
// mixed_err out [1, 5]
// mixed_err garb [2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]
// distr_right in [0, 3, 1, 5]
ctrl @ swap q[0], q[1], q[3];
ctrl @ swap q[0], q[1], q[5];
// distr_right out [0, 3, 1, 5]
// adjoint in [0, 3, 1, 5]
// adjoint prep [7]
// assoc flag [7]
// assoc out [0, 3, 1, 5]
negctrl @ swap q[0], q[5], q[7];
negctrl @ swap q[0], q[1], q[5];
negctrl @ swap q[0], q[3], q[1];
ctrl @ x q[3], q[0];
ctrl @ swap q[3], q[0], q[1];
ctrl @ swap q[3], q[1], q[5];
ctrl @ swap q[3], q[5], q[7];
// assoc in [3, 0, 1, 5, 7]
// adjoint out [3, 0, 1, 5, 7]
// adjoint in [3, 0, 1, 5, 7]
// left out [3, 0, 1, 5, 7]
// left prep [3]
// left in [0, 1, 5, 7]
// adjoint out [0, 1, 5, 7]
// adjoint flag [3]
// adjoint in [0, 1, 5, 7]
// distr_left out [0, 1, 5, 7]
// distr_left in [1, 0, 5, 7]
// adjoint out [1, 0, 5, 7]
// discard in [0, 5, 7]
reset q[0];
reset q[5];
reset q[7];
// discard garb [0, 5, 7]
out[0] = measure q[1];
err[0] = measure q[3];
