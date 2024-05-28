OPENQASM 3.0;
include "stdgates.inc";
gate cx c, t {
  ctrl @ U(pi, 0, pi) c, t;
}
gate cswap_o0 _gate_q_0, _gate_q_1, _gate_q_2 {
  x _gate_q_0;
  cswap _gate_q_0, _gate_q_1, _gate_q_2;
  x _gate_q_0;
}
bit[1] out;
bit[2] err;
qubit[10] q;
h q[0];
x q[1];
x q[2];
cx_o0 q[0], q[2];
cswap_o0 q[0], q[1], q[3];
reset q[0];
x q[0];
h q[0];
reset q[3];
x q[3];
reset q[4];
cswap_o0 q[2], q[8], q[9];
cswap_o0 q[2], q[0], q[8];
cswap_o0 q[2], q[3], q[0];
cswap_o0 q[2], q[1], q[3];
swap q[2], q[1];
cx q[2], q[1];
cswap q[2], q[1], q[3];
reset q[1];
cswap q[2], q[3], q[0];
cswap q[2], q[0], q[8];
reset q[0];
cswap q[2], q[8], q[9];
reset q[8];
out[0] = measure q[3];
err[0] = measure q[9];
err[1] = measure q[2];
