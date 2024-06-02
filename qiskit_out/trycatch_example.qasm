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
bit[1] err;
qubit[6] q;
x q[0];
h q[2];
cx_o0 q[2], q[0];
x q[3];
swap q[3], q[1];
cswap_o0 q[2], q[3], q[1];
reset q[1];
cswap q[0], q[1], q[3];
reset q[2];
reset q[2];
x q[4];
h q[4];
cswap q[0], q[1], q[4];
cswap_o0 q[0], q[4], q[5];
cswap_o0 q[0], q[1], q[4];
cswap_o0 q[0], q[3], q[1];
cx q[3], q[0];
cswap q[3], q[0], q[1];
reset q[0];
cswap q[3], q[1], q[4];
cswap q[3], q[4], q[5];
reset q[4];
reset q[5];
out[0] = measure q[1];
err[0] = measure q[3];
