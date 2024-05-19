OPENQASM 3.0;
include "stdgates.inc";
gate gphase(1.57) _gate_q_0 {
}
bit[5] c0;
qubit[5] q0;
h q0[0];
cx q0[0], q0[1];
cx q0[1], q0[2];
cx q0[1], q0[3];
gphase(1.57) q0[1];
h q0[3];
cx q0[2], q0[4];
x q0[2];
c0[0] = measure q0[0];
c0[1] = measure q0[1];
c0[2] = measure q0[3];
c0[3] = measure q0[2];
c0[4] = measure q0[4];
