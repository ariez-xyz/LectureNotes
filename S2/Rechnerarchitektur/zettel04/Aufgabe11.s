;   a0 = 0;
;   a1 = 1;
;   n = n - 1;
;   while (n != 0) {
;   a2 = a0 + a1;
;   a0 = a1;
;   a1 = a2;
;   n = n - 1;
;   }
;   f = a1;


.data
n:  .word 10
f:  .word 0

.text
.global main
main:
    ; REGISTER LAYOUT
    ; r1:   n
    ; r2:   a0
    ; r3:   a1
    ; r4:   a2

    ; Initialize a0, a1, n
    addi r2, r0, 0
    addi r3, r0, 1
    lw r1, n
    subi r1, r1, 1

loop:
    beqz r1, loopend
    nop

    add r4, r2, r3
    add r2, r0, r3
    add r3, r0, r4
    subi r1, r1, 1

    j loop
    nop
loopend:
    sw f, r3
