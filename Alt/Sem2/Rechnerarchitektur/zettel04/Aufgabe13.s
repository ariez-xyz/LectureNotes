.data
temps: .space 124
adc_bits: .word 16

.text
.global main

checkAdc:
    ; function checkAdc
    ;   -> R1 ADC value to check
    ;   <- R2 1 if value is a valid 1-out-of-n code, 0 otherwise
    ;
    ; register mapping
    ;   R3: counter
    ;   R4: ADC bits (will be loaded from adc_len)
    add r2, r0, r0

    ; The validity can be proven easily:
    ;   Find the first one in our half-word and shift it outside.
    ;   If the result is zero, it is valid
    ;   Otherwise, if no one is encountered, the input is invalid
    sw temps, r3
    addi r3, r0, 4
    sw temps(r3), r4

    ; Load ADC bits
    lw r4, adc_bits

    ; If r1 = 0, we can break immediately
    ; We use snei 0 and beqz because the result is invalid and finish
    ; shall thus return 0.
    snei r2, r1, 0
    beqz r2, finish
    nop

    add r3, r0, r0
loop:
    ; Shift bit [r3] to LSB and mask it
    srl r2, r1, r3
    andi r2, r2, 1

    ; If it is 1, we have to check whether the rest after bit [r3] is zero
    seqi r2, r2, 1
    bnez r2, finishloop
    nop

    ; Otherwise, increment r3 and move on with the loop…
    addi r3, r3, 1

    ; …at least, until we hit the bit limit
    ; we use sne/beqz for the same reason as above, as it saves an
    ; add r2, r0, r0
    sne r2, r3, r4
    beqz r2, finish
    nop
    j loop
    nop

finishloop:
    ; bit [r3] is set, thus if we remove it, we shell receive zero
    ; we know bits 0..[r3-1] are zero, otherwise we wouldn't be here
    ; -> shift to bit [r3] and clear it
    srl r2, r1, r3
    ; We don't need counter r3 anymore - reuse it for masking
    ; because 0b1111 1111 ' 1111 1111 ' 1111 1111 ' 1111 1110 (= 0xFFFFFFFE)
    ; is too big as an immediate value
    ; TODO: Ugly hack: 0x00000000 - 0x00000002 = 0xFFFFFFFE
    add r3, r0, r0
    subui r3, r3, 2
    and r2, r2, r3

    seqi r2, r2, 0
    ; Falling through…

finish:
    ; Restore registers R4 and R3
    addi r3, r0, 4
    lw r4, temps(r3)
    lw r3, temps

    ; Jump back
    jr r31

;=========================================================================
main:
    addi r1, r0, 8
    addi r3, r0, 0x0123
    addi r4, r0, 0x4567
    jal checkAdc
    nop

    addi r1, r0, 1
    slli r1, r1, 15
    jal checkAdc
    nop

    add r1, r0, r0
    jal checkAdc
    nop

    addi r1, r0, 0x0e
    jal checkAdc
    nop

    trap 0
