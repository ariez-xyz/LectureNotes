.data

buf: .space 16
bufend:

.text
.global main

countByteRange:
    ; function countByteRange
    ;   -> R1:  byte to check
    ;   -> R2:  start address
    ;   -> R3:  end address (assume exclusive)
    ;   <- R1:  count of occurrences
    ;
    ; register mapping:
    ;   R2: current address
    ;   R4: counter
    ;   R5: temporary for byte loads

    ; Sanitize input - mask LSByte
    andi r1, r1, 0xFF

    ; Set counter to zero
    xor r4, r4, r4

loop:
    ; Load byte at current address
    lb r5, (r2)

    ; If the current byte is equal to the byte to look for, +1
    seq r5, r5, r1
    beqz r5, endif
    nop
    addi r4, r4, 1

endif:
    ; Increment the pointer
    addi r2, r2, 1

    ; If the pointer reached the end pointer, don't branch
    seq r5, r2, r3
    beqz r5, loop
    nop

    ; ======

    ; Move counter to R1
    add r1, r0, r4

    jr r31
    nop

main:

    ; Fill range with 0x00..0x0F for debugging
    xor r1, r1, r1
    addi r2, r0, buf
fillLoop:
    sb (r2), r1
    addi r1, r1, 4
    andi r1, r1, 0xf

    addi r2, r2, 1
    seqi r3, r2, bufend
    beqz r3, fillLoop
    nop


    ; Search for 0x04
    addi r1, r0, 4
    addi r2, r0, buf
    addi r3, r0, bufend

    jal countByteRange
    nop
    trap 0
