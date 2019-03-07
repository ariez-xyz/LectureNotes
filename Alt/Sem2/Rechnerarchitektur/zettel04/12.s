	.data
reg4:	.word	0
reg5:	.word	0
reg6:	.word	0
	
	.text
	.global main

main:	sw	reg4, r4
	sw 	reg5, r5
	sw	reg6, r6
	addi	r6, r0, 7

	;r3: ergebnis
	;r4: letztes bit aus r1
	;r5: r1 aber shr nach jedem durchgang

	;fuelle r3, r5
	add 	r5, r1, r0
	addi	r3, r0, 1

	;wenn wir eine gerade anzahl 1-bits hatten:
even:	jal	next
	nop
	xor	r3, r3, r4
	xor	r3, r3, r2
	beqz	r3, uneven
	nop
	j	even
	nop

	;wenn wir eine ungerade anzahl 1-bits hatten:
uneven:	jal	next
	nop
	xor	r3, r3, r4
	xor	r3, r3, r2
	bnez	r3, even
	nop
	j 	uneven
	nop

	;nimm das naechste bit aus r1 und tu es in r4, dann jumpe zurueck
next:	beqz	r6, end
	nop
	subi	r6, r6, 1
	add	r4, r5, r0
	andi	r4, r4, 1
	srli	r5, r5, 1
	jr	r31
	nop 

end:	lw	r4, reg4
	lw	r5, reg5
	lw	r6, reg6
	trap	0
