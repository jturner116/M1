	.meta source "\"autos/neg.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 1 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 12, 6
	sub r13, r0, r12
	set r5, r13
	set r10, r5
	sub r11, r0, r10
	set r6, r11
	set r7, r5
	set r8, r6
	add r7, r7, r8
	sub r9, r0, r7
	invoke 4, 9, 0
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
