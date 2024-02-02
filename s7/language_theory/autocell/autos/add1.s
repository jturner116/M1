	.meta source "\"autos/add1.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 9 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 14, 0
	invoke 5, 15, 6
	add r14, r14, r15
	set r5, r14
	set r12, r5
	invoke 5, 13, 2
	add r12, r12, r13
	set r6, r12
	invoke 5, 10, 5
	set r11, r5
	add r10, r10, r11
	set r7, r10
	set r8, r6
	set r9, r7
	add r8, r8, r9
	invoke 4, 8, 0
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
