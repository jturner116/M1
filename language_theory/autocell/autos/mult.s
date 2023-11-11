	.meta source "\"autos/mult.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 9 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 21, 0
	seti r22, #3
	mul r21, r21, r22
	set r5, r21
	seti r18, #1
	invoke 5, 19, 1
	set r20, r5
	mul r19, r19, r20
	mul r18, r18, r19
	invoke 4, 18, 0
	seti r15, #1
	set r16, r5
	invoke 5, 17, 0
	mul r16, r16, r17
	add r15, r15, r16
	set r6, r15
	set r12, r6
	invoke 5, 13, 0
	mul r12, r12, r13
	seti r14, #1
	add r12, r12, r14
	set r7, r12
	seti r9, #1
	set r10, r7
	add r9, r9, r10
	invoke 5, 11, 0
	mul r9, r9, r11
	set r8, r9
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
