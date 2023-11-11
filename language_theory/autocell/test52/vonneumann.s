	.meta source "\"test52/vonneumann.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 1 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	seti r27, #0
	set r5, r27
	seti r11, #1
	invoke 1005, 12, 11
	set r7, r12
	set r13, r5
	set r14, r-1
	add r13, r13, r14
	set r6, r13
	seti r15, #3
	invoke 1005, 16, 15
	set r7, r16
	set r17, r5
	set r18, r-1
	add r17, r17, r18
	set r6, r17
	seti r19, #7
	invoke 1005, 20, 19
	set r7, r20
	set r21, r5
	set r22, r-1
	add r21, r21, r22
	set r6, r21
	seti r23, #5
	invoke 1005, 24, 23
	set r7, r24
	set r25, r5
	set r26, r-1
	add r25, r25, r26
	set r6, r25
	set r9, r6
	seti r10, #2
	goto_le L2, r9, r10
L3:
	seti r8, #1
	invoke 4, 8, 0
	goto L4
L2:
L4:
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
