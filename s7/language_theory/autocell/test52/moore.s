	.meta source "\"test52/moore.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 1 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	seti r29, #0
	set r5, r29
	seti r11, #2
	invoke 1005, 12, 11
	set r7, r12
	set r13, r5
	set r14, r-1
	add r13, r13, r14
	set r6, r13
	seti r11, #1
	invoke 1005, 12, 11
	set r7, r12
	set r15, r5
	set r16, r-1
	add r15, r15, r16
	set r6, r15
	seti r11, #8
	invoke 1005, 12, 11
	set r7, r12
	set r17, r5
	set r18, r-1
	add r17, r17, r18
	set r6, r17
	seti r11, #3
	invoke 1005, 12, 11
	set r7, r12
	set r19, r5
	set r20, r-1
	add r19, r19, r20
	set r6, r19
	seti r11, #7
	invoke 1005, 12, 11
	set r7, r12
	set r21, r5
	set r22, r-1
	add r21, r21, r22
	set r6, r21
	seti r11, #4
	invoke 1005, 12, 11
	set r7, r12
	set r23, r5
	set r24, r-1
	add r23, r23, r24
	set r6, r23
	seti r11, #5
	invoke 1005, 12, 11
	set r7, r12
	set r25, r5
	set r26, r-1
	add r25, r25, r26
	set r6, r25
	seti r11, #6
	invoke 1005, 12, 11
	set r7, r12
	set r27, r5
	set r28, r-1
	add r27, r27, r28
	set r6, r27
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
