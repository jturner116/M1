	.meta source "\"autos/conway.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 1 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 17, 2
	invoke 5, 18, 1
	add r17, r17, r18
	invoke 5, 19, 8
	add r17, r17, r19
	invoke 5, 20, 7
	add r17, r17, r20
	invoke 5, 21, 6
	add r17, r17, r21
	invoke 5, 22, 5
	add r17, r17, r22
	invoke 5, 23, 4
	add r17, r17, r23
	invoke 5, 24, 3
	add r17, r17, r24
	set r5, r17
	invoke 5, 15, 0
	seti r16, #1
	goto_ne L2, r15, r16
L3:
	set r13, r5
	seti r14, #2
	goto_ge L8, r13, r14
L9:
	seti r12, #0
	invoke 4, 12, 0
	goto L10
L8:
	set r10, r5
	seti r11, #3
	goto_le L11, r10, r11
L12:
	seti r9, #0
	invoke 4, 9, 0
	goto L13
L11:
L13:
L10:
	goto L4
L2:
	set r7, r5
	seti r8, #3
	goto_ne L5, r7, r8
L6:
	seti r6, #1
	invoke 4, 6, 0
	goto L7
L5:
L7:
L4:
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
