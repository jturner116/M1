@ Type exercise 4 here
SETI R0, #0 
SETI R1, #0 
SETI R2, #1 

INVOKE 1, 3, 4 

L0: 

    GOTO_GE L2, 0, 4
    
    L1: 
        
        GOTO_GE L3, R1, R3
        
        GOTO_EQ L4, R1, #0 
        ADD R5, R1, R2 
        INVOKE 3, R5, R0 
        INVOKE 5, R6, 3 
        
        L4:
        GOTO_NE L5, R6, R2 
        INVOKE 3, R1, R0 
        INVOKE 4, R2, #0 
        
        L5:
        ADD R1, R1, R2 
        GOTO L1
    
    L3:
    ADD R0, R0, R2 
    SET R1, #0 
    GOTO L0

L2:
STOP

