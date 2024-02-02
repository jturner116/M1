@ Type exercise 3 here
SETI R0, #0 
SETI R1, #0   
SETI R2, #1

INVOKE 1, 3, 4  

RowLoop:
    GOTO_GE EndProgram 1, 4
    
    ColLoop:
        GOTO_GE NextRow, 0, 3
        
        INVOKE 3, 0, 1
        
        INVOKE 4, 2, #0
        
        ADD R0, R0, R2
        GOTO ColLoop
    
    NextRow:
    SETI R0, #0   
    ADD R1, R1, R2 
    GOTO RowLoop

    EndProgram:
	STOP
