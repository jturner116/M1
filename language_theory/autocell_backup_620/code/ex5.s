SETI R0, #0
SETI R1, #0
SETI R2, #1
INVOKE 1, R3, R4

LRowLoop:
GOTO_GE LEnd, R0, R4
    
LColumnLoop:
GOTO_GE LNextRow, R1, R3
INVOKE 3, R1, R0
INVOKE 5, R5, 0
GOTO_EQ LCheckNeighbours, R5, #0
INVOKE 4, #0, #0
GOTO LNextColumn
        
LCheckNeighbours:
SETI R6, #0
SETI R7, #1

LNeighbourLoop:
GOTO_GE LSetCell, R7, #9
INVOKE 5, R8, R7
ADD R6, R6, R8
ADD R7, R7, R2
GOTO LNeighbourLoop
        
LSetCell:
GOTO_EQ LNextColumn, R6, #0
INVOKE 4, R2, #0
        
LNextColumn:
ADD R1, R1, R2
GOTO LColumnLoop
    
LNextRow:
ADD R0, R0, R2
SET R1, #0
GOTO LRowLoop

LEnd:
STOP