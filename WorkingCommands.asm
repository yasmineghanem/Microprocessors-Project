.MODEL HUGE
.STACK 64
.386
.DATA            ;MOV    ADD   SUB   DIV   MUL   XOR    AND   OR    NOP   SHR   SHL    CLC   ROL   ROR    INC   DEC
COMMAND_CODES DW 0FB82H,9610H,1ADEH,8398H,970CH,0B2D0H,42B8H,29C0H,85A0H,7A30H,0EE20H,34ACH,1AFCH,8328H,0D23AH,0CBFCH
CURRENT_COMMAND DW ?
CURRENT_DESTINATION DW ?
CURRENT_SOURCE DW ?
ACTUAL_SIZE DB ?
SOURCE_SIZE DB ?
VALUE_WORD DW ?
VALUE_BYTE DB ?
NEWLINE DB 10,13,'$'
;                    AX  ,  AH  ,  AL ,  BX  ,  BH  ,  BL  ,  CX  ,  CH  ,  CL ,   DX   ,  DH  ,  DL  ,  SI  ,  DI ,  SP  ,  BP
REGISTERS_CODES DW 1658H, 1248H, 134CH, 16B0H, 1290H, 1398H, 1708H, 12D8H, 13E4H, 1760H, 1320H, 1430H, 17ABH, 1364H, 19F0H, 14A0H
ERROR_MESSAGE_SYNTAX DB "INVALID INPUT COMMAND", '$'
ERROR_MESSAGE_SIZE_MISMATCH DB "SIZE MISMATCH ERROR",'$'
INPUTCOMMAND DB 20,?,20 DUP('$') 

PLAYER1_REGS DW  8 DUP(0000H)
PLAYER2_REGS DW  8 DUP(0000H)

INPUTCOMMAND_PLAYER1 DB 20,?,20 DUP('$')
INPUTCOMMAND_PLAYER2 DB 20,?,20 DUP('$')

ERROR DB 0

OUTPUTMESSAGE MACRO MSG ;OUTPUT ANY MESSAGE
    MOV DX, OFFSET MSG
    MOV AH, 9H
    INT 21H    
ENDM


.CODE
CONVERT_TO_NUMBER PROC FAR           
    CMP SOURCE_SIZE, 1
    JE CONVERT_ONE
    CMP SOURCE_SIZE, 2
    JE CONVERT_TWO
    CMP SOURCE_SIZE, 3
    JE CONVERT_THREE
    
CONVERT_FOUR:
    INC SI
     
    MOV AX, 0000H
    MOV AL, [SI]
    CMP AL,41H
    JGE CHARACTER1
    ;digit
    SUB AL,30H
    JMP NEXT1
    CHARACTER1:
    SUB AX, 55D
    
    NEXT1:
    MOV CX, 1000H
    
    MUL CX
    MOV BX, AX
    
    INC SI
    MOV AX, 0000H
    MOV AL, [SI]
    CMP AL,41H
    JGE CHARACTER2
    ;LETTER
    SUB AL,30H
    JMP NEXT2
    CHARACTER2:
    SUB AX, 55D
              ;COVERT FIRST FROM CHARACTER TO NUMBER
    NEXT2:
;MULTIPLY FIRST CHARACTER BY 10 TO CONVERT TO TENS    
    MOV CX, 100H
    MUL CX 
    
    ADD BX, AX   ;TO GET NEXT CHARACTER 
    INC SI

    MOV AX, 0000H
    MOV AL, [SI]  
    CMP AL,41H
    JGE CHARACTER3
    ;LETTER
    SUB AL,30H
    JMP NEXT3
    CHARACTER3:
    SUB AX, 55D
         ;COVERT SECOND FROM CHARACTER TO NUMBER
    NEXT3:
    MOV CX, 10H
    MUL CX 
    
    ADD BX, AX
    
    INC SI
    
    MOV AX, 0000H
    MOV AL, [SI]
    CMP AL,41H
    JGE CHARACTER4
    ;LETTER
    SUB AL,30H
    JMP NEXT4
    CHARACTER4:
    SUB AX, 55D
    
    NEXT4:
    ADD AX, BX  
    
    MOV VALUE_WORD, AX 
    JMP EXIT_CONVERT
    

CONVERT_THREE:
    INC SI
    MOV AX, 0000H
    MOV AL, [SI]

    CMP AL,41H
    JGE CHARACTER11
    ;LETTER
    SUB AL,30H
    JMP NEXT11
    CHARACTER11:
    SUB AX, 55D  ;COVERT FIRST FROM CHARACTER TO NUMBER
    
    NEXT11:
;MULTIPLY FIRST CHARACTER BY 10 TO CONVERT TO TENS    
    MOV CX, 100H
    MUL CX 
    
    MOV BX, AX   ;TO GET NEXT CHARACTER 
    INC SI

    MOV AX, 0000H
    MOV AL, [SI]  
    
    CMP AL,41H
    JGE CHARACTER22
    ;LETTER
    SUB AL,30H
    JMP NEXT22
    CHARACTER22:
    SUB AX, 55D     ;COVERT SECOND FROM CHARACTER TO NUMBER
    
    NEXT22:
    MOV CX, 10H
    MUL CX 
    
    ADD BX, AX
    
    INC SI
    
    MOV AX, 0000H
    MOV AL, [SI]
    
    CMP AL,41H
    JGE CHARACTER33
    ;LETTER
    SUB AL,30H
    JMP NEXT33
    CHARACTER33:
    SUB AX, 55D
    
    NEXT33:
    ADD AX, BX  
    
    MOV VALUE_WORD, AX 
    JMP EXIT_CONVERT
    

CONVERT_TWO:
    INC SI

    MOV AX, 0000H
    MOV AL, [SI]

    CMP AL,41H
    JGE CHARACTER111
    ;LETTER
    SUB AL,30H
    JMP NEXT111
    CHARACTER111:
    SUB AX, 55D  ;COVERT FIRST FROM CHARACTER TO NUMBER
    
    NEXT111:
;MULTIPLY FIRST CHARACTER BY 10 TO CONVERT TO TENS    
    MOV CX, 10H
    MUL CX 
    
    MOV BX,AX   ;TO GET NEXT CHARACTER 
    INC SI
    MOV AX, 0000H
    MOV AL, [SI]

    CMP AL,41H
    JGE CHARACTER222
    ;LETTER
    SUB AL,30H
    JMP NEXT222
    CHARACTER222:
    SUB AX, 55D ;COVERT SECOND FROM CHARACTER TO NUMBER
    
    NEXT222:
    ADD AX,BX   ;GET WHOLE ACTUAL NUMBER
    MOV VALUE_WORD, AX
    MOV VALUE_BYTE,AL 
    JMP EXIT_CONVERT

CONVERT_ONE:
    INC SI
    MOV AX, 0000H
    MOV AL, [SI]

    CMP AL,41H
    JGE CHARACTER1111
    ;LETTER
    SUB AL,30H
    JMP NEXT1111
    CHARACTER1111:
    SUB AX, 55D ;COVERT SECOND FROM CHARACTER TO NUMBER
    NEXT1111:
    MOV VALUE_WORD, AX
    MOV VALUE_BYTE, AL

EXIT_CONVERT:
RET 
CONVERT_TO_NUMBER ENDP

GET_CURRENT_DESTINATION PROC FAR
    INC SI
    MOV CX, 2
    MOV AX, 0001H
    MOV BH, 00H

GET_ASCII_MUL_DEST: 
    MOV BL, [SI]
    MUL BX
    INC SI
    LOOP GET_ASCII_MUL_DEST   
    MOV CURRENT_DESTINATION, AX
RET
GET_CURRENT_DESTINATION ENDP

;GETS ASCII MULTIPLICATION FOR SOURCE   
GET_CURRENT_SOURCE PROC FAR
    INC SI
    MOV CX, 2
    MOV AX, 0001H
    MOV BH, 00H

GET_ASCII_MUL_SOURCE: 
    MOV BL, [SI]
    MUL BX
    INC SI
    LOOP GET_ASCII_MUL_SOURCE   
    MOV CURRENT_SOURCE, AX
RET    
GET_CURRENT_SOURCE ENDP 

;CHECKS FOR SPACE IN INPUT COMMAND
CHECK_FOR_SPACE PROC FAR
    INC SI
    CMP [SI], 20H
    JE EXIT_SPACE
    MOV AL, 1
    MOV ERROR, AL
EXIT_SPACE:
RET
CHECK_FOR_SPACE ENDP

;CHECKS FOR COMMA
 CHECK_FOR_COMMA PROC FAR
    INC SI
    CMP [SI], 2CH
    JE EXIT_COMMA
    MOV AL, 1
    MOV ERROR, AL
EXIT_COMMA:
RET
CHECK_FOR_COMMA ENDP

EXECUTE_COMMAND PROC FAR 
    MOV AL, INPUTCOMMAND + 1
    MOV ACTUAL_SIZE, AL
    SUB AL, 7
    MOV SOURCE_SIZE, AL
;GETS ASCII MULTIPLICATION OF COMMAND 
    MOV SI, OFFSET INPUTCOMMAND + 2
    MOV CX, 3
    MOV AX, 0001H
    MOV BH, 00H

GET_ASCII_MUL_EXECUTE: 
    MOV BL, [SI]
    MUL BX
    INC SI
    LOOP GET_ASCII_MUL_EXECUTE   
    MOV CURRENT_COMMAND, AX
      
    MOV DI, OFFSET COMMAND_CODES 
    MOV BH, 0
    MOV CX, 16

;CHECKS WHICH COMMAND
CHOOSE_COMMAND:
    MOV AX, CURRENT_COMMAND
    CMP [DI], AX
    JE CHOSEN_COMMAND
    INC BH   
    INC DI
    LOOP CHOOSE_COMMAND

    ;INC SI
CHOSEN_COMMAND:
;----------------------------------------------MOVE COMMAND-----------------------------------------------
MOVE:
CMP BH, 0 ;MOV
    CALL CHECK_FOR_SPACE
    CMP ERROR, 1
    JE EXIT_SYNTAX

    CALL GET_CURRENT_DESTINATION
    MOV AX, CURRENT_DESTINATION
    MOV DI, OFFSET REGISTERS_CODES
    MOV CX, 16

    CHOOSE_DESTINANTION_MOVE:
        CMP [DI], AX
        JE CHOSEN_DESTINATION_MOVE
        INC BH   
        INC DI
        INC DI
        LOOP CHOOSE_DESTINANTION_MOVE

CHOSEN_DESTINATION_MOVE:
        AX_DEST_MOVE:
        CMP BH, 0  ; AX
        JNE AH_DEST_MOVE
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_MOVE_AX
        CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS[0]
            MOV BX, VALUE_WORD
            MOV PLAYER1_REGS[0], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_MOVE_AX:
            CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX,16

            CHOOSE_SOURCE_MOVE_AX:
            
            CMP [DI], AX
            JE CHOSEN_SOURCE_MOVE_AX
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_MOVE_AX
        CHOSEN_SOURCE_MOVE_AX:
            AX_SRC_MOVE_AX:
            CMP BH, 0  ; AX
            JNE AH_SRC_MOVE_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                NOP
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------ 
            AH_SRC_MOVE_AX:
            CMP BH, 1  ; AH
            JNE AL_SRC_MOVE_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            AL_SRC_MOVE_AX:
            CMP BH, 2  ; AL
            JNE BX_SRC_MOVE_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BX_SRC_MOVE_AX:
            CMP BH, 3  ; BX
            JNE BH_SRC_MOVE_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[2]
                MOV AX,BX
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BH_SRC_MOVE_AX:
            CMP BH, 4  ; BH
            JNE BL_SRC_MOVE_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BL_SRC_MOVE_AX:
            CMP BH, 5  ; BL
            JNE CX_SRC_MOVE_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CX_SRC_MOVE_AX:
            CMP BH, 6  ; CX
            JNE CH_SRC_MOVE_AX
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[4]
                MOV AX,BX
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CH_SRC_MOVE_AX:
            CMP BH, 7  ; CH
            JNE CL_SRC_MOVE_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CL_SRC_MOVE_AX:
            CMP BH, 8  ; CL
            JNE DX_SRC_MOVE_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            DX_SRC_MOVE_AX:
            CMP BH, 9  ; DX
            JNE DH_SRC_MOVE_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[6]
                MOV AX,BX
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            DH_SRC_MOVE_AX:
            CMP BH, 10  ; DH
            JNE DL_SRC_MOVE_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            DL_SRC_MOVE_AX:
            CMP BH, 11  ; DL
            JNE SI_SRC_MOVE_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            SI_SRC_MOVE_AX:
            CMP BH, 12  ; SI
            JNE DI_SRC_MOVE_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[8]
                MOV AX,BX
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            DI_SRC_MOVE_AX:
            CMP BH, 13  ; DI
            JNE SP_SRC_MOVE_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[10]
                MOV AX,BX
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            SP_SRC_MOVE_AX:
            CMP BH, 14  ; SP
            JNE BP_SRC_MOVE_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[12]
                MOV AX,BX
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            BP_SRC_MOVE_AX:
            CMP BH, 15D  ; BP
            JNE VAL_SRC_MOVE_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[14]
                MOV AX,BX
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            VAL_SRC_MOVE_AX:
            
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
            CALL CONVERT_TO_NUMBER
                MOV AX, VALUE_WORD
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
                
            ;------------------------------------------------------------------

AH_DEST_MOVE:
        CMP BH, 1  ; AH
        JNE AL_DEST_MOVE
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_MOVE_AH
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS[0]
            MOV BH, VALUE_BYTE
            MOV  PLAYER1_REGS[0], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_MOVE_AH:
            CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            
            MOV CX,16D
            CHOOSE_SOURCE_MOVE_AH:
            CMP [DI], AX
            JE CHOSEN_SOURCE_MOVE_AH
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_MOVE_AH
        CHOSEN_SOURCE_MOVE_AH:
            AX_SRC_MOVE_AH:
            CMP BH, 0  ; AX
            JNE AH_SRC_MOVE_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------ 
            AH_SRC_MOVE_AH:
            CMP BH, 1D  ; AH
            JNE AL_SRC_MOVE_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                NOP
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            AL_SRC_MOVE_AH:
            CMP BH, 2D  ; AL
            JNE BX_SRC_MOVE_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[0]
                MOV BH,BL
                MOV PLAYER1_REGS[0],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BX_SRC_MOVE_AH:
            CMP BH, 3D  ; BX
            JNE BH_SRC_MOVE_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BH_SRC_MOVE_AH:
            CMP BH, 4D  ; BH
            JNE BL_SRC_MOVE_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX,PLAYER1_REGS[0]
                MOV AH,BH
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BL_SRC_MOVE_AH:
            CMP BH, 5D  ; BL
            JNE CX_SRC_MOVE_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX,PLAYER1_REGS[0]
                MOV AH,BL
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CX_SRC_MOVE_AH:
            CMP BH, 6D  ; CX
            JNE CH_SRC_MOVE_AH
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CH_SRC_MOVE_AH:
            CMP BH, 7D  ; CH
            JNE CL_SRC_MOVE_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX,PLAYER1_REGS[0]
                MOV AH,BH
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CL_SRC_MOVE_AH:
            CMP BH, 8D  ; CL
            JNE DX_SRC_MOVE_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX,PLAYER1_REGS[0]
                MOV AH,BL
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            DX_SRC_MOVE_AH:
            CMP BH, 9D  ; DX
            JNE DH_SRC_MOVE_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            DH_SRC_MOVE_AH:
            CMP BH, 10D  ; DH
            JNE DL_SRC_MOVE_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX,PLAYER1_REGS[0]
                MOV AH,BH
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            DL_SRC_MOVE_AH:
            CMP BH, 11D  ; DL
            JNE SI_SRC_MOVE_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX,PLAYER1_REGS[0]
                MOV AH,BL
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            SI_SRC_MOVE_AH:
            CMP BH, 12D  ; SI
            JNE DI_SRC_MOVE_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            DI_SRC_MOVE_AH:
            CMP BH, 13D  ; DI
            JNE SP_SRC_MOVE_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            SP_SRC_MOVE_AH:
            CMP BH, 14D  ; SP
            JNE BP_SRC_MOVE_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            BP_SRC_MOVE_AH:
            CMP BH, 15D  ; BP
            JNE VAL_SRC_MOVE_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            VAL_SRC_MOVE_AH:
            
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
            CALL CONVERT_TO_NUMBER
                MOV BX,PLAYER1_REGS[0]
                MOV BH, VALUE_BYTE
                MOV PLAYER1_REGS[0],BX
                JMP EXIT_EXECUTE
                
            ;------------------------------------------------------------------ 
            AL_DEST_MOVE:
        CMP BH, 2  ; AL
        JNE BX_DEST_MOVE
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_MOVE_AL
        CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS[0]
            MOV BL, VALUE_BYTE
            MOV  PLAYER1_REGS[0], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_MOVE_AL:
            CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX,16D

            CHOOSE_SOURCE_MOVE_AL:
            
            CMP [DI], AX
            JE CHOSEN_SOURCE_MOVE_AL
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_MOVE_AL
        CHOSEN_SOURCE_MOVE_AL:
            AX_SRC_MOVE_AL:
            CMP BH, 0  ; AX
            JNE AH_SRC_MOVE_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------ 
            AH_SRC_MOVE_AL:
            CMP BH, 1  ; AH
            JNE AL_SRC_MOVE_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV AX,PLAYER1_REGS[0]
                MOV AL,AH
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            AL_SRC_MOVE_AL:
            CMP BH, 2  ; AL
            JNE BX_SRC_MOVE_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                NOP
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BX_SRC_MOVE_AL:
            CMP BH, 3  ; BX
            JNE BH_SRC_MOVE_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BH_SRC_MOVE_AL:
            CMP BH, 4  ; BH
            JNE BL_SRC_MOVE_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX,PLAYER1_REGS[0]
                MOV AL,BH
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BL_SRC_MOVE_AL:
            CMP BH, 5  ; BL
            JNE CX_SRC_MOVE_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX,PLAYER1_REGS[0]
                MOV AL,BL
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CX_SRC_MOVE_AL:
            CMP BH, 6  ; CX
            JNE CH_SRC_MOVE_AL
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CH_SRC_MOVE_AL:
            CMP BH, 7  ; CH
            JNE CL_SRC_MOVE_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX,PLAYER1_REGS[0]
                MOV AL,BH
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CL_SRC_MOVE_AL:
            CMP BH, 8  ; CL
            JNE DX_SRC_MOVE_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX,PLAYER1_REGS[0]
                MOV AL,BL
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            DX_SRC_MOVE_AL:
            CMP BH, 9  ; DX
            JNE DH_SRC_MOVE_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            DH_SRC_MOVE_AL:
            CMP BH, 10  ; DH
            JNE DL_SRC_MOVE_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX,PLAYER1_REGS[0]
                MOV AL,BH
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            DL_SRC_MOVE_AL:
            CMP BH, 11  ; DL
            JNE SI_SRC_MOVE_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX,PLAYER1_REGS[0]
                MOV AL,BL
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            SI_SRC_MOVE_AL:
            CMP BH, 12  ; SI
            JNE DI_SRC_MOVE_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            DI_SRC_MOVE_AL:
            CMP BH, 13  ; DI
            JNE SP_SRC_MOVE_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            SP_SRC_MOVE_AL:
            CMP BH, 14  ; SP
            JNE BP_SRC_MOVE_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            BP_SRC_MOVE_AL:
            CMP BH, 15  ; BP
            JNE VAL_SRC_MOVE_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            VAL_SRC_MOVE_AL:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
            CALL CONVERT_TO_NUMBER
                MOV AX, PLAYER1_REGS[0]
                MOV AL, VALUE_BYTE
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
                
            ;------------------------------------------------------------------

        BX_DEST_MOVE:
        CMP BH, 3  ; BX
        JNE BH_DEST_MOVE
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_MOVE_BX
        CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS[2]
            MOV BX, VALUE_WORD
            MOV  PLAYER1_REGS[2], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_MOVE_BX:
            CALL GET_CURRENT_SOURCE
            
            MOV BH,0
             MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX,16D

            CHOOSE_SOURCE_MOVE_BX:
            
            CMP [DI], AX
            JE CHOSEN_SOURCE_MOVE_BX
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_MOVE_BX
        CHOSEN_SOURCE_MOVE_BX:
            AX_SRC_MOVE_BX:
            CMP BH, 0  ; AX
            JNE AH_SRC_MOVE_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[0]
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;--------------------------------------------------------------------- 
            AH_SRC_MOVE_BX:
            CMP BH, 1  ; AH
            JNE AL_SRC_MOVE_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            AL_SRC_MOVE_BX:
            CMP BH, 2  ; AL
            JNE BX_SRC_MOVE_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BX_SRC_MOVE_BX:
            CMP BH, 3  ; BX
            JNE BH_SRC_MOVE_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                NOP
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BH_SRC_MOVE_BX:
            CMP BH, 4  ; BH
            JNE BL_SRC_MOVE_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BL_SRC_MOVE_BX:
            CMP BH, 5  ; BL
            JNE CX_SRC_MOVE_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CX_SRC_MOVE_BX:
            CMP BH, 6  ; CX
            JNE CH_SRC_MOVE_BX
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CH_SRC_MOVE_BX:
            CMP BH, 7  ; CH
            JNE CL_SRC_MOVE_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CL_SRC_MOVE_BX:
            CMP BH, 8  ; CL
            JNE DX_SRC_MOVE_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            DX_SRC_MOVE_BX:
            CMP BH, 9  ; DX
            JNE DH_SRC_MOVE_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            DH_SRC_MOVE_BX:
            CMP BH, 10  ; DH
            JNE DL_SRC_MOVE_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            DL_SRC_MOVE_BX:
            CMP BH, 11  ; DL
            JNE SI_SRC_MOVE_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            SI_SRC_MOVE_BX:
            CMP BH, 12  ; SI
            JNE DI_SRC_MOVE_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[8]
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            DI_SRC_MOVE_BX:
            CMP BH, 13  ; DI
            JNE SP_SRC_MOVE_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[10]
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            SP_SRC_MOVE_BX:
            CMP BH, 14  ; SP
            JNE BP_SRC_MOVE_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[12]
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            BP_SRC_MOVE_BX:
            CMP BH, 15  ; BP
            JNE VAL_SRC_MOVE_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[14]
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            VAL_SRC_MOVE_BX:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
            CALL CONVERT_TO_NUMBER
                MOV AX, VALUE_WORD
                MOV PLAYER1_REGS[2], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------


        BH_DEST_MOVE:
        CMP BH, 4  ; BH
        JNE BL_DEST_MOVE
             CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JMP EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_MOVE_BH
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS[2]
            MOV BH, VALUE_BYTE
            MOV  PLAYER1_REGS[2], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_MOVE_BH:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX,16D

            CHOOSE_SOURCE_MOVE_BH:
            
            CMP [DI], AX
            JE CHOSEN_SOURCE_MOVE_BH
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_MOVE_BH
        CHOSEN_SOURCE_MOVE_BH:
            AX_SRC_MOVE_BH:
            CMP BH, 0  ; AX
            JNE AH_SRC_MOVE_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------ 
            AH_SRC_MOVE_BH:
            CMP BH, 1  ; AH
            JNE AL_SRC_MOVE_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[0]
                MOV BH,AH
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            AL_SRC_MOVE_BH:
            CMP BH, 2  ; AL
            JNE BX_SRC_MOVE_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[0]
                MOV BH,AL
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BX_SRC_MOVE_BH:
            CMP BH, 3  ; BX
            JNE BH_SRC_MOVE_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BH_SRC_MOVE_BH:
            CMP BH, 4  ; BH
            JNE BL_SRC_MOVE_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                NOP
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BL_SRC_MOVE_BH:
            CMP BH, 5  ; BL
            JNE CX_SRC_MOVE_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV BH,BL
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CX_SRC_MOVE_BH:
            CMP BH, 6  ; CX
            JNE CH_SRC_MOVE_BH
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CH_SRC_MOVE_BH:
            CMP BH, 7  ; CH
            JNE CL_SRC_MOVE_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[4]
                MOV BH,AH
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CL_SRC_MOVE_BH:
            CMP BH, 8  ; CL
            JNE DX_SRC_MOVE_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[4]
                MOV BH,AL
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            DX_SRC_MOVE_BH:
            CMP BH, 9  ; DX
            JNE DH_SRC_MOVE_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            DH_SRC_MOVE_BH:
            CMP BH, 10  ; DH
            JNE DL_SRC_MOVE_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[6]
                MOV BH,AH
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            DL_SRC_MOVE_BH:
            CMP BH, 11  ; DL
            JNE SI_SRC_MOVE_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[6]
                MOV BH,AL
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            SI_SRC_MOVE_BH:
            CMP BH, 12  ; SI
            JNE DI_SRC_MOVE_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            DI_SRC_MOVE_BH:
            CMP BH, 13  ; DI
            JNE SP_SRC_MOVE_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            SP_SRC_MOVE_BH:
            CMP BH, 14  ; SP
            JNE BP_SRC_MOVE_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            BP_SRC_MOVE_BH:
            CMP BH, 15  ; BP
            JNE VAL_SRC_MOVE_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            VAL_SRC_MOVE_BH:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX,PLAYER1_REGS[2]
                MOV BH, VALUE_BYTE
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
                
            ;------------------------------------------------------------------
 
;------------------------------------------------------------
        BL_DEST_MOVE:
        CMP BH, 5  ; BL
        JNE CX_DEST_MOVE
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_MOVE_BL
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS[2]
            MOV BL, VALUE_BYTE
            MOV  PLAYER1_REGS[2], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_MOVE_BL:
            CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX,16D

            CHOOSE_SOURCE_MOVE_BL:
            
            CMP [DI], AX
            JE CHOSEN_SOURCE_MOVE_BL
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_MOVE_BL
        CHOSEN_SOURCE_MOVE_BL:
            AX_SRC_MOVE_BL:
            CMP BH, 0  ; AX
            JNE AH_SRC_MOVE_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------ 
            AH_SRC_MOVE_BL:
            CMP BH, 1  ; AH
            JNE AL_SRC_MOVE_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[0]
                MOV BL,AH
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            AL_SRC_MOVE_BL:
            CMP BH, 2  ; AL
            JNE BX_SRC_MOVE_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[0]
                MOV BL,AL
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BX_SRC_MOVE_BL:
            CMP BH, 3  ; BX
            JNE BH_SRC_MOVE_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BH_SRC_MOVE_BL:
            CMP BH, 4  ; BH
            JNE BL_SRC_MOVE_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV BL,BH
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BL_SRC_MOVE_BL:
            CMP BH, 5  ; BL
            JNE CX_SRC_MOVE_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                NOP
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CX_SRC_MOVE_BL:
            CMP BH, 6  ; CX
            JNE CH_SRC_MOVE_BL
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CH_SRC_MOVE_BL:
            CMP BH, 7  ; CH
            JNE CL_SRC_MOVE_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[4]
                MOV BL,AH
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CL_SRC_MOVE_BL:
            CMP BH, 8  ; CL
            JNE DX_SRC_MOVE_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[4]
                MOV BL,AL
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            DX_SRC_MOVE_BL:
            CMP BH, 9  ; DX
            JNE DH_SRC_MOVE_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            DH_SRC_MOVE_BL:
            CMP BH, 10  ; DH
            JNE DL_SRC_MOVE_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[6]
                MOV BL,AH
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            DL_SRC_MOVE_BL:
            CMP BH, 11  ; DL
            JNE SI_SRC_MOVE_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[6]
                MOV BL,AL
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            SI_SRC_MOVE_BL:
            CMP BH, 12  ; SI
            JNE DI_SRC_MOVE_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            DI_SRC_MOVE_BL:
            CMP BH, 13  ; DI
            JNE SP_SRC_MOVE_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            SP_SRC_MOVE_BL:
            CMP BH, 14  ; SP
            JNE BP_SRC_MOVE_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            BP_SRC_MOVE_BL:
            CMP BH, 15  ; BP
            JNE VAL_SRC_MOVE_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            VAL_SRC_MOVE_BL:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX,PLAYER1_REGS[2]
                MOV BL, VALUE_BYTE
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
                
            ;------------------------------------------------------------------ 
        CX_DEST_MOVE:
        CMP BH, 6  ; CX
        JNE CH_DEST_MOVE        
             CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_MOVE_CX
            CALL CONVERT_TO_NUMBER
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[4]
            MOV BX, VALUE_WORD
            MOV  PLAYER1_REGS[4], BX
            JMP EXIT_EXECUTE
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_MOVE_CX:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
             MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX,16D

            CHOOSE_SOURCE_MOVE_CX:
            
            CMP [DI], AX
            JE CHOSEN_SOURCE_MOVE_CX
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_MOVE_CX
        CHOSEN_SOURCE_MOVE_CX:
            AX_SRC_MOVE_CX:
            CMP BH, 0  ; AX
            JNE AH_SRC_MOVE_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[0]
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            AH_SRC_MOVE_CX:
            CMP BH, 1  ; AH
            JNE AL_SRC_MOVE_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            AL_SRC_MOVE_CX:
            CMP BH, 2  ; AL
            JNE BX_SRC_MOVE_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BX_SRC_MOVE_CX:
            CMP BH, 3  ; BX
            JNE BH_SRC_MOVE_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BH_SRC_MOVE_CX:
            CMP BH, 4  ; BH
            JNE BL_SRC_MOVE_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BL_SRC_MOVE_CX:
            CMP BH, 5  ; BL
            JNE CX_SRC_MOVE_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CX_SRC_MOVE_CX:
            CMP BH, 6  ; CX
            JNE CH_SRC_MOVE_CX
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                NOP
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CH_SRC_MOVE_CX:
            CMP BH, 7  ; CH
            JNE CL_SRC_MOVE_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CL_SRC_MOVE_CX:
            CMP BH, 8  ; CL
            JNE DX_SRC_MOVE_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            DX_SRC_MOVE_CX:
            CMP BH, 9  ; DX
            JNE DH_SRC_MOVE_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            DH_SRC_MOVE_CX:
            CMP BH, 10  ; DH
            JNE DL_SRC_MOVE_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            DL_SRC_MOVE_CX:
            CMP BH, 11  ; DL
            JNE SI_SRC_MOVE_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            SI_SRC_MOVE_CX:
            CMP BH, 12  ; SI
            JNE DI_SRC_MOVE_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[8]
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            DI_SRC_MOVE_CX:
            CMP BH, 13  ; DI
            JNE SP_SRC_MOVE_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[10]
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            SP_SRC_MOVE_CX:
            CMP BH, 14  ; SP
            JNE BP_SRC_MOVE_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[12]
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            BP_SRC_MOVE_CX:
            CMP BH, 15  ; BP
            JNE VAL_SRC_MOVE_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[14]
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            VAL_SRC_MOVE_CX:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV AX, VALUE_WORD
                MOV PLAYER1_REGS[4],AX
                JMP EXIT_EXECUTE
                
            ;------------------------------------------------------------------
        CH_DEST_MOVE:
        CMP BH, 7  ; CH
        JNE CL_DEST_MOVE
             CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JMP EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_MOVE_CH
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS[4]
            MOV BH, VALUE_BYTE
            MOV  PLAYER1_REGS[4], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_MOVE_CH:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX,16D

            CHOOSE_SOURCE_MOVE_CH:
            CMP [DI], AX
            JE CHOSEN_SOURCE_MOVE_CH
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_MOVE_CH
        CHOSEN_SOURCE_MOVE_CH:
            AX_SRC_MOVE_CH:
            CMP BH, 0  ; AX
            JNE AH_SRC_MOVE_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------ 
            AH_SRC_MOVE_CH:
            CMP BH, 1  ; AH
            JNE AL_SRC_MOVE_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[0]
                MOV BH,AH
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            AL_SRC_MOVE_CH:
            CMP BH, 2  ; AL
            JNE BX_SRC_MOVE_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[0]
                MOV BH,AL
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BX_SRC_MOVE_CH:
            CMP BH, 3  ; BX
            JNE BH_SRC_MOVE_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BH_SRC_MOVE_CH:
            CMP BH, 4  ; BH
            JNE BL_SRC_MOVE_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[2]
                MOV BH,AH
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BL_SRC_MOVE_CH:
            CMP BH, 5  ; BL
            JNE CX_SRC_MOVE_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[2]
                MOV BH,AL
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CX_SRC_MOVE_CH:
            CMP BH, 6  ; CX
            JNE CH_SRC_MOVE_CH
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CH_SRC_MOVE_CH:
            CMP BH, 7  ; CH
            JNE CL_SRC_MOVE_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                NOP
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CL_SRC_MOVE_CH:
            CMP BH, 8  ; CL
            JNE DX_SRC_MOVE_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV BH,BL
                MOV PLAYER1_REGS[2],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            DX_SRC_MOVE_CH:
            CMP BH, 9  ; DX
            JNE DH_SRC_MOVE_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            DH_SRC_MOVE_CH:
            CMP BH, 10  ; DH
            JNE DL_SRC_MOVE_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[6]
                MOV BH,AH
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            DL_SRC_MOVE_CH:
            CMP BH, 11  ; DL
            JNE SI_SRC_MOVE_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[6]
                MOV BH,AL
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            SI_SRC_MOVE_CH:
            CMP BH, 12  ; SI
            JNE DI_SRC_MOVE_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            DI_SRC_MOVE_CH:
            CMP BH, 13  ; DI
            JNE SP_SRC_MOVE_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            SP_SRC_MOVE_CH:
            CMP BH, 14  ; SP
            JNE BP_SRC_MOVE_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            BP_SRC_MOVE_CH:
            CMP BH, 15  ; BP
            JNE VAL_SRC_MOVE_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            VAL_SRC_MOVE_CH:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX,PLAYER1_REGS[4]
                MOV BH, VALUE_BYTE
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
                
            ;------------------------------------------------------------------

        CL_DEST_MOVE:    
        CMP BH, 8  ; CL
        JNE DX_DEST_MOVE
             CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_MOVE_CL
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS [4]
            MOV BL, VALUE_BYTE
            MOV  PLAYER1_REGS[4], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_MOVE_CL:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX,16D

            CHOOSE_SOURCE_MOVE_CL:
            
            CMP [DI], AX
            JE CHOSEN_SOURCE_MOVE_CL
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_MOVE_CL
        CHOSEN_SOURCE_MOVE_CL:
            AX_SRC_MOVE_CL:
            CMP BH, 0  ; AX
            JNE AH_SRC_MOVE_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------ 
            AH_SRC_MOVE_CL:
            CMP BH, 1  ; AH
            JNE AL_SRC_MOVE_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[0]
                MOV BL,AH
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            AL_SRC_MOVE_CL:
            CMP BH, 2  ; AL
            JNE BX_SRC_MOVE_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[0]
                MOV BL,AL
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BX_SRC_MOVE_CL:
            CMP BH, 3  ; BX
            JNE BH_SRC_MOVE_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BH_SRC_MOVE_CL:
            CMP BH, 4  ; BH
            JNE BL_SRC_MOVE_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[2]
                MOV BL,AH
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BL_SRC_MOVE_CL:
            CMP BH, 5  ; BL
            JNE CX_SRC_MOVE_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[2]
                MOV BL,AL
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CX_SRC_MOVE_CL:
            CMP BH, 6  ; CX
            JNE CH_SRC_MOVE_CL
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CH_SRC_MOVE_CL:
            CMP BH, 7  ; CH
            JNE CL_SRC_MOVE_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV BL,BH
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CL_SRC_MOVE_CL:
            CMP BH, 8  ; CL
            JNE DX_SRC_MOVE_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                NOP
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            DX_SRC_MOVE_CL:
            CMP BH, 9  ; DX
            JNE DH_SRC_MOVE_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            DH_SRC_MOVE_CL:
            CMP BH, 10  ; DH
            JNE DL_SRC_MOVE_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[6]
                MOV BL,AH
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            DL_SRC_MOVE_CL:
            CMP BH, 11  ; DL
            JNE SI_SRC_MOVE_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[6]
                MOV BL,AL
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            SI_SRC_MOVE_CL:
            CMP BH, 12  ; SI
            JNE DI_SRC_MOVE_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            DI_SRC_MOVE_CL:
            CMP BH, 13  ; DI
            JNE SP_SRC_MOVE_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            SP_SRC_MOVE_CL:
            CMP BH, 14  ; SP
            JNE BP_SRC_MOVE_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            BP_SRC_MOVE_CL:
            CMP BH, 15  ; BP
            JNE VAL_SRC_MOVE_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            VAL_SRC_MOVE_CL:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX,PLAYER1_REGS[4]
                MOV BL, VALUE_BYTE
                MOV PLAYER1_REGS[4],BX
                JMP EXIT_EXECUTE
                
            ;------------------------------------------------------------------
        DX_DEST_MOVE:
        CMP BH, 9  ; DX
        JNE DH_DEST_MOVE
             CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_MOVE_DX
            CALL CONVERT_TO_NUMBER
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[6]
            MOV BX, VALUE_WORD
            MOV  PLAYER1_REGS[6], BX
            JMP EXIT_EXECUTE
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_MOVE_DX:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
             MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX,16D

            CHOOSE_SOURCE_MOVE_DX:
            
            CMP [DI], AX
            JE CHOSEN_SOURCE_MOVE_DX
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_MOVE_DX
        CHOSEN_SOURCE_MOVE_DX:
            AX_SRC_MOVE_DX:
            CMP BH, 0  ; AX
            JNE AH_SRC_MOVE_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[0]
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            AH_SRC_MOVE_DX:
            CMP BH, 1  ; AH
            JNE AL_SRC_MOVE_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            AL_SRC_MOVE_DX:
            CMP BH, 2  ; AL
            JNE BX_SRC_MOVE_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BX_SRC_MOVE_DX:
            CMP BH, 3  ; BX
            JNE BH_SRC_MOVE_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[2]
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BH_SRC_MOVE_DX:
            CMP BH, 4  ; BH
            JNE BL_SRC_MOVE_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BL_SRC_MOVE_DX:
            CMP BH, 5  ; BL
            JNE CX_SRC_MOVE_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CX_SRC_MOVE_DX:
            CMP BH, 6  ; CX
            JNE CH_SRC_MOVE_DX
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[4]
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CH_SRC_MOVE_DX:
            CMP BH, 7  ; CH
            JNE CL_SRC_MOVE_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CL_SRC_MOVE_DX:
            CMP BH, 8  ; CL
            JNE DX_SRC_MOVE_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            DX_SRC_MOVE_DX:
            CMP BH, 9  ; DX
            JNE DH_SRC_MOVE_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                NOP
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            DH_SRC_MOVE_DX:
            CMP BH, 10  ; DH
            JNE DL_SRC_MOVE_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            DL_SRC_MOVE_DX:
            CMP BH, 11  ; DL
            JNE SI_SRC_MOVE_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            SI_SRC_MOVE_DX:
            CMP BH, 12  ; SI
            JNE DI_SRC_MOVE_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[8]
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            DI_SRC_MOVE_DX:
            CMP BH, 13  ; DI
            JNE SP_SRC_MOVE_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[10]
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            SP_SRC_MOVE_DX:
            CMP BH, 14  ; SP
            JNE BP_SRC_MOVE_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[12]
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            BP_SRC_MOVE_DX:
            CMP BH, 15  ; BP
            JNE VAL_SRC_MOVE_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[14]
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            VAL_SRC_MOVE_DX:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV AX, VALUE_WORD
                MOV PLAYER1_REGS[6],AX
                JMP EXIT_EXECUTE
                
            ;------------------------------------------------------------------

        DH_DEST_MOVE:
        CMP BH, 10 ; DH
        JNE DL_DEST_MOVE
             CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JMP EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_MOVE_DH
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS[6]
            MOV BH, VALUE_BYTE
            MOV  PLAYER1_REGS[6], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_MOVE_DH:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX,16D

            CHOOSE_SOURCE_MOVE_DH:
           
            CMP [DI], AX
            JE CHOSEN_SOURCE_MOVE_DH
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_MOVE_DH
        CHOSEN_SOURCE_MOVE_DH:
            AX_SRC_MOVE_DH:
            CMP BH, 0  ; AX
            JNE AH_SRC_MOVE_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------ 
            AH_SRC_MOVE_DH:
            CMP BH, 1  ; AH
            JNE AL_SRC_MOVE_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[0]
                MOV BH,AH
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            AL_SRC_MOVE_DH:
            CMP BH, 2  ; AL
            JNE BX_SRC_MOVE_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[0]
                MOV BH,AL
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BX_SRC_MOVE_DH:
            CMP BH, 3  ; BX
            JNE BH_SRC_MOVE_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BH_SRC_MOVE_DH:
            CMP BH, 4  ; BH
            JNE BL_SRC_MOVE_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[2]
                MOV BH,AH
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BL_SRC_MOVE_DH:
            CMP BH, 5  ; BL
            JNE CX_SRC_MOVE_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[2]
                MOV BH,AL
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CX_SRC_MOVE_DH:
            CMP BH, 6  ; CX
            JNE CH_SRC_MOVE_DH
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CH_SRC_MOVE_DH:
            CMP BH, 7  ; CH
            JNE CL_SRC_MOVE_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[4]
                MOV BH,AH
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CL_SRC_MOVE_DH:
            CMP BH, 8  ; CL
            JNE DX_SRC_MOVE_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[4]
                MOV BH,AL
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            DX_SRC_MOVE_DH:
            CMP BH, 9  ; DX
            JNE DH_SRC_MOVE_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            DH_SRC_MOVE_DH:
            CMP BH, 10  ; DH
            JNE DL_SRC_MOVE_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                NOP
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            DL_SRC_MOVE_DH:
            CMP BH, 11  ; DL
            JNE SI_SRC_MOVE_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV BH,BL
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            SI_SRC_MOVE_DH:
            CMP BH, 12  ; SI
            JNE DI_SRC_MOVE_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            DI_SRC_MOVE_DH:
            CMP BH, 13  ; DI
            JNE SP_SRC_MOVE_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            SP_SRC_MOVE_DH:
            CMP BH, 14  ; SP
            JNE BP_SRC_MOVE_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            BP_SRC_MOVE_DH:
            CMP BH, 15  ; BP
            JNE VAL_SRC_MOVE_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            VAL_SRC_MOVE_DH:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX,PLAYER1_REGS[6]
                MOV BH, VALUE_BYTE
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
                
            ;------------------------------------------------------------------
        DL_DEST_MOVE:
        CMP BH, 11 ; DL
        JNE SI_DEST_MOVE
             CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_MOVE_DL
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS [6]
            MOV BL, VALUE_BYTE
            MOV  PLAYER1_REGS[6], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_MOVE_DL:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX,16D

            CHOOSE_SOURCE_MOVE_DL:
            
            CMP [DI], AX
            JE CHOSEN_SOURCE_MOVE_DL
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_MOVE_DL
        CHOSEN_SOURCE_MOVE_DL:
            AX_SRC_MOVE_DL:
            CMP BH, 0  ; AX
            JNE AH_SRC_MOVE_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------ 
            AH_SRC_MOVE_DL:
            CMP BH, 1  ; AH
            JNE AL_SRC_MOVE_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[0]
                MOV BL,AH
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            AL_SRC_MOVE_DL:
            CMP BH, 2  ; AL
            JNE BX_SRC_MOVE_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[0]
                MOV BL,AL
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BX_SRC_MOVE_DL:
            CMP BH, 3  ; BX
            JNE BH_SRC_MOVE_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BH_SRC_MOVE_DL:
            CMP BH, 4  ; BH
            JNE BL_SRC_MOVE_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[2]
                MOV BL,AH
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BL_SRC_MOVE_DL:
            CMP BH, 5  ; BL
            JNE CX_SRC_MOVE_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[2]
                MOV BL,AL
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CX_SRC_MOVE_DL:
            CMP BH, 6  ; CX
            JNE CH_SRC_MOVE_DL
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CH_SRC_MOVE_DL:
            CMP BH, 7  ; CH
            JNE CL_SRC_MOVE_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[4]
                MOV BL,AH
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CL_SRC_MOVE_DL:
            CMP BH, 8  ; CL
            JNE DX_SRC_MOVE_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[4]
                MOV BL,AL
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            DX_SRC_MOVE_DL:
            CMP BH, 9  ; DX
            JNE DH_SRC_MOVE_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            DH_SRC_MOVE_DL:
            CMP BH, 10  ; DH
            JNE DL_SRC_MOVE_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV BL,BH
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            DL_SRC_MOVE_DL:
            CMP BH, 11  ; DL
            JNE SI_SRC_MOVE_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                NOP
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            SI_SRC_MOVE_DL:
            CMP BH, 12  ; SI
            JNE DI_SRC_MOVE_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            DI_SRC_MOVE_DL:
            CMP BH, 13  ; DI
            JNE SP_SRC_MOVE_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            SP_SRC_MOVE_DL:
            CMP BH, 14  ; SP
            JNE BP_SRC_MOVE_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            BP_SRC_MOVE_DL:
            CMP BH, 15  ; BP
            JNE VAL_SRC_MOVE_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            VAL_SRC_MOVE_DL:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX,PLAYER1_REGS[6]
                MOV BL, VALUE_BYTE
                MOV PLAYER1_REGS[6],BX
                JMP EXIT_EXECUTE
                
            ;------------------------------------------------------------------
        SI_DEST_MOVE:
        CMP BH, 12 ; SI
        JNE DI_DEST_MOVE
             CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_MOVE_SI
            CALL CONVERT_TO_NUMBER
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[8]
            MOV BX, VALUE_WORD
            MOV PLAYER1_REGS[8], BX
            JMP EXIT_EXECUTE
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_MOVE_SI:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
             MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX,16D

            CHOOSE_SOURCE_MOVE_SI:
            
            CMP [DI], AX
            JE CHOSEN_SOURCE_MOVE_SI
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_MOVE_SI
        CHOSEN_SOURCE_MOVE_SI:
            AX_SRC_MOVE_SI:
            CMP BH, 0  ; AX
            JNE AH_SRC_MOVE_SI
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[0]
                MOV PLAYER1_REGS[8],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            AH_SRC_MOVE_SI:
            CMP BH, 1  ; AH
            JNE AL_SRC_MOVE_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            AL_SRC_MOVE_SI:
            CMP BH, 2  ; AL
            JNE BX_SRC_MOVE_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BX_SRC_MOVE_SI:
            CMP BH, 3  ; BX
            JNE BH_SRC_MOVE_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[2]
                MOV PLAYER1_REGS[8],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BH_SRC_MOVE_SI:
            CMP BH, 4  ; BH
            JNE BL_SRC_MOVE_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BL_SRC_MOVE_SI:
            CMP BH, 5  ; BL
            JNE CX_SRC_MOVE_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CX_SRC_MOVE_SI:
            CMP BH, 6  ; CX
            JNE CH_SRC_MOVE_SI
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[4]
                MOV PLAYER1_REGS[8],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CH_SRC_MOVE_SI:
            CMP BH, 7  ; CH
            JNE CL_SRC_MOVE_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CL_SRC_MOVE_SI:
            CMP BH, 8  ; CL
            JNE DX_SRC_MOVE_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            DX_SRC_MOVE_SI:
            CMP BH, 9  ; DX
            JNE DH_SRC_MOVE_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               MOV BX,PLAYER1_REGS[6]
                MOV PLAYER1_REGS[8],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            DH_SRC_MOVE_SI:
            CMP BH, 10  ; DH
            JNE DL_SRC_MOVE_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            DL_SRC_MOVE_SI:
            CMP BH, 11  ; DL
            JNE SI_SRC_MOVE_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            SI_SRC_MOVE_SI:
            CMP BH, 12  ; SI
            JNE DI_SRC_MOVE_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                NOP
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            DI_SRC_MOVE_SI:
            CMP BH, 13  ; DI
            JNE SP_SRC_MOVE_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[10]
                MOV PLAYER1_REGS[8],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            SP_SRC_MOVE_SI:
            CMP BH, 14  ; SP
            JNE BP_SRC_MOVE_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               MOV BX,PLAYER1_REGS[12]
                MOV PLAYER1_REGS[8],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            BP_SRC_MOVE_SI:
            CMP BH, 15  ; BP
            JNE VAL_SRC_MOVE_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[14]
                MOV PLAYER1_REGS[8],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            VAL_SRC_MOVE_SI:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV AX, VALUE_WORD
                MOV PLAYER1_REGS[8],AX
                JMP EXIT_EXECUTE
                
            ;------------------------------------------------------------------

        DI_DEST_MOVE:
        CMP BH, 13 ; DI
        JNE SP_DEST_MOVE
             CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_MOVE_DI
            CALL CONVERT_TO_NUMBER
;----------------------EXECUTE COMMAND----------------------- 
            MOV AX, VALUE_WORD
            MOV PLAYER1_REGS[10], AX
            JMP EXIT_EXECUTE
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_MOVE_DI:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX,16D

            CHOOSE_SOURCE_MOVE_DI:
            CMP [DI], AX
            JE CHOSEN_SOURCE_MOVE_DI
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_MOVE_DI
        CHOSEN_SOURCE_MOVE_DI:
            AX_SRC_MOVE_DI:
            CMP BH, 0  ; AX
            JNE AH_SRC_MOVE_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[0]
                MOV PLAYER1_REGS[10],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            AH_SRC_MOVE_DI:
            CMP BH, 1  ; AH
            JNE AL_SRC_MOVE_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            AL_SRC_MOVE_DI:
            CMP BH, 2  ; AL
            JNE BX_SRC_MOVE_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BX_SRC_MOVE_DI:
            CMP BH, 3  ; BX
            JNE BH_SRC_MOVE_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[2]
                MOV PLAYER1_REGS[10],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BH_SRC_MOVE_DI:
            CMP BH, 4  ; BH
            JNE BL_SRC_MOVE_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BL_SRC_MOVE_DI:
            CMP BH, 5  ; BL
            JNE CX_SRC_MOVE_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CX_SRC_MOVE_DI:
            CMP BH, 6  ; CX
            JNE CH_SRC_MOVE_DI
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[4]
                MOV PLAYER1_REGS[10],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CH_SRC_MOVE_DI:
            CMP BH, 7  ; CH
            JNE CL_SRC_MOVE_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CL_SRC_MOVE_DI:
            CMP BH, 8  ; CL
            JNE DX_SRC_MOVE_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            DX_SRC_MOVE_DI:
            CMP BH, 9  ; DX
            JNE DH_SRC_MOVE_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[6]
                MOV PLAYER1_REGS[10],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            DH_SRC_MOVE_DI:
            CMP BH, 10  ; DH
            JNE DL_SRC_MOVE_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            DL_SRC_MOVE_DI:
            CMP BH, 11  ; DL
            JNE SI_SRC_MOVE_DI

            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            SI_SRC_MOVE_DI:
            CMP BH, 12  ; SI
            JNE DI_SRC_MOVE_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[8]
                MOV PLAYER1_REGS[10],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            DI_SRC_MOVE_DI:
            CMP BH, 13  ; DI
            JNE SP_SRC_MOVE_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                NOP
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            SP_SRC_MOVE_DI:
            CMP BH, 14  ; SP
            JNE BP_SRC_MOVE_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[12]
                MOV PLAYER1_REGS[10],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            BP_SRC_MOVE_DI:
            CMP BH, 15  ; BP
            JNE VAL_SRC_MOVE_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[14]
                MOV PLAYER1_REGS[10],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            VAL_SRC_MOVE_DI:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV AX, VALUE_WORD
                MOV PLAYER1_REGS[10],AX
                JMP EXIT_EXECUTE
                
            ;------------------------------------------------------------------
        SP_DEST_MOVE:
        CMP BH, 14 ; SP
        JNE BP_DEST_MOVE
             CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_MOVE_SP
            CALL CONVERT_TO_NUMBER
;----------------------EXECUTE COMMAND----------------------- 
            MOV AX, VALUE_WORD
            MOV PLAYER1_REGS[12], AX
            JMP EXIT_EXECUTE
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_MOVE_SP:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
             MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX,16D

            CHOOSE_SOURCE_MOVE_SP:
           
            CMP [DI], AX
            JE CHOSEN_SOURCE_MOVE_SP
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_MOVE_SP
        CHOSEN_SOURCE_MOVE_SP:
            AX_SRC_MOVE_SP:
            CMP BH, 0  ; AX
            JNE AH_SRC_MOVE_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[0]
                MOV PLAYER1_REGS[12],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            AH_SRC_MOVE_SP:
            CMP BH, 1  ; AH
            JNE AL_SRC_MOVE_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            AL_SRC_MOVE_SP:
            CMP BH, 2  ; AL
            JNE BX_SRC_MOVE_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BX_SRC_MOVE_SP:
            CMP BH, 3  ; BX
            JNE BH_SRC_MOVE_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 MOV BX,PLAYER1_REGS[2]
                MOV PLAYER1_REGS[12],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BH_SRC_MOVE_SP:
            CMP BH, 4  ; BH
            JNE BL_SRC_MOVE_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BL_SRC_MOVE_SP:
            CMP BH, 5  ; BL
            JNE CX_SRC_MOVE_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CX_SRC_MOVE_SP:
            CMP BH, 6  ; CX
            JNE CH_SRC_MOVE_SP
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 MOV BX,PLAYER1_REGS[4]
                MOV PLAYER1_REGS[12],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CH_SRC_MOVE_SP:
            CMP BH, 7  ; CH
            JNE CL_SRC_MOVE_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CL_SRC_MOVE_SP:
            CMP BH, 8  ; CL
            JNE DX_SRC_MOVE_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            DX_SRC_MOVE_SP:
            CMP BH, 9  ; DX
            JNE DH_SRC_MOVE_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 MOV BX,PLAYER1_REGS[6]
                MOV PLAYER1_REGS[12],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            DH_SRC_MOVE_SP:
            CMP BH, 10  ; DH
            JNE DL_SRC_MOVE_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            DL_SRC_MOVE_SP:
            CMP BH, 11  ; DL
            JNE SI_SRC_MOVE_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            SI_SRC_MOVE_SP:
            CMP BH, 12  ; SI
            JNE DI_SRC_MOVE_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 MOV BX,PLAYER1_REGS[8]
                MOV PLAYER1_REGS[12],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            DI_SRC_MOVE_SP:
            CMP BH, 13  ; DI
            JNE SP_SRC_MOVE_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 MOV BX,PLAYER1_REGS[10]
                MOV PLAYER1_REGS[12],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            SP_SRC_MOVE_SP:
            CMP BH, 14  ; SP
            JNE BP_SRC_MOVE_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                NOP
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            BP_SRC_MOVE_SP:
            CMP BH, 15  ; BP
            JNE VAL_SRC_MOVE_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 MOV BX,PLAYER1_REGS[14]
                MOV PLAYER1_REGS[12],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            VAL_SRC_MOVE_SP:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV AX, VALUE_WORD
                MOV PLAYER1_REGS[12],AX
                JMP EXIT_EXECUTE
                
            ;------------------------------------------------------------------
            
        BP_DEST_MOVE:
        CMP BH, 15 ; BP
        JNE EXIT_SYNTAX
             CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_MOVE_BP
            CALL CONVERT_TO_NUMBER
;----------------------EXECUTE COMMAND----------------------- 
            MOV AX, VALUE_WORD
            MOV PLAYER1_REGS[14], AX
            JMP EXIT_EXECUTE
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_MOVE_BP:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
             MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX,16D

            CHOOSE_SOURCE_MOVE_BP:
            CMP [DI], AX
            JE CHOSEN_SOURCE_MOVE_BP
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_MOVE_BP
        CHOSEN_SOURCE_MOVE_BP:
           AX_SRC_MOVE_BP:
            CMP BH, 0  ; AX
            JNE AH_SRC_MOVE_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[0]
                MOV PLAYER1_REGS[14],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------ 
            AH_SRC_MOVE_BP:
            CMP BH, 1  ; AH
            JNE AL_SRC_MOVE_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            AL_SRC_MOVE_BP:
            CMP BH, 2  ; AL
            JNE BX_SRC_MOVE_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BX_SRC_MOVE_BP:
            CMP BH, 3  ; BX
            JNE BH_SRC_MOVE_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[2]
                MOV PLAYER1_REGS[14],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BH_SRC_MOVE_BP:
            CMP BH, 4  ; BH
            JNE BL_SRC_MOVE_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BL_SRC_MOVE_BP:
            CMP BH, 5  ; BL
            JNE CX_SRC_MOVE_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CX_SRC_MOVE_BP:
            CMP BH, 6  ; CX
            JNE CH_SRC_MOVE_BP
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[4]
                MOV PLAYER1_REGS[14],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CH_SRC_MOVE_BP:
            CMP BH, 7  ; CH
            JNE CL_SRC_MOVE_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CL_SRC_MOVE_BP:
            CMP BH, 8  ; CL
            JNE DX_SRC_MOVE_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            DX_SRC_MOVE_BP:
            CMP BH, 9  ; DX
            JNE DH_SRC_MOVE_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[6]
                MOV PLAYER1_REGS[14],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            DH_SRC_MOVE_BP:
            CMP BH, 10  ; DH
            JNE DL_SRC_MOVE_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            DL_SRC_MOVE_BP:
            CMP BH, 11  ; DL
            JNE SI_SRC_MOVE_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            SI_SRC_MOVE_BP:
            CMP BH, 12  ; SI
            JNE DI_SRC_MOVE_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[8]
                MOV PLAYER1_REGS[14],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            DI_SRC_MOVE_BP:
            CMP BH, 13  ; DI
            JNE SP_SRC_MOVE_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[10]
                MOV PLAYER1_REGS[14],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            SP_SRC_MOVE_BP:
            CMP BH, 14  ; SP
            JNE BP_SRC_MOVE_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[12]
                MOV PLAYER1_REGS[14],BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            BP_SRC_MOVE_BP:
            CMP BH, 15  ; BP
            JNE VAL_SRC_MOVE_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                NOP
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            VAL_SRC_MOVE_BP:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV AX, VALUE_WORD
                MOV PLAYER1_REGS[14],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

;----------------------------------------------ADDITION COMMAND-----------------------------------------------
ADDITION:
CMP BH, 1  ;ADD
JNE SUBTRACTION
    CALL CHECK_FOR_SPACE
    CMP ERROR, 1
    JE EXIT_SYNTAX

    CALL GET_CURRENT_DESTINATION
    MOV AX, CURRENT_DESTINATION
    MOV DI, OFFSET REGISTERS_CODES
    MOV CX, 16

    CHOOSE_DESTINANTION_ADDITION:
        CMP [DI], AX
        JE CHOSEN_DESTINATION_ADDITION
        INC BH   
        INC DI
        INC DI
        LOOP CHOOSE_DESTINANTION_ADDITION
        JMP EXIT_SYNTAX

CHOSEN_DESTINATION_ADDITION:
      AX_DEST_ADDITION:
        CMP BH, 0  ; AX
        JNE AH_DEST_ADDITION
        CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_ADDITION_AX
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS[0]
            MOV AX, VALUE_WORD
            ADD BX, AX
            MOV PLAYER1_REGS[0], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_ADDITION_AX:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_ADDITION_AX:

            CMP [DI], AX
            JE CHOSEN_SOURCE_ADDITION_AX
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_ADDITION_AX

        CHOSEN_SOURCE_ADDITION_AX:
          AX_SRC_ADDITION_AX:
            CMP BH, 0  ; AX
            JNE AH_SRC_ADDITION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[0]
                ADD BX, BX
                MOV PLAYER1_REGS[0], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------ 
          AH_SRC_ADDITION_AX:
            CMP BH, 1  ; AH
            JNE AL_SRC_ADDITION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          AL_SRC_ADDITION_AX:
            CMP BH, 2  ; AL
            JNE BX_SRC_ADDITION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BX_SRC_ADDITION_AX:
            CMP BH, 3  ; BX
            JNE BH_SRC_ADDITION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[0]
                ADD AX, BX
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BH_SRC_ADDITION_AX:
            CMP BH, 4  ; BH
            JNE BL_SRC_ADDITION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BL_SRC_ADDITION_AX:
            CMP BH, 5  ; BL
            JNE CX_SRC_ADDITION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CX_SRC_ADDITION_AX:
            CMP BH, 6  ; CX
            JNE CH_SRC_ADDITION_AX
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[0]
                ADD AX, BX
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CH_SRC_ADDITION_AX:
            CMP BH, 7  ; CH
            JNE CL_SRC_ADDITION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CL_SRC_ADDITION_AX:
            CMP BH, 8  ; CL
            JNE DX_SRC_ADDITION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DX_SRC_ADDITION_AX:
            CMP BH, 9  ; DX
            JNE DH_SRC_ADDITION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[0]
                ADD AX, BX
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DH_SRC_ADDITION_AX:
            CMP BH, 10  ; DH
            JNE DL_SRC_ADDITION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DL_SRC_ADDITION_AX:
            CMP BH, 11  ; DL
            JNE SI_SRC_ADDITION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          SI_SRC_ADDITION_AX:
            CMP BH, 12  ; SI
            JNE DI_SRC_ADDITION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[8]
                MOV AX, PLAYER1_REGS[0]
                ADD AX, BX
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DI_SRC_ADDITION_AX:
            CMP BH, 13  ; DI
            JNE SP_SRC_ADDITION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[10]
                MOV AX, PLAYER1_REGS[0]
                ADD AX, BX
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          SP_SRC_ADDITION_AX:
            CMP BH, 14  ; SP
            JNE BP_SRC_ADDITION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[12]
                MOV AX, PLAYER1_REGS[0]
                ADD AX, BX
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          BP_SRC_ADDITION_AX:
            CMP BH, 15  ; BP
            JNE VAL_SRC_ADDITION_AX 
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[14]
                MOV AX, PLAYER1_REGS[0]
                ADD AX, BX
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          VAL_SRC_ADDITION_AX:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX, VALUE_WORD
                MOV AX, PLAYER1_REGS[0]
                ADD AX, BX
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
       
       AH_DEST_ADDITION:
       CMP BH, 1  ; AH
        JNE AL_DEST_ADDITION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,4
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_ADDITION_AH
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS[0]
            MOV AH, VALUE_BYTE
            ADD BH, AH
            MOV  PLAYER1_REGS[0], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_ADDITION_AH:
             CALL GET_CURRENT_SOURCE
            
            MOV BH, 0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_ADDITION_AH:
            CMP [DI], AX
            JE CHOSEN_SOURCE_ADDITION_AH
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_ADDITION_AH


        CHOSEN_SOURCE_ADDITION_AH:
          AX_SRC_ADDITION_AH:
            CMP BH, 0  ; AX
            JNE AH_SRC_ADDITION_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------ 
          AH_SRC_ADDITION_AH:
            CMP BH, 1  ; AH
            JNE AL_SRC_ADDITION_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[0]
                ADD BH, BH
                MOV PLAYER1_REGS[0], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          AL_SRC_ADDITION_AH:
            CMP BH, 2  ; AL
            JNE BX_SRC_ADDITION_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[0]
                ADD BH, BL
                MOV PLAYER1_REGS[0], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BX_SRC_ADDITION_AH:
            CMP BH, 3  ; BX
            JNE BH_SRC_ADDITION_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;----------------------------------------------------------------------

          BH_SRC_ADDITION_AH:
            CMP BH, 4  ; BH
            JNE BL_SRC_ADDITION_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[2] 
                ADD AH, BH
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BL_SRC_ADDITION_AH:
            CMP BH, 5  ; BL
            JNE CX_SRC_ADDITION_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[2] 
                ADD AH, BL
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CX_SRC_ADDITION_AH:
            CMP BH, 6  ; CX
            JNE CH_SRC_ADDITION_AH
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CH_SRC_ADDITION_AH:
            CMP BH, 7  ; CH
            JNE CL_SRC_ADDITION_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[4]
                ADD AH, BH
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CL_SRC_ADDITION_AH:
            CMP BH, 8  ; CL
            JNE DX_SRC_ADDITION_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[4]
                ADD AH, BL
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------

          DX_SRC_ADDITION_AH:
            CMP BH, 9  ; DX
            JNE DH_SRC_ADDITION_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------
          DH_SRC_ADDITION_AH:
            CMP BH, 10  ; DH
            JNE DL_SRC_ADDITION_AH
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[6]
                ADD AH, BH
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          DL_SRC_ADDITION_AH:
            CMP BH, 11  ; DL
            JNE SI_SRC_ADDITION_AH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[6]
                ADD AH, BL
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          SI_SRC_ADDITION_AH:
            CMP BH, 12  ; SI
            JNE DI_SRC_ADDITION_AH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          DI_SRC_ADDITION_AH:
            CMP BH, 13  ; DI
            JNE SP_SRC_ADDITION_AH
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          SP_SRC_ADDITION_AH:
            CMP BH, 14  ; SP
            JNE BP_SRC_ADDITION_AH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          BP_SRC_ADDITION_AH:
            CMP BH, 15  ; BP
            JNE VAL_SRC_ADDITION_AH
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          VAL_SRC_ADDITION_AH:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
            CALL CONVERT_TO_NUMBER
                MOV BX ,PLAYER1_REGS[0]
                MOV AH, VALUE_BYTE
                ADD BH, AH
                MOV PLAYER1_REGS[0], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

AL_DEST_ADDITION:
        CMP BH, 2  ; AL
        JNE BX_DEST_ADDITION
        CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_ADDITION_AL
        CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS[0]
            MOV AH, VALUE_BYTE
            ADD BL, AH
            MOV PLAYER1_REGS[0], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_ADDITION_AL:
             CALL GET_CURRENT_SOURCE
            
            MOV BH, 0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_ADDITION_AL:
            CMP [DI], AX
            JE CHOSEN_SOURCE_ADDITION_AL
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_ADDITION_AL

        CHOSEN_SOURCE_ADDITION_AL:
            AX_SRC_ADDITION_AL:
            CMP BH, 0  ; AX
            JNE AH_SRC_ADDITION_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;----------------------------------------------------------------------- 
            AH_SRC_ADDITION_AL:
            CMP BH, 1  ; AH
            JNE AL_SRC_ADDITION_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[0]
                ADD BL, BH
                MOV PLAYER1_REGS[0], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            AL_SRC_ADDITION_AL:
            CMP BH, 2  ; AL
            JNE BX_SRC_ADDITION_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[0]
                ADD BL, BL
                MOV PLAYER1_REGS[0], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BX_SRC_ADDITION_AL:
            CMP BH, 3  ; BX
            JNE BH_SRC_ADDITION_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BH_SRC_ADDITION_AL:
            CMP BH, 4  ; BH
            JNE BL_SRC_ADDITION_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[2] 
                ADD AL, BH
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BL_SRC_ADDITION_AL:
            CMP BH, 5  ; BL
            JNE CX_SRC_ADDITION_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[2] 
                ADD AL, BL
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CX_SRC_ADDITION_AL:
            CMP BH, 6  ; CX
            JNE CH_SRC_ADDITION_AL
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CH_SRC_ADDITION_AL:
            CMP BH, 7  ; CH
            JNE CL_SRC_ADDITION_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[4]
                ADD AL, BH
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CL_SRC_ADDITION_AL:
            CMP BH, 8  ; CL
            JNE DX_SRC_ADDITION_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[4]
                ADD AL, BL
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------

            DX_SRC_ADDITION_AL:
            CMP BH, 9  ; DX
            JNE DH_SRC_ADDITION_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

            DH_SRC_ADDITION_AL:
            CMP BH, 10  ; DH
            JNE DL_SRC_ADDITION_AL
            ;----------------------EXECUTE COMMAND REG TO REG------------------------ 
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[6]
                ADD AL, BH
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            DL_SRC_ADDITION_AL:
            CMP BH, 11  ; DL
            JNE SI_SRC_ADDITION_AL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[6]
                ADD AL, BL
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            SI_SRC_ADDITION_AL:
            CMP BH, 12  ; SI
            JNE DI_SRC_ADDITION_AL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            DI_SRC_ADDITION_AL:
            CMP BH, 13  ; DI
            JNE SP_SRC_ADDITION_AL
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            SP_SRC_ADDITION_AL:
            CMP BH, 14  ; SP
            JNE BP_SRC_ADDITION_AL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            BP_SRC_ADDITION_AL:
            CMP BH, 15  ; BP
            JNE VAL_SRC_ADDITION_AL
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            VAL_SRC_ADDITION_AL:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
            CALL CONVERT_TO_NUMBER
                MOV BX, PLAYER1_REGS[0]
                MOV AH, VALUE_BYTE
                ADD BL, AH
                MOV PLAYER1_REGS[0], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
    BX_DEST_ADDITION:
      CMP BH, 3  ; BX
        JNE BH_DEST_ADDITION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_ADDITION_BX
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS[2]
            MOV AX, VALUE_WORD
            ADD BX, AX
            MOV PLAYER1_REGS[2], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_ADDITION_BX:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_ADDITION_BX:
            CMP [DI], AX
            JE CHOSEN_SOURCE_ADDITION_BX
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_ADDITION_BX

        CHOSEN_SOURCE_ADDITION_BX:
            AX_SRC_ADDITION_BX:
            CMP BH, 0  ; AX
            JNE AH_SRC_ADDITION_BX
            ;----------------------EXECUTE COMMAND REG TO REG--------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[0]
                ADD BX, AX
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;--------------------------------------------------------------------- 
            AH_SRC_ADDITION_BX:
            CMP BH, 1  ; AH
            JNE AL_SRC_ADDITION_BX
            ;----------------------EXECUTE COMMAND REG TO REG--------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;---------------------------------------------------------------------
            AL_SRC_ADDITION_BX:
            CMP BH, 2  ; AL
            JNE BX_SRC_ADDITION_BX
            ;----------------------EXECUTE COMMAND REG TO REG--------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;---------------------------------------------------------------------

            BX_SRC_ADDITION_BX:
            CMP BH, 3  ; BX
            JNE BH_SRC_ADDITION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                ADD BX, BX
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BH_SRC_ADDITION_BX:
            CMP BH, 4  ; BH
            JNE BL_SRC_ADDITION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BL_SRC_ADDITION_BX:
            CMP BH, 5  ; BL
            JNE CX_SRC_ADDITION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CX_SRC_ADDITION_BX:
            CMP BH, 6  ; CX
            JNE CH_SRC_ADDITION_BX
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[4]
                ADD BX, AX
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CH_SRC_ADDITION_BX:
            CMP BH, 7  ; CH
            JNE CL_SRC_ADDITION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CL_SRC_ADDITION_BX:
            CMP BH, 8  ; CL
            JNE DX_SRC_ADDITION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            DX_SRC_ADDITION_BX:
            CMP BH, 9  ; DX
            JNE DH_SRC_ADDITION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[6]
                ADD BX, AX
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            DH_SRC_ADDITION_BX:
            CMP BH, 10  ; DH
            JNE DL_SRC_ADDITION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            DL_SRC_ADDITION_BX:
            CMP BH, 11  ; DL
            JNE SI_SRC_ADDITION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            SI_SRC_ADDITION_BX:
            CMP BH, 12  ; SI
            JNE DI_SRC_ADDITION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[8]
                ADD BX, AX
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            DI_SRC_ADDITION_BX:
            CMP BH, 13  ; DI
            JNE SP_SRC_ADDITION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[10]
                ADD BX, AX
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            SP_SRC_ADDITION_BX:
            CMP BH, 14  ; SP
            JNE BP_SRC_ADDITION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[12]
                ADD BX, AX
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            BP_SRC_ADDITION_BX:
            CMP BH, 15  ; BP
            JNE VAL_SRC_ADDITION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[14]
                ADD BX, AX
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            VAL_SRC_ADDITION_BX:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV AX, VALUE_WORD
                MOV BX, PLAYER1_REGS[2]
                ADD BX, AX
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

      BH_DEST_ADDITION:
        CMP BH, 4  ; BH
        JNE BL_DEST_ADDITION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_ADDITION_BH
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS [2]
            MOV AH, VALUE_BYTE
            ADD BH, AH
            MOV  PLAYER1_REGS[2], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_ADDITION_BH:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_ADDITION_BH:
            CMP [DI], AX
            JE CHOSEN_SOURCE_ADDITION_BH
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_ADDITION_BH

        CHOSEN_SOURCE_ADDITION_BH:
          AX_SRC_ADDITION_BH:
            CMP BH, 0  ; AX
            JNE AH_SRC_ADDITION_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;----------------------------------------------------------------------- 
          AH_SRC_ADDITION_BH:
            CMP BH, 1  ; AH
            JNE AL_SRC_ADDITION_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[0]
                ADD BH, AH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          AL_SRC_ADDITION_BH:
            CMP BH, 2  ; AL
            JNE BX_SRC_ADDITION_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[0]
                ADD BH, AL
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BX_SRC_ADDITION_BH:
            CMP BH, 3  ; BX
            JNE BH_SRC_ADDITION_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BH_SRC_ADDITION_BH:
            CMP BH, 4  ; BH
            JNE BL_SRC_ADDITION_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                ADD BH, BH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BL_SRC_ADDITION_BH:
            CMP BH, 5  ; BL
            JNE CX_SRC_ADDITION_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2] 
                ADD BH, BL
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CX_SRC_ADDITION_BH:
            CMP BH, 6  ; CX
            JNE CH_SRC_ADDITION_BH
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CH_SRC_ADDITION_BH:
            CMP BH, 7  ; CH
            JNE CL_SRC_ADDITION_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[4]
                ADD BH, AH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CL_SRC_ADDITION_BH:
            CMP BH, 8  ; CL
            JNE DX_SRC_ADDITION_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[4]
                ADD BH, AL
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------

          DX_SRC_ADDITION_BH:
            CMP BH, 9  ; DX
            JNE DH_SRC_ADDITION_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          DH_SRC_ADDITION_BH:
            CMP BH, 10  ; DH
            JNE DL_SRC_ADDITION_BH
            ;----------------------EXECUTE COMMAND REG TO REG------------------------ 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[6]
                ADD BH, AH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          DL_SRC_ADDITION_BH:
            CMP BH, 11  ; DL
            JNE SI_SRC_ADDITION_BH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[6]
                ADD BH, AL
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          SI_SRC_ADDITION_BH:
            CMP BH, 12  ; SI
            JNE DI_SRC_ADDITION_BH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          DI_SRC_ADDITION_BH:
            CMP BH, 13  ; DI
            JNE SP_SRC_ADDITION_BH
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          SP_SRC_ADDITION_BH:
            CMP BH, 14  ; SP
            JNE BP_SRC_ADDITION_BH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          BP_SRC_ADDITION_BH:
            CMP BH, 15  ; BP
            JNE VAL_SRC_ADDITION_BH
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          VAL_SRC_ADDITION_BH:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX ,PLAYER1_REGS[2]
                MOV AH, VALUE_BYTE
                ADD BH, AH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

      BL_DEST_ADDITION:
        CMP BH, 5  ; BL
        JNE CX_DEST_ADDITION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_ADDITION_BL
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS [2]
            MOV AH, VALUE_BYTE
            ADD BL, AH
            MOV  PLAYER1_REGS[2], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_ADDITION_BL:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_ADDITION_BL:
            CMP [DI], AX
            JE CHOSEN_SOURCE_ADDITION_BL
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_ADDITION_BL

        CHOSEN_SOURCE_ADDITION_BL:
          AX_SRC_ADDITION_BL:
            CMP BH, 0  ; AX
            JNE AH_SRC_ADDITION_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;----------------------------------------------------------------------- 
          AH_SRC_ADDITION_BL:
            CMP BH, 1  ; AH
            JNE AL_SRC_ADDITION_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[0]
                ADD BL, AH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          AL_SRC_ADDITION_BL:
            CMP BH, 2  ; AL
            JNE BX_SRC_ADDITION_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[0]
                ADD BL, AL
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BX_SRC_ADDITION_BL:
            CMP BH, 3  ; BX
            JNE BH_SRC_ADDITION_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BH_SRC_ADDITION_BL:
            CMP BH, 4  ; BH
            JNE BL_SRC_ADDITION_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                ADD BL, BH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BL_SRC_ADDITION_BL:
            CMP BH, 5  ; BL
            JNE CX_SRC_ADDITION_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2] 
                ADD BL, BL
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CX_SRC_ADDITION_BL:
            CMP BH, 6  ; CX
            JNE CH_SRC_ADDITION_BL
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CH_SRC_ADDITION_BL:
            CMP BH, 7  ; CH
            JNE CL_SRC_ADDITION_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[4]
                ADD BL, AH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CL_SRC_ADDITION_BL:
            CMP BH, 8  ; CL
            JNE DX_SRC_ADDITION_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[4]
                ADD BL, AL
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------

          DX_SRC_ADDITION_BL:
            CMP BH, 9  ; DX
            JNE DH_SRC_ADDITION_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          DH_SRC_ADDITION_BL:
            CMP BH, 10  ; DH
            JNE DL_SRC_ADDITION_BL
            ;----------------------EXECUTE COMMAND REG TO REG------------------------ 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[6]
                ADD BL, AH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          DL_SRC_ADDITION_BL:
            CMP BH, 11  ; DL
            JNE SI_SRC_ADDITION_BL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[6]
                ADD BL, AL
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          SI_SRC_ADDITION_BL:
            CMP BL, 12  ; SI
            JNE DI_SRC_ADDITION_BL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          DI_SRC_ADDITION_BL:
            CMP BH, 13  ; DI
            JNE SP_SRC_ADDITION_BL
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          SP_SRC_ADDITION_BL:
            CMP BH, 14  ; SP
            JNE BP_SRC_ADDITION_BL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          BP_SRC_ADDITION_BL:
            CMP BH, 15  ; BP
            JNE VAL_SRC_ADDITION_BL
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          VAL_SRC_ADDITION_BL:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX ,PLAYER1_REGS[2]
                MOV AH, VALUE_BYTE
                ADD BH, AH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------


;---------MOV CX,  ----------          
      CX_DEST_ADDITION:
        CMP BH, 6  ; CX
        JNE CH_DEST_ADDITION        
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_ADDITION_CX
            CALL CONVERT_TO_NUMBER
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[4]
            MOV AX, VALUE_WORD
            ADD BX, AX
            MOV PLAYER1_REGS[4], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_ADDITION_CX:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_ADDITION_CX:
            CMP [DI], AX
            JE CHOSEN_SOURCE_ADDITION
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_ADDITION_CX

        CHOSEN_SOURCE_ADDITION:
          AX_SRC_ADDITION_CX:
            CMP BH, 0  ; AX
            JNE AH_SRC_ADDITION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[0]
                ADD BX, AX
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------

          AH_SRC_ADDITION_CX:
            CMP BH, 1  ; AH
            JNE AL_SRC_ADDITION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          AL_SRC_ADDITION_CX:
            CMP BH, 2  ; AL
            JNE BX_SRC_ADDITION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          BX_SRC_ADDITION_CX:
            CMP BH, 3  ; BX
            JNE BH_SRC_ADDITION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[2]
                ADD BX, AX
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BH_SRC_ADDITION_CX:
            CMP BH, 4  ; BH
            JNE BL_SRC_ADDITION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BL_SRC_ADDITION_CX:
            CMP BH, 5  ; BL
            JNE CX_SRC_ADDITION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CX_SRC_ADDITION_CX:
            CMP BH, 6  ; CX
            JNE CH_SRC_ADDITION_CX
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                ADD BX, BX
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CH_SRC_ADDITION_CX:
            CMP BH, 7  ; CH
            JNE CL_SRC_ADDITION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CL_SRC_ADDITION_CX:
            CMP BH, 8  ; CL
            JNE DX_SRC_ADDITION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DX_SRC_ADDITION_CX:
            CMP BH, 9  ; DX
            JNE DH_SRC_ADDITION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[6]
                ADD BX, AX
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DH_SRC_ADDITION_CX:
            CMP BH, 10  ; DH
            JNE DL_SRC_ADDITION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DL_SRC_ADDITION_CX:
            CMP BH, 11  ; DL
            JNE SI_SRC_ADDITION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          SI_SRC_ADDITION_CX:
            CMP BH, 12  ; SI
            JNE DI_SRC_ADDITION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[8]
                ADD BX, AX
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DI_SRC_ADDITION_CX:
            CMP BH, 13  ; DI
            JNE SP_SRC_ADDITION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[10]
                ADD BX, AX
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          SP_SRC_ADDITION_CX:
            CMP BH, 14  ; SP
            JNE BP_SRC_ADDITION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[12]
                ADD BX, AX
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          BP_SRC_ADDITION_CX:
            CMP BH, 15  ; BP
            JNE VAL_SRC_ADDITION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[14]
                ADD BX, AX
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          VAL_SRC_ADDITION_CX:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX, PLAYER1_REGS[4]
                MOV AX, VALUE_WORD
                ADD BX, AX
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

      CH_DEST_ADDITION:
        CMP BH, 7  ; CH
        JNE CL_DEST_ADDITION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_ADDITION_CH
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS [4]
            MOV AH, VALUE_BYTE
            ADD BH, AH
            MOV  PLAYER1_REGS[4], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_ADDITION_CH:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_ADDITION_CH:
            CMP [DI], AX
            JE CHOSEN_SOURCE_ADDITION_CH
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_ADDITION_CH

        CHOSEN_SOURCE_ADDITION_CH:
          AX_SRC_ADDITION_CH:
            CMP BH, 0  ; AX
            JNE AH_SRC_ADDITION_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;----------------------------------------------------------------------- 
          AH_SRC_ADDITION_CH: ;ADD CH,AH
            CMP BH, 1  ; AH
            JNE AL_SRC_ADDITION_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[0]
                ADD BH, AH
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          AL_SRC_ADDITION_CH: ;ADD CH,AL
            CMP BH, 2  ; AL
            JNE BX_SRC_ADDITION_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[0]
                ADD BH, AL
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BX_SRC_ADDITION_CH:
            CMP BH, 3  ; BX
            JNE BH_SRC_ADDITION_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BH_SRC_ADDITION_CH: ;ADD CH,BH
            CMP BH, 4  ; BH
            JNE BL_SRC_ADDITION_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[2]
                ADD BH, AH
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BL_SRC_ADDITION_CH: ;ADD CH,BL
            CMP BH, 5  ; BL
            JNE CX_SRC_ADDITION_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[2]
                ADD BH, AL
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CX_SRC_ADDITION_CH:
            CMP BH, 6  ; CX
            JNE CH_SRC_ADDITION_CH
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CH_SRC_ADDITION_CH: ;ADD CH,CH
            CMP BH, 7  ; CH
            JNE CL_SRC_ADDITION_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                ADD BH, BH
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CL_SRC_ADDITION_CH: ;ADD CH,CL
            CMP BH, 8  ; CL
            JNE DX_SRC_ADDITION_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                ADD BH, BL
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------

          DX_SRC_ADDITION_CH: 
            CMP BH, 9  ; DX
            JNE DH_SRC_ADDITION_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          DH_SRC_ADDITION_CH: ;ADD CH,DH
            CMP BH, 10  ; DH
            JNE DL_SRC_ADDITION_CH
            ;----------------------EXECUTE COMMAND REG TO REG------------------------ 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[6]
                ADD BH, AH
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          DL_SRC_ADDITION_CH: ;ADD CH, DL
            CMP BH, 11  ; DL
            JNE SI_SRC_ADDITION_CH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[6]
                ADD BH, AL
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          SI_SRC_ADDITION_CH:
            CMP BH, 12  ; SI
            JNE DI_SRC_ADDITION_CH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          DI_SRC_ADDITION_CH:
            CMP BH, 13  ; DI
            JNE SP_SRC_ADDITION_CH
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          SP_SRC_ADDITION_CH:
            CMP BH, 14  ; SP
            JNE BP_SRC_ADDITION_CH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          BP_SRC_ADDITION_CH:
            CMP BH, 15  ; BP
            JNE VAL_SRC_ADDITION_CH
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          VAL_SRC_ADDITION_CH:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX ,PLAYER1_REGS[2]
                MOV AH, VALUE_BYTE
                ADD BH, AH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------


      CL_DEST_ADDITION:    
        CMP BH, 8  ; CL
        JNE DX_DEST_ADDITION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_ADDITION_CL
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS [2]
            MOV AH, VALUE_BYTE
            ADD BL, AH
            MOV  PLAYER1_REGS[2], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_ADDITION_CL:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_ADDITION_CL:
            CMP [DI], AX
            JE CHOSEN_SOURCE_ADDITION_CL
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_ADDITION_CL

        CHOSEN_SOURCE_ADDITION_CL:
          AX_SRC_ADDITION_CL:
            CMP BH, 0  ; AX
            JNE AH_SRC_ADDITION_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          AH_SRC_ADDITION_CL: ; ADD CL,AH
            CMP BH, 1   ; AH
            JNE AL_SRC_ADDITION_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[0]
                ADD BL, AH
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          AL_SRC_ADDITION_CL: ;ADD CL, AL
            CMP BH, 2  ; AL
            JNE BX_SRC_ADDITION_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[0]
                ADD BL, AL
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BX_SRC_ADDITION_CL:
            CMP BH, 3  ; BX
            JNE BH_SRC_ADDITION_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BH_SRC_ADDITION_CL: ; ADD CL, BH
            CMP BH, 4  ; BH
            JNE BL_SRC_ADDITION_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[2]
                ADD BL, AH
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BL_SRC_ADDITION_CL: ; ADD CL, BL
            CMP BH, 5  ; BL
            JNE CX_SRC_ADDITION_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[2]
                ADD BL, AL
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CX_SRC_ADDITION_CL:
            CMP BH, 6  ; CX
            JNE CH_SRC_ADDITION_CL
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CH_SRC_ADDITION_CL: ;ADD CL, CH
            CMP BH, 7  ; CH
            JNE CL_SRC_ADDITION_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                ADD BL, BH
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CL_SRC_ADDITION_CL: ;ADD CL,CL
            CMP BH, 8  ; CL
            JNE DX_SRC_ADDITION_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                ADD BL, BL
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------

          DX_SRC_ADDITION_CL:
            CMP BH, 9  ; DX
            JNE DH_SRC_ADDITION_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          DH_SRC_ADDITION_CL: ;ADD CL, DH
            CMP BH, 10  ; DH
            JNE DL_SRC_ADDITION_CL
            ;----------------------EXECUTE COMMAND REG TO REG------------------------ 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[6]
                ADD BL, AH
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          DL_SRC_ADDITION_CL: ;ADD CL, DL
            CMP BH, 11  ; DL
            JNE SI_SRC_ADDITION_CL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[6]
                ADD BL, AL
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          SI_SRC_ADDITION_CL:
            CMP BL, 12  ; SI
            JNE DI_SRC_ADDITION_CL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          DI_SRC_ADDITION_CL:
            CMP BH, 13  ; DI
            JNE SP_SRC_ADDITION_CL
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          SP_SRC_ADDITION_CL:
            CMP BH, 14  ; SP
            JNE BP_SRC_ADDITION_CL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          BP_SRC_ADDITION_CL:
            CMP BH, 15  ; BP
            JNE VAL_SRC_ADDITION_CL
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          VAL_SRC_ADDITION_CL:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX ,PLAYER1_REGS[4]
                MOV AH, VALUE_BYTE
                ADD BL, AH
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

      DX_DEST_ADDITION:
        CMP BH, 9  ; DX
        JNE DH_DEST_ADDITION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_ADDITION_DX
            CALL CONVERT_TO_NUMBER
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[6]
            MOV AX, VALUE_WORD
            ADD BX, AX
            MOV PLAYER1_REGS[6], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_ADDITION_DX:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_ADDITION_DX:
            CMP [DI], AX
            JE CHOSEN_SOURCE_ADDITION_DX
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_ADDITION_DX

        CHOSEN_SOURCE_ADDITION_DX:
          AX_SRC_ADDITION_DX:
            CMP BH, 0  ; AX
            JNE AH_SRC_ADDITION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[0]
                ADD BX, AX
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------
          AH_SRC_ADDITION_DX:
            CMP BH, 1  ; AH
            JNE AL_SRC_ADDITION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------
          AL_SRC_ADDITION_DX:
            CMP BH, 2  ; AL
            JNE BX_SRC_ADDITION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          BX_SRC_ADDITION_DX:
            CMP BH, 3  ; BX
            JNE BH_SRC_ADDITION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[2]
                ADD BX, AX
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BH_SRC_ADDITION_DX:
            CMP BH, 4  ; BH
            JNE BL_SRC_ADDITION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BL_SRC_ADDITION_DX:
            CMP BH, 5  ; BL
            JNE CX_SRC_ADDITION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CX_SRC_ADDITION_DX:
            CMP BH, 6  ; CX
            JNE CH_SRC_ADDITION_DX
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[4]
                ADD BX, AX
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CH_SRC_ADDITION_DX:
            CMP BH, 7  ; CH
            JNE CL_SRC_ADDITION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CL_SRC_ADDITION_DX:
            CMP BH, 8  ; CL
            JNE DX_SRC_ADDITION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DX_SRC_ADDITION_DX:
            CMP BH, 9  ; DX
            JNE DH_SRC_ADDITION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                ADD BX, BX
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------

          DH_SRC_ADDITION_DX:
            CMP BH, 10  ; DH
            JNE DL_SRC_ADDITION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          DL_SRC_ADDITION_DX:
            CMP BH, 11  ; DL
            JNE SI_SRC_ADDITION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          SI_SRC_ADDITION_DX:
            CMP BH, 12  ; SI
            JNE DI_SRC_ADDITION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[8]
                ADD BX, AX
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DI_SRC_ADDITION_DX:
            CMP BH, 13  ; DI
            JNE SP_SRC_ADDITION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[10]
                ADD BX, AX
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          SP_SRC_ADDITION_DX:
            CMP BH, 14  ; SP
            JNE BP_SRC_ADDITION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[12]
                ADD BX, AX
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          BP_SRC_ADDITION_DX:
            CMP BH, 15  ; BP
            JNE VAL_SRC_ADDITION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[14]
                ADD BX, AX
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          VAL_SRC_ADDITION_DX:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX, PLAYER1_REGS[6]
                MOV AX, VALUE_WORD
                ADD BX, AX
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------

      DH_DEST_ADDITION:
        CMP BH, 10 ; DH
        JNE DL_DEST_ADDITION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_ADDITION_DH
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS [6]
            MOV AH, VALUE_BYTE
            ADD BH, AH
            MOV  PLAYER1_REGS[6], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_ADDITION_DH:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_ADDITION_DH:
            CMP [DI], AX
            JE CHOSEN_SOURCE_ADDITION_DH
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_ADDITION_DH

        CHOSEN_SOURCE_ADDITION_DH:
          AX_SRC_ADDITION_DH:
            CMP BH, 0  ; AX
            JNE AH_SRC_ADDITION_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;----------------------------------------------------------------------- 
          AH_SRC_ADDITION_DH: ;ADD DH,AH
            CMP BH, 1  ; AH
            JNE AL_SRC_ADDITION_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[0]
                ADD BH, AH
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          AL_SRC_ADDITION_DH: ;ADD DH,AL
            CMP BH, 2  ; AL
            JNE BX_SRC_ADDITION_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[0]
                ADD BH, AL
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BX_SRC_ADDITION_DH:
            CMP BH, 3  ; BX
            JNE BH_SRC_ADDITION_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BH_SRC_ADDITION_DH: ;ADD DH,BH
            CMP BH, 4  ; BH
            JNE BL_SRC_ADDITION_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[2]
                ADD BH, AH
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BL_SRC_ADDITION_DH: ;ADD DH,BL
            CMP BH, 5  ; BL
            JNE CX_SRC_ADDITION_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[2]
                ADD BH, AL
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CX_SRC_ADDITION_DH:
            CMP BH, 6  ; CX
            JNE CH_SRC_ADDITION_DH
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CH_SRC_ADDITION_DH: ;ADD DH,CH
            CMP BH, 7  ; CH
            JNE CL_SRC_ADDITION_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[4]
                ADD BH, AH
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CL_SRC_ADDITION_DH: ;ADD DH,CL
            CMP BH, 8  ; CL
            JNE DX_SRC_ADDITION_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[4]
                ADD BH, AL
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------

          DX_SRC_ADDITION_DH: 
            CMP BH, 9  ; DX
            JNE DH_SRC_ADDITION_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          DH_SRC_ADDITION_DH: ;ADD DH,DH
            CMP BH, 10  ; DH
            JNE DL_SRC_ADDITION_DH
            ;----------------------EXECUTE COMMAND REG TO REG------------------------ 
                MOV BX, PLAYER1_REGS[6]
                ADD BH, BH
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          DL_SRC_ADDITION_DH: ;ADD DH, DL
            CMP BH, 11  ; DL
            JNE SI_SRC_ADDITION_DH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                MOV BX, PLAYER1_REGS[6]
                ADD BH, BL
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          SI_SRC_ADDITION_DH:
            CMP BH, 12  ; SI
            JNE DI_SRC_ADDITION_DH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          DI_SRC_ADDITION_DH:
            CMP BH, 13  ; DI
            JNE SP_SRC_ADDITION_DH
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          SP_SRC_ADDITION_DH:
            CMP BH, 14  ; SP
            JNE BP_SRC_ADDITION_DH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          BP_SRC_ADDITION_DH:
            CMP BH, 15  ; BP
            JNE VAL_SRC_ADDITION_DH
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          VAL_SRC_ADDITION_DH:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX ,PLAYER1_REGS[6]
                MOV AH, VALUE_BYTE
                ADD BH, AH
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

      DL_DEST_ADDITION:
        CMP BH, 11 ; DL
        JNE SI_DEST_ADDITION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_ADDITION_DL
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS [6]
            MOV AH, VALUE_BYTE
            ADD BL, AH
            MOV  PLAYER1_REGS[6], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_ADDITION_DL:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_ADDITION_DL:
            CMP [DI], AX
            JE CHOSEN_SOURCE_ADDITION_DL
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_ADDITION_DL

        CHOSEN_SOURCE_ADDITION_DL:
          AX_SRC_ADDITION_DL:
            CMP BH, 0  ; AX
            JNE AH_SRC_ADDITION_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;----------------------------------------------------------------------- 
          AH_SRC_ADDITION_DL: ;ADD DL,AH
            CMP BH, 1  ; AH
            JNE AL_SRC_ADDITION_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[0]
                ADD BL, AH
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          AL_SRC_ADDITION_DL: ;ADD DL,AL
            CMP BH, 2  ; AL
            JNE BX_SRC_ADDITION_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[0]
                ADD BL, AL
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BX_SRC_ADDITION_DL:
            CMP BH, 3  ; BX
            JNE BH_SRC_ADDITION_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BH_SRC_ADDITION_DL: ;ADD DL,BH
            CMP BH, 4  ; BH
            JNE BL_SRC_ADDITION_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[2]
                ADD BL, AH
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BL_SRC_ADDITION_DL: ;ADD DL,BL
            CMP BH, 5  ; BL
            JNE CX_SRC_ADDITION_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[2]
                ADD BL, AL
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CX_SRC_ADDITION_DL:
            CMP BH, 6  ; CX
            JNE CH_SRC_ADDITION_DL
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CH_SRC_ADDITION_DL: ;ADD DL,CH
            CMP BH, 7  ; CH
            JNE CL_SRC_ADDITION_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[4]
                ADD BL, AH
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CL_SRC_ADDITION_DL: ;ADD DL,CL
            CMP BH, 8  ; CL
            JNE DX_SRC_ADDITION_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[4]
                ADD BL, AL
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------

          DX_SRC_ADDITION_DL: 
            CMP BH, 9  ; DX
            JNE DL_SRC_ADDITION_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          DH_SRC_ADDITION_DL: ;ADD DL,DH
            CMP BH, 10  ; DH
            JNE DL_SRC_ADDITION_DL
            ;----------------------EXECUTE COMMAND REG TO REG------------------------ 
                MOV BX, PLAYER1_REGS[6]
                ADD BL, BH
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          DL_SRC_ADDITION_DL: ;ADD DL, DL
            CMP BH, 11  ; DL
            JNE SI_SRC_ADDITION_DL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                MOV BX, PLAYER1_REGS[6]
                ADD BL, BL
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          SI_SRC_ADDITION_DL:
            CMP BH, 12  ; SI
            JNE DI_SRC_ADDITION_DL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          DI_SRC_ADDITION_DL:
            CMP BH, 13  ; DI
            JNE SP_SRC_ADDITION_DL
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          SP_SRC_ADDITION_DL:
            CMP BH, 14  ; SP
            JNE BP_SRC_ADDITION_DL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          BP_SRC_ADDITION_DL:
            CMP BH, 15  ; BP
            JNE VAL_SRC_ADDITION_DL
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          VAL_SRC_ADDITION_DL:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX ,PLAYER1_REGS[6]
                MOV AH, VALUE_BYTE
                ADD BL, AH
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

      SI_DEST_ADDITION:
        CMP BH, 12 ; SI
        JNE DI_DEST_ADDITION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_ADDITION_SI
            CALL CONVERT_TO_NUMBER
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[8]
            MOV AX, VALUE_WORD
            ADD BX, AX
            MOV PLAYER1_REGS[8], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_ADDITION_SI:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_ADDITION_SI:
            CMP [DI], AX
            JE CHOSEN_SOURCE_ADDITION_SI
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_ADDITION_SI

        CHOSEN_SOURCE_ADDITION_SI:
          AX_SRC_ADDITION_SI:
            CMP BH, 0  ; AX
            JNE AH_SRC_ADDITION_SI
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[8]
                MOV AX, PLAYER1_REGS[0]
                ADD BX, AX
                MOV PLAYER1_REGS[8], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          AH_SRC_ADDITION_SI:
            CMP BH, 1  ; AH
            JNE AL_SRC_ADDITION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          AL_SRC_ADDITION_SI:
            CMP BH, 2  ; AL
            JNE BX_SRC_ADDITION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BX_SRC_ADDITION_SI:
            CMP BH, 3  ; BX
            JNE BH_SRC_ADDITION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[8]
                MOV AX, PLAYER1_REGS[2]
                ADD BX, AX
                MOV PLAYER1_REGS[8], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BH_SRC_ADDITION_SI:
            CMP BH, 4  ; BH
            JNE BL_SRC_ADDITION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BL_SRC_ADDITION_SI:
            CMP BH, 5  ; BL
            JNE CX_SRC_ADDITION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CX_SRC_ADDITION_SI:
            CMP BH, 6  ; CX
            JNE CH_SRC_ADDITION_SI
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[8]
                MOV AX, PLAYER1_REGS[4]
                ADD BX, AX
                MOV PLAYER1_REGS[8], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CH_SRC_ADDITION_SI:
            CMP BH, 7  ; CH
            JNE CL_SRC_ADDITION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CL_SRC_ADDITION_SI:
            CMP BH, 8  ; CL
            JNE DX_SRC_ADDITION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DX_SRC_ADDITION_SI:
            CMP BH, 9  ; DX
            JNE DH_SRC_ADDITION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[8]
                MOV AX, PLAYER1_REGS[6]
                ADD BX, AX
                MOV PLAYER1_REGS[8], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DH_SRC_ADDITION_SI:
            CMP BH, 10  ; DH
            JNE DL_SRC_ADDITION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DL_SRC_ADDITION_SI:
            CMP BH, 11  ; DL
            JNE SI_SRC_ADDITION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          SI_SRC_ADDITION_SI:
            CMP BH, 12  ; SI
            JNE DI_SRC_ADDITION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[8]
                ADD BX, BX
                MOV PLAYER1_REGS[8], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DI_SRC_ADDITION_SI:
            CMP BH, 13  ; DI
            JNE SP_SRC_ADDITION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[8]
                MOV AX, PLAYER1_REGS[10]
                ADD BX, AX
                MOV PLAYER1_REGS[8], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          SP_SRC_ADDITION_SI:
            CMP BH, 14  ; SP
            JNE BP_SRC_ADDITION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[8]
                MOV AX, PLAYER1_REGS[12]
                ADD BX, AX
                MOV PLAYER1_REGS[8], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          BP_SRC_ADDITION_SI:
            CMP BH, 15  ; BP
            JNE VAL_SRC_ADDITION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[8]
                MOV AX, PLAYER1_REGS[14]
                ADD BX, AX
                MOV PLAYER1_REGS[8], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          VAL_SRC_ADDITION_SI:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX, PLAYER1_REGS[8]
                MOV AX, VALUE_WORD
                ADD BX, AX
                MOV PLAYER1_REGS[8], BX
                JMP EXIT_EXECUTE
                
            ;------------------------------------------------------------------

      DI_DEST_ADDITION:
        CMP BH, 13 ; DI
        JNE SP_DEST_ADDITION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_ADDITION_DI
            CALL CONVERT_TO_NUMBER
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[10]
            MOV AX, VALUE_WORD
            ADD BX, AX
            MOV PLAYER1_REGS[10], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_ADDITION_DI:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_ADDITION_DI:
            CMP [DI], AX
            JE CHOSEN_SOURCE_ADDITION_DI
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_ADDITION_DI

        CHOSEN_SOURCE_ADDITION_DI:
          AX_SRC_ADDITION_DI:
            CMP BH, 0  ; AX
            JNE AH_SRC_ADDITION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[10]
                MOV AX, PLAYER1_REGS[0]
                ADD BX, AX
                MOV PLAYER1_REGS[10], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          AH_SRC_ADDITION_DI:
            CMP BH, 1   ; AH
            JNE AL_SRC_ADDITION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          AL_SRC_ADDITION_DI:
            CMP BH, 2  ; AL
            JNE BX_SRC_ADDITION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BX_SRC_ADDITION_DI:
            CMP BH, 3  ; BX
            JNE BH_SRC_ADDITION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[10]
                MOV AX, PLAYER1_REGS[2]
                ADD BX, AX
                MOV PLAYER1_REGS[10], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BH_SRC_ADDITION_DI:
            CMP BH, 4  ; BH
            JNE BL_SRC_ADDITION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BL_SRC_ADDITION_DI:
            CMP BH, 5  ; BL
            JNE CX_SRC_ADDITION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CX_SRC_ADDITION_DI:
            CMP BH, 6  ; CX
            JNE CH_SRC_ADDITION_DI
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[10]
                MOV AX, PLAYER1_REGS[4]
                ADD BX, AX
                MOV PLAYER1_REGS[10], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CH_SRC_ADDITION_DI:
            CMP BH, 7  ; CH
            JNE CL_SRC_ADDITION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CL_SRC_ADDITION_DI:
            CMP BH, 8  ; CL
            JNE DX_SRC_ADDITION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DX_SRC_ADDITION_DI:
            CMP BH, 9  ; DX
            JNE DH_SRC_ADDITION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[10]
                MOV AX, PLAYER1_REGS[6]
                ADD BX, AX
                MOV PLAYER1_REGS[10], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DH_SRC_ADDITION_DI:
            CMP BH, 10  ; DH
            JNE DL_SRC_ADDITION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DL_SRC_ADDITION_DI:
            CMP BH, 11  ; DL
            JNE SI_SRC_ADDITION_DI

            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          SI_SRC_ADDITION_DI:
            CMP BH, 12  ; SI
            JNE DI_SRC_ADDITION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[10]
                MOV AX, PLAYER1_REGS[8]
                ADD BX, AX
                MOV PLAYER1_REGS[10], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DI_SRC_ADDITION_DI:
            CMP BH, 13  ; DI
            JNE SP_SRC_ADDITION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[10]
                ADD BX, BX
                MOV PLAYER1_REGS[10], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          SP_SRC_ADDITION_DI:
            CMP BH, 14  ; SP
            JNE BP_SRC_ADDITION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[10]
                MOV AX, PLAYER1_REGS[12]
                ADD BX, AX
                MOV PLAYER1_REGS[10], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          BP_SRC_ADDITION_DI:
            CMP BH, 15  ; BP
            JNE VAL_SRC_ADDITION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[10]
                MOV AX, PLAYER1_REGS[4]
                ADD BX, AX
                MOV PLAYER1_REGS[10], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          VAL_SRC_ADDITION_DI:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV AX, VALUE_WORD
                MOV PLAYER1_REGS[10],AX
                JMP EXIT_EXECUTE
                
            ;------------------------------------------------------------------
      SP_DEST_ADDITION:
        CMP BH, 14 ; SP
        JNE BP_DEST_ADDITION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_ADDITION_SP
            CALL CONVERT_TO_NUMBER
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[12]
            MOV AX, VALUE_WORD
            ADD BX, AX
            MOV PLAYER1_REGS[12], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_ADDITION_SP:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_ADDITION_SP:
            CMP [DI], AX
            JE CHOSEN_SOURCE_ADDITION_SP
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_ADDITION_SP

        CHOSEN_SOURCE_ADDITION_SP:
          AX_SRC_ADDITION_SP:
            CMP BH, 0  ; AX
            JNE AH_SRC_ADDITION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[12]
                MOV AX, PLAYER1_REGS[0]
                ADD BX, AX
                MOV PLAYER1_REGS[12], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          AH_SRC_ADDITION_SP:
            CMP BH, 1  ; AH
            JNE AL_SRC_ADDITION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          AL_SRC_ADDITION_SP:
            CMP BH, 2  ; AL
            JNE BX_SRC_ADDITION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BX_SRC_ADDITION_SP:
            CMP BH, 3  ; BX
            JNE BH_SRC_ADDITION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[12]
                MOV AX, PLAYER1_REGS[2]
                ADD BX, AX
                MOV PLAYER1_REGS[12], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BH_SRC_ADDITION_SP:
            CMP BH, 4  ; BH
            JNE BL_SRC_ADDITION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BL_SRC_ADDITION_SP:
            CMP BH, 5  ; BL
            JNE CX_SRC_ADDITION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CX_SRC_ADDITION_SP:
            CMP BH, 6  ; CX
            JNE CH_SRC_ADDITION_SP
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[12]
                MOV AX, PLAYER1_REGS[4]
                ADD BX, AX
                MOV PLAYER1_REGS[12], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CH_SRC_ADDITION_SP:
            CMP BH, 7  ; CH
            JNE CL_SRC_ADDITION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CL_SRC_ADDITION_SP:
            CMP BH, 8  ; CL
            JNE DX_SRC_ADDITION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DX_SRC_ADDITION_SP:
            CMP BH, 9  ; DX
            JNE DH_SRC_ADDITION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[12]
                MOV AX, PLAYER1_REGS[6]
                ADD BX, AX
                MOV PLAYER1_REGS[12], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DH_SRC_ADDITION_SP:
            CMP BH, 10  ; DH
            JNE DL_SRC_ADDITION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DL_SRC_ADDITION_SP:
            CMP BH, 11  ; DL
            JNE SI_SRC_ADDITION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          SI_SRC_ADDITION_SP:
            CMP BH, 12  ; SI
            JNE DI_SRC_ADDITION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[12]
                MOV AX, PLAYER1_REGS[8]
                ADD BX, AX
                MOV PLAYER1_REGS[12], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DI_SRC_ADDITION_SP:
            CMP BH, 13  ; DI
            JNE SP_SRC_ADDITION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[12]
                MOV AX, PLAYER1_REGS[10]
                ADD BX, AX
                MOV PLAYER1_REGS[12], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          SP_SRC_ADDITION_SP:
            CMP BH, 14  ; SP
            JNE BP_SRC_ADDITION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[12]
                ADD BX, BX
                MOV PLAYER1_REGS[12], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          BP_SRC_ADDITION_SP:
            CMP BH, 15  ; BP
            JNE VAL_SRC_ADDITION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[12]
                MOV AX, PLAYER1_REGS[14]
                ADD BX, AX
                MOV PLAYER1_REGS[12], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          VAL_SRC_ADDITION_SP:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX, PLAYER1_REGS[12]
                MOV AX, VALUE_WORD
                ADD BX, AX
                MOV PLAYER1_REGS[12], BX
                JMP EXIT_EXECUTE  
            ;------------------------------------------------------------------

      BP_DEST_ADDITION:
        CMP BH, 15 ; BP
        JNE EXIT_SYNTAX
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_ADDITION_BP
            CALL CONVERT_TO_NUMBER
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[14]
            MOV AX, VALUE_WORD
            ADD BX, AX
            MOV PLAYER1_REGS[14], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_ADDITION_BP:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_ADDITION_BP:
            CMP [DI], AX
            JE CHOSEN_SOURCE_ADDITION_BP
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_ADDITION_BP

        CHOSEN_SOURCE_ADDITION_BP:
          AX_SRC_ADDITION_BP:
            CMP BH, 0  ; AX
            JNE AH_SRC_ADDITION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[14]
                MOV AX, PLAYER1_REGS[0]
                ADD BX, AX
                MOV PLAYER1_REGS[14], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------ 
          AH_SRC_ADDITION_BP:
            CMP BH, 1  ; AH
            JNE AL_SRC_ADDITION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          AL_SRC_ADDITION_BP:
            CMP BH, 2  ; AL
            JNE BX_SRC_ADDITION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BX_SRC_ADDITION_BP:
            CMP BH, 3  ; BX
            JNE BH_SRC_ADDITION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[14]
                MOV AX, PLAYER1_REGS[2]
                ADD BX, AX
                MOV PLAYER1_REGS[14], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BH_SRC_ADDITION_BP:
            CMP BH, 4  ; BH
            JNE BL_SRC_ADDITION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BL_SRC_ADDITION_BP:
            CMP BH, 5  ; BL
            JNE CX_SRC_ADDITION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CX_SRC_ADDITION_BP:
            CMP BH, 6  ; CX
            JNE CH_SRC_ADDITION_BP
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[14]
                MOV AX, PLAYER1_REGS[4]
                ADD BX, AX
                MOV PLAYER1_REGS[14], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CH_SRC_ADDITION_BP:
            CMP BH, 7  ; CH
            JNE CL_SRC_ADDITION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CL_SRC_ADDITION_BP:
            CMP BH, 8  ; CL
            JNE DX_SRC_ADDITION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DX_SRC_ADDITION_BP:
            CMP BH, 9  ; DX
            JNE DH_SRC_ADDITION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 MOV BX, PLAYER1_REGS[14]
                MOV AX, PLAYER1_REGS[6]
                ADD BX, AX
                MOV PLAYER1_REGS[14], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DH_SRC_ADDITION_BP:
            CMP BH, 10  ; DH
            JNE DL_SRC_ADDITION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DL_SRC_ADDITION_BP:
            CMP BH, 11  ; DL
            JNE SI_SRC_ADDITION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          SI_SRC_ADDITION_BP:
            CMP BH, 12  ; SI
            JNE DI_SRC_ADDITION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[14]
                MOV AX, PLAYER1_REGS[8]
                ADD BX, AX
                MOV PLAYER1_REGS[14], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DI_SRC_ADDITION_BP:
            CMP BH, 13  ; DI
            JNE SP_SRC_ADDITION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[14]
                MOV AX, PLAYER1_REGS[10]
                ADD BX, AX
                MOV PLAYER1_REGS[14], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          SP_SRC_ADDITION_BP:
            CMP BH, 14  ; SP
            JNE BP_SRC_ADDITION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[14]
                MOV AX, PLAYER1_REGS[12]
                ADD BX, AX
                MOV PLAYER1_REGS[14], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          BP_SRC_ADDITION_BP:
            CMP BH, 15  ; BP
            JNE VAL_SRC_ADDITION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[14]
                ADD BX, BX
                MOV PLAYER1_REGS[14], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          VAL_SRC_ADDITION_BP:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX, PLAYER1_REGS[14]
                MOV AX, VALUE_WORD
                ADD BX, AX
                MOV PLAYER1_REGS[14], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

;----------------------------------------------SUBTRACTION COMMAND-----------------------------------------------
SUBTRACTION:
CMP BH, 2  ;SUB
JNE DIVISION
    CALL CHECK_FOR_SPACE
    CMP ERROR, 1
    JE EXIT_SYNTAX

    CALL GET_CURRENT_DESTINATION
    MOV AX, CURRENT_DESTINATION
    MOV DI, OFFSET REGISTERS_CODES
    MOV CX, 16

    CHOOSE_DESTINANTION_SUBTRACTION:
        CMP [DI], AX
        JE CHOSEN_DESTINATION_SUBTRACTION
        INC BH   
        INC DI
        INC DI
        LOOP CHOOSE_DESTINANTION_SUBTRACTION
        JMP EXIT_SYNTAX

CHOSEN_DESTINATION_SUBTRACTION:
      AX_DEST_SUBTRACTION:
        CMP BH, 0  ; AX
        JNE AH_DEST_SUBTRACTION
        CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_SUBTRACTION_AX
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS[0]
            MOV AX, VALUE_WORD
            SUB BX, AX
            MOV PLAYER1_REGS[0], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_SUBTRACTION_AX:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_SUBTRACTION_AX:

            CMP [DI], AX
            JE CHOSEN_SOURCE_SUBTRACTION_AX
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_SUBTRACTION_AX

        CHOSEN_SOURCE_SUBTRACTION_AX:
          AX_SRC_SUBTRACTION_AX:
            CMP BH, 0  ; AX
            JNE AH_SRC_SUBTRACTION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[0]
                SUB BX, BX
                MOV PLAYER1_REGS[0], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------ 
          AH_SRC_SUBTRACTION_AX:
            CMP BH, 1  ; AH
            JNE AL_SRC_SUBTRACTION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          AL_SRC_SUBTRACTION_AX:
            CMP BH, 2  ; AL
            JNE BX_SRC_SUBTRACTION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BX_SRC_SUBTRACTION_AX:
            CMP BH, 3  ; BX
            JNE BH_SRC_SUBTRACTION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[0]
                SUB AX, BX
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BH_SRC_SUBTRACTION_AX:
            CMP BH, 4  ; BH
            JNE BL_SRC_SUBTRACTION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BL_SRC_SUBTRACTION_AX:
            CMP BH, 5  ; BL
            JNE CX_SRC_SUBTRACTION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CX_SRC_SUBTRACTION_AX:
            CMP BH, 6  ; CX
            JNE CH_SRC_SUBTRACTION_AX
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[0]
                SUB AX, BX
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CH_SRC_SUBTRACTION_AX:
            CMP BH, 7  ; CH
            JNE CL_SRC_SUBTRACTION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CL_SRC_SUBTRACTION_AX:
            CMP BH, 8  ; CL
            JNE DX_SRC_SUBTRACTION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DX_SRC_SUBTRACTION_AX:
            CMP BH, 9  ; DX
            JNE DH_SRC_SUBTRACTION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[0]
                SUB AX, BX
                MOV PLAYER1_REGS[0],AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DH_SRC_SUBTRACTION_AX:
            CMP BH, 10  ; DH
            JNE DL_SRC_SUBTRACTION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DL_SRC_SUBTRACTION_AX:
            CMP BH, 11  ; DL
            JNE SI_SRC_SUBTRACTION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          SI_SRC_SUBTRACTION_AX:
            CMP BH, 12  ; SI
            JNE DI_SRC_SUBTRACTION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[8]
                MOV AX, PLAYER1_REGS[0]
                SUB AX, BX
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DI_SRC_SUBTRACTION_AX:
            CMP BH, 13  ; DI
            JNE SP_SRC_SUBTRACTION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[10]
                MOV AX, PLAYER1_REGS[0]
                SUB AX, BX
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          SP_SRC_SUBTRACTION_AX:
            CMP BH, 14  ; SP
            JNE BP_SRC_SUBTRACTION_AX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[12]
                MOV AX, PLAYER1_REGS[0]
                SUB AX, BX
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          BP_SRC_SUBTRACTION_AX:
            CMP BH, 15  ; BP
            JNE VAL_SRC_SUBTRACTION_AX 
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX,PLAYER1_REGS[14]
                MOV AX, PLAYER1_REGS[0]
                SUB AX, BX
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          VAL_SRC_SUBTRACTION_AX:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX, VALUE_WORD
                MOV AX, PLAYER1_REGS[0]
                SUB AX, BX
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
       
       AH_DEST_SUBTRACTION:
       CMP BH, 1  ; AH
        JNE AL_DEST_SUBTRACTION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,4
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_SUBTRACTION_AH
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS[0]
            MOV AH, VALUE_BYTE
            SUB BH, AH
            MOV  PLAYER1_REGS[0], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_SUBTRACTION_AH:
             CALL GET_CURRENT_SOURCE
            
            MOV BH, 0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_SUBTRACTION_AH:
            CMP [DI], AX
            JE CHOSEN_SOURCE_SUBTRACTION_AH
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_SUBTRACTION_AH


        CHOSEN_SOURCE_SUBTRACTION_AH:
          AX_SRC_SUBTRACTION_AH:
            CMP BH, 0  ; AX
            JNE AH_SRC_SUBTRACTION_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------ 
          AH_SRC_SUBTRACTION_AH:
            CMP BH, 1  ; AH
            JNE AL_SRC_SUBTRACTION_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[0]
                SUB BH, BH
                MOV PLAYER1_REGS[0], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          AL_SRC_SUBTRACTION_AH:
            CMP BH, 2  ; AL
            JNE BX_SRC_SUBTRACTION_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[0]
                SUB BH, BL
                MOV PLAYER1_REGS[0], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BX_SRC_SUBTRACTION_AH:
            CMP BH, 3  ; BX
            JNE BH_SRC_SUBTRACTION_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;----------------------------------------------------------------------

          BH_SRC_SUBTRACTION_AH:
            CMP BH, 4  ; BH
            JNE BL_SRC_SUBTRACTION_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[2] 
                SUB AH, BH
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BL_SRC_SUBTRACTION_AH:
            CMP BH, 5  ; BL
            JNE CX_SRC_SUBTRACTION_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[2] 
                SUB AH, BL
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CX_SRC_SUBTRACTION_AH:
            CMP BH, 6  ; CX
            JNE CH_SRC_SUBTRACTION_AH
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CH_SRC_SUBTRACTION_AH:
            CMP BH, 7  ; CH
            JNE CL_SRC_SUBTRACTION_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[4]
                SUB AH, BH
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CL_SRC_SUBTRACTION_AH:
            CMP BH, 8  ; CL
            JNE DX_SRC_SUBTRACTION_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[4]
                SUB AH, BL
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------

          DX_SRC_SUBTRACTION_AH:
            CMP BH, 9  ; DX
            JNE DH_SRC_SUBTRACTION_AH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------
          DH_SRC_SUBTRACTION_AH:
            CMP BH, 10  ; DH
            JNE DL_SRC_SUBTRACTION_AH
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[6]
                SUB AH, BH
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          DL_SRC_SUBTRACTION_AH:
            CMP BH, 11  ; DL
            JNE SI_SRC_SUBTRACTION_AH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[6]
                SUB AH, BL
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          SI_SRC_SUBTRACTION_AH:
            CMP BH, 12  ; SI
            JNE DI_SRC_SUBTRACTION_AH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          DI_SRC_SUBTRACTION_AH:
            CMP BH, 13  ; DI
            JNE SP_SRC_SUBTRACTION_AH
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          SP_SRC_SUBTRACTION_AH:
            CMP BH, 14  ; SP
            JNE BP_SRC_SUBTRACTION_AH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          BP_SRC_SUBTRACTION_AH:
            CMP BH, 15  ; BP
            JNE VAL_SRC_SUBTRACTION_AH
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          VAL_SRC_SUBTRACTION_AH:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
            CALL CONVERT_TO_NUMBER
                MOV BX ,PLAYER1_REGS[0]
                MOV AH, VALUE_BYTE
                SUB BH, AH
                MOV PLAYER1_REGS[0], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

AL_DEST_SUBTRACTION:
        CMP BH, 2  ; AL
        JNE BX_DEST_SUBTRACTION
        CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_SUBTRACTION_AL
        CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS[0]
            MOV AH, VALUE_BYTE
            SUB BL, AH
            MOV PLAYER1_REGS[0], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_SUBTRACTION_AL:
             CALL GET_CURRENT_SOURCE
            
            MOV BH, 0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_SUBTRACTION_AL:
            CMP [DI], AX
            JE CHOSEN_SOURCE_SUBTRACTION_AL
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_SUBTRACTION_AL

        CHOSEN_SOURCE_SUBTRACTION_AL:
            AX_SRC_SUBTRACTION_AL:
            CMP BH, 0  ; AX
            JNE AH_SRC_SUBTRACTION_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;----------------------------------------------------------------------- 
            AH_SRC_SUBTRACTION_AL:
            CMP BH, 1  ; AH
            JNE AL_SRC_SUBTRACTION_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[0]
                SUB BL, BH
                MOV PLAYER1_REGS[0], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            AL_SRC_SUBTRACTION_AL:
            CMP BH, 2  ; AL
            JNE BX_SRC_SUBTRACTION_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[0]
                SUB BL, BL
                MOV PLAYER1_REGS[0], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BX_SRC_SUBTRACTION_AL:
            CMP BH, 3  ; BX
            JNE BH_SRC_SUBTRACTION_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BH_SRC_SUBTRACTION_AL:
            CMP BH, 4  ; BH
            JNE BL_SRC_SUBTRACTION_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[2] 
                SUB AL, BH
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BL_SRC_SUBTRACTION_AL:
            CMP BH, 5  ; BL
            JNE CX_SRC_SUBTRACTION_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[2] 
                SUB AL, BL
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CX_SRC_SUBTRACTION_AL:
            CMP BH, 6  ; CX
            JNE CH_SRC_SUBTRACTION_AL
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CH_SRC_SUBTRACTION_AL:
            CMP BH, 7  ; CH
            JNE CL_SRC_SUBTRACTION_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[4]
                SUB AL, BH
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CL_SRC_SUBTRACTION_AL:
            CMP BH, 8  ; CL
            JNE DX_SRC_SUBTRACTION_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[4]
                SUB AL, BL
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------

            DX_SRC_SUBTRACTION_AL:
            CMP BH, 9  ; DX
            JNE DH_SRC_SUBTRACTION_AL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

            DH_SRC_SUBTRACTION_AL:
            CMP BH, 10  ; DH
            JNE DL_SRC_SUBTRACTION_AL
            ;----------------------EXECUTE COMMAND REG TO REG------------------------ 
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[6]
                SUB AL, BH
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            DL_SRC_SUBTRACTION_AL:
            CMP BH, 11  ; DL
            JNE SI_SRC_SUBTRACTION_AL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                MOV AX, PLAYER1_REGS[0]
                MOV BX, PLAYER1_REGS[6]
                SUB AL, BL
                MOV PLAYER1_REGS[0], AX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            SI_SRC_SUBTRACTION_AL:
            CMP BH, 12  ; SI
            JNE DI_SRC_SUBTRACTION_AL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            DI_SRC_SUBTRACTION_AL:
            CMP BH, 13  ; DI
            JNE SP_SRC_SUBTRACTION_AL
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            SP_SRC_SUBTRACTION_AL:
            CMP BH, 14  ; SP
            JNE BP_SRC_SUBTRACTION_AL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            BP_SRC_SUBTRACTION_AL:
            CMP BH, 15  ; BP
            JNE VAL_SRC_SUBTRACTION_AL
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
            VAL_SRC_SUBTRACTION_AL:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
            CALL CONVERT_TO_NUMBER
                MOV BX, PLAYER1_REGS[0]
                MOV AH, VALUE_BYTE
                SUB BL, AH
                MOV PLAYER1_REGS[0], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
    BX_DEST_SUBTRACTION:
      CMP BH, 3  ; BX
        JNE BH_DEST_SUBTRACTION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_SUBTRACTION_BX
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS[2]
            MOV AX, VALUE_WORD
            SUB BX, AX
            MOV PLAYER1_REGS[2], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_SUBTRACTION_BX:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_SUBTRACTION_BX:
            CMP [DI], AX
            JE CHOSEN_SOURCE_SUBTRACTION_BX
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_SUBTRACTION_BX

        CHOSEN_SOURCE_SUBTRACTION_BX:
            AX_SRC_SUBTRACTION_BX:
            CMP BH, 0  ; AX
            JNE AH_SRC_SUBTRACTION_BX
            ;----------------------EXECUTE COMMAND REG TO REG--------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[0]
                SUB BX, AX
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;--------------------------------------------------------------------- 
            AH_SRC_SUBTRACTION_BX:
            CMP BH, 1  ; AH
            JNE AL_SRC_SUBTRACTION_BX
            ;----------------------EXECUTE COMMAND REG TO REG--------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;---------------------------------------------------------------------
            AL_SRC_SUBTRACTION_BX:
            CMP BH, 2  ; AL
            JNE BX_SRC_SUBTRACTION_BX
            ;----------------------EXECUTE COMMAND REG TO REG--------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;---------------------------------------------------------------------

            BX_SRC_SUBTRACTION_BX:
            CMP BH, 3  ; BX
            JNE BH_SRC_SUBTRACTION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                SUB BX, BX
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            BH_SRC_SUBTRACTION_BX:
            CMP BH, 4  ; BH
            JNE BL_SRC_SUBTRACTION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            BL_SRC_SUBTRACTION_BX:
            CMP BH, 5  ; BL
            JNE CX_SRC_SUBTRACTION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CX_SRC_SUBTRACTION_BX:
            CMP BH, 6  ; CX
            JNE CH_SRC_SUBTRACTION_BX
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[4]
                SUB BX, AX
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

            CH_SRC_SUBTRACTION_BX:
            CMP BH, 7  ; CH
            JNE CL_SRC_SUBTRACTION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            CL_SRC_SUBTRACTION_BX:
            CMP BH, 8  ; CL
            JNE DX_SRC_SUBTRACTION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            DX_SRC_SUBTRACTION_BX:
            CMP BH, 9  ; DX
            JNE DH_SRC_SUBTRACTION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[6]
                SUB BX, AX
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            DH_SRC_SUBTRACTION_BX:
            CMP BH, 10  ; DH
            JNE DL_SRC_SUBTRACTION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            DL_SRC_SUBTRACTION_BX:
            CMP BH, 11  ; DL
            JNE SI_SRC_SUBTRACTION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

            SI_SRC_SUBTRACTION_BX:
            CMP BH, 12  ; SI
            JNE DI_SRC_SUBTRACTION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[8]
                SUB BX, AX
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            DI_SRC_SUBTRACTION_BX:
            CMP BH, 13  ; DI
            JNE SP_SRC_SUBTRACTION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[10]
                SUB BX, AX
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            SP_SRC_SUBTRACTION_BX:
            CMP BH, 14  ; SP
            JNE BP_SRC_SUBTRACTION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[12]
                SUB BX, AX
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            BP_SRC_SUBTRACTION_BX:
            CMP BH, 15  ; BP
            JNE VAL_SRC_SUBTRACTION_BX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[14]
                SUB BX, AX
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
            VAL_SRC_SUBTRACTION_BX:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV AX, VALUE_WORD
                MOV BX, PLAYER1_REGS[2]
                SUB BX, AX
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

      BH_DEST_SUBTRACTION:
        CMP BH, 4  ; BH
        JNE BL_DEST_SUBTRACTION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_SUBTRACTION_BH
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS [2]
            MOV AH, VALUE_BYTE
            SUB BH, AH
            MOV  PLAYER1_REGS[2], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_SUBTRACTION_BH:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_SUBTRACTION_BH:
            CMP [DI], AX
            JE CHOSEN_SOURCE_SUBTRACTION_BH
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_SUBTRACTION_BH

        CHOSEN_SOURCE_SUBTRACTION_BH:
          AX_SRC_SUBTRACTION_BH:
            CMP BH, 0  ; AX
            JNE AH_SRC_SUBTRACTION_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;----------------------------------------------------------------------- 
          AH_SRC_SUBTRACTION_BH:
            CMP BH, 1  ; AH
            JNE AL_SRC_SUBTRACTION_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[0]
                SUB BH, AH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          AL_SRC_SUBTRACTION_BH:
            CMP BH, 2  ; AL
            JNE BX_SRC_SUBTRACTION_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[0]
                SUB BH, AL
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BX_SRC_SUBTRACTION_BH:
            CMP BH, 3  ; BX
            JNE BH_SRC_SUBTRACTION_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BH_SRC_SUBTRACTION_BH:
            CMP BH, 4  ; BH
            JNE BL_SRC_SUBTRACTION_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                SUB BH, BH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BL_SRC_SUBTRACTION_BH:
            CMP BH, 5  ; BL
            JNE CX_SRC_SUBTRACTION_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2] 
                SUB BH, BL
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CX_SRC_SUBTRACTION_BH:
            CMP BH, 6  ; CX
            JNE CH_SRC_SUBTRACTION_BH
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CH_SRC_SUBTRACTION_BH:
            CMP BH, 7  ; CH
            JNE CL_SRC_SUBTRACTION_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[4]
                SUB BH, AH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CL_SRC_SUBTRACTION_BH:
            CMP BH, 8  ; CL
            JNE DX_SRC_SUBTRACTION_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[4]
                SUB BH, AL
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------

          DX_SRC_SUBTRACTION_BH:
            CMP BH, 9  ; DX
            JNE DH_SRC_SUBTRACTION_BH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          DH_SRC_SUBTRACTION_BH:
            CMP BH, 10  ; DH
            JNE DL_SRC_SUBTRACTION_BH
            ;----------------------EXECUTE COMMAND REG TO REG------------------------ 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[6]
                SUB BH, AH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          DL_SRC_SUBTRACTION_BH:
            CMP BH, 11  ; DL
            JNE SI_SRC_SUBTRACTION_BH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[6]
                SUB BH, AL
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          SI_SRC_SUBTRACTION_BH:
            CMP BH, 12  ; SI
            JNE DI_SRC_SUBTRACTION_BH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          DI_SRC_SUBTRACTION_BH:
            CMP BH, 13  ; DI
            JNE SP_SRC_SUBTRACTION_BH
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          SP_SRC_SUBTRACTION_BH:
            CMP BH, 14  ; SP
            JNE BP_SRC_SUBTRACTION_BH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          BP_SRC_SUBTRACTION_BH:
            CMP BH, 15  ; BP
            JNE VAL_SRC_SUBTRACTION_BH
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          VAL_SRC_SUBTRACTION_BH:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX ,PLAYER1_REGS[2]
                MOV AH, VALUE_BYTE
                SUB BH, AH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

      BL_DEST_SUBTRACTION:
        CMP BH, 5  ; BL
        JNE CX_DEST_SUBTRACTION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_SUBTRACTION_BL
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS [2]
            MOV AH, VALUE_BYTE
            SUB BL, AH
            MOV  PLAYER1_REGS[2], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_SUBTRACTION_BL:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_SUBTRACTION_BL:
            CMP [DI], AX
            JE CHOSEN_SOURCE_SUBTRACTION_BL
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_SUBTRACTION_BL

        CHOSEN_SOURCE_SUBTRACTION_BL:
          AX_SRC_SUBTRACTION_BL:
            CMP BH, 0  ; AX
            JNE AH_SRC_SUBTRACTION_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;----------------------------------------------------------------------- 
          AH_SRC_SUBTRACTION_BL:
            CMP BH, 1  ; AH
            JNE AL_SRC_SUBTRACTION_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[0]
                SUB BL, AH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          AL_SRC_SUBTRACTION_BL:
            CMP BH, 2  ; AL
            JNE BX_SRC_SUBTRACTION_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[0]
                SUB BL, AL
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BX_SRC_SUBTRACTION_BL:
            CMP BH, 3  ; BX
            JNE BH_SRC_SUBTRACTION_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BH_SRC_SUBTRACTION_BL:
            CMP BH, 4  ; BH
            JNE BL_SRC_SUBTRACTION_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                SUB BL, BH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BL_SRC_SUBTRACTION_BL:
            CMP BH, 5  ; BL
            JNE CX_SRC_SUBTRACTION_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2] 
                SUB BL, BL
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CX_SRC_SUBTRACTION_BL:
            CMP BH, 6  ; CX
            JNE CH_SRC_SUBTRACTION_BL
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CH_SRC_SUBTRACTION_BL:
            CMP BH, 7  ; CH
            JNE CL_SRC_SUBTRACTION_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[4]
                SUB BL, AH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CL_SRC_SUBTRACTION_BL:
            CMP BH, 8  ; CL
            JNE DX_SRC_SUBTRACTION_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[4]
                SUB BL, AL
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------

          DX_SRC_SUBTRACTION_BL:
            CMP BH, 9  ; DX
            JNE DH_SRC_SUBTRACTION_BL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          DH_SRC_SUBTRACTION_BL:
            CMP BH, 10  ; DH
            JNE DL_SRC_SUBTRACTION_BL
            ;----------------------EXECUTE COMMAND REG TO REG------------------------ 
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[6]
                SUB BL, AH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          DL_SRC_SUBTRACTION_BL:
            CMP BH, 11  ; DL
            JNE SI_SRC_SUBTRACTION_BL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                MOV BX, PLAYER1_REGS[2]
                MOV AX, PLAYER1_REGS[6]
                SUB BL, AL
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          SI_SRC_SUBTRACTION_BL:
            CMP BL, 12  ; SI
            JNE DI_SRC_SUBTRACTION_BL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          DI_SRC_SUBTRACTION_BL:
            CMP BH, 13  ; DI
            JNE SP_SRC_SUBTRACTION_BL
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          SP_SRC_SUBTRACTION_BL:
            CMP BH, 14  ; SP
            JNE BP_SRC_SUBTRACTION_BL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          BP_SRC_SUBTRACTION_BL:
            CMP BH, 15  ; BP
            JNE VAL_SRC_SUBTRACTION_BL
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          VAL_SRC_SUBTRACTION_BL:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX ,PLAYER1_REGS[2]
                MOV AH, VALUE_BYTE
                SUB BH, AH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------


;---------MOV CX,  ----------          
      CX_DEST_SUBTRACTION:
        CMP BH, 6  ; CX
        JNE CH_DEST_SUBTRACTION        
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_SUBTRACTION_CX
            CALL CONVERT_TO_NUMBER
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[4]
            MOV AX, VALUE_WORD
            SUB BX, AX
            MOV PLAYER1_REGS[4], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_SUBTRACTION_CX:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_SUBTRACTION_CX:
            CMP [DI], AX
            JE CHOSEN_SOURCE_SUBTRACTION
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_SUBTRACTION_CX

        CHOSEN_SOURCE_SUBTRACTION:
          AX_SRC_SUBTRACTION_CX:
            CMP BH, 0  ; AX
            JNE AH_SRC_SUBTRACTION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[0]
                SUB BX, AX
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------

          AH_SRC_SUBTRACTION_CX:
            CMP BH, 1  ; AH
            JNE AL_SRC_SUBTRACTION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          AL_SRC_SUBTRACTION_CX:
            CMP BH, 2  ; AL
            JNE BX_SRC_SUBTRACTION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          BX_SRC_SUBTRACTION_CX:
            CMP BH, 3  ; BX
            JNE BH_SRC_SUBTRACTION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[2]
                SUB BX, AX
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BH_SRC_SUBTRACTION_CX:
            CMP BH, 4  ; BH
            JNE BL_SRC_SUBTRACTION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BL_SRC_SUBTRACTION_CX:
            CMP BH, 5  ; BL
            JNE CX_SRC_SUBTRACTION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CX_SRC_SUBTRACTION_CX:
            CMP BH, 6  ; CX
            JNE CH_SRC_SUBTRACTION_CX
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                SUB BX, BX
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CH_SRC_SUBTRACTION_CX:
            CMP BH, 7  ; CH
            JNE CL_SRC_SUBTRACTION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CL_SRC_SUBTRACTION_CX:
            CMP BH, 8  ; CL
            JNE DX_SRC_SUBTRACTION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DX_SRC_SUBTRACTION_CX:
            CMP BH, 9  ; DX
            JNE DH_SRC_SUBTRACTION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[6]
                SUB BX, AX
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DH_SRC_SUBTRACTION_CX:
            CMP BH, 10  ; DH
            JNE DL_SRC_SUBTRACTION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DL_SRC_SUBTRACTION_CX:
            CMP BH, 11  ; DL
            JNE SI_SRC_SUBTRACTION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          SI_SRC_SUBTRACTION_CX:
            CMP BH, 12  ; SI
            JNE DI_SRC_SUBTRACTION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[8]
                SUB BX, AX
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DI_SRC_SUBTRACTION_CX:
            CMP BH, 13  ; DI
            JNE SP_SRC_SUBTRACTION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[10]
                SUB BX, AX
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          SP_SRC_SUBTRACTION_CX:
            CMP BH, 14  ; SP
            JNE BP_SRC_SUBTRACTION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[12]
                SUB BX, AX
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          BP_SRC_SUBTRACTION_CX:
            CMP BH, 15  ; BP
            JNE VAL_SRC_SUBTRACTION_CX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[14]
                SUB BX, AX
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          VAL_SRC_SUBTRACTION_CX:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX, PLAYER1_REGS[4]
                MOV AX, VALUE_WORD
                SUB BX, AX
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

      CH_DEST_SUBTRACTION:
        CMP BH, 7  ; CH
        JNE CL_DEST_SUBTRACTION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_SUBTRACTION_CH
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS [4]
            MOV AH, VALUE_BYTE
            SUB BH, AH
            MOV  PLAYER1_REGS[4], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_SUBTRACTION_CH:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_SUBTRACTION_CH:
            CMP [DI], AX
            JE CHOSEN_SOURCE_SUBTRACTION_CH
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_SUBTRACTION_CH

        CHOSEN_SOURCE_SUBTRACTION_CH:
          AX_SRC_SUBTRACTION_CH:
            CMP BH, 0  ; AX
            JNE AH_SRC_SUBTRACTION_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;----------------------------------------------------------------------- 
          AH_SRC_SUBTRACTION_CH: ;SUB CH,AH
            CMP BH, 1  ; AH
            JNE AL_SRC_SUBTRACTION_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[0]
                SUB BH, AH
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          AL_SRC_SUBTRACTION_CH: ;SUB CH,AL
            CMP BH, 2  ; AL
            JNE BX_SRC_SUBTRACTION_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[0]
                SUB BH, AL
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BX_SRC_SUBTRACTION_CH:
            CMP BH, 3  ; BX
            JNE BH_SRC_SUBTRACTION_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BH_SRC_SUBTRACTION_CH: ;SUB CH,BH
            CMP BH, 4  ; BH
            JNE BL_SRC_SUBTRACTION_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[2]
                SUB BH, AH
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BL_SRC_SUBTRACTION_CH: ;SUB CH,BL
            CMP BH, 5  ; BL
            JNE CX_SRC_SUBTRACTION_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[2]
                SUB BH, AL
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CX_SRC_SUBTRACTION_CH:
            CMP BH, 6  ; CX
            JNE CH_SRC_SUBTRACTION_CH
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CH_SRC_SUBTRACTION_CH: ;SUB CH,CH
            CMP BH, 7  ; CH
            JNE CL_SRC_SUBTRACTION_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                SUB BH, BH
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CL_SRC_SUBTRACTION_CH: ;SUB CH,CL
            CMP BH, 8  ; CL
            JNE DX_SRC_SUBTRACTION_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                SUB BH, BL
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------

          DX_SRC_SUBTRACTION_CH: 
            CMP BH, 9  ; DX
            JNE DH_SRC_SUBTRACTION_CH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          DH_SRC_SUBTRACTION_CH: ;SUB CH,DH
            CMP BH, 10  ; DH
            JNE DL_SRC_SUBTRACTION_CH
            ;----------------------EXECUTE COMMAND REG TO REG------------------------ 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[6]
                SUB BH, AH
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          DL_SRC_SUBTRACTION_CH: ;SUB CH, DL
            CMP BH, 11  ; DL
            JNE SI_SRC_SUBTRACTION_CH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[6]
                SUB BH, AL
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          SI_SRC_SUBTRACTION_CH:
            CMP BH, 12  ; SI
            JNE DI_SRC_SUBTRACTION_CH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          DI_SRC_SUBTRACTION_CH:
            CMP BH, 13  ; DI
            JNE SP_SRC_SUBTRACTION_CH
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          SP_SRC_SUBTRACTION_CH:
            CMP BH, 14  ; SP
            JNE BP_SRC_SUBTRACTION_CH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          BP_SRC_SUBTRACTION_CH:
            CMP BH, 15  ; BP
            JNE VAL_SRC_SUBTRACTION_CH
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          VAL_SRC_SUBTRACTION_CH:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX ,PLAYER1_REGS[2]
                MOV AH, VALUE_BYTE
                SUB BH, AH
                MOV PLAYER1_REGS[2], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------


      CL_DEST_SUBTRACTION:    
        CMP BH, 8  ; CL
        JNE DX_DEST_SUBTRACTION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_SUBTRACTION_CL
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS [2]
            MOV AH, VALUE_BYTE
            SUB BL, AH
            MOV  PLAYER1_REGS[2], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_SUBTRACTION_CL:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_SUBTRACTION_CL:
            CMP [DI], AX
            JE CHOSEN_SOURCE_SUBTRACTION_CL
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_SUBTRACTION_CL

        CHOSEN_SOURCE_SUBTRACTION_CL:
          AX_SRC_SUBTRACTION_CL:
            CMP BH, 0  ; AX
            JNE AH_SRC_SUBTRACTION_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          AH_SRC_SUBTRACTION_CL: ; SUB CL,AH
            CMP BH, 1   ; AH
            JNE AL_SRC_SUBTRACTION_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[0]
                SUB BL, AH
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          AL_SRC_SUBTRACTION_CL: ;SUB CL, AL
            CMP BH, 2  ; AL
            JNE BX_SRC_SUBTRACTION_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[0]
                SUB BL, AL
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BX_SRC_SUBTRACTION_CL:
            CMP BH, 3  ; BX
            JNE BH_SRC_SUBTRACTION_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BH_SRC_SUBTRACTION_CL: ; SUB CL, BH
            CMP BH, 4  ; BH
            JNE BL_SRC_SUBTRACTION_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[2]
                SUB BL, AH
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BL_SRC_SUBTRACTION_CL: ; SUB CL, BL
            CMP BH, 5  ; BL
            JNE CX_SRC_SUBTRACTION_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[2]
                SUB BL, AL
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CX_SRC_SUBTRACTION_CL:
            CMP BH, 6  ; CX
            JNE CH_SRC_SUBTRACTION_CL
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CH_SRC_SUBTRACTION_CL: ;SUB CL, CH
            CMP BH, 7  ; CH
            JNE CL_SRC_SUBTRACTION_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                SUB BL, BH
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CL_SRC_SUBTRACTION_CL: ;SUB CL,CL
            CMP BH, 8  ; CL
            JNE DX_SRC_SUBTRACTION_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[4]
                SUB BL, BL
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------

          DX_SRC_SUBTRACTION_CL:
            CMP BH, 9  ; DX
            JNE DH_SRC_SUBTRACTION_CL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          DH_SRC_SUBTRACTION_CL: ;SUB CL, DH
            CMP BH, 10  ; DH
            JNE DL_SRC_SUBTRACTION_CL
            ;----------------------EXECUTE COMMAND REG TO REG------------------------ 
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[6]
                SUB BL, AH
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          DL_SRC_SUBTRACTION_CL: ;SUB CL, DL
            CMP BH, 11  ; DL
            JNE SI_SRC_SUBTRACTION_CL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                MOV BX, PLAYER1_REGS[4]
                MOV AX, PLAYER1_REGS[6]
                SUB BL, AL
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          SI_SRC_SUBTRACTION_CL:
            CMP BL, 12  ; SI
            JNE DI_SRC_SUBTRACTION_CL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          DI_SRC_SUBTRACTION_CL:
            CMP BH, 13  ; DI
            JNE SP_SRC_SUBTRACTION_CL
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          SP_SRC_SUBTRACTION_CL:
            CMP BH, 14  ; SP
            JNE BP_SRC_SUBTRACTION_CL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          BP_SRC_SUBTRACTION_CL:
            CMP BH, 15  ; BP
            JNE VAL_SRC_SUBTRACTION_CL
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          VAL_SRC_SUBTRACTION_CL:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX ,PLAYER1_REGS[4]
                MOV AH, VALUE_BYTE
                SUB BL, AH
                MOV PLAYER1_REGS[4], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

      DX_DEST_SUBTRACTION:
        CMP BH, 9  ; DX
        JNE DH_DEST_SUBTRACTION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_SUBTRACTION_DX
            CALL CONVERT_TO_NUMBER
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[6]
            MOV AX, VALUE_WORD
            SUB BX, AX
            MOV PLAYER1_REGS[6], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_SUBTRACTION_DX:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_SUBTRACTION_DX:
            CMP [DI], AX
            JE CHOSEN_SOURCE_SUBTRACTION_DX
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_SUBTRACTION_DX

        CHOSEN_SOURCE_SUBTRACTION_DX:
          AX_SRC_SUBTRACTION_DX:
            CMP BH, 0  ; AX
            JNE AH_SRC_SUBTRACTION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[0]
                SUB BX, AX
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------
          AH_SRC_SUBTRACTION_DX:
            CMP BH, 1  ; AH
            JNE AL_SRC_SUBTRACTION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------
          AL_SRC_SUBTRACTION_DX:
            CMP BH, 2  ; AL
            JNE BX_SRC_SUBTRACTION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          BX_SRC_SUBTRACTION_DX:
            CMP BH, 3  ; BX
            JNE BH_SRC_SUBTRACTION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[2]
                SUB BX, AX
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BH_SRC_SUBTRACTION_DX:
            CMP BH, 4  ; BH
            JNE BL_SRC_SUBTRACTION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BL_SRC_SUBTRACTION_DX:
            CMP BH, 5  ; BL
            JNE CX_SRC_SUBTRACTION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CX_SRC_SUBTRACTION_DX:
            CMP BH, 6  ; CX
            JNE CH_SRC_SUBTRACTION_DX
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[4]
                SUB BX, AX
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CH_SRC_SUBTRACTION_DX:
            CMP BH, 7  ; CH
            JNE CL_SRC_SUBTRACTION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CL_SRC_SUBTRACTION_DX:
            CMP BH, 8  ; CL
            JNE DX_SRC_SUBTRACTION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DX_SRC_SUBTRACTION_DX:
            CMP BH, 9  ; DX
            JNE DH_SRC_SUBTRACTION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                SUB BX, BX
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------

          DH_SRC_SUBTRACTION_DX:
            CMP BH, 10  ; DH
            JNE DL_SRC_SUBTRACTION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          DL_SRC_SUBTRACTION_DX:
            CMP BH, 11  ; DL
            JNE SI_SRC_SUBTRACTION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          SI_SRC_SUBTRACTION_DX:
            CMP BH, 12  ; SI
            JNE DI_SRC_SUBTRACTION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[8]
                SUB BX, AX
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DI_SRC_SUBTRACTION_DX:
            CMP BH, 13  ; DI
            JNE SP_SRC_SUBTRACTION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[10]
                SUB BX, AX
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          SP_SRC_SUBTRACTION_DX:
            CMP BH, 14  ; SP
            JNE BP_SRC_SUBTRACTION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[12]
                SUB BX, AX
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          BP_SRC_SUBTRACTION_DX:
            CMP BH, 15  ; BP
            JNE VAL_SRC_SUBTRACTION_DX
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[14]
                SUB BX, AX
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          VAL_SRC_SUBTRACTION_DX:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX, PLAYER1_REGS[6]
                MOV AX, VALUE_WORD
                SUB BX, AX
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------

      DH_DEST_SUBTRACTION:
        CMP BH, 10 ; DH
        JNE DL_DEST_SUBTRACTION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_SUBTRACTION_DH
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS [6]
            MOV AH, VALUE_BYTE
            SUB BH, AH
            MOV  PLAYER1_REGS[6], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_SUBTRACTION_DH:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_SUBTRACTION_DH:
            CMP [DI], AX
            JE CHOSEN_SOURCE_SUBTRACTION_DH
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_SUBTRACTION_DH

        CHOSEN_SOURCE_SUBTRACTION_DH:
          AX_SRC_SUBTRACTION_DH:
            CMP BH, 0  ; AX
            JNE AH_SRC_SUBTRACTION_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;----------------------------------------------------------------------- 
          AH_SRC_SUBTRACTION_DH: ;SUB DH,AH
            CMP BH, 1  ; AH
            JNE AL_SRC_SUBTRACTION_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[0]
                SUB BH, AH
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          AL_SRC_SUBTRACTION_DH: ;SUB DH,AL
            CMP BH, 2  ; AL
            JNE BX_SRC_SUBTRACTION_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[0]
                SUB BH, AL
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BX_SRC_SUBTRACTION_DH:
            CMP BH, 3  ; BX
            JNE BH_SRC_SUBTRACTION_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BH_SRC_SUBTRACTION_DH: ;SUB DH,BH
            CMP BH, 4  ; BH
            JNE BL_SRC_SUBTRACTION_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[2]
                SUB BH, AH
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BL_SRC_SUBTRACTION_DH: ;SUB DH,BL
            CMP BH, 5  ; BL
            JNE CX_SRC_SUBTRACTION_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[2]
                SUB BH, AL
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CX_SRC_SUBTRACTION_DH:
            CMP BH, 6  ; CX
            JNE CH_SRC_SUBTRACTION_DH
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CH_SRC_SUBTRACTION_DH: ;SUB DH,CH
            CMP BH, 7  ; CH
            JNE CL_SRC_SUBTRACTION_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[4]
                SUB BH, AH
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CL_SRC_SUBTRACTION_DH: ;SUB DH,CL
            CMP BH, 8  ; CL
            JNE DX_SRC_SUBTRACTION_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[4]
                SUB BH, AL
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------

          DX_SRC_SUBTRACTION_DH: 
            CMP BH, 9  ; DX
            JNE DH_SRC_SUBTRACTION_DH
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          DH_SRC_SUBTRACTION_DH: ;SUB DH,DH
            CMP BH, 10  ; DH
            JNE DL_SRC_SUBTRACTION_DH
            ;----------------------EXECUTE COMMAND REG TO REG------------------------ 
                MOV BX, PLAYER1_REGS[6]
                SUB BH, BH
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          DL_SRC_SUBTRACTION_DH: ;SUB DH, DL
            CMP BH, 11  ; DL
            JNE SI_SRC_SUBTRACTION_DH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                MOV BX, PLAYER1_REGS[6]
                SUB BH, BL
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          SI_SRC_SUBTRACTION_DH:
            CMP BH, 12  ; SI
            JNE DI_SRC_SUBTRACTION_DH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          DI_SRC_SUBTRACTION_DH:
            CMP BH, 13  ; DI
            JNE SP_SRC_SUBTRACTION_DH
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          SP_SRC_SUBTRACTION_DH:
            CMP BH, 14  ; SP
            JNE BP_SRC_SUBTRACTION_DH
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          BP_SRC_SUBTRACTION_DH:
            CMP BH, 15  ; BP
            JNE VAL_SRC_SUBTRACTION_DH
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          VAL_SRC_SUBTRACTION_DH:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX ,PLAYER1_REGS[6]
                MOV AH, VALUE_BYTE
                SUB BH, AH
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

      DL_DEST_SUBTRACTION:
        CMP BH, 11 ; DL
        JNE SI_DEST_SUBTRACTION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE,3
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE,4
            JE EXIT_SIZE_MISMATCH
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_SUBTRACTION_DL
            CALL CONVERT_TO_NUMBER          
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS [6]
            MOV AH, VALUE_BYTE
            SUB BL, AH
            MOV  PLAYER1_REGS[6], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_SUBTRACTION_DL:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_SUBTRACTION_DL:
            CMP [DI], AX
            JE CHOSEN_SOURCE_SUBTRACTION_DL
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_SUBTRACTION_DL

        CHOSEN_SOURCE_SUBTRACTION_DL:
          AX_SRC_SUBTRACTION_DL:
            CMP BH, 0  ; AX
            JNE AH_SRC_SUBTRACTION_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;----------------------------------------------------------------------- 
          AH_SRC_SUBTRACTION_DL: ;SUB DL,AH
            CMP BH, 1  ; AH
            JNE AL_SRC_SUBTRACTION_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[0]
                SUB BL, AH
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          AL_SRC_SUBTRACTION_DL: ;SUB DL,AL
            CMP BH, 2  ; AL
            JNE BX_SRC_SUBTRACTION_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[0]
                SUB BL, AL
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BX_SRC_SUBTRACTION_DL:
            CMP BH, 3  ; BX
            JNE BH_SRC_SUBTRACTION_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BH_SRC_SUBTRACTION_DL: ;SUB DL,BH
            CMP BH, 4  ; BH
            JNE BL_SRC_SUBTRACTION_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[2]
                SUB BL, AH
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BL_SRC_SUBTRACTION_DL: ;SUB DL,BL
            CMP BH, 5  ; BL
            JNE CX_SRC_SUBTRACTION_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[2]
                SUB BL, AL
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CX_SRC_SUBTRACTION_DL:
            CMP BH, 6  ; CX
            JNE CH_SRC_SUBTRACTION_DL
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CH_SRC_SUBTRACTION_DL: ;SUB DL,CH
            CMP BH, 7  ; CH
            JNE CL_SRC_SUBTRACTION_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[4]
                SUB BL, AH
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CL_SRC_SUBTRACTION_DL: ;SUB DL,CL
            CMP BH, 8  ; CL
            JNE DX_SRC_SUBTRACTION_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[6]
                MOV AX, PLAYER1_REGS[4]
                SUB BL, AL
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;-----------------------------------------------------------------------

          DX_SRC_SUBTRACTION_DL: 
            CMP BH, 9  ; DX
            JNE DL_SRC_SUBTRACTION_DL
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 JMP EXIT_SIZE_MISMATCH
            ;-----------------------------------------------------------------------

          DH_SRC_SUBTRACTION_DL: ;SUB DL,DH
            CMP BH, 10  ; DH
            JNE DL_SRC_SUBTRACTION_DL
            ;----------------------EXECUTE COMMAND REG TO REG------------------------ 
                MOV BX, PLAYER1_REGS[6]
                SUB BL, BH
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          DL_SRC_SUBTRACTION_DL: ;SUB DL, DL
            CMP BH, 11  ; DL
            JNE SI_SRC_SUBTRACTION_DL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                MOV BX, PLAYER1_REGS[6]
                SUB BL, BL
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          SI_SRC_SUBTRACTION_DL:
            CMP BH, 12  ; SI
            JNE DI_SRC_SUBTRACTION_DL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          DI_SRC_SUBTRACTION_DL:
            CMP BH, 13  ; DI
            JNE SP_SRC_SUBTRACTION_DL
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          SP_SRC_SUBTRACTION_DL:
            CMP BH, 14  ; SP
            JNE BP_SRC_SUBTRACTION_DL
            ;----------------------EXECUTE COMMAND REG TO REG------------------
                 JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          BP_SRC_SUBTRACTION_DL:
            CMP BH, 15  ; BP
            JNE VAL_SRC_SUBTRACTION_DL
            ;----------------------EXECUTE COMMAND REG TO REG------------------ 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          VAL_SRC_SUBTRACTION_DL:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX ,PLAYER1_REGS[6]
                MOV AH, VALUE_BYTE
                SUB BL, AH
                MOV PLAYER1_REGS[6], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

      SI_DEST_SUBTRACTION:
        CMP BH, 12 ; SI
        JNE DI_DEST_SUBTRACTION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_SUBTRACTION_SI
            CALL CONVERT_TO_NUMBER
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[8]
            MOV AX, VALUE_WORD
            SUB BX, AX
            MOV PLAYER1_REGS[8], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_SUBTRACTION_SI:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_SUBTRACTION_SI:
            CMP [DI], AX
            JE CHOSEN_SOURCE_SUBTRACTION_SI
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_SUBTRACTION_SI

        CHOSEN_SOURCE_SUBTRACTION_SI:
          AX_SRC_SUBTRACTION_SI:
            CMP BH, 0  ; AX
            JNE AH_SRC_SUBTRACTION_SI
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[8]
                MOV AX, PLAYER1_REGS[0]
                SUB BX, AX
                MOV PLAYER1_REGS[8], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          AH_SRC_SUBTRACTION_SI:
            CMP BH, 1  ; AH
            JNE AL_SRC_SUBTRACTION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          AL_SRC_SUBTRACTION_SI:
            CMP BH, 2  ; AL
            JNE BX_SRC_SUBTRACTION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BX_SRC_SUBTRACTION_SI:
            CMP BH, 3  ; BX
            JNE BH_SRC_SUBTRACTION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[8]
                MOV AX, PLAYER1_REGS[2]
                SUB BX, AX
                MOV PLAYER1_REGS[8], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BH_SRC_SUBTRACTION_SI:
            CMP BH, 4  ; BH
            JNE BL_SRC_SUBTRACTION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BL_SRC_SUBTRACTION_SI:
            CMP BH, 5  ; BL
            JNE CX_SRC_SUBTRACTION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CX_SRC_SUBTRACTION_SI:
            CMP BH, 6  ; CX
            JNE CH_SRC_SUBTRACTION_SI
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[8]
                MOV AX, PLAYER1_REGS[4]
                SUB BX, AX
                MOV PLAYER1_REGS[8], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CH_SRC_SUBTRACTION_SI:
            CMP BH, 7  ; CH
            JNE CL_SRC_SUBTRACTION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CL_SRC_SUBTRACTION_SI:
            CMP BH, 8  ; CL
            JNE DX_SRC_SUBTRACTION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DX_SRC_SUBTRACTION_SI:
            CMP BH, 9  ; DX
            JNE DH_SRC_SUBTRACTION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[8]
                MOV AX, PLAYER1_REGS[6]
                SUB BX, AX
                MOV PLAYER1_REGS[8], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DH_SRC_SUBTRACTION_SI:
            CMP BH, 10  ; DH
            JNE DL_SRC_SUBTRACTION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DL_SRC_SUBTRACTION_SI:
            CMP BH, 11  ; DL
            JNE SI_SRC_SUBTRACTION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          SI_SRC_SUBTRACTION_SI:
            CMP BH, 12  ; SI
            JNE DI_SRC_SUBTRACTION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[8]
                SUB BX, BX
                MOV PLAYER1_REGS[8], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DI_SRC_SUBTRACTION_SI:
            CMP BH, 13  ; DI
            JNE SP_SRC_SUBTRACTION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[8]
                MOV AX, PLAYER1_REGS[10]
                SUB BX, AX
                MOV PLAYER1_REGS[8], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          SP_SRC_SUBTRACTION_SI:
            CMP BH, 14  ; SP
            JNE BP_SRC_SUBTRACTION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[8]
                MOV AX, PLAYER1_REGS[12]
                SUB BX, AX
                MOV PLAYER1_REGS[8], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          BP_SRC_SUBTRACTION_SI:
            CMP BH, 15  ; BP
            JNE VAL_SRC_SUBTRACTION_SI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[8]
                MOV AX, PLAYER1_REGS[14]
                SUB BX, AX
                MOV PLAYER1_REGS[8], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          VAL_SRC_SUBTRACTION_SI:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX, PLAYER1_REGS[8]
                MOV AX, VALUE_WORD
                SUB BX, AX
                MOV PLAYER1_REGS[8], BX
                JMP EXIT_EXECUTE
                
            ;------------------------------------------------------------------

      DI_DEST_SUBTRACTION:
        CMP BH, 13 ; DI
        JNE SP_DEST_SUBTRACTION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_SUBTRACTION_DI
            CALL CONVERT_TO_NUMBER
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[10]
            MOV AX, VALUE_WORD
            SUB BX, AX
            MOV PLAYER1_REGS[10], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_SUBTRACTION_DI:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_SUBTRACTION_DI:
            CMP [DI], AX
            JE CHOSEN_SOURCE_SUBTRACTION_DI
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_SUBTRACTION_DI

        CHOSEN_SOURCE_SUBTRACTION_DI:
          AX_SRC_SUBTRACTION_DI:
            CMP BH, 0  ; AX
            JNE AH_SRC_SUBTRACTION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[10]
                MOV AX, PLAYER1_REGS[0]
                SUB BX, AX
                MOV PLAYER1_REGS[10], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          AH_SRC_SUBTRACTION_DI:
            CMP BH, 1   ; AH
            JNE AL_SRC_SUBTRACTION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          AL_SRC_SUBTRACTION_DI:
            CMP BH, 2  ; AL
            JNE BX_SRC_SUBTRACTION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BX_SRC_SUBTRACTION_DI:
            CMP BH, 3  ; BX
            JNE BH_SRC_SUBTRACTION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[10]
                MOV AX, PLAYER1_REGS[2]
                SUB BX, AX
                MOV PLAYER1_REGS[10], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BH_SRC_SUBTRACTION_DI:
            CMP BH, 4  ; BH
            JNE BL_SRC_SUBTRACTION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BL_SRC_SUBTRACTION_DI:
            CMP BH, 5  ; BL
            JNE CX_SRC_SUBTRACTION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CX_SRC_SUBTRACTION_DI:
            CMP BH, 6  ; CX
            JNE CH_SRC_SUBTRACTION_DI
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[10]
                MOV AX, PLAYER1_REGS[4]
                SUB BX, AX
                MOV PLAYER1_REGS[10], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CH_SRC_SUBTRACTION_DI:
            CMP BH, 7  ; CH
            JNE CL_SRC_SUBTRACTION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CL_SRC_SUBTRACTION_DI:
            CMP BH, 8  ; CL
            JNE DX_SRC_SUBTRACTION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DX_SRC_SUBTRACTION_DI:
            CMP BH, 9  ; DX
            JNE DH_SRC_SUBTRACTION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[10]
                MOV AX, PLAYER1_REGS[6]
                SUB BX, AX
                MOV PLAYER1_REGS[10], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DH_SRC_SUBTRACTION_DI:
            CMP BH, 10  ; DH
            JNE DL_SRC_SUBTRACTION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DL_SRC_SUBTRACTION_DI:
            CMP BH, 11  ; DL
            JNE SI_SRC_SUBTRACTION_DI

            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          SI_SRC_SUBTRACTION_DI:
            CMP BH, 12  ; SI
            JNE DI_SRC_SUBTRACTION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[10]
                MOV AX, PLAYER1_REGS[8]
                SUB BX, AX
                MOV PLAYER1_REGS[10], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DI_SRC_SUBTRACTION_DI:
            CMP BH, 13  ; DI
            JNE SP_SRC_SUBTRACTION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[10]
                SUB BX, BX
                MOV PLAYER1_REGS[10], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          SP_SRC_SUBTRACTION_DI:
            CMP BH, 14  ; SP
            JNE BP_SRC_SUBTRACTION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[10]
                MOV AX, PLAYER1_REGS[12]
                SUB BX, AX
                MOV PLAYER1_REGS[10], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          BP_SRC_SUBTRACTION_DI:
            CMP BH, 15  ; BP
            JNE VAL_SRC_SUBTRACTION_DI
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[10]
                MOV AX, PLAYER1_REGS[4]
                SUB BX, AX
                MOV PLAYER1_REGS[10], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          VAL_SRC_SUBTRACTION_DI:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV AX, VALUE_WORD
                MOV PLAYER1_REGS[10],AX
                JMP EXIT_EXECUTE
                
            ;------------------------------------------------------------------
      SP_DEST_SUBTRACTION:
        CMP BH, 14 ; SP
        JNE BP_DEST_SUBTRACTION
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_SUBTRACTION_SP
            CALL CONVERT_TO_NUMBER
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[12]
            MOV AX, VALUE_WORD
            SUB BX, AX
            MOV PLAYER1_REGS[12], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_SUBTRACTION_SP:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_SUBTRACTION_SP:
            CMP [DI], AX
            JE CHOSEN_SOURCE_SUBTRACTION_SP
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_SUBTRACTION_SP

        CHOSEN_SOURCE_SUBTRACTION_SP:
          AX_SRC_SUBTRACTION_SP:
            CMP BH, 0  ; AX
            JNE AH_SRC_SUBTRACTION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[12]
                MOV AX, PLAYER1_REGS[0]
                SUB BX, AX
                MOV PLAYER1_REGS[12], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          AH_SRC_SUBTRACTION_SP:
            CMP BH, 1  ; AH
            JNE AL_SRC_SUBTRACTION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          AL_SRC_SUBTRACTION_SP:
            CMP BH, 2  ; AL
            JNE BX_SRC_SUBTRACTION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BX_SRC_SUBTRACTION_SP:
            CMP BH, 3  ; BX
            JNE BH_SRC_SUBTRACTION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[12]
                MOV AX, PLAYER1_REGS[2]
                SUB BX, AX
                MOV PLAYER1_REGS[12], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BH_SRC_SUBTRACTION_SP:
            CMP BH, 4  ; BH
            JNE BL_SRC_SUBTRACTION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BL_SRC_SUBTRACTION_SP:
            CMP BH, 5  ; BL
            JNE CX_SRC_SUBTRACTION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CX_SRC_SUBTRACTION_SP:
            CMP BH, 6  ; CX
            JNE CH_SRC_SUBTRACTION_SP
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[12]
                MOV AX, PLAYER1_REGS[4]
                SUB BX, AX
                MOV PLAYER1_REGS[12], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CH_SRC_SUBTRACTION_SP:
            CMP BH, 7  ; CH
            JNE CL_SRC_SUBTRACTION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CL_SRC_SUBTRACTION_SP:
            CMP BH, 8  ; CL
            JNE DX_SRC_SUBTRACTION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DX_SRC_SUBTRACTION_SP:
            CMP BH, 9  ; DX
            JNE DH_SRC_SUBTRACTION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[12]
                MOV AX, PLAYER1_REGS[6]
                SUB BX, AX
                MOV PLAYER1_REGS[12], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DH_SRC_SUBTRACTION_SP:
            CMP BH, 10  ; DH
            JNE DL_SRC_SUBTRACTION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DL_SRC_SUBTRACTION_SP:
            CMP BH, 11  ; DL
            JNE SI_SRC_SUBTRACTION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          SI_SRC_SUBTRACTION_SP:
            CMP BH, 12  ; SI
            JNE DI_SRC_SUBTRACTION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[12]
                MOV AX, PLAYER1_REGS[8]
                SUB BX, AX
                MOV PLAYER1_REGS[12], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DI_SRC_SUBTRACTION_SP:
            CMP BH, 13  ; DI
            JNE SP_SRC_SUBTRACTION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[12]
                MOV AX, PLAYER1_REGS[10]
                SUB BX, AX
                MOV PLAYER1_REGS[12], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          SP_SRC_SUBTRACTION_SP:
            CMP BH, 14  ; SP
            JNE BP_SRC_SUBTRACTION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[12]
                SUB BX, BX
                MOV PLAYER1_REGS[12], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          BP_SRC_SUBTRACTION_SP:
            CMP BH, 15  ; BP
            JNE VAL_SRC_SUBTRACTION_SP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[12]
                MOV AX, PLAYER1_REGS[14]
                SUB BX, AX
                MOV PLAYER1_REGS[12], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          VAL_SRC_SUBTRACTION_SP:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX, PLAYER1_REGS[12]
                MOV AX, VALUE_WORD
                SUB BX, AX
                MOV PLAYER1_REGS[12], BX
                JMP EXIT_EXECUTE  
            ;------------------------------------------------------------------

      BP_DEST_SUBTRACTION:
        CMP BH, 15 ; BP
        JNE EXIT_SYNTAX
            CALL CHECK_FOR_COMMA 
            CMP ERROR, 1
            JE EXIT_SYNTAX
            CMP SOURCE_SIZE, 2
            JE REGISTER_OR_VALUE_SUBTRACTION_BP
            CALL CONVERT_TO_NUMBER
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[14]
            MOV AX, VALUE_WORD
            SUB BX, AX
            MOV PLAYER1_REGS[14], BX
            JMP EXIT_EXECUTE  
;--------------------IF SOURCE_SIZE = 2 THEN IT CAN BE REG OR VALUE, GET MULTIPLICATION AND IF NOT EQUAL ONE OF THE REGISTERS THEN IT IS A VALUE-----------------------------------------------              
        REGISTER_OR_VALUE_SUBTRACTION_BP:
             CALL GET_CURRENT_SOURCE
            
            MOV BH,0
            MOV AX, CURRENT_SOURCE
            MOV DI, OFFSET REGISTERS_CODES
            MOV CX, 16

            CHOOSE_SOURCE_SUBTRACTION_BP:
            CMP [DI], AX
            JE CHOSEN_SOURCE_SUBTRACTION_BP
            INC BH   
            INC DI 
            INC DI
            LOOP CHOOSE_SOURCE_SUBTRACTION_BP

        CHOSEN_SOURCE_SUBTRACTION_BP:
          AX_SRC_SUBTRACTION_BP:
            CMP BH, 0  ; AX
            JNE AH_SRC_SUBTRACTION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[14]
                MOV AX, PLAYER1_REGS[0]
                SUB BX, AX
                MOV PLAYER1_REGS[14], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------ 
          AH_SRC_SUBTRACTION_BP:
            CMP BH, 1  ; AH
            JNE AL_SRC_SUBTRACTION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------
          AL_SRC_SUBTRACTION_BP:
            CMP BH, 2  ; AL
            JNE BX_SRC_SUBTRACTION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BX_SRC_SUBTRACTION_BP:
            CMP BH, 3  ; BX
            JNE BH_SRC_SUBTRACTION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[14]
                MOV AX, PLAYER1_REGS[2]
                SUB BX, AX
                MOV PLAYER1_REGS[14], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          BH_SRC_SUBTRACTION_BP:
            CMP BH, 4  ; BH
            JNE BL_SRC_SUBTRACTION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
               JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          BL_SRC_SUBTRACTION_BP:
            CMP BH, 5  ; BL
            JNE CX_SRC_SUBTRACTION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CX_SRC_SUBTRACTION_BP:
            CMP BH, 6  ; CX
            JNE CH_SRC_SUBTRACTION_BP
           ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[14]
                MOV AX, PLAYER1_REGS[4]
                SUB BX, AX
                MOV PLAYER1_REGS[14], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------

          CH_SRC_SUBTRACTION_BP:
            CMP BH, 7  ; CH
            JNE CL_SRC_SUBTRACTION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          CL_SRC_SUBTRACTION_BP:
            CMP BH, 8  ; CL
            JNE DX_SRC_SUBTRACTION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DX_SRC_SUBTRACTION_BP:
            CMP BH, 9  ; DX
            JNE DH_SRC_SUBTRACTION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                 MOV BX, PLAYER1_REGS[14]
                MOV AX, PLAYER1_REGS[6]
                SUB BX, AX
                MOV PLAYER1_REGS[14], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DH_SRC_SUBTRACTION_BP:
            CMP BH, 10  ; DH
            JNE DL_SRC_SUBTRACTION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          DL_SRC_SUBTRACTION_BP:
            CMP BH, 11  ; DL
            JNE SI_SRC_SUBTRACTION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                JMP EXIT_SIZE_MISMATCH
            ;------------------------------------------------------------------

          SI_SRC_SUBTRACTION_BP:
            CMP BH, 12  ; SI
            JNE DI_SRC_SUBTRACTION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[14]
                MOV AX, PLAYER1_REGS[8]
                SUB BX, AX
                MOV PLAYER1_REGS[14], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          DI_SRC_SUBTRACTION_BP:
            CMP BH, 13  ; DI
            JNE SP_SRC_SUBTRACTION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[14]
                MOV AX, PLAYER1_REGS[10]
                SUB BX, AX
                MOV PLAYER1_REGS[14], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          SP_SRC_SUBTRACTION_BP:
            CMP BH, 14  ; SP
            JNE BP_SRC_SUBTRACTION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[14]
                MOV AX, PLAYER1_REGS[12]
                SUB BX, AX
                MOV PLAYER1_REGS[14], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          BP_SRC_SUBTRACTION_BP:
            CMP BH, 15  ; BP
            JNE VAL_SRC_SUBTRACTION_BP
            ;----------------------EXECUTE COMMAND REG TO REG----------------------- 
                MOV BX, PLAYER1_REGS[14]
                SUB BX, BX
                MOV PLAYER1_REGS[14], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------
          VAL_SRC_SUBTRACTION_BP:
            CMP BH, 16  ; VALUE
            ;----------------------EXECUTE COMMAND VALUE----------------------- 
                DEC SI
                DEC SI
                DEC SI  
                CALL CONVERT_TO_NUMBER
                MOV BX, PLAYER1_REGS[14]
                MOV AX, VALUE_WORD
                SUB BX, AX
                MOV PLAYER1_REGS[14], BX
                JMP EXIT_EXECUTE
            ;------------------------------------------------------------------ 

;----------------------------------------------DIV COMMAND-----------------------------------------------
DIVISION:
CMP BH, 3 ;DIV
JNE MULTIPLICATION

;------------------------------------------------MUL COMMAND----------------------------------------------
MULTIPLICATION:
CMP BH, 4 ;MUL
JNE XORING

;------------------------------------------------XOR COMMAND------------------------------------------------
XORING:
CMP BH, 5 ;XOR
JNE ANDING

;-------------------------------------------------AND COMMAND----------------------------------------------
ANDING:
CMP BH, 6 ;AND
JNE ORING

;---------------------------------------------------OR COMMAND----------------------------------------------
ORING:
CMP BH, 7 ;OR
JNE NOOPERATION

;-------------------------------------------------NOP COMMAND--------------------------------------------------
NOOPERATION: ;DOES NOTHING
CMP BH, 8    ;NOP
JNE SHIFTRIGHT
    CMP ACTUAL_SIZE, 3
    JNE EXIT_SYNTAX
    JMP EXIT_EXECUTE

;----------------------------------------------------SHR COMMAND-----------------------------------------------
SHIFTRIGHT:
CMP BH, 9 ;SHR
JNE SHIFTLEFT

;-----------------------------------------------------SHL COMMAND-----------------------------------------------
SHIFTLEFT:
CMP BH, 10 ;SHL
JNE CLEARC

;--------------------------------------------------CLC COMMAND------------------------------------------------
CLEARC:
CMP BH, 11 ;CLC
JNE ROTATERIGHT
;----------------------EXECUTE COMMAND----------------------- 
    CMP ACTUAL_SIZE, 3
    JNE EXIT_SYNTAX
    CLC
    JMP EXIT_EXECUTE
;------------------------------------------------------------

;--------------------------------------------------ROR COMMAND-------------------------------------------------
ROTATERIGHT:
CMP BH, 12 ;ROR
JNE ROTATELEFT

;---------------------------------------------------ROL COMMAND-------------------------------------------------
ROTATELEFT:
CMP BH, 13 ;ROL
JNE INCREMENT

;----------------------------------------------INCREMENT COMMAND-----------------------------------------------
INCREMENT:
CMP BH, 14 ;INC
JNE DECREMENT
    CMP ACTUAL_SIZE, 6
    JNE EXIT_SYNTAX
    
    CALL CHECK_FOR_SPACE
    CMP ERROR, 1
    JE EXIT_SYNTAX

    CALL GET_CURRENT_DESTINATION
            
        MOV CX,16
        MOV AX, CURRENT_DESTINATION
        MOV DI, OFFSET REGISTERS_CODES  
          
    CHOOSE_DESTINATION_INCREMENT:
        CMP [DI], AX
        JE CHOSEN_DESTINATION_INCREMENT
        INC BH   
        INC DI
        INC DI
        LOOP CHOOSE_DESTINATION_INCREMENT

CHOSEN_DESTINATION_INCREMENT:
        AX_DEST_INCREMENT:
        CMP BH, 0  ; AX
        JNE AH_DEST_INCREMENT     
;----------------------EXECUTE COMMAND-----------------------
            MOV BX, PLAYER1_REGS[0]
            INC BX
            MOV PLAYER1_REGS[0], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------------- 
        AH_DEST_INCREMENT:
        CMP BH, 1  ; AH
        JNE AL_DEST_INCREMENT  
;----------------------EXECUTE COMMAND-----------------------
            MOV BX, PLAYER1_REGS[0] 
            INC BH
            MOV PLAYER1_REGS[0], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------
        AL_DEST_INCREMENT:
        CMP BH, 2  ; AL
        JNE BX_DEST_INCREMENT
;----------------------EXECUTE COMMAND-----------------------
            MOV BX, PLAYER1_REGS[0] 
            INC BL
            MOV PLAYER1_REGS[0], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------
        BX_DEST_INCREMENT:
        CMP BH, 3  ; BX
        JNE BH_DEST_INCREMENT     
;----------------------EXECUTE COMMAND -----------------------
            MOV BX, PLAYER1_REGS[2] 
            INC BX
            MOV PLAYER1_REGS[2], BX
            JMP EXIT_EXECUTE 
;-------------------------------------------------------------                
        BH_DEST_INCREMENT:
        CMP BH, 4  ; BH
        JNE BL_DEST_INCREMENT
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[2] 
            INC BH
            MOV PLAYER1_REGS[2], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------
        BL_DEST_INCREMENT:
        CMP BH, 5  ; BL
        JNE CX_DEST_INCREMENT
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[2] 
            INC BL
            MOV PLAYER1_REGS[2], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------
        CX_DEST_INCREMENT:
        CMP BH, 6  ; CX
        JNE CH_DEST_INCREMENT        
;----------------------EXECUTE COMMAND-----------------------
            MOV BX, PLAYER1_REGS[4] 
            INC BX
            MOV PLAYER1_REGS[4], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------
        CH_DEST_INCREMENT:
        CMP BH, 7  ; CH
        JNE CL_DEST_INCREMENT
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[4] 
            INC BH
            MOV PLAYER1_REGS[4], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------
        CL_DEST_INCREMENT:    
        CMP BH, 8  ; CL
        JNE DX_DEST_INCREMENT
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[4] 
            INC BL
            MOV PLAYER1_REGS[4], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------
        DX_DEST_INCREMENT:
        CMP BH, 9  ; DX
        JNE DH_DEST_INCREMENT
;----------------------EXECUTE COMMAND-----------------------
            MOV BX, PLAYER1_REGS[6] 
            INC BX
            MOV PLAYER1_REGS[6], BX
            JMP EXIT_EXECUTE   
;------------------------------------------------------------
        DH_DEST_INCREMENT:
        CMP BH, 10 ; DH
        JNE DL_DEST_INCREMENT
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[6] 
            INC BH
            MOV PLAYER1_REGS[6], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------
        DL_DEST_INCREMENT:
        CMP BH, 11 ; DL
        JNE SI_DEST_INCREMENT
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[6] 
            INC BL
            MOV PLAYER1_REGS[6], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------
        SI_DEST_INCREMENT:
        CMP BH, 12 ; SI
        JNE DI_DEST_INCREMENT
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[8] 
            INC BX
            MOV PLAYER1_REGS[8], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------
        DI_DEST_INCREMENT:
        CMP BH, 13 ; DI
        JNE SP_DEST_INCREMENT
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[10] 
            INC BX
            MOV PLAYER1_REGS[10], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------
        SP_DEST_INCREMENT:
        CMP BH, 14 ; SP
        JNE BP_DEST_INCREMENT
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[12] 
            INC BX
            MOV PLAYER1_REGS[12], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------
        BP_DEST_INCREMENT:
        CMP BH, 15 ; BP
        JNE EXIT_SYNTAX
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[14] 
            INC BX
            MOV PLAYER1_REGS[14], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------


;----------------------------------------------DECREMENT COMMAND-----------------------------------------------
DECREMENT:
CMP BH, 15 ;DEC
JNE EXIT_SYNTAX
    CALL CHECK_FOR_SPACE
    CMP ERROR, 1
    JE EXIT_SYNTAX

    CALL GET_CURRENT_DESTINATION
         MOV AX, CURRENT_DESTINATION
         MOV DI, OFFSET REGISTERS_CODES

    CHOOSE_DESTINATION_DECREMENT:

        CMP [DI], AX
        JE CHOSEN_DESTINATION_DECREMENT
        INC BH   
        INC DI
        INC DI
        LOOP CHOOSE_DESTINATION_DECREMENT

CHOSEN_DESTINATION_DECREMENT:
        AX_DEST_DECREMENT:
        CMP BH, 0  ; AX
        JNE AH_DEST_DECREMENT      
;----------------------EXECUTE COMMAND VALUE----------------------- 
            MOV BX, PLAYER1_REGS[0]
            DEC BX
            MOV PLAYER1_REGS[0], BX
            JMP EXIT_EXECUTE  
;------------------------------------------------------------------- 
        AH_DEST_DECREMENT:
        CMP BH, 1  ; AH
        JNE AL_DEST_DECREMENT   
;----------------------EXECUTE COMMAND-----------------------
            MOV BX, PLAYER1_REGS[0]
            DEC BH
            MOV PLAYER1_REGS[0], BX
            JMP EXIT_EXECUTE  
;------------------------------------------------------------
        AL_DEST_DECREMENT:
        CMP BH, 2  ; AL
        JNE BX_DEST_DECREMENT
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[0]
            DEC BL
            MOV PLAYER1_REGS[0], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------
        BX_DEST_DECREMENT:
        CMP BH, 3  ; BX
        JNE BH_DEST_DECREMENT     
;----------------------EXECUTE COMMAND ----------------------- 
            MOV BX, PLAYER1_REGS[2]
            DEC BX
            MOV PLAYER1_REGS[2], BX
            JMP EXIT_EXECUTE 
;-------------------------------------------------------------            
        BH_DEST_DECREMENT:
        CMP BH, 4  ; BH
        JNE BL_DEST_DECREMENT
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[2]
            DEC BH
            MOV PLAYER1_REGS[2], BX
            JMP EXIT_EXECUTE  
;------------------------------------------------------------
        BL_DEST_DECREMENT:
        CMP BH, 5  ; BL
        JNE CX_DEST_DECREMENT
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[2]
            DEC BL
            MOV PLAYER1_REGS[2], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------
        CX_DEST_DECREMENT:
        CMP BH, 6  ; CX
        JNE CH_DEST_DECREMENT        
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[4]
            DEC BX
            MOV PLAYER1_REGS[4], BX
            JMP EXIT_EXECUTE  
;------------------------------------------------------------
        CH_DEST_DECREMENT:
        CMP BH, 7  ; CH
        JNE CL_DEST_DECREMENT
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[4]
            DEC BH
            MOV PLAYER1_REGS[4], BX
            JMP EXIT_EXECUTE   
;------------------------------------------------------------
        CL_DEST_DECREMENT:    
        CMP BH, 8  ; CL
        JNE DX_DEST_DECREMENT
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[4]
            DEC BL
            MOV PLAYER1_REGS[4], BX
            JMP EXIT_EXECUTE   
;------------------------------------------------------------
        DX_DEST_DECREMENT:
        CMP BH, 9  ; DX
        JNE DH_DEST_DECREMENT
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[6]
            DEC BX
            MOV PLAYER1_REGS[6], BX
            JMP EXIT_EXECUTE   
;------------------------------------------------------------
        DH_DEST_DECREMENT:
        CMP BH, 10 ; DH
        JNE DL_DEST_DECREMENT
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[6]
            DEC BH
            MOV PLAYER1_REGS[6], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------
        DL_DEST_DECREMENT:
        CMP BH, 11 ; DL
        JNE SI_DEST_DECREMENT
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[6]
            DEC BH
            MOV PLAYER1_REGS[6], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------
        SI_DEST_DECREMENT:
        CMP BH, 12 ; SI
        JNE DI_DEST_DECREMENT
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[8]
            DEC BX
            MOV PLAYER1_REGS[8], BX
            JMP EXIT_EXECUTE  
;------------------------------------------------------------
        DI_DEST_DECREMENT:
        CMP BH, 13 ; DI
        JNE SP_DEST_DECREMENT
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[10]
            DEC BX
            MOV PLAYER1_REGS[10], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------
        SP_DEST_DECREMENT:
        CMP BH, 14 ; SP
        JNE BP_DEST_DECREMENT
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[12]
            DEC BX
            MOV PLAYER1_REGS[12], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------
        BP_DEST_DECREMENT:
        CMP BH, 15 ; BP
        JNE EXIT_SYNTAX
;----------------------EXECUTE COMMAND----------------------- 
            MOV BX, PLAYER1_REGS[14]
            DEC BX
            MOV PLAYER1_REGS[14], BX
            JMP EXIT_EXECUTE 
;------------------------------------------------------------



EXIT_SYNTAX:
OUTPUTMESSAGE NEWLINE
OUTPUTMESSAGE ERROR_MESSAGE_SYNTAX
MOV AL, 0
MOV ERROR, AL
JMP EXIT_EXECUTE

EXIT_SIZE_MISMATCH:
OUTPUTMESSAGE NEWLINE
OUTPUTMESSAGE ERROR_MESSAGE_SIZE_MISMATCH

EXIT_EXECUTE:
RET
EXECUTE_COMMAND ENDP
           
MAIN PROC FAR
    MOV AX, @DATA
    MOV DS, AX

    MOV AH, 0AH
    MOV DX, OFFSET INPUTCOMMAND
    INT 21H
    
    CALL EXECUTE_COMMAND
    
    MOV AH, 4CH
    INT 21H
MAIN ENDP
END MAIN            