.8086
.model tiny

.code
org 100h

start:
CALL $-77775o
JMP $-77774o
CALL $-77773o
JMP $-77772o
CALL $-77771o
CALL $-401o
JMP $-400o
CALL $-200o
JMP $-177o
CALL $-176o
CALL $-175o
CALL $-3o
CALL $-2o
CALL $-1o
CALL $-0o
CALL $+1o
CALL $+2o
CALL $+3o
CALL $+4o
CALL $+5o
CALL $+22o
CALL $+41o
CALL $+43o
CALL $+55o
CALL $+134o
CALL $+177o
CALL $+200o
CALL $+201o
JMP $+202o
CALL $+202o
CALL $+203o
CALL $+77777o
JMP $+100000o
CALL $+100001o
JMP $+100002o
JO $-176o
JNO $-175o
JB $-3o
JAE $-2o
JE $-1o
JNE $-0o
JBE $+1o
JA $+2o
JS $+3o
JNS $+22o
JP $+41o
JNP $+47o
JL $+55o
JGE $+134o
JLE $+177o
JG $+200o
LOOPNE $+201o
LOOPE $-176o
LOOP $-175o
JCXZ $-3o
JMP short $-176o
JMP short $-2o
JMP short $-0o
JMP short $+3o
JMP short $+41o
JMP short $+55o
JMP short $+201o
CALL ES:[BX+SI+111o]
CALL DS:[111111o]
CALL SS:[111111o]
CALL [BX+SI]
CALL [BX+SI+111o]
CALL [BX+SI+177511o]
CALL [BX+SI+311o]
CALL [BX+SI+177311o]
CALL [BX+SI+177611o]
CALL [BX+SI+211o]
CALL [BX+SI+711o]
CALL [BX+SI+111111o]
CALL DS:[111111o]
CALL AX
JMP [BX+SI]
JMP AX
CALL dword ptr [BX+SI+111o]
CALL dword ptr [BX+SI+111111o]
JMP dword ptr [BX+SI+111o]
JMP dword ptr [BX+SI+111111o]
ROL byte ptr [BX+SI], 1
ROR DX, 1
RCL byte ptr [BP+111o], CL
RCR word ptr [BP+DI+111111o], CL
SHL AL, 1
SHR BP, 1
SAR word ptr DS:[111111o], CL
MOV word ptr [BX+SI], CS
MOV SS, DX
LEA SI, dword ptr [BP+DI+111111o]
LES SI, dword ptr [BP+DI+111111o]
LDS DI, dword ptr [BP+111o]
POP word ptr [BX+SI]
INC byte ptr DS:[111111o]
INC word ptr [BP+DI+111111o]
DEC AL
DEC DX
PUSH word ptr DS:[111111o]
PUSH word ptr [BP+DI+111111o]
PUSH BP
TEST byte ptr [SI+177622o], 333o
TEST byte ptr [BX+DI], 333o
MOV word ptr [SI+111111o], 162333o
MOV SP, 111111o
NOT byte ptr DS:[111111o]
NEG BP
MUL byte ptr DS:[111111o]
IMUL BP
DIV byte ptr DS:[111111o]
IDIV BP
TEST BL, byte ptr DS:[111111o]
TEST DX, word ptr [SI]
XCHG AL, byte ptr [BP+DI+111111o]
XCHG CX, SP
ADD byte ptr [BX+SI], DL
ADD word ptr [BX+SI], DX
ADD byte ptr [SI], DL
ADD word ptr [SI], DX
ADD AL, CL
OR AX, CX
ADD byte ptr DS:[111111o], BL
ADD word ptr DS:[111111o], BX
ADD CL, AL
ADD CX, AX
ADD DL, byte ptr [SI+111o]
ADD DX, word ptr [SI+111o]
ADD DL, byte ptr [SI+111111o]
ADD DX, word ptr [SI+111111o]
OR BL, byte ptr DS:[111111o]
ADD BX, word ptr DS:[111111o]
ADC byte ptr [SI+111o], DL
SBB word ptr [SI+111o], DX
SBB DL, byte ptr [BX+SI]
ADC DX, word ptr [BX+SI]
AND byte ptr [SI+111111o], DL
SUB word ptr [SI+111111o], DX
SUB DL, byte ptr [SI]
AND DX, word ptr [SI]
XOR byte ptr [BP+DI+111o], AL
CMP word ptr [BP+DI+111o], AX
CMP AL, byte ptr [BP+DI+111111o]
XOR AX, word ptr [BP+DI+111111o]
ADD byte ptr [SI+177622o], 333o
ADD word ptr [SI+111111o], 177733o
OR byte ptr [SI+177622o], 333o
ADC word ptr [SI+111111o], 177733o
ADD word ptr [SI+111111o], 162333o
ADD byte ptr [SI+111111o], 333o
ADD byte ptr DS:[111111o], 333o
SBB byte ptr [SI+177622o], 333o
AND word ptr [SI+111111o], 177733o
SUB byte ptr [SI+177622o], 333o
XOR byte ptr [SI+177622o], 333o
CMP word ptr [SI+111111o], 177733o
MOV byte ptr [BP+DI+111111o], AL
MOV word ptr [BP+DI+111111o], AX
MOV AL, byte ptr [BP+DI+111o]
MOV AX, word ptr [BP+DI+111o]
ADD AL, 111o
ADD AX, 111111o
POP ES
OR AX, CX
OR AL, 111o
OR AX, 111111o
PUSH CS
ADC AL, 111o
ADC AX, 111111o
POP SS
PUSH DS
DAA
SUB AL, 111o
SUB AX, 111111o
CMP AL, 111o
CMP AX, 111111o
AAS
INC SI
DEC BX
PUSH AX
POP CX
NOP
XCHG BP, AX
CBW
WAIT
LAHF
MOV AL, byte ptr DS:[111111o]
MOV AL, byte ptr ES:[111111o]
MOV AX, word ptr DS:[111111o]
MOV byte ptr SS:[111111o], AL
MOV word ptr DS:[111111o], AX
MOVSB
CMPSW
TEST AL, 111o
TEST AX, 111111o
STOSB
LODSW
SCASW
MOV AH, 11o
MOV DX, 736o
RET 111111o
RET
RETF 111111o
RETF
INT 41o
INTO
AAM
XLAT
IN AL, 111o
OUT 1o, AX
IN AX, DX
OUT DX, AL
LOCK
REP
HLT
STC
CLI
CLD
end start
