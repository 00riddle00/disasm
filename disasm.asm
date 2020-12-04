; ============================================================
;  DESCRIPTION
; ============================================================
; Programa: Disasembleris
;
; Uzduoties salyga: 
    ; Programa, kuri priima 'input' faila (.com arba .exe 
    ; pavidalu) ir disasembliuoja i nurodyta 'output' faila
    ; (.asm pavidalu). 

    ; output failas po programos ivykdymo atrodo taip:
        ; 0100:	B409		mov	ah, 09
        ; 0102:	BADE01		mov	dx, 01DE
        ; 0105:	CD21		int	21
        ; 01F9:   65        NEATPAZINTA
        ; ...
    ; t.y. vienoje eiluteje turi buti komandos Hex adresas, 
    ; komandos Hex dump'as ir atpazinta komanda (arba 
    ; NEATPAZINTA, jei tokios komandos nera).
;
; Atliko: Tomas Giedraitis
; ============================================================

; ============================================================
;  MACROS
; ============================================================

include macros.asm

; ============================================================
;  SETTINGS
; ============================================================
.model small     ; one code segment one data segment
.stack 100h
jumps

; ============================================================
;  CONSTANTS
; ============================================================


; ============================================================
;  DATA
; ============================================================

.data
    data_oct        db 2, 6, 4, 0, 1, 1, 0FFh, 0FFh
    ;data_oct       db 264o, 011o, 272o, 336o, 001o, 315o, 041o 
    ;data_hex       db 0B4h, 009h, 0BAh, 0DEh,  01h, 0CDh,  21h

    ; Rb = Byte-sized register
    Rb dw 'AL', 'CL', 'DL', 'BL', 'AH', 'CH', 'DL', 'BH'
    ; Rw = Word-sized register
    Rw dw 'AX', 'CX', 'DX', 'BX', 'SP', 'BP', 'SI', 'DI', '$'

    sep1               db '=============================================================================$'

; ============================================================
;  CODE
; ============================================================

.code

; ------------------------------------------------
; PROCEDURES
; ------------------------------------------------


; ------------------------------------------------/

start:
    mov ax, @data                  ; perkelti data i registra ax
    mov ds, ax                     ; perkelti ax (data) i data segmenta
    mov es, ax                     ; perkelti ax (data) i data segmenta

    ; isvesti programos aprasa
    m_println sep1
    m_putsln 'DISASM'
    m_println sep1

    xor ax, ax
    xor si, si
; ------------------------------------------------/
_xxx:
    mov al, byte ptr [data_oct+si]

    cmp al, 0FFh
    je exit_program

    cmp al, 3
    je _3xx
    ja undefined

    cmp al, 1
    jb _0xx
    je _1xx
    jmp _2xx

    inc si
    jmp _xxx
; ------------------------------------------------/
undefined:
    m_putsln 'UNDEFINED'
    inc si
    xor ax, ax
    jmp _xxx

_0xx:
    m_putsln '0xx'
    inc si
    jmp _xxx

_1xx:
    m_putsln '1xx'
    inc si
    jmp _xxx

_2xx:
    inc si
    mov al, byte ptr [data_oct+si]

    cmp al, 7
    je _27x_mov_reg_imm_word
    ja undefined

    cmp al, 4
    jb __2_0123_x
    je _24x
    jmp __2_567_x

    __2_01_x:
        cmp al, 1
        jb _20x
        jmp _21x

    __2_0123_x:
        cmp al, 2
        jb __2_01_x
        je _22x
        jmp _23x

    __2_567_x:
        cmp al, 6
        jb _25x
        je  _26x_mov_reg_imm_byte
        jmp _27x_mov_reg_imm_word

_3xx:
    m_putsln '3xx'
    inc si
    jmp _xxx
; ------------------------------------------------/
_20x:
    m_putsln '20x'
    jmp exit_program

_21x:
    m_putsln '21x'
    jmp exit_program

_22x:
    m_putsln '22x'
    jmp exit_program

_23x:
    inc si
    mov al, byte ptr [data_oct+si]

    cmp al, 4
    jb __23_0123
    je _234
    jmp __23_567

    __23_01:
        cmp al, 1
        jb _230
        jmp _231

    __23_0123:
        cmp al, 2
        jb __23_01
        je _232
        jmp _233

    __23_567:
        cmp al, 6
        jb _235
        je _236
        jmp _237

_24x:
    m_putsln '24x'
    jmp exit_program

_25x:
    m_putsln '25x'
    jmp exit_program

_26x_mov_reg_imm_byte:

    inc si
    mov al, byte ptr [data_oct+si]

    cmp al, 7
    ja undefined

    m_puts 'MOV '

    mov bl, al
    shl bl, 1; times 2

    mov dl, byte ptr [Rb+bx+1]
    mov ah, 02h
    int 21h

    mov dl, byte ptr [Rb+bx]
    ;mov dl, byte ptr [Rb+8]
    mov ah, 02h
    int 21h

    m_puts ', '

    inc si
    mov al, byte ptr [data_oct+si]

    inc si
    mov al, byte ptr [data_oct+si]

    inc si
    mov al, byte ptr [data_oct+si]

    m_putsln '09h'

    inc si

    jmp _xxx

_27x_mov_reg_imm_word:
    m_putsln '27x_mov_reg_imm_word'
    jmp exit_program
; ------------------------------------------------/
_230:
    m_putsln '230'
    jmp exit_program


_231:
    m_putsln '231'
    jmp exit_program


_232:
    m_putsln '232'
    jmp exit_program


_233:
    m_putsln '233'
    jmp exit_program


_234:
    m_putsln '234'
    jmp exit_program


_235:
    m_putsln '235'
    jmp exit_program


_236:
    m_putsln '236'
    jmp exit_program


_237:
    m_putsln '237'
    jmp exit_program
; ------------------------------------------------/

exit_program:
    m_exit0

end start
