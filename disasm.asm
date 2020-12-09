; ============================================================
;  DESCRIPTION
; ============================================================
; Subject: Computer Architecture
; LAB-2 and LAB-3: x8086 Disassembler
; Vilnius University, MIF
; Author: Tomas Giedraitis
;
; Assignment: 
;    A program which takes 'input' file (.com or .exe) 
;    and disassembles it, writing to the 'output' file (.asm)
;
;    output file after the program's execution should look
;    like this:
;         ...
;         0100: B409    | MOV AH, 09
;         0102: BADE01  | MOV DX, 01DE
;         0105: CD21    | INT 21
;         01F9: 65      | UNDEFINED
;         ...
;         ...
;    i.e. on every line there should be Hex address of the 
;    command, it's bytes written in Hex and then the
;    recognized command (undefined, if no such command exists)
;
; ============================================================

; ============================================================
;  MACROS
; ============================================================

include macros.asm

; print register name
; registers can be byte, word, and segment registers
; ::param:: reg_group - address where reg names are
; Also, BX must contain the offset from reg_group where
; the required register is.
m_print_reg macro reg_group
   push ax bx dx
   add bx, offset reg_group
   mov dl, byte ptr [bx+1]
   mov ah, 02h
   int 21h
   mov dl, byte ptr [bx]
   int 21h
   pop dx bx ax
endm

; ============================================================
;  SETTINGS
; ============================================================
.model small     ; one code segment one data segment
.stack 100h
jumps

; ============================================================
;  CONSTANTS
; ============================================================
; ...
; -----------------------------------------------------------/

; ============================================================
;  DATA
; ============================================================

.data

   data_octal  db 0, 0, 4,  1, 1, 1            ; 0???: ??      | ADD AL, 111
               db 0, 0, 5,  1, 1, 1,  2, 2, 2  ; 0???: ??      | ADD AX, 222111
               db 0, 0, 7                      ; 0???: 07      | POP ES
               db 0, 1, 1                      ; 0???: 09      | 011
               db 0, 1, 4,  1, 1, 1            ; 0???: ??      | OR AL, 111
               db 0, 1, 5,  1, 1, 1,  2, 2, 2  ; 0???: ??      | OR AX, 222111
               db 0, 1, 6                      ; 01FA: 0E      | PUSH CS
               db 0, 2, 4,  1, 1, 1            ; 0???: ??      | ADC AL, 111
               db 0, 2, 5                      ; 0???: 15      | 025
               db 0, 2, 5,  1, 1, 1,  2, 2, 2  ; 0???: ??      | ADC AX, 222111
               db 0, 2, 7                      ; 0???: 17      | POP SS 
               db 0, 3, 6                      ; 0???: 1E      | PUSH DS
               db 0, 4, 7                      ; 0???: ??      | DAA
               db 0, 5, 4,  1, 1, 1            ; 0???: ??      | SUB AL, 111
               db 0, 5, 5,  1, 1, 1,  2, 2, 2  ; 0???: ??      | SUB AX, 222111
               db 0, 7, 4,  1, 1, 1            ; 0???: ??      | XOR AL, 111
               db 0, 7, 5,  1, 1, 1,  2, 2, 2  ; 0???: ??      | XOR AX, 222111
               db 0, 7, 7                      ; 0???: ??      | AAS

               db 1, 0, 6                      ; 0???: ??      | INC SI
               db 1, 1, 3                      ; 0???: ??      | DEC BX
               db 1, 2, 0                      ; 0???: ??      | PUSH AX
               db 1, 3, 1                      ; 0???: ??      | POP CX
               db 1, 4, 5                      ; 01F9: 65      | UNDEFINED

               db 2, 2, 0                      ; 0???: ??      | NOP
               db 2, 2, 5                      ; 0???: ??      | XCHG BP, AX
               db 2, 3, 0                      ; 0???: ??      | CBW
               db 2, 3, 3                      ; 0???: ??      | WAIT
               db 2, 3, 7                      ; 0???: ??      | LAHF
               db 2, 4, 0,  1, 1, 1,  2, 2, 2  ; 0???: ??      | MOV AL, [222111]
               db 2, 4, 1,  1, 1, 1,  2, 2, 2  ; 0???: ??      | MOV AX, [222111]
               db 2, 4, 2,  1, 1, 1,  2, 2, 2  ; 0???: ??      | MOV [222111], AL
               db 2, 4, 3,  1, 1, 1,  2, 2, 2  ; 0???: ??      | MOV [222111], AX
               db 2, 4, 4                      ; 0???: ??      | MOVSB
               db 2, 4, 7                      ; 0???: ??      | CMPSW
               db 2, 5, 0,  1, 1, 1            ; 0???: ????    | TEST AL, 043
               db 2, 5, 1,  1, 1, 1,  2, 2, 2  ; 0???: ??????  | TEST AX, 336001
               db 2, 5, 2                      ; 0???: ??      | STOSB
               db 2, 5, 5                      ; 0???: ??      | LODSW
               db 2, 5, 7                      ; 0???: ??      | SCASW
               db 2, 6, 4,  0, 1, 1            ; 0100: B409    | MOV AH, 011
               db 2, 7, 2,  3, 3, 6,  0, 0, 1  ; 0102: BADE01  | MOV DX, 001336
               
               db 3, 0, 2,  1, 1, 1,  2, 2, 2  ; 0???: ??      | RET 222111
               db 3, 0, 3                      ; 0???: ??      | RET
               db 3, 1, 2,  1, 1, 1,  2, 2, 2  ; 0???: ??      | RETF 222111
               db 3, 1, 3                      ; 0???: ??      | RETF
               db 3, 1, 5,  0, 4, 1            ; 0105: CD21    | INT 041
               db 3, 1, 5,  0, 4, 1            ; 0???: ??      | INT 041
               db 3, 1, 6                      ; 0???: ??      | INTO
               db 3, 2, 4                      ; 0???: ??      | UNDEFINED
               db 3, 2, 4,  0, 1, 2            ; 0???: ??      | AAM
               db 3, 2, 6                      ; 0???: D6      | UNDEFINED
               db 3, 2, 7                      ; 0???: ??      | XLAT
               db 3, 4, 4,  1, 1, 1            ; 0???: ??      | IN AL, 111
               db 3, 4, 7,  0, 0, 1            ; 0???: ??      | OUT 001, AX
               db 3, 5, 5                      ; 0???: ??      | IN AX, DX
               db 3, 5, 6                      ; 0???: ??      | OUT DX, AL
               db 3, 6, 0                      ; 0???: ??      | LOCK
               db 3, 6, 1                      ; 0???: ??      | UNDEFINED
               db 3, 6, 3                      ; 0???: ??      | REP
               db 3, 6, 4                      ; 0???: ??      | HLT

               db 0FFh

    ; Byte-sized register
    Rb dw 'AL', 'CL', 'DL', 'BL', 'AH', 'CH', 'DL', 'BH'

    ; Word-sized register
    Rw dw 'AX', 'CX', 'DX', 'BX', 'SP', 'BP', 'SI', 'DI'

    ; Segment register:
    SR dw 'ES', 'CS', 'SS', 'DS'  

    sep db '=============================================================================$'

; ============================================================
;  CODE
; ============================================================

.code

; ------------------------------------------------------------
; PROCEDURES
; ------------------------------------------------------------

; increases SI by 3
proc p_print_next_byte
    push ax dx
    inc si
    mov dl, byte ptr [data_octal+si]
    add dl, 30h
    mov ah, 02h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si]
    add dl, 30h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si]
    add dl, 30h
    int 21h
    pop dx ax
    ret
endp

; increases SI by 6
proc p_print_next_word
    push ax dx
    inc si
    mov dl, byte ptr [data_octal+si+3]
    add dl, 30h
    mov ah, 02h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si+3]
    add dl, 30h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si+3]
    add dl, 30h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si-3]
    add dl, 30h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si-3]
    add dl, 30h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si-3]
    add dl, 30h
    int 21h

    pop dx ax
    ret
endp
; -----------------------------------------------------------/

start:
    mov ax, @data                  ; perkelti data i registra ax
    mov ds, ax                     ; perkelti ax (data) i data segmenta
    mov es, ax                     ; perkelti ax (data) i data segmenta

    ; isvesti programos aprasa
    m_println sep
    m_puts   '                                   '
    m_puts   'DISASM'
    m_putsln '                                   '
    m_println sep

    xor ax, ax
    xor si, si

    mov si, 0FFFFh

_xxx:
    ; get 1st octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 0FFh
    je short exit_program

    cmp al, 3
    je _3xx
    ja short undefined_1st_octal

    cmp al, 1
    jb short _0xx
    je _1xx
    jmp _2xx

    jmp short _xxx

undefined_1st_octal:
    inc si
    inc si
    jmp short undefined

undefined_2nd_octal:
    inc si
    jmp short undefined

undefined:
    m_putsln 'UNDEFINED'
    jmp short _xxx

exit_program:
    m_exit0

; ============================================================
;  _0XX
; ============================================================
_0xx:
    ; get 2nd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    ; get 3rd octal digit
    inc si ; SI now also points to 3rd octal
    mov bl, byte ptr [data_octal+si]

    cmp al, 7
    ja undefined ; undefined_2nd_octal is not used here, since 
                 ; SI already points to the last octal digit

    ; --------- check the 3rd octal digit -------------

    ; check if it's a (POP seg)/(adjust) operation
    cmp bl, 7
    je short __0x7
    ja undefined

    ; check if it's a (PUSH seg)/(seg change prefix) operation
    cmp bl, 6
    je short __0x6

    ; --------- check the 2nd octal digit -------------
    cmp al, 4
    jb short __0_0123_x
    je _04x
    jmp short __0_567_x
    
    __0_0123_x:
        cmp al, 2
        jb short __0_01_x
        je _02x
        jmp _03x

    __0_567_x:
        cmp al, 6
        jb _05x
        je _06x
        jmp _07x

    __0_01_x:
        cmp al, 1
        jb short _00x
        jmp _01x

    ; if it's a (PUSH seg)/(seg change prefix) operation
    __0x6:
        cmp al, 4
        jb _0x6_push_seg
        jmp _0x6_seg_change_prefix

    ; if it's a (POP seg)/(adjust) operation
    __0x7:
        cmp al, 4
        jb _0x7_pop_seg
        je _047_add_sub_adjust
        
        cmp al, 6
        jb _057_add_sub_adjust
        jb _067_add_sub_adjust
        jmp _077_add_sub_adjust

; ------------------------------------------------------------
;  _00X
; ------------------------------------------------------------
_00x:
    ; 3rd octal digit is already in BL, and it 
    ; cannot be 6 or 7 (since it was checked before)
    mov al, bl

    cmp al, 4
    jb short __00_0123
    je _004_add_acc_imm_byte
    jmp _005_add_acc_imm_word

    __00_0123:
        cmp al, 2
        jb short __00_01
        je _002_add_reg_rm_byte
        jmp _003_add_reg_rm_word

    __00_01:
        cmp al, 1
        je _001_add_rm_reg_word

; ------------------------------------------------------------
_000_add_rm_reg_byte:
    m_putsln '_000_add_rm_reg_byte'
    jmp _xxx

; ------------------------------------------------------------
_001_add_rm_reg_word:
    m_putsln '_001_add_rm_reg_word'
    jmp _xxx

; ------------------------------------------------------------
_002_add_reg_rm_byte:
    m_putsln '_002_add_reg_rm_byte'
    jmp _xxx

; ------------------------------------------------------------
_003_add_reg_rm_word:
    m_putsln '_003_add_reg_rm_word'
    jmp _xxx

; ************************************************************
_004_add_acc_imm_byte:
    m_puts 'ADD AL, '
    call p_print_next_byte
    m_print_nl
    jmp _xxx

; ************************************************************
_005_add_acc_imm_word:
    m_puts 'ADD AX, '
    call p_print_next_word
    m_print_nl
    jmp _xxx

; ------------------------------------------------------------
;  _01X
; ------------------------------------------------------------
_01x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 4
    jb short __01_0123
    je _014_or_acc_imm_byte
    jmp _015_or_acc_imm_word

    __01_0123:
        cmp al, 2
        jb short __01_01
        je _012_or_reg_rm_byte
        jmp _013_or_reg_rm_word

    __01_01:
        cmp al, 1
        je _011_or_rm_reg_word

; ------------------------------------------------------------
_010_or_rm_reg_byte:
    m_putsln '_010_or_rm_reg_byte'
    jmp _xxx

; ------------------------------------------------------------
_011_or_rm_reg_word:
    m_putsln '_011_or_rm_reg_word'
    jmp _xxx

; ------------------------------------------------------------
_012_or_reg_rm_byte:
    m_putsln '_012_or_reg_rm_byte'
    jmp _xxx

; ------------------------------------------------------------
_013_or_reg_rm_word:
    m_putsln '_013_or_reg_rm_word'
    jmp _xxx

; ************************************************************
_014_or_acc_imm_byte:
    m_puts 'OR AL, '
    call p_print_next_byte
    m_print_nl
    jmp _xxx

; ************************************************************
_015_or_acc_imm_word:
    m_puts 'OR AX, '
    call p_print_next_word
    m_print_nl
    jmp _xxx

; ------------------------------------------------------------
;  _02X
; ------------------------------------------------------------
_02x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 4
    jb short __02_0123
    je _024_adc_acc_imm_byte
    jmp _025_adc_acc_imm_word

    __02_0123:
        cmp al, 2
        jb short __02_01
        je _022_adc_reg_rm_byte
        jmp _023_adc_reg_rm_word

    __02_01:
        cmp al, 1
        je _021_adc_rm_reg_word

; ------------------------------------------------------------
_020_adc_rm_reg_byte:
    m_putsln '_020_adc_rm_reg_byte'
    jmp _xxx

; ------------------------------------------------------------
_021_adc_rm_reg_word:
    m_putsln '_021_adc_rm_reg_word'
    jmp _xxx

; ------------------------------------------------------------
_022_adc_reg_rm_byte:
    m_putsln '_022_adc_reg_rm_byte'
    jmp _xxx

; ------------------------------------------------------------
_023_adc_reg_rm_word:
    m_putsln '_023_adc_reg_rm_word'
    jmp _xxx

; ************************************************************
_024_adc_acc_imm_byte:
    m_puts 'ADC AL, '
    call p_print_next_byte
    m_print_nl
    jmp _xxx

; ************************************************************
_025_adc_acc_imm_word:
    m_puts 'ADC AX, '
    call p_print_next_word
    m_print_nl
    jmp _xxx

; ------------------------------------------------------------
;  _03X
; ------------------------------------------------------------
_03x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 4
    jb short __03_0123
    je _034_sbb_acc_imm_byte
    jmp _035_sbb_acc_imm_word

    __03_0123:
        cmp al, 2
        jb short __03_01
        je _032_sbb_reg_rm_byte
        jmp _033_sbb_reg_rm_word

    __03_01:
        cmp al, 1
        je _031_sbb_rm_reg_word

; ------------------------------------------------------------
_030_sbb_rm_reg_byte:
    m_putsln '_030_sbb_rm_reg_byte'
    jmp _xxx

; ------------------------------------------------------------
_031_sbb_rm_reg_word:
    m_putsln '_031_sbb_rm_reg_word'
    jmp _xxx

; ------------------------------------------------------------
_032_sbb_reg_rm_byte:
    m_putsln '_032_sbb_reg_rm_byte'
    jmp _xxx

; ------------------------------------------------------------
_033_sbb_reg_rm_word:
    m_putsln '_033_sbb_reg_rm_word'
    jmp _xxx

; ************************************************************
_034_sbb_acc_imm_byte:
    m_puts 'SBB AL, '
    call p_print_next_byte
    m_print_nl
    jmp _xxx

; ************************************************************
_035_sbb_acc_imm_word:
    m_puts 'SBB AX, '
    call p_print_next_word
    m_print_nl
    jmp _xxx

; ------------------------------------------------------------
;  _04X
; ------------------------------------------------------------
_04x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 4
    jb short __04_0123
    je _044_and_acc_imm_byte
    jmp _045_and_acc_imm_word

    __04_0123:
        cmp al, 2
        jb short __04_01
        je _042_and_reg_rm_byte
        jmp _043_and_reg_rm_word

    __04_01:
        cmp al, 1
        je _041_and_rm_reg_word

; ------------------------------------------------------------
_040_and_rm_reg_byte:
    m_putsln '_040_and_rm_reg_byte'
    jmp _xxx

; ------------------------------------------------------------
_041_and_rm_reg_word:
    m_putsln '_041_and_rm_reg_word'
    jmp _xxx

; ------------------------------------------------------------
_042_and_reg_rm_byte:
    m_putsln '_042_and_reg_rm_byte'
    jmp _xxx

; ------------------------------------------------------------
_043_and_reg_rm_word:
    m_putsln '_043_and_reg_rm_word'
    jmp _xxx

; ************************************************************
_044_and_acc_imm_byte:
    m_puts 'AND AL, '
    call p_print_next_byte
    m_print_nl
    jmp _xxx

; ************************************************************
_045_and_acc_imm_word:
    m_puts 'AND AX, '
    call p_print_next_word
    m_print_nl
    jmp _xxx

; ------------------------------------------------------------
;  _05X
; ------------------------------------------------------------
_05x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 4
    jb short __05_0123
    je _054_sub_acc_imm_byte
    jmp _055_sub_acc_imm_word

    __05_0123:
        cmp al, 2
        jb short __05_01
        je _052_sub_reg_rm_byte
        jmp _053_sub_reg_rm_word

    __05_01:
        cmp al, 1
        je _051_sub_rm_reg_word

; ------------------------------------------------------------
_050_sub_rm_reg_byte:
    m_putsln '_050_sub_rm_reg_byte'
    jmp _xxx

; ------------------------------------------------------------
_051_sub_rm_reg_word:
    m_putsln '_051_sub_rm_reg_word'
    jmp _xxx

; ------------------------------------------------------------
_052_sub_reg_rm_byte:
    m_putsln '_052_sub_reg_rm_byte'
    jmp _xxx

; ------------------------------------------------------------
_053_sub_reg_rm_word:
    m_putsln '_053_sub_reg_rm_word'
    jmp _xxx

; ************************************************************
_054_sub_acc_imm_byte:
    m_puts 'SUB AL, '
    call p_print_next_byte
    m_print_nl
    jmp _xxx

; ************************************************************
_055_sub_acc_imm_word:
    m_puts 'SUB AX, '
    call p_print_next_word
    m_print_nl
    jmp _xxx

; ------------------------------------------------------------
;  _06X
; ------------------------------------------------------------
_06x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 4
    jb short __06_0123
    je _064_xor_acc_imm_byte
    jmp _065_xor_acc_imm_word

    __06_0123:
        cmp al, 2
        jb short __06_01
        je _062_xor_reg_rm_byte
        jmp _063_xor_reg_rm_word

    __06_01:
        cmp al, 1
        je _061_xor_rm_reg_word

; ------------------------------------------------------------
_060_xor_rm_reg_byte:
    m_putsln '_060_xor_rm_reg_byte'
    jmp _xxx

; ------------------------------------------------------------
_061_xor_rm_reg_word:
    m_putsln '_061_xor_rm_reg_word'
    jmp _xxx

; ------------------------------------------------------------
_062_xor_reg_rm_byte:
    m_putsln '_062_xor_reg_rm_byte'
    jmp _xxx

; ------------------------------------------------------------
_063_xor_reg_rm_word:
    m_putsln '_063_xor_reg_rm_word'
    jmp _xxx

; ************************************************************
_064_xor_acc_imm_byte:
    m_puts 'XOR AL, '
    call p_print_next_byte
    m_print_nl
    jmp _xxx

; ************************************************************
_065_xor_acc_imm_word:
    m_puts 'XOR AX, '
    call p_print_next_word
    m_print_nl
    jmp _xxx

; ------------------------------------------------------------
;  _07X
; ------------------------------------------------------------
_07x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 4
    jb short __07_0123
    je _074_cmp_acc_imm_byte
    jmp _075_cmp_acc_imm_word

    __07_0123:
        cmp al, 2
        jb short __07_01
        je _072_cmp_reg_rm_byte
        jmp _073_cmp_reg_rm_word

    __07_01:
        cmp al, 1
        je _071_cmp_rm_reg_word

; ------------------------------------------------------------
_070_cmp_rm_reg_byte:
    m_putsln '_070_cmp_rm_reg_byte'
    jmp _xxx

; ------------------------------------------------------------
_071_cmp_rm_reg_word:
    m_putsln '_071_cmp_rm_reg_word'
    jmp _xxx

; ------------------------------------------------------------
_072_cmp_reg_rm_byte:
    m_putsln '_072_cmp_reg_rm_byte'
    jmp _xxx

; ------------------------------------------------------------
_073_cmp_reg_rm_word:
    m_putsln '_073_cmp_reg_rm_word'
    jmp _xxx

; ************************************************************
_074_cmp_acc_imm_byte:
    m_puts 'CMP AL, '
    call p_print_next_byte
    m_print_nl
    jmp _xxx

; ************************************************************
_075_cmp_acc_imm_word:
    m_puts 'CMP AX, '
    call p_print_next_word
    m_print_nl
    jmp _xxx

; ************************************************************
;  _0X6
; ************************************************************
_0x6_push_seg:
    ; 2nd octal digit is already in AL
    ; it is one of {0,1,2,3}

    m_puts 'PUSH '

    mov bl, al 
    shl bl, 1 ; times 2
    m_print_reg SR

    m_print_nl
    jmp _xxx

_0x6_seg_change_prefix:
    ; 2nd octal digit is already in AL
    ; AL is one of {4,5,6,7}

    m_putsln '0x6_seg_change_prefix'
    jmp _xxx

; ************************************************************
;  _0X7
; ************************************************************
_0x7_pop_seg:
    ; 2nd octal digit is already in AL
    ; it is one of {0,1,2,3}

    m_puts 'POP '

    mov bl, al 
    shl bl, 1 ; times 2
    m_print_reg SR

    m_print_nl
    jmp _xxx

; ************************************************************
_047_add_sub_adjust:
    m_putsln 'DAA'
    jmp _xxx

; ************************************************************
_057_add_sub_adjust:
    m_putsln 'DAS'
    jmp _xxx

; ************************************************************
_067_add_sub_adjust:
    m_putsln 'AAA'
    jmp _xxx

; ************************************************************
_077_add_sub_adjust:
    m_putsln 'AAS'
    jmp _xxx

; ============================================================
;  _1XX
; ============================================================
_1xx:
    ; get 2nd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    je _17x
    ja undefined_2nd_octal

    cmp al, 3
    jb short __1_012_x
    je _13_pop_reg_word
    jmp short __1_456_x

    __1_012_x:
        cmp al, 1
        jb short _10x_inc_reg_word
        je _11_dec_reg_word
        jmp _12_push_reg_word

    __1_456_x:
        cmp al, 6
        je _16x
        jmp undefined_2nd_octal ; _14x, _15x

; ************************************************************
;  _10X
; ************************************************************
_10x_inc_reg_word:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    ja undefined

    m_puts 'INC '

    mov bl, al
    shl bl, 1 ; times 2
    m_print_reg Rw

    m_print_nl
    jmp _xxx

; ************************************************************
;  _11X
; ************************************************************
_11_dec_reg_word:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    ja undefined

    m_puts 'DEC '

    mov bl, al
    shl bl, 1; times 2. bl = 4
    m_print_reg Rw

    m_print_nl
    jmp _xxx

; ************************************************************
;  _12X
; ************************************************************
_12_push_reg_word:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    ja undefined

    m_puts 'PUSH '

    mov bl, al
    shl bl, 1; times 2. bl = 4
    m_print_reg Rw

    m_print_nl
    jmp _xxx

; ************************************************************
;  _13X
; ************************************************************
_13_pop_reg_word:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    ja undefined

    m_puts 'POP '

    mov bl, al
    shl bl, 1; times 2. bl = 4
    m_print_reg Rw

    m_print_nl
    jmp _xxx

; ------------------------------------------------------------
;  _16X
; ------------------------------------------------------------
_16x:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    je _167
    ja undefined

    cmp al, 3
    jb short __16_012
    je _163
    jmp short __16_456

    __16_012:
        cmp al, 1
        jb short _160
        je _161
        jmp _162

    __16_456:
        cmp al, 5
        jb _164
        je _165
        jmp _166

; ------------------------------------------------------------
_160:
    m_putsln '160'
    jmp _xxx

; ------------------------------------------------------------
_161:
    m_putsln '161'
    jmp _xxx

; ------------------------------------------------------------
_162:
    m_putsln '162'
    jmp _xxx

; ------------------------------------------------------------
_163:
    m_putsln '163'
    jmp _xxx

; ------------------------------------------------------------
_164:
    m_putsln '164'
    jmp _xxx

; ------------------------------------------------------------
_165:
    m_putsln '165'
    jmp _xxx

; ------------------------------------------------------------
_166:
    m_putsln '166'
    jmp _xxx

; ------------------------------------------------------------
_167:
    m_putsln '167'
    jmp _xxx

; ------------------------------------------------------------
;  _17X
; ------------------------------------------------------------
_17x:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    je _177
    ja undefined

    cmp al, 3
    jb short __17_012
    je _173
    jmp short __17_456

    __17_012:
        cmp al, 1
        jb short _170
        je _171
        jmp _172

    __17_456:
        cmp al, 5
        jb _174
        je _175
        jmp _176

; ------------------------------------------------------------
_170:
    m_putsln '170'
    jmp _xxx

; ------------------------------------------------------------
_171:
    m_putsln '171'
    jmp _xxx

; ------------------------------------------------------------
_172:
    m_putsln '172'
    jmp _xxx

; ------------------------------------------------------------
_173:
    m_putsln '173'
    jmp _xxx

; ------------------------------------------------------------
_174:
    m_putsln '174'
    jmp _xxx

; ------------------------------------------------------------
_175:
    m_putsln '175'
    jmp _xxx

; ------------------------------------------------------------
_176:
    m_putsln '176'
    jmp _xxx

; ------------------------------------------------------------
_177:
    m_putsln '177'
    jmp _xxx

; ============================================================
;  _2XX
; ============================================================
_2xx:
    ; get 2nd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    je _27x_mov_reg_imm_word
    ja undefined_2nd_octal

    cmp al, 3
    jb short __2_012_x
    je _23x
    jmp short __2_456_x

    __2_012_x:
        cmp al, 1
        jb short _20x
        je _21x
        jmp _22x_xchg_reg_ax

    __2_456_x:
        cmp al, 5
        jb _24x
        je _25x
        jmp _26x_mov_reg_imm_byte

; ------------------------------------------------------------
;  _20X
; ------------------------------------------------------------
_20x:
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    je _207
    ja undefined

    cmp al, 3
    jb short __20_012
    je _203
    jmp short __20_456

    __20_012:
        cmp al, 1
        jb short _200
        je _201
        jmp _202

    __20_456:
        cmp al, 5
        jb _204
        je _205
        jmp _206

; ------------------------------------------------------------
_200:
    m_putsln '200'
    jmp _xxx

; ------------------------------------------------------------
_201:
    m_putsln '201'
    jmp _xxx

; ------------------------------------------------------------
_202:
    m_putsln '202'
    jmp _xxx

; ------------------------------------------------------------
_203:
    m_putsln '203'
    jmp _xxx

; ------------------------------------------------------------
_204:
    m_putsln '204'
    jmp _xxx

; ------------------------------------------------------------
_205:
    m_putsln '205'
    jmp _xxx

; ------------------------------------------------------------
_206:
    m_putsln '206'
    jmp _xxx

; ------------------------------------------------------------
_207:
    m_putsln '207'
    jmp _xxx

; ------------------------------------------------------------
;  _21X
; ------------------------------------------------------------
_21x:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    je _217
    ja undefined

    cmp al, 3
    jb short __21_012
    je _213
    jmp short __21_456

    __21_012:
        cmp al, 1
        jb short _210
        je _211
        jmp _212

    __21_456:
        cmp al, 5
        jb _214
        je _215
        jmp _216

; ------------------------------------------------------------
_210:
    m_putsln '210'
    jmp _xxx

; ------------------------------------------------------------
_211:
    m_putsln '211'
    jmp _xxx

; ------------------------------------------------------------
_212:
    m_putsln '212'
    jmp _xxx

; ------------------------------------------------------------
_213:
    m_putsln '213'
    jmp _xxx

; ------------------------------------------------------------
_214:
    m_putsln '214'
    jmp _xxx

; ------------------------------------------------------------
_215:
    m_putsln '215'
    jmp _xxx

; ------------------------------------------------------------
_216:
    m_putsln '216'
    jmp _xxx

; ------------------------------------------------------------
_217:
    m_putsln '217'
    jmp _xxx

; ************************************************************
;  _22X
; ************************************************************
_22x_xchg_reg_ax:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    ja undefined

    cmp al, 0
    je short _22x_nop

    m_puts 'XCHG '

    mov bl, al
    shl bl, 1; times 2. bl = 4
    m_print_reg Rw

    m_putsln ', AX'
    jmp _xxx

    _22x_nop:
        m_putsln 'NOP'
        jmp _xxx

; ------------------------------------------------------------
;  _23X
; ------------------------------------------------------------
_23x:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    ja undefined

    cmp al, 4
    jb short __23_0123
    je _234_pushf

    cmp al, 6
    jb _235_popf
    je _236_sahf
    jmp _237_lahf

    __23_0123:
        cmp al, 2
        jb short __23_01
        je _232_call_label_far_absolute
        jmp _233_wait

    __23_01:
        cmp al, 1
        jb short _230_cbw
        jmp _231_cwd

; ************************************************************
_230_cbw:
    m_putsln 'CBW'
    jmp _xxx

; ************************************************************
_231_cwd:
    m_putsln 'CWD'
    jmp _xxx

; ------------------------------------------------------------
_232_call_label_far_absolute:
    m_putsln '_232_call_label_far_absolute'
    jmp _xxx

; ************************************************************
_233_wait:
    m_putsln 'WAIT'
    jmp _xxx

; ************************************************************
_234_pushf:
    m_putsln 'PUSHF'
    jmp _xxx

; ************************************************************
_235_popf:
    m_putsln 'POPF'
    jmp _xxx

; ************************************************************
_236_sahf:
    m_putsln 'SAHF'
    jmp _xxx

; ************************************************************
_237_lahf:
    m_putsln 'LAHF'
    jmp _xxx

; ------------------------------------------------------------
;  _24X
; ------------------------------------------------------------
_24x:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    ja undefined

    cmp al, 4
    jb short __24_0123
    je _244_movsb

    cmp al, 6
    jb _245_movsw
    je _246_cmpsb
    jmp _247_cmpsw

    __24_0123:
        cmp al, 2
        jb short __24_01
        je _242_mov_mem_acc_byte
        jmp _243_mov_mem_acc_word

    __24_01:
        cmp al, 1
        jb short _240_mov_acc_mem_byte
        jmp _241_mov_acc_mem_word

; ************************************************************
_240_mov_acc_mem_byte:
    m_puts 'MOV AL, ['
    call p_print_next_word
    m_putsln ']'
    jmp _xxx

; ************************************************************
_241_mov_acc_mem_word:
    m_puts 'MOV AX, ['
    call p_print_next_word
    m_putsln ']'
    jmp _xxx

; ************************************************************
_242_mov_mem_acc_byte:
    m_puts 'MOV ['
    call p_print_next_word
    m_putsln '], AL'
    jmp _xxx

; ************************************************************
_243_mov_mem_acc_word:
    m_puts 'MOV ['
    call p_print_next_word
    m_putsln '], AX'
    jmp _xxx

; ************************************************************
_244_movsb:
    m_putsln 'MOVSB'
    jmp _xxx

; ************************************************************
_245_movsw:
    m_putsln 'MOVSW'
    jmp _xxx

; ************************************************************
_246_cmpsb:
    m_putsln 'CMPSB'
    jmp _xxx

; ************************************************************
_247_cmpsw:
    m_putsln 'CMPSW'
    jmp _xxx

; ------------------------------------------------------------
;  _25X
; ------------------------------------------------------------
_25x:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    ja undefined

    cmp al, 4
    jb short __25_0123
    je _254_lodsb

    cmp al, 6
    jb _255_lodsw
    je _256_scasb
    jmp _257_scasw

    __25_0123:
        cmp al, 2
        jb short __25_01
        je _252_stosb
        jmp _253_stosw

    __25_01:
        cmp al, 1
        jb _250_test_acc_imm_byte
        jmp _251_test_acc_imm_word

; ************************************************************
_250_test_acc_imm_byte:
    m_puts 'TEST AL, '
    call p_print_next_byte

    m_print_nl
    jmp _xxx

; ************************************************************
_251_test_acc_imm_word:
    m_puts 'TEST AX, '
    call p_print_next_word

    m_print_nl
    jmp _xxx

; ------------------------------------------------------------
_252_stosb:
    m_putsln 'STOSB'
    jmp _xxx

; ------------------------------------------------------------
_253_stosw:
    m_putsln 'STOSW'
    jmp _xxx

; ------------------------------------------------------------
_254_lodsb:
    m_putsln 'LODSB'
    jmp _xxx

; ------------------------------------------------------------
_255_lodsw:
    m_putsln 'LODSW'
    jmp _xxx

; ------------------------------------------------------------
_256_scasb:
    m_putsln 'SCASB'
    jmp _xxx

; ------------------------------------------------------------
_257_scasw:
    m_putsln 'SCASW'
    jmp _xxx

; ************************************************************
;  _26X
; ************************************************************
_26x_mov_reg_imm_byte:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    ja undefined

    m_puts 'MOV '

    mov bl, al
    shl bl, 1 ; times 2
    m_print_reg Rb
    m_puts ', '
    call p_print_next_byte

    m_print_nl
    jmp _xxx

; ************************************************************
;  _27X
; ************************************************************
_27x_mov_reg_imm_word:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    ja undefined

    m_puts 'MOV '

    mov bl, al
    shl bl, 1; times 2. bl = 4
    m_print_reg Rw
    m_puts ', '
    call p_print_next_word

    m_print_nl
    jmp _xxx

; ============================================================
;  _3XX
; ============================================================
_3xx:
    ; get 2nd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    je _37x
    ja undefined_2nd_octal

    cmp al, 3
    jb short __3_012_x
    je _33x
    jmp short __3_456_x

    __3_012_x:
        cmp al, 1
        jb short _30x
        je _31x
        jmp _32x

    __3_456_x:
        cmp al, 5
        jb _34x
        je _35x
        jmp _36x

; ------------------------------------------------------------
;  _30X
; ------------------------------------------------------------
_30x:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    ja undefined

    cmp al, 4
    jb short __30_23
    je _304_les_reg_mem

    cmp al, 6
    jb _305_lds_reg_mem
    je _306_mov_rm_imm_byte
    jmp _307_mov_rm_imm_word

    __30_23:
        cmp al, 3
        jb short _302_ret_imm
        jmp _303_ret

; ************************************************************
_302_ret_imm:
    m_puts 'RET '
    call p_print_next_word
    m_print_nl
    jmp _xxx

; ************************************************************
_303_ret:
    m_putsln 'RET'
    jmp _xxx

; ------------------------------------------------------------
_304_les_reg_mem:
    m_putsln '304_les_reg_mem'
    jmp _xxx

; ------------------------------------------------------------
_305_lds_reg_mem:
    m_putsln '_305_lds_reg_mem'
    jmp _xxx

; ------------------------------------------------------------
_306_mov_rm_imm_byte:
    m_putsln '_306_mov_rm_imm_byte'
    jmp _xxx

; ------------------------------------------------------------
_307_mov_rm_imm_word:
    m_putsln '_307_mov_rm_imm_word'
    jmp _xxx

; ------------------------------------------------------------
;  _31X
; ------------------------------------------------------------
_31x:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    ja undefined

    cmp al, 5
    jb short __31_234
    je _315_int_number

    cmp al, 6
    je _316_into
    jmp _317_iret

    __31_234:
        cmp al, 3
        jb _312_retf_imm
        je _313_retf
        jmp _314_int3

; ************************************************************
_312_retf_imm:
    m_puts 'RETF '
    call p_print_next_word
    m_print_nl
    jmp _xxx

; ************************************************************
_313_retf:
    m_putsln 'RETF'
    jmp _xxx

; ************************************************************
_314_int3:
    m_putsln 'INT 3'
    jmp _xxx

; ************************************************************
_315_int_number:
    m_puts 'INT '
    call p_print_next_byte
    m_print_nl
    jmp _xxx

; ************************************************************
_316_into:
    m_putsln 'INTO'
    jmp _xxx

; ************************************************************
_317_iret:
    m_putsln 'IRET'
    jmp _xxx

; ------------------------------------------------------------
;  _32X
; ------------------------------------------------------------
_32x:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    ja undefined

    cmp al, 4
    jb short __32_0123

    cmp al, 6
    jb short __32_45
    je undefined ; _326
    jmp _327_xlat

    __32_0123:
        ; TODO
        ; ...

    __32_45:
        ; check if next byte is part of AAM/AAD opcode
        cmp byte ptr [data_octal+si+1], 0
        jne undefined

        mov byte ptr [data_octal+si+2], 1
        jne undefined

        mov byte ptr [data_octal+si+3], 2
        jne undefined

        ; if AAM/AAD is recognized, move index to the 
        ; end of next byte, which is part of the opcode
        inc si
        inc si
        inc si

        cmp al, 4
        je _324_aam
        jmp _325_aad

; ************************************************************
_324_aam:
    m_putsln 'AAM'
    jmp _xxx

; ************************************************************
_325_aad:
    m_putsln 'AAD'
    jmp _xxx

; ************************************************************
_327_xlat:
    m_putsln 'XLAT'
    jmp _xxx

; ------------------------------------------------------------
;  _33X
; ------------------------------------------------------------
_33x:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    je _337
    ja undefined

    cmp al, 3
    jb short __33_012
    je _333
    jmp short __33_456

    __33_012:
        cmp al, 1
        jb short _330
        je _331
        jmp _332

    __33_456:
        cmp al, 5
        jb _334
        je _335
        jmp _336

; ------------------------------------------------------------
_330:
    m_putsln '330'
    jmp _xxx

; ------------------------------------------------------------
_331:
    m_putsln '331'
    jmp _xxx

; ------------------------------------------------------------
_332:
    m_putsln '332'
    jmp _xxx

; ------------------------------------------------------------
_333:
    m_putsln '333'
    jmp _xxx

; ------------------------------------------------------------
_334:
    m_putsln '334'
    jmp _xxx

; ------------------------------------------------------------
_335:
    m_putsln '335'
    jmp _xxx

; ------------------------------------------------------------
_336:
    m_putsln '336'
    jmp _xxx

; ------------------------------------------------------------
_337:
    m_putsln '337'
    jmp _xxx

; ------------------------------------------------------------
;  _34X
; ------------------------------------------------------------
_34x:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    ja undefined

    cmp al, 4
    jb short __34_0123
    je _344_in_acc_port_direct_byte

    cmp al, 6
    jb _345_in_acc_port_direct_word
    je _346_out_acc_port_direct_byte
    jmp _347_out_acc_port_direct_word

    __34_0123:
        cmp al, 2
        jb short __34_01
        je _342_loop_label
        jmp _343_jcxz_label

    __34_01:
        cmp al, 1
        jb short _340_loopne_label
        jmp _341_loope_label

; ------------------------------------------------------------
_340_loopne_label:
    m_putsln '_340_loopne_label'
    jmp _xxx

; ------------------------------------------------------------
_341_loope_label:
    m_putsln '_341_loope_label'
    jmp _xxx

; ------------------------------------------------------------
_342_loop_label:
    m_putsln '_342_loop_label'
    jmp _xxx

; ------------------------------------------------------------
_343_jcxz_label:
    m_putsln '343_343_jcxz_label'
    jmp _xxx

; ************************************************************
_344_in_acc_port_direct_byte:
    m_puts 'IN AL, '
    call p_print_next_byte
    m_print_nl
    jmp _xxx

; ************************************************************
_345_in_acc_port_direct_word:
    m_puts 'IN AX, '
    call p_print_next_byte
    m_print_nl
    jmp _xxx

; ************************************************************
_346_out_acc_port_direct_byte:
    m_puts 'OUT ' 
    call p_print_next_byte
    m_putsln ', AL'
    jmp _xxx

; ************************************************************
_347_out_acc_port_direct_word:
    m_puts 'OUT ' 
    call p_print_next_byte
    m_putsln ', AX'
    jmp _xxx

; ------------------------------------------------------------
;  _35X
; ------------------------------------------------------------
_35x:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    ja undefined

    cmp al, 4
    jb short __35_0123
    je _354_in_acc_port_indirect_byte

    cmp al, 6
    jb _355_in_acc_port_indirect_word
    je _356_out_acc_port_indirect_byte
    jmp _357_out_acc_port_indirect_word

    __35_0123:
        cmp al, 2
        jb short __35_01
        je _352_jmp_label_far_absolute
        jmp _353_jmp_label_short_relative

    __35_01:
        cmp al, 1
        jb short _350_call_label_near_relative
        jmp _351_jmp_label_near_relative

; ------------------------------------------------------------
_350_call_label_near_relative:
    m_putsln '_350_call_label_near_relative'
    jmp _xxx

; ------------------------------------------------------------
_351_jmp_label_near_relative:
    m_putsln '_351_jmp_label_near_relative'
    jmp _xxx

; ------------------------------------------------------------
_352_jmp_label_far_absolute:
    m_putsln '_352_jmp_label_far_absolute'
    jmp _xxx

; ------------------------------------------------------------
_353_jmp_label_short_relative:
    m_putsln '_353_jmp_label_short_relative'
    jmp _xxx

; ************************************************************
_354_in_acc_port_indirect_byte:
    m_putsln 'IN AL, DX'
    jmp _xxx

; ************************************************************
_355_in_acc_port_indirect_word:
    m_putsln 'IN AX, DX'
    jmp _xxx

; ************************************************************
_356_out_acc_port_indirect_byte:
    m_putsln 'OUT DX, AL'
    jmp _xxx

; ************************************************************
_357_out_acc_port_indirect_word:
    m_putsln 'OUT DX, AX'
    jmp _xxx

; ------------------------------------------------------------
;  _36X
; ------------------------------------------------------------
_36x:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    ja undefined

    cmp al, 6
    jb short __36_012345

    ; TODO __36_67
    ; ...

    __36_012345:
        cmp al, 3
        jb short __36_012
        je _36_3_rep

    cmp al, 5
    jb _36_4_hlt
    jmp _36_5_cmc

    __36_012:
        cmp al, 1
        jb _360_lock
        je undefined ; _361
        jmp _362_repne

; ************************************************************
_360_lock:
    m_putsln 'LOCK'
    jmp _xxx

; ************************************************************
_362_repne:
    m_putsln 'REPNE'
    jmp _xxx

; ************************************************************
_36_3_rep:
    m_putsln 'REP'
    jmp _xxx

; ************************************************************
_36_4_hlt:
    m_putsln 'HLT'
    jmp _xxx

; ************************************************************
_36_5_cmc:
    m_putsln 'CMC'
    jmp _xxx

; ------------------------------------------------------------
;  _37X
; ------------------------------------------------------------
_37x:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    je _377
    ja undefined

    cmp al, 3
    jb short __37_012
    je _373
    jmp short __37_456

    __37_012:
        cmp al, 1
        jb short _370
        je _371
        jmp _372

    __37_456:
        cmp al, 5
        jb _374
        je _375
        jmp _376

; ------------------------------------------------------------
_370:
    m_putsln '370'
    jmp _xxx

; ------------------------------------------------------------
_371:
    m_putsln '371'
    jmp _xxx

; ------------------------------------------------------------
_372:
    m_putsln '372'
    jmp _xxx

; ------------------------------------------------------------
_373:
    m_putsln '373'
    jmp _xxx

; ------------------------------------------------------------
_374:
    m_putsln '374'
    jmp _xxx

; ------------------------------------------------------------
_375:
    m_putsln '375'
    jmp _xxx

; ------------------------------------------------------------
_376:
    m_putsln '376'
    jmp _xxx

; ------------------------------------------------------------
_377:
    m_putsln '377'
    jmp _xxx
; -----------------------------------------------------------/

end start
