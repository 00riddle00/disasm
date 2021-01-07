; ============================================================
;  DESCRIPTION
; ============================================================
; Subject: Computer Architecture
; LAB-2 and LAB-3: Intel 8088 Disassembler
;     written in TASM, intended for TASM as well.
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
;         MOV AH, 09h     ; 0100: B409  
;         MOV DX, 01DEh   ; 0102: BADE01
;         INT 21h         ; 0105: CD21
;         ; UNDEFINED     ; 01F9: 65
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
   push ax bx cx dx
   add bx, offset reg_group
   mov dx, bx
   inc dx

   mov ah, 40h
   push bx
   mov bx, out_handle
   mov cx, 1
   int 21h
   pop bx

   ; PUTCHAR to STDOUT
   push ax dx
   xor dx, dx

   mov ah, 2
   mov dl, byte ptr [bx+1]
   int 21h

   pop dx ax
   ; END

   dec dx
   mov ah, 40h
   push bx
   mov bx, out_handle
   int 21h
   pop bx

   ; PUTCHAR to STDOUT
   push ax dx
   xor dx, dx

   mov ah, 2
   mov dl, byte ptr [bx]
   int 21h

   pop dx ax
 
   pop dx cx bx ax
endm

; print asm pointer directive
; ::param:: DL can be 0 (=byte), 1 (=word),
;           or else the ptr will not be printed
;           (no ptr has its use case for
;            printing ESC codes)
m_print_ptr macro
local @@word_ptr, @@endm_print_ptr
    cmp dl, 2
    jae @@endm_print_ptr

    cmp dl, 1
    je @@word_ptr

    ; byte_ptr
    mov si, 9
    m_putsf 'byte ptr '
    jmp @@endm_print_ptr

    @@word_ptr:
    mov si, 9
    m_putsf 'word ptr '

@@endm_print_ptr:
endm

m_octal_byte_to_number macro
    ; SI already points to the first 
    ; octal digit of the offset byte
    xor ax, ax
    mov bx, 8

    add al, byte ptr [data_octal+di]
    mul bl
    inc di

    add al, byte ptr [data_octal+di]
    mul bl
    inc di

    add al, byte ptr [data_octal+di]
endm

m_octal_word_to_number macro
    ; SI already points to the first 
    ; octal digit of the first offset byte (lsb)
    inc di
    inc di
    inc di ; point SI to msb

    xor ax, ax
    xor cx, cx
    mov bx, 8

    add al, byte ptr [data_octal+di]
    mul bl
    inc di

    add al, byte ptr [data_octal+di]
    mul bl
    inc di

    add al, byte ptr [data_octal+di]

    mul bx ; TODO comment
    shr ax, 1 ; divide by 2

    dec di
    dec di
    dec di
    dec di
    dec di ; point SI to lsb

    mov cl, byte ptr [data_octal+di]
    add ax, cx
    mul bx
    inc di

    mov cl, byte ptr [data_octal+di]
    add ax, cx
    mul bx
    inc di

    mov cl, byte ptr [data_octal+di]
    add ax, cx

    inc di
    inc di
    inc di ; point SI to end of msb
endm

; TODO description
m_print_near_offset_byte macro
local @@save_number, @@label_00, @@label_01, @@label_end, @@macro_end, @@endm_m
    m_putfchar '$'

    @@save_number:
        cmp ax, 377o
        jb @@label_00

        ; je case:
            mov si, 3
            m_putsf '+1o'
            jmp @@endm_m

        @@label_00:
            cmp ax, 200o
            jb @@label_01

            sub ax, 376o
            neg ax
            m_putfchar '-'
            jmp @@label_end

        @@label_01:
            add ax, 2
            m_putfchar '+'
            
        @@label_end:
            m_number_to_octal_digit
            jmp @@macro_end

        @@endm_m:
            ret ; jmp _xxx
    @@macro_end:
endm

; TODO description
m_print_near_offset_word macro
local @@save_number, @@label_00, @@label_01, @@label_end, @@macro_end, @@endm_m
    m_putfchar '$'

    @@save_number:
        cmp ax, 100000o
        jb @@label_01

        ; jae case:
        cmp ax, 177776o
        jb @@label_00

        neg ax
        sub ax, 3
        neg ax
        m_putfchar '+'
        jmp @@label_end

        @@label_00:
            neg ax
            sub ax, 3
            m_putfchar '-'
            jmp @@label_end

        @@label_01:
            add ax, 3
            m_putfchar '+'
            
        @@label_end:
            m_number_to_octal_digit
            jmp @@macro_end

        @@endm_m:
            ret ; jmp _xxx
    @@macro_end:
endm

m_number_to_octal_digit macro
local @@conversion, @@convert, @@converted, @@print_result, @@process_lowercase, @@process_uppercase, @@process_number, @@process_space, @@process_symbol, @@print_symbol, @@endm_m_print_near_offset_byte
        ; number is in AX
        ;mov ax, 18
        xor cx, cx
        mov bx, 8

        @@conversion:
            inc cl
            
            xor dx, dx                   ; isvalyti dx, nes cia bus liekana po div
            div bx                       ; div ax/bx, ir liekana padedama dx
            push dx                      ; padeti skaitmeni
            
            cmp ax, 0                    ; jei jau skaicius isdalintas
            jz short @@converted           ; tai eiti i pabaiga

            xchg ax, dx
            mov al, cl

            jmp short @@convert

            ;push 99                      ; let 99 here mean 'space'
            ;jmp short @@convert            ; kitu atveju imti kita skaitmeni

        @@convert:                         ; this extra block is used to put the remainder
            xchg ax, dx                  ; back to AX before next iteration 
            jmp short @@conversion

        @@converted:
            ;m_putsf 'Rezultatas: '

        @@print_result:
            mov ah, 2                    ; atspausdinti skaitmenis
            pop dx
            cmp dx, 99                   ; in case of 'space'
            je short @@process_space

            cmp dx, 10
            jb short @@process_number
            cmp dx, 36
            jb short @@process_uppercase

            @@process_lowercase:
                sub dx, 35
                add dx, 60h
                jmp short @@print_symbol

            @@process_uppercase:
                sub dx, 9
                add dx, 40h
                jmp short @@print_symbol

            @@process_number:
                add dx, '0'
                jmp short @@print_symbol

            @@process_space:
                mov dx, 20h
                inc cl
                jmp short @@print_symbol

            @@print_symbol:
                int 21h
                dec cl
                jnz @@print_result

        m_putfchar 'o'
endm

; TODO description
m_print_sr_prefix_default macro
local @@sr_prefix_default, @@sr_prefix_done
    mov bl, dh
    cmp bl, 0
    je @@sr_prefix_default

    dec bl
    shl bl, 1 ; times 2
    m_print_reg SR16
    m_putfchar ':'
    jmp @@sr_prefix_done

    @@sr_prefix_default:
        mov si, 3
        m_putsf 'DS:'

@@sr_prefix_done:
endm

; TODO description
m_print_sr_prefix macro
local @@sr_prefix_done
    mov bl, dh
    cmp bl, 0
    je @@sr_prefix_done

    dec bl
    shl bl, 1 ; times 2
    m_print_reg SR16
    m_putfchar ':'

@@sr_prefix_done:
endm

; TODO description, this is one of the vaguest points!
m_before_decode macro
    mov dl, al
    and dl, 001b ; will be used for decode procedure
    inc di ; si points to 'mod' now
endm

; TODO description
m_move_index macro
    local @@loop_start, @@si_in_right_place
    ; point SI to 'r/m'
    inc di
    inc di

    cmp cl, 0
    je @@si_in_right_place ; offset was not used

    ; offset was used
    ; point SI to the last byte read
    ;
    ; cl contains information about how many 
    ; bytes were read as an offset or direct address
    xor ch, ch
    @@loop_start:
        inc di
    loop @@loop_start

    @@si_in_right_place:
endm

; ============================================================
;  SETTINGS
; ============================================================
.model small     ; one code segment, one data segment
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

; *************************************************************************************

.DATA

    ; longest command = 6 bytes * 3 octal digits = 18 bytes
    data_octal db 18 dup(?)

    ; Byte-sized registers
    Rb dw 'AL', 'CL', 'DL', 'BL', 'AH', 'CH', 'DH', 'BH'

    ; Word-sized registers
    Rw dw 'AX', 'CX', 'DX', 'BX', 'SP', 'BP', 'SI', 'DI'

    ; Segment registers
    SR16 dw 'ES', 'CS', 'SS', 'DS'  

    ; Registers used as base in EA formation
    EAb dw 'BX', 'BX', 'BP', 'BP', 'SI', 'DI', 'BP', 'BX'

    ; Registers used as index in EA formation
    EAi dw 'SI', 'DI', 'SI', 'DI'

    arg_msg         DB "Intel 8088 Disasembler",13,10
    arg2_msg        DB "Written in TASM, intended for files assembled with TASM as well$"
    cant_open       DB 13,10,"Can't open",13,10,'$'     ; Message for file open error
    err_msg         DB 13,10,"Error",13,10,'$'      ; Message for file write error
    file_n          DB 40 DUP(0)                ; Input File name
    output_n        DB 40 DUP(0)                ; Output file name
    in_handle       DW 0                        ; File handles
    out_handle      DW 0
    temp_index      DW 0                        ; Temporary index for input file
    bytes_read      DW 0                        ; Bytes read on input

    ;WRITING SYMBOLS
    new_line        DB 13,10
    doublepoint     DB ":"
    space           DB 9 DUP(" ")

    v_arg_1         DB 40 DUP("$")      ; First argument string
    v_arg_2         DB 40 DUP("$")      ; Second argument string

    v_arg_index     DW 0                ; Argument index (For both v_arg_1 and v_arg_2)
    cur_arg_buff    DW 0                ; Adress of the current argument buffer = offset (v_arg_1 or v_arg_2)

    temp_bytes      DB 25 DUP(" "), '$' ; The read bytes for current command
                    DB '$'              ; This line is for debugging "temp_bytes" (see m_printf macro and its usage)
    temp_b_index    DW 0                ; The index of temp_bytes buffer

    ip_index        DW 100h             ; The current IP value
    ip_value        DB 4 DUP("$")       ; The current IP value in ASCII
                    DB '$'              ; This line is for debugging "ip_value" (see m_printf macro and its usage)
    ip_arr_index    DW 0                ; The index of ip_value buffer
    temp_ip_add     DW 0                ; The number of bytes currently read (For IP adding)

    needs_convert   DW 0
    counter_convert DW 0

    ; Analyze byte
    hex             DB "0123456789ABCDEF"   ; Hex base

; *************************************************************************************

; ==================================== TESTING ===============================================

; --------------------------------------------------------------------------------------------
;                                     CASES 1                                                ;
; --------------------------------------------------------------------------------------------
    data_octal1 db 8, 8, 8                     ; 0???: ??      | UNDEFINED

    db 3, 5, 0,  0, 0, 0,  2, 0, 0            ; 0???: ??      | CALL $-077775o
    db 3, 5, 1,  0, 0, 1,  2, 0, 0            ; 0???: ??      | JMP  $-077774o
    db 3, 5, 0,  0, 0, 2,  2, 0, 0            ; 0???: ??      | CALL $-077773o
    db 3, 5, 1,  0, 0, 3,  2, 0, 0            ; 0???: ??      | JMP  $-077772o
    db 3, 5, 0,  0, 0, 4,  2, 0, 0            ; 0???: ??      | CALL $-077771o

    db 3, 5, 0,  3, 7, 4,  3, 7, 6            ; 0???: ??      | CALL $-000401o
    db 3, 5, 1,  3, 7, 5,  3, 7, 6            ; 0???: ??      | JMP  $-000400o

    db 3, 5, 0,  1, 7, 5,  3, 7, 7            ; 0???: ??      | CALL $-000200o
    db 3, 5, 1,  1, 7, 6,  3, 7, 7            ; 0???: ??      | JMP  $-000177o
    db 3, 5, 0,  1, 7, 7,  3, 7, 7            ; 0???: ??      | CALL $-000176o
    db 3, 5, 1,  2, 0, 0,  3, 7, 7            ; 0???: ??      | JMP  $-000175o

    db 3, 5, 0,  3, 7, 2,  3, 7, 7            ; 0???: ??      | CALL $-000003o
    db 3, 5, 1,  3, 7, 3,  3, 7, 7            ; 0???: ??      | JMP  $-000002o
    db 3, 5, 0,  3, 7, 4,  3, 7, 7            ; 0???: ??      | CALL $-000001o
    db 3, 5, 1,  3, 7, 5,  3, 7, 7            ; 0???: ??      | JMP  $-000000o
    db 3, 5, 0,  3, 7, 6,  3, 7, 7            ; 0???: ??      | CALL $+000001o
    db 3, 5, 0,  3, 7, 7,  3, 7, 7            ; 0???: ??      | CALL $+000002o

    ;db 0FFh

    db 3, 5, 1,  0, 0, 0,  0, 0, 0            ; 0???: ??      | JMP  $+000003o
    db 3, 5, 0,  0, 0, 1,  0, 0, 0            ; 0???: ??      | CALL $+000004o
    db 3, 5, 0,  0, 0, 2,  0, 0, 0            ; 0???: ??      | CALL $+000005o

    db 3, 5, 0,  0, 1, 7,  0, 0, 0            ; 0???: ??      | CALL $+000022o
    db 3, 5, 1,  0, 3, 6,  0, 0, 0            ; 0???: ??      | JMP  $+000041o
    db 3, 5, 0,  0, 4, 0,  0, 0, 0            ; 0???: ??      | CALL $+000043o
    db 3, 5, 1,  0, 5, 2,  0, 0, 0            ; 0???: ??      | JMP  $+000055o
    db 3, 5, 0,  1, 3, 1,  0, 0, 0            ; 0???: ??      | CALL $+000134o

    db 3, 5, 0,  1, 7, 4,  0, 0, 0            ; 0???: ??      | CALL $+000177o
    db 3, 5, 0,  1, 7, 5,  0, 0, 0            ; 0???: ??      | CALL $+000200o
    db 3, 5, 1,  1, 7, 6,  0, 0, 0            ; 0???: ??      | JMP  $+000201o
    db 3, 5, 0,  1, 7, 7,  0, 0, 0            ; 0???: ??      | CALL $+000202o
    db 3, 5, 0,  2, 0, 0,  0, 0, 0            ; 0???: ??      | CALL $+000203o

    db 3, 5, 0,  3, 7, 4,  1, 7, 7            ; 0???: ??      | CALL $+077777o
    db 3, 5, 1,  3, 7, 5,  1, 7, 7            ; 0???: ??      | JMP  $+100000o
    db 3, 5, 0,  3, 7, 6,  1, 7, 7            ; 0???: ??      | CALL $+100001o
    db 3, 5, 1,  3, 7, 7,  1, 7, 7            ; 0???: ??      | JMP  $+100002o

    ;db 0FFh
; --------------------------------------------------------------------------------------------

    db 1, 6, 0,  2, 0, 0                      ; 0???: ??      | JO  $-176o
    db 1, 6, 1,  2, 0, 1                      ; 0???: ??      | JNO $-175o
    db 1, 6, 2,  3, 7, 3                      ; 0???: ??      | JB  $-003o
    db 1, 6, 3,  3, 7, 4                      ; 0???: ??      | JAE $-002o
    db 1, 6, 4,  3, 7, 5                      ; 0???: ??      | JE  $-001o
    db 1, 6, 5,  3, 7, 6                      ; 0???: ??      | JNE $-000o
    db 1, 6, 6,  3, 7, 7                      ; 0???: ??      | JBE $+001o
    db 1, 6, 7,  0, 0, 0                      ; 0???: ??      | JA  $+002o

    db 1, 7, 0,  0, 0, 1                      ; 0???: ??      | JS  $+003o
    db 1, 7, 1,  0, 2, 0                      ; 0???: ??      | JNS $+022o
    db 1, 7, 2,  0, 3, 7                      ; 0???: ??      | JP  $+041o
    db 1, 7, 3,  0, 4, 5                      ; 0???: ??      | JNP $+047o
    db 1, 7, 4,  0, 5, 3                      ; 0???: ??      | JL  $+055o    
    db 1, 7, 5,  1, 3, 2                      ; 0???: ??      | JGE $+134o
    db 1, 7, 6,  1, 7, 5                      ; 0???: ??      | JLE $+177o
    db 1, 7, 7,  1, 7, 6                      ; 0???: ??      | JG  $+200o

    db 3, 4, 0,  1, 7, 7                      ; 0???: ??      | LOOPNE $+201o
    db 3, 4, 1,  2, 0, 0                      ; 0???: ??      | LOOPE  $-176o
    db 3, 4, 2,  2, 0, 1                      ; 0???: ??      | LOOP   $-175o
    db 3, 4, 3,  3, 7, 3                      ; 0???: ??      | JCXZ   $-003o

    db 3, 5, 3,  3, 7, 4                      ; 0???: ??      | JMP SHORT $-002o

    ;db 0FFh
; --------------------------------------------------------------------------------------------

    db 2, 3, 2,  1, 7, 0,  1, 2, 6,  0, 6, 4,  0, 2, 2   ; 0???: ??      | CALL 022064:126170 (=9A 78 56 34 12) (=JMP 1234h:5678h)
    db 3, 5, 2,  1, 7, 0,  1, 2, 6,  0, 6, 4,  0, 2, 2   ; 0???: ??      | JMP 022064:126170  (=EA 78 56 34 12) (=JMP 1234h:5678h)

    db 0, 4, 6,  3, 7, 7,  1, 2, 0,  1, 1, 1             ; 0???: ??      | CALL ES:[BX+SI+000111] (=26 FF 50 49)
    db 3, 7, 7,  0, 2, 6,  1, 1, 1,  2, 2, 2             ; 0???: ??      | CALL DS:[222111] (=FF 16 49 92)
    db 0, 6, 6,  3, 7, 7,  0, 2, 6,  1, 1, 1,  2, 2, 2   ; 0???: ??      | CALL SS:[222111] (=36 FF 16 49 92)

    db 3, 7, 7,  0, 2, 0                                 ; 0???: ??      | CALL [BX+SI] (=FF 10)
    db 3, 7, 7,  1, 2, 0,  1, 1, 1                       ; 0???: ??      | CALL [BX+SI+000111] (=FF 50 49)
    db 3, 7, 7,  2, 2, 0,  1, 1, 1,  3, 7, 7             ; 0???: ??      | CALL [BX+SI+377111] (=FF 90 49 FF)

    db 3, 7, 7,  2, 2, 0,  3, 1, 1,  0, 0, 0             ; 0???: ??      | CALL [BX+SI+000311] (=FF 90 C9 00) 
    db 3, 7, 7,  2, 2, 0,  3, 1, 1,  3, 7, 6             ; 0???: ??      | CALL [BX+SI+376311] (=FF 90 C9 FE) 

    db 3, 7, 7,  1, 2, 0,  2, 1, 1                       ; 0???: ??      | CALL [BX+SI+377211] ; (=FF 50 89)
    db 3, 7, 7,  2, 2, 0,  2, 1, 1,  0, 0, 0             ; 0???: ??      | CALL [BX+SI+000211]  ; (=FF 90 89 00)

    db 3, 7, 7,  2, 2, 0,  3, 1, 1,  0, 0, 1             ; 0???: ??      | CALL [BX+SI+001311]  (=FF 90 C9 01)

    db 3, 7, 7,  2, 2, 0,  1, 1, 1,  2, 2, 2             ; 0???: ??      | CALL [BX+SI+222111] (=FF 90 49 92) 

    db 3, 7, 7,  0, 2, 6,  1, 1, 1,  2, 2, 2             ; 0???: ??      | CALL DS:[222111] (=FF 16 49 92)
    db 3, 7, 7,  3, 2, 0                                 ; 0???: ??      | CALL AX      (=FF D0)

    db 3, 7, 7,  0, 4, 0                                 ; 0???: ??      | JMP [BX+SI]  (=FF 20)
    db 3, 7, 7,  3, 4, 0                                 ; 0???: ??      | JMP AX       (=FF E0)

    db 3, 7, 7,  1, 3, 0,  1, 1, 1                       ; 0???: ??      | CALL dword ptr [BX+SI+000111] (=FF 58 49)
    db 3, 7, 7,  2, 3, 0,  1, 1, 1,  2, 2, 2             ; 0???: ??      | CALL dword ptr [BX+SI+222111] (=FF 98 49 92)
    db 3, 7, 7,  3, 3, 0                                 ; 0???: ??      | UNDEFINED

    db 3, 7, 7,  1, 5, 0,  1, 1, 1                       ; 0???: ??      | JMP dword ptr [BX+SI+000111]  (=FF 68 49)
    db 3, 7, 7,  2, 5, 0,  1, 1, 1,  2, 2, 2             ; 0???: ??      | JMP dword ptr [BX+SI+222111] (=FF A8 49 92)
    db 3, 7, 7,  3, 5, 0                                 ; 0???: ??      | UNDEFINED

    ;db 0FFh
; --------------------------------------------------------------------------------------------

    db 3, 2, 0,  0, 0, 0                       ; 0???: ??      | ROL byte ptr [BX+SI], 1
    db 3, 2, 1,  3, 1, 2                       ; 0???: ??      | ROR DX, 1
    db 3, 2, 2,  1, 2, 6,  1, 1, 1             ; 0???: ??      | RCL byte ptr [BP+000111], CL
    db 3, 2, 3,  2, 3, 3,  1, 1, 1,  2, 2, 2   ; 0???: ??      | RCR word ptr [BP+DI+222111], CL
    db 3, 2, 0,  3, 4, 0                       ; 0???: ??      | SHL AL, 1
    db 3, 2, 1,  3, 5, 5                       ; 0???: ??      | SHR BP, 1
    db 3, 2, 2,  3, 6, 5                       ; 0???: ??      | UNDEFINED
    db 3, 2, 3,  0, 7, 6,  1, 1, 1,  2, 2, 2   ; 0???: ??      | SAR word ptr DS:[222111], CL

    db 3, 3, 0,  0, 4, 0                       ; 0???: ??      | <ESC code> [BX+SI]
    db 3, 3, 1,  0, 3, 6,  1, 1, 1,  2, 2, 2   ; 0???: ??      | <ESC code> DS:[222111]
    db 3, 3, 2,  1, 2, 6,  1, 1, 1             ; 0???: ??      | <ESC code> [BP+000111]
    db 3, 3, 3,  2, 1, 6,  1, 1, 1,  2, 2, 2   ; 0???: ??      | <ESC code> [BP+222111]
    db 3, 3, 4,  3, 0, 0                       ; 0???: ??      | <ESC code> 

    db 2, 1, 4,  0, 1, 0                       ; 0???: ??      | MOV word ptr [BX+SI], CS
    db 2, 1, 4,  0, 4, 0                       ; 0???: ??      | UNDEFINED

    db 2, 1, 6,  3, 2, 2                       ; 0???: ??      | MOV SS, DX
    db 2, 1, 6,  3, 1, 2                       ; 0???: ??      | UNDEFINED

    db 2, 1, 5,  2, 6, 3,  1, 1, 1,  2, 2, 2   ; 0???: ??      | LEA SI, dword ptr [BP+DI+222111] 
    db 2, 1, 5,  3, 7, 6                       ; 0???: ??      | UNDEFINED

    db 3, 0, 4,  2, 6, 3,  1, 1, 1,  2, 2, 2   ; 0???: ??      | LES SI, dword ptr [BP+DI+222111] 
    db 3, 0, 5,  1, 7, 6,  1, 1, 1             ; 0???: ??      | LDS DI, dword ptr [BP+000111] 
    db 3, 0, 5,  3, 0, 3                       ; 0???: ??      | UNDEFINED

    ;db 0FFh
; --------------------------------------------------------------------------------------------

    db 2, 1, 7,  0, 6, 0                                           ; 0???: ??      | POP word ptr [BX+SI]
    db 2, 1, 7,  0, 7, 0                                           ; 0???: ??      | UNDEFINED

    db 3, 7, 6,  0, 0, 6,  1, 1, 1,  2, 2, 2                       ; 0???: ??      | INC byte ptr DS:[222111]
    db 3, 7, 7,  2, 0, 3,  1, 1, 1,  2, 2, 2                       ; 0???: ??      | INC word ptr [BP+DI+222111]

    db 3, 7, 6,  3, 1, 0                                           ; 0???: ??      | DEC AL
    db 3, 7, 7,  3, 1, 2                                           ; 0???: ??      | DEC DX

    db 3, 7, 7,  0, 6, 6,  1, 1, 1,  2, 2, 2                       ; 0???: ??      | PUSH word ptr DS:[222111]
    db 3, 7, 7,  2, 6, 3,  1, 1, 1,  2, 2, 2                       ; 0???: ??      | PUSH word ptr [BP+DI+222111]
    db 3, 7, 7,  3, 6, 5                                           ; 0???: ??      | PUSH BP

    db 3, 6, 6,  1, 0, 4,  2, 2, 2,  3, 3, 3                       ; 0???: ??      | TEST byte ptr [SI+377222], 333
    db 3, 6, 6,  0, 0, 1,  3, 3, 3                                 ; 0???: ??      | TEST byte ptr [BX+DI], 333
    db 3, 0, 7,  2, 0, 4,  1, 1, 1,  2, 2, 2,  3, 3, 3,  4, 4, 4   ; 0???: ??      | MOV word ptr [SI+222111], 444333
    db 3, 0, 7,  3, 0, 4,  1, 1, 1,  2, 2, 2                       ; 0???: ??      | MOV SP, 222111

    ;db 0FFh
; --------------------------------------------------------------------------------------------

    db 3, 6, 6,  0, 2, 6,  1, 1, 1,  2, 2, 2   ; 0???: ??      | NOT byte ptr DS:[222111]
    db 3, 6, 7,  3, 3, 5                       ; 0???: ??      | NEG BP

    db 3, 6, 6,  0, 4, 6,  1, 1, 1,  2, 2, 2   ; 0???: ??      | MUL byte ptr DS:[222111]
    db 3, 6, 7,  3, 5, 5                       ; 0???: ??      | IMUL BP

    db 3, 6, 6,  0, 6, 6,  1, 1, 1,  2, 2, 2   ; 0???: ??      | DIV byte ptr DS:[222111]
    db 3, 6, 7,  3, 7, 5                       ; 0???: ??      | IDIV BP

    db 2, 0, 4,  0, 3, 6,  1, 1, 1,  2, 2, 2   ; 0???: ??      | TEST BL, byte ptr DS:[222111]
    db 2, 0, 5,  0, 2, 4                       ; 0???: ??      | TEST DX, word ptr [SI]

    db 2, 0, 6,  2, 0, 3,  1, 1, 1,  2, 2, 2   ; 0???: ??      | XCHG AL, byte ptr [BP+DI+222111]
    db 2, 0, 7,  3, 1, 4                       ; 0???: ??      | XCHG CX, SP

    ;db 0FFh

; --------------------------------------------------------------------------------------------
;                                     CASES 2                                                ;
; --------------------------------------------------------------------------------------------

; ------------------------------------- GROUP 0 ----------------------------------------------
    db 0, 0, 0,  0, 2, 0                       ; 0???: ??      | ADD byte ptr [BX+SI], DL
    db 0, 0, 1,  0, 2, 0                       ; 0???: ??      | ADD word ptr [BX+SI], DX

    db 0, 0, 0,  0, 2, 4                       ; 0???: ??      | ADD byte ptr [SI], DL
    db 0, 0, 1,  0, 2, 4                       ; 0???: ??      | ADD word ptr [SI], DX

    db 0, 0, 0,  3, 1, 0                       ; 0???: ??      | ADD AL, CL
    db 0, 1, 1,  3, 1, 0                       ; 0???: ??      | OR AX, CX

    db 0, 0, 0,  0, 3, 6,  1, 1, 1,  2, 2, 2   ; 0???: ??      | ADD byte ptr DS:[222111], BL
    db 0, 0, 1,  0, 3, 6,  1, 1, 1,  2, 2, 2   ; 0???: ??      | ADD word ptr DS:[222111], BX

    db 0, 0, 2,  3, 1, 0                       ; 0???: ??      | ADD CL, AL
    db 0, 0, 3,  3, 1, 0                       ; 0???: ??      | ADD CX, AX

    db 0, 0, 2,  1, 2, 4,  1, 1, 1             ; 0???: ??      | ADD DL, byte ptr [SI+000111]
    db 0, 0, 3,  1, 2, 4,  1, 1, 1             ; 0???: ??      | ADD DX, word ptr [SI+000111]

    db 0, 0, 2,  2, 2, 4,  1, 1, 1,  2, 2, 2   ; 0???: ??      | ADD DL, byte ptr [SI+222111]
    db 0, 0, 3,  2, 2, 4,  1, 1, 1,  2, 2, 2   ; 0???: ??      | ADD DX, word ptr [SI+222111]

    db 0, 1, 2,  0, 3, 6,  1, 1, 1,  2, 2, 2   ; 0???: ??      | OR BL, byte ptr DS:[222111]
    db 0, 0, 3,  0, 3, 6,  1, 1, 1,  2, 2, 2   ; 0???: ??      | ADD BX, word ptr DS:[222111]

    db 0, 2, 0,  1, 2, 4,  1, 1, 1             ; 0???: ??      | ADC byte ptr [SI+000111], DL
    db 0, 3, 1,  1, 2, 4,  1, 1, 1             ; 0???: ??      | SBB word ptr [SI+000111], DX

    db 0, 3, 2,  0, 2, 0                       ; 0???: ??      | SBB DL, byte ptr [BX+SI]
    db 0, 2, 3,  0, 2, 0                       ; 0???: ??      | ADC DX, word ptr [BX+SI]

    db 0, 4, 0,  2, 2, 4,  1, 1, 1,  2, 2, 2   ; 0???: ??      | AND byte ptr [SI+222111], DL
    db 0, 5, 1,  2, 2, 4,  1, 1, 1,  2, 2, 2   ; 0???: ??      | SUB word ptr [SI+222111], DX

    db 0, 5, 2,  0, 2, 4                       ; 0???: ??      | SUB DL, byte ptr [SI]
    db 0, 4, 3,  0, 2, 4                       ; 0???: ??      | AND DX, word ptr [SI]

    db 0, 6, 0,  1, 0, 3,  1, 1, 1             ; 0???: ??      | XOR byte ptr [BP+DI+000111], AL
    db 0, 7, 1,  1, 0, 3,  1, 1, 1             ; 0???: ??      | CMP word ptr [BP+DI+000111], AX

    db 0, 7, 2,  2, 0, 3,  1, 1, 1,  2, 2, 2   ; 0???: ??      | CMP AL, byte ptr [BP+DI+222111]
    db 0, 6, 3,  2, 0, 3,  1, 1, 1,  2, 2, 2   ; 0???: ??      | XOR AX, word ptr [BP+DI+222111]

    ;db 0FFh

; ------------------------------------- GROUP 2 ----------------------------------------------
    db 2, 0, 0,  1, 0, 4,  2, 2, 2,  3, 3, 3                       ; 0???: ??      | ADD byte ptr [SI+377222], 333
    ;     80        44        92        DB                                          =ADD byte ptr [SI+92h], 0DBh
    ;     80        44        92        DB                                          =ADD byte ptr [SI-06Eh], 0DBh

    db 2, 0, 3,  2, 0, 4,  1, 1, 1,  2, 2, 2,  3, 3, 3             ; 0???: ??      | ADD word ptr [SI+222111], 377333
    ;     83        84        49        92        DB                                =ADD word ptr [SI+9249h], 0FFDBh
    ;                                                                               =ADD word ptr [SI-6DB7h], 0FFDBh

    db 2, 0, 0,  1, 1, 4,  2, 2, 2,  3, 3, 3                       ; 0???: ??      | OR byte ptr [SI+377222], 333
    db 2, 0, 3,  2, 2, 4,  1, 1, 1,  2, 2, 2,  3, 3, 3             ; 0???: ??      | ADC word ptr [SI+222111], 377333

    db 2, 0, 1,  2, 0, 4,  1, 1, 1,  2, 2, 2,  3, 3, 3,  4, 4, 4   ; 0???: ??      | ADD word ptr [SI+222111], 444333
    db 2, 0, 2,  2, 0, 4,  1, 1, 1,  2, 2, 2,  3, 3, 3             ; 0???: ??      | ADD byte ptr [SI+222111], 333
    db 2, 0, 2,  0, 0, 6,  1, 1, 1,  2, 2, 2,  3, 3, 3             ; 0???: ??      | ADD byte ptr DS:[222111], 333

    db 2, 0, 0,  1, 3, 4,  2, 2, 2,  3, 3, 3                       ; 0???: ??      | SBB byte ptr [SI+377222], 333
    db 2, 0, 3,  2, 4, 4,  1, 1, 1,  2, 2, 2,  3, 3, 3             ; 0???: ??      | AND word ptr [SI+222111], 377333

    db 2, 0, 0,  1, 5, 4,  2, 2, 2,  3, 3, 3                       ; 0???: ??      | SUB byte ptr [SI+377222], 333
    db 2, 0, 0,  1, 6, 4,  2, 2, 2,  3, 3, 3                       ; 0???: ??      | XOR byte ptr [SI+377222], 333
    db 2, 0, 3,  2, 7, 4,  1, 1, 1,  2, 2, 2,  3, 3, 3             ; 0???: ??      | CMP word ptr [SI+222111], 377333

    db 2, 1, 0,  2, 0, 3,  1, 1, 1,  2, 2, 2                       ; 0???: ??      | MOV byte ptr [BP+DI+222111], AL
    db 2, 1, 1,  2, 0, 3,  1, 1, 1,  2, 2, 2                       ; 0???: ??      | MOV word ptr [BP+DI+222111], AX

    db 2, 1, 2,  1, 0, 3,  1, 1, 1                                 ; 0???: ??      | MOV AL, byte ptr [BP+DI+000111]
    db 2, 1, 3,  1, 0, 3,  1, 1, 1                                 ; 0???: ??      | MOV AX, word ptr [BP+DI+000111]

    ;db 0FFh

; --------------------------------------------------------------------------------------------
;                                     CASES 3                                                ;
; --------------------------------------------------------------------------------------------

; ------------------------------------- GROUP 0 ----------------------------------------------
    db 0, 0, 4,  1, 1, 1                       ; 0???: ??      | ADD AL, 111
    db 0, 0, 5,  1, 1, 1,  2, 2, 2             ; 0???: ??      | ADD AX, 222111
    db 0, 0, 7                                 ; 0???: ??      | POP ES
    db 0, 1, 1,  3, 1, 0                       ; 0???: ??      | OR AX, CX
    db 0, 1, 4,  1, 1, 1                       ; 0???: ??      | OR AL, 111
    db 0, 1, 5,  1, 1, 1,  2, 2, 2             ; 0???: ??      | OR AX, 222111
    db 0, 1, 6                                 ; 0???: ??      | PUSH CS
    db 0, 2, 4,  1, 1, 1                       ; 0???: ??      | ADC AL, 111
    db 0, 2, 5,  1, 1, 1,  2, 2, 2             ; 0???: ??      | ADC AX, 222111
    db 0, 2, 7                                 ; 0???: ??      | POP SS 
    db 0, 3, 6                                 ; 0???: ??      | PUSH DS
    db 0, 4, 7                                 ; 0???: ??      | DAA
    db 0, 5, 4,  1, 1, 1                       ; 0???: ??      | SUB AL, 111
    db 0, 5, 5,  1, 1, 1,  2, 2, 2             ; 0???: ??      | SUB AX, 222111
    db 0, 7, 4,  1, 1, 1                       ; 0???: ??      | CMP AL, 111
    db 0, 7, 5,  1, 1, 1,  2, 2, 2             ; 0???: ??      | CMP AX, 222111
    db 0, 7, 7                                 ; 0???: ??      | AAS

; ------------------------------------- GROUP 1 ----------------------------------------------
    db 1, 0, 6                                 ; 0???: ??      | inc di
    db 1, 1, 3                                 ; 0???: ??      | DEC BX
    db 1, 2, 0                                 ; 0???: ??      | PUSH AX
    db 1, 3, 1                                 ; 0???: ??      | POP CX
    db 1, 4, 5                                 ; 0???: ??      | UNDEFINED

; ------------------------------------- GROUP 2 ----------------------------------------------
    db 2, 2, 0                                 ; 0???: ??      | NOP
    db 2, 2, 5                                 ; 0???: ??      | XCHG BP, AX
    db 2, 3, 0                                 ; 0???: ??      | CBW
    db 2, 3, 3                                 ; 0???: ??      | WAIT
    db 2, 3, 7                                 ; 0???: ??      | LAHF
    db 2, 4, 0,  1, 1, 1,  2, 2, 2             ; 0???: ??      | MOV AL, byte ptr DS:[222111]
    db 0, 4, 6,  2, 4, 0,  1, 1, 1,  2, 2, 2   ; 0???: ??      | MOV AL, byte ptr ES:[222111]
    db 2, 4, 1,  1, 1, 1,  2, 2, 2             ; 0???: ??      | MOV AX, word ptr DS:[222111]
    db 0, 6, 6,  2, 4, 2,  1, 1, 1,  2, 2, 2   ; 0???: ??      | MOV byte ptr SS:[222111], AL
    db 2, 4, 3,  1, 1, 1,  2, 2, 2             ; 0???: ??      | MOV word ptr DS:[222111], AX
    db 2, 4, 4                                 ; 0???: ??      | MOVSB
    db 2, 4, 7                                 ; 0???: ??      | CMPSW
    db 2, 5, 0,  1, 1, 1                       ; 0???: ??      | TEST AL, 111
    db 2, 5, 1,  1, 1, 1,  2, 2, 2             ; 0???: ??      | TEST AX, 222111
    db 2, 5, 2                                 ; 0???: ??      | STOSB
    db 2, 5, 5                                 ; 0???: ??      | LODSW
    db 2, 5, 7                                 ; 0???: ??      | SCASW
    db 2, 6, 4,  0, 1, 1                       ; 0???: ??      | MOV AH, 011
    db 2, 7, 2,  3, 3, 6,  0, 0, 1             ; 0???: ??      | MOV DX, 001336
    
; ------------------------------------- GROUP 3 ----------------------------------------------
    db 3, 0, 2,  1, 1, 1,  2, 2, 2             ; 0???: ??      | RET 222111
    db 3, 0, 3                                 ; 0???: ??      | RET
    db 3, 1, 2,  1, 1, 1,  2, 2, 2             ; 0???: ??      | RETF 222111
    db 3, 1, 3                                 ; 0???: ??      | RETF
    db 3, 1, 5,  0, 4, 1                       ; 0???: ??      | INT 041
    db 3, 1, 6                                 ; 0???: ??      | INTO
    db 3, 2, 4                                 ; 0???: ??      | UNDEFINED
    db 3, 2, 4,  0, 1, 2                       ; 0???: ??      | AAM
    db 3, 2, 6                                 ; 0???: ??      | UNDEFINED
    db 3, 2, 7                                 ; 0???: ??      | XLAT
    db 3, 4, 4,  1, 1, 1                       ; 0???: ??      | IN AL, 111
    db 3, 4, 7,  0, 0, 1                       ; 0???: ??      | OUT 001, AX
    db 3, 5, 5                                 ; 0???: ??      | IN AX, DX
    db 3, 5, 6                                 ; 0???: ??      | OUT DX, AL
    db 3, 6, 0                                 ; 0???: ??      | LOCK
    db 3, 6, 1                                 ; 0???: ??      | UNDEFINED
    db 3, 6, 3                                 ; 0???: ??      | REP
    db 3, 6, 4                                 ; 0???: ??      | HLT
    db 3, 7, 1                                 ; 0???: ??      | STC
    db 3, 7, 2                                 ; 0???: ??      | CLI
    db 3, 7, 4                                 ; 0???: ??      | CLD

    db 0FFh
; ============================================================================================

.DATA?
input_buff      DB 257 DUP(?)           ; Input buffer
outpt_buff      DB 257 DUP(?)           ; Output buffer

; *************************************************************************************

; ============================================================
;  CODE
; ============================================================

.code

; ------------------------------------------------------------
; PROCEDURES
; ------------------------------------------------------------

; Before call: SI must point to the first octal 
; digit of the byte to be printed FIXME -> wrong!
;
; After call: SI increases by 3
proc p_print_next_byte

    push ax dx
    inc di
    m_octal_byte_to_number
    m_number_to_octal_digit

    pop dx ax
    ret
endp

; Before call: SI must point to the first octal 
; digit of the byte to be printed
;
; After call: DI increases by 3
proc p_print_next_byte_sign_extended

    push ax dx
    xor ax, ax ; make AX zero
    inc di 

    cmp byte ptr [data_octal+di], 2
    jb padding_done

    one_padding:
        ;m_putsf "377"
        mov ax, 377o

    padding_done:

    mov bx, 8
    xor cx, cx

    mul bx ; TODO comment
    shr ax, 1 ; divide by 2

    mov cl, byte ptr [data_octal+di]
    add ax, cx
    mul bx
    inc di

    mov cl, byte ptr [data_octal+di]
    add ax, cx
    mul bx
    inc di ; point DI to end of msb

    mov cl, byte ptr [data_octal+di]
    add ax, cx

    m_number_to_octal_digit

    pop dx ax
    ret
endp

; Before call: DI must point to the first octal 
; digit of the youngest byte
;
; After call: DI increases by 6
proc p_print_next_word
    push ax dx
    inc di

    m_octal_word_to_number
    m_number_to_octal_digit

    pop dx ax
    ret
endp

; Decode 'reg' from 'mod reg r/m [offset]'
; Before call: DI must point to 'mod' byte
;              DL must be either 0 (=byte) or 1 (=word)
;
; After call: DI is not changed.
proc p_decode_reg
    push ax bx dx
 
    ; get 'reg' value (3 bits, represented as an octal number)
    inc di
    mov al, byte ptr [data_octal+di]

    ; check if it's byte or word instruction
    cmp dl, 1
    jb print_byte_reg

    print_word_reg:
        ; TODO move this part to m_print_reg macro
        mov bl, al
        shl bl, 1 ; times 2
        m_print_reg Rw
        jmp reg_printed

    ; print byte register according to 'reg' value
    print_byte_reg:
        ; TODO move this part to m_print_reg macro
        mov bl, al
        shl bl, 1 ; times 2
        m_print_reg Rb

    reg_printed:
    ; place DI back to point at 'mod'
    dec di
    pop dx bx ax
    ret
endp

; Decode 'r/m' from 'mod reg r/m [offset]'
; Before call: DI must point to 'mod' byte
;              DL should be either 0 (=byte) or 1 (=word)
;              or else ptr directive will not be printed
;              (this effect is also useful for ESC codes)
;
; After call: DI is not changed.
;             CL will contain how many bytes (as in octal digits)
;                were used for offset (or direct address)
proc p_decode_rm
    push ax bx dx

    ; get 'mod' value (2 bits, represented as an octal number)
    mov al, byte ptr [data_octal+di]

    ; check 'mod' value
    cmp al, 3
    jb __rm_is_mem ; so 'r/m' is memory (according to 'mod')

    ; 'r/m' is register (according to 'mod')
    _rm_is_reg:
        ; point DI to 'reg' and call decode_reg procedure.
        inc di
        ; It will think that DI points to 'mod', and will treat
        ; 'r/m' as 'reg', which is what is needed here.
        ; TODO comment: it will use DL
        call p_decode_reg 
        ; point DI back to 'mod'
        dec di

        ; save in CL how many additional bytes (in octal) were read after 'r/m' byte
        mov cl, 0 
        jmp endp_decode_rm

    ; 'r/m' is memory (according to 'mod')
    __rm_is_mem:
        ; check 'mod' value again
        cmp al, 1
        jb _rm_is_mem_no_offset ; so offset is not used for EA (according to 'mod')

        ; offset is used for EA (according to 'mod')
        _rm_is_mem_with_offset:
            ; place 'mod' value in CH (needed later for specifying offset)
            mov ch, al

            ; get 'r/m' value (3 bits, represented as an octal number)
            inc di
            inc di
            mov al, byte ptr [data_octal+di]

            m_print_ptr
            m_print_sr_prefix
            m_putfchar '['

            mov bl, al 
            shl bl, 1 ; times 2
            ; print register (used as a base) according to 'r/m' value
            m_print_reg EAb
            m_putfchar '+'

            ; check 'r/m' value again
            cmp al, 4
            jae no_index_L0; so index register is not used for EA

            ; index register is also used for EA
            add_index_L0:
                ; print register (used as an index) according to 'r/m' value
                m_print_reg EAi
                m_putfchar '+'

            ; if jumped to this label, index register is not used for EA
            no_index_L0:
            ; CH contains 'mod' value
            cmp ch, 2
            jb print_offset_byte ; offset is one byte (according to 'mod')

            ; offset is two bytes (according to 'mod')
            print_offset_word:
                call p_print_next_word
                ; save in CL how many additional bytes (in octal) were read after 'r/m' byte
                mov cl, 6
                ; place DI back to point at 'r/m'
                sub di, 6

                jmp offset_printed

            ; offset is one byte (according to 'mod')
            print_offset_byte: ; FIXME actually prints two bytes!

                ;call p_print_next_byte
                call p_print_next_byte_sign_extended
                ; save in CL how many additional bytes (in octal) were read after 'r/m' byte
                mov cl, 3
                ; place DI back to point at 'r/m'
                sub di, 3

            offset_printed:
            m_putfchar ']'

            ; place DI back to point at 'mod'
            dec di
            dec di
            jmp endp_decode_rm

        ; offset is not used for EA (according to 'mod')
        _rm_is_mem_no_offset:
            ; get 'r/m' value (3 bits, represented as an octal number)
            inc di
            inc di
            mov al, byte ptr [data_octal+di]

            ; check 'r/m' value for a special case - direct address
            cmp al, 6
            je _rm_is_mem_no_offset_direct_address ; so only direct address is used for EA

            m_print_ptr
            m_putfchar '['

            mov bl, al 
            shl bl, 1 ; times 2
            ; print register (used as a base) according to 'r/m' value
            m_print_reg EAb

            ; check 'r/m' value again
            cmp al, 4
            jae short no_index_L1; so index register is not used for EA

            ; index register is also used for EA
            add_index_L1:
                ; print register (used as an index) according to 'r/m' value
                m_putfchar '+'
                m_print_reg EAi

            ; if jumped to this label, index register is not used for EA
            no_index_L1:
            m_putfchar ']'

            ; save in CL how many additional bytes (in octal) were read after 'r/m' byte
            mov cl, 0 
            ; place DI back to point at 'mod'
            dec di
            dec di
            jmp endp_decode_rm

            ; only direct address is used for EA
            _rm_is_mem_no_offset_direct_address:
                m_print_ptr
                m_print_sr_prefix_default
                m_putfchar '['

                ; print direct address (two bytes)
                call p_print_next_word
                m_putfchar ']'

                ; save in CL how many additional bytes (in octal) were read after 'r/m' byte
                mov cl, 6

                ; place DI back to point at 'r/m'
                sub di, 6

                ; place DI back to point at 'mod'
                dec di
                dec di

    endp_decode_rm:
        pop dx bx ax
    ret
endp

; Handles printing "r/m, immediate" for the 
; commands of the format:
; 1000 00sw mod XXX r/m [offset] lsb_immediate [msb_immediate]
; where each 'X' is one of 0 or 1.
; 
; Before call: DI should point to the octal digit for '0sw',
;              AL should contain the value of '0sw' as an octal digit
;
; After call: DI points to the last byte read in a command
proc p_op_0sw_rm_imm
    push ax bx dx

    ; AL so far contains 3 bits '0sw' as an octal number.
    inc di ; DI must point to 'mod' before calling 
           ; the decode procedure
    mov dl, al
    and dl, 01b
    ; now DL has information (w=0)/(w=1)
    ; which is expected by the decode procedure
    call p_decode_rm
    ; after the procedure, CL contains how many bytes
    ; the offset took (if any)

    mov si, 2
    m_putsf ', '
    
    ; TODO wrap this in macro/proc
    ; point DI to the last byte read
    inc di 
    inc di ; DI points to r/m now

    cmp cl, 0
    je si_in_right_place_L0 ; offset was not used

    ; offset was used
    ; point DI to the last byte read
    ;
    ; cl contains information about how many 
    ; bytes (=octal digits?) were read as an offset or direct address
    xor ch, ch
    loop_L0:
        inc di
    loop loop_L0

    si_in_right_place_L0:

    ; AL still contains '0sw'
    ; DL still contains (w=0) or (w=1)

    ; check 'w' bit
    cmp dl, 1
    jb imm_1_byte

    ; w = 1
    ; check 's' bit (info about immediate operand)
    cmp al, 2
    jb imm_2_bytes ; so s = 0

    ; w = 1, s = 1
    call p_print_next_byte_sign_extended
    jmp endp_op_0sw_rm_imm

    ; w = 0, s = 0 or 1
    imm_1_byte: ; or in case of 'byte to word sign extended', print the required byte after sign padding byte.
        call p_print_next_byte
        jmp endp_op_0sw_rm_imm

    ; w = 1, s = 0
    imm_2_bytes:
        call p_print_next_word

    endp_op_0sw_rm_imm:
        pop dx bx ax
    ret
endp

; Handles printing "reg, r/m" or "r/m, reg" for the 
; commands of the format:
;   XXXX X0dw mod reg r/m [offset]  
; where each 'X' is one of 0 or 1.
;
; Before call: DI should point to the octal digit for '0dw',
;              AL should contain the value of '0dw' as an octal digit
;
; After call: DI points to the last byte read in a command
; TODO make this proc apply to XXdw, not only X0dw
proc p_op_0dw_reg_rm
    push ax bx dx
    inc di ; DI must point to 'mod' before calling decode procedures

    ; AL so far contains 3 bits '0dw' as an octal number.
    ; check 'd' (destination) bit
    cmp al, 2
    jb __op_rm_reg  ; so d = 0

    ; d = 1
    __op_reg_rm:
        ; AL currently contains either 2 (w=0) or 3 (w=1)
        ; By subtracting 2 from AL, AL will contain either 0 (w=0) or 1 (w=1)
        ; This information will be used by the decode procedures
        sub al, 2
        ; place (w=0)/(w=1) information in DL,
        ; which is expected by decode procedures
        mov dl, al
        ; decode which register is used in place of 'reg'
        call p_decode_reg
        mov si, 2
        m_putsf ', '
        ; decode what should be used in place of 'r/m'
        call p_decode_rm
        jmp move_index

    ; d = 0
    __op_rm_reg:
        ; AL currently contains either 0 (w=0) or 1 (w=1)
        ; This information will be used by the decode procedures
        ;
        ; place (w=0)/(w=1) information in DL,
        ; which is expected by decode procedures
        mov dl, al
        call p_decode_rm
        mov si, 2
        m_putsf ', '
        call p_decode_reg

    ; move DI to the last byte read
    move_index:
        ; point DI to 'r/m'
        inc di
        inc di

        cmp cl, 0
        je si_in_right_place_L1 ; offset was not used

        ; offset was used
        ; point DI to the last byte read
        ;
        ; cl contains information about how many 
        ; bytes were read as an offset or direct address
        xor ch, ch
        loop_L1:
            inc di
        loop loop_L1

    si_in_right_place_L1:
    pop dx bx ax
    ret
endp

convert_number proc
    ; Convert given bytes as a number in hex
    ; CL is how many bytes the number has
    ; AX is number to be converted
    MOV     BX, cur_arg_buff
    MOV     DX, offset v_arg_index
    MOV     needs_convert, BX
    MOV     counter_convert, DX
    CALL    number_to_ascii
    RET
convert_number endp

clear_temp_bytes proc
    ;Clear the read bytes buffer
    PUSH    AX
    PUSH    BX
    PUSH    CX
    PUSH    DX

    MOV     CX, 0
    MOV     temp_b_index, CX
    MOV     DX, " "
    MOV     BX, offset temp_bytes

    CLEAR_LOOP:
        MOV     [BX], DX
        ADD     BX, 1
        CMP     CX, 24
        JE      EXIT_CLEAR
        INC     CX
        JMP CLEAR_LOOP

    EXIT_CLEAR:
        POP     DX
        POP     CX
        POP     BX
        POP     AX

        RET
clear_temp_bytes endp

write_proc proc
    ; Writes analyzed command and resets all buffers for further work
    PUSH    AX
    PUSH    BX
    PUSH    CX
    PUSH    DX

    ; IP COUNTER PRINT
    MOV     ip_arr_index, 0

    MOV     BX, offset ip_value
    MOV     AX, offset ip_arr_index
    MOV     needs_convert, BX           ; The converted value buffer (Adress)
    MOV     counter_convert, AX         ; The index for output buffer (Adress)
    MOV     CL, 4                       ; Convert 4 bytes
    MOV     AX, ip_index                ; The value that needs convertsion

    CALL    number_to_ascii             ; Convert value

    MOV     AX, temp_ip_add
    ADD     ip_index, AX                ; Add accumulated ip value
    MOV     temp_ip_add, 0              ; Zero out the acuumulation

    mov si, 4
    m_printf ip_value                   ; Write 4 bytes (of IP) to file

; -------------------------------------------------------
    m_putfchar ':'
; -------------------------------------------------------
    ;MOV     DX, offset doublepoint  ; Write ":"

     ;;DX is buffer for symbol writing
     ;;RESULT: updated output file
    ;MOV     CX, 1                   ; Write result
    ;MOV     BX, out_handle
    ;MOV     AX, 4000h               ; Write
    ;INT     21h
; -------------------------------------------------------

    mov si, 4
    m_putsf '    '
    ;IP COUNTER PRINT END

    ;BYTE PRINT
    MOV     si, 25                  ; Write result
    m_printf temp_bytes

    CALL    clear_temp_bytes
    ;BYTE PRINT END

    m_printf_nl

    POP     DX
    POP     CX
    POP     BX
    POP     AX

    RET


check_read proc
    ; Check if input needs to be replenished
    ; RESULT: nothing or replenished input_buff
    PUSH    AX
    MOV     AX, [bytes_read]
    CMP     AX, 0
    JLE     READ
    POP     AX
    RET

    READ:
        CALL    read_input
        POP     AX
        RET
check_read endp

read_input proc
    ; Updates bytes_read with new value. Does not change registers
    PUSH    AX
    PUSH    BX
    PUSH    CX
    PUSH    DX

    MOV     BX, in_handle
    MOV     CX, C_BUFFSIZE                 ; Read 255 bytes
    MOV     DX, OFFSET input_buff
    MOV     AX, 3F00h
    INT     21h

    MOV     [bytes_read], AX
    MOV     AX, 0
    MOV     [temp_index], AX

    POP     DX
    POP     CX
    POP     BX
    POP     AX

    CALL    check_carry             ; Check if successful

    RET
read_input endp

store_next_byte proc
    ; Reads and stores next byte in DL
    PUSH    AX
    PUSH    BX
    PUSH    CX

    CALL    check_read              ; Check if input needs repleneshing has to be read

    MOV     BX, temp_index          ; Set bx value to temp_index
    MOV     DH, 0                   ; Set DH to 0
    MOV     DL, [input_buff + BX]   ; Get from input buffer

    INC     BX
    MOV     [temp_index], BX        ; Increase temp_index

    MOV     CX, [bytes_read]        ; Decrease the bytes_read
    DEC     CX
    MOV     [bytes_read], CX

    ; Store read byte in temp_bytes for printing
    MOV     BX, offset temp_bytes
    MOV     AX, offset temp_b_index
    MOV     needs_convert, BX
    MOV     counter_convert, AX
    MOV     CL, 2
    MOV     AH, 0
    MOV     AL, DL

    CALL    number_to_ascii
    INC     temp_b_index        ; Increase the temp_b_index (to add a space between them)

    INC     temp_ip_add         ; Increase IP

    ; Store read byte in data_octal,
    ; where my DISASM could read it

    ; Stores xx, yyy, zzz in appropriate variables from adress byte
    ; RESULT: [data_octal+di] = xx, yyy, zzz
    PUSH    AX
    PUSH    BX
    PUSH    CX
    PUSH    DX

    PUSH    DX
    MOV     BX, 11000000b
    AND     DX, BX

    ROR     DX, 6           ; Shift right 6 times

    MOV byte ptr [data_octal+di+1], DL ; nes DI yra 0FFFFh

    POP     DX

    PUSH    DX
    MOV     BX, 00111000b
    AND     DX, BX

    ROR     DX, 3           ; Shift right 3 times

    MOV byte ptr [data_octal+di+2], DL
    POP     DX

    MOV     BX, 00000111b
    AND     DX, BX

    MOV byte ptr [data_octal+di+3], DL

    POP     DX
    POP     CX
    POP     BX
    POP     AX

    POP     CX
    POP     BX
    POP     AX

    RET
store_next_byte endp

number_to_ascii proc
    ; Convert read byte to ascii and store in temp_bytes
    ; CL is number of chars
    ; AX is adress to value
    ; BX is adress to buffer of writing
    ; RESULT: [BX] <- ASCII number

    PUSH    AX
    PUSH    BX
    PUSH    CX
    PUSH    DX

    MOV     CH, CL

    CONV:
        CMP     CL, 0
        JE      FIN_CONV
        DEC     CL

        MOV     BX, 16
        MOV     DX, 0
        DIV     BX

        MOV     BX, offset hex
        ADD     BX, DX
        MOV     DL, [BX]
        MOV     DH, 0

        PUSH    DX
    JMP CONV

    FIN_CONV:
        CMP     CH, 0
        JE      FIN
        DEC     CH

        POP     DX

        MOV     BX, needs_convert
        MOV     SI, counter_convert ;siac

        ADD     BX, [SI] ;siac
        MOV     [BX], DL

        INC     word ptr [SI] ;siac
    JMP FIN_CONV

    FIN:
        POP     DX
        POP     CX
        POP     BX
        POP     AX

        RET
number_to_ascii endp

check_carry proc
    JC      STOP_PROGRAM            ; If carry flag is set, stop
    RET                             ; Else ret

    STOP_PROGRAM:
        MOV     DX, offset cant_open    ; Output cant open msg
        MOV     AH, 09
        INT     21h

        MOV     DX, OFFSET arg_msg      ; Output "/?"
        MOV     AH, 09
        INT     21h

        MOV     AX, 4C00H               ; Terminate
        INT     21h
check_carry endp

; -----------------------------------------------------------/

start:
    mov ax, @data                  ; move @data to AX (@data - address where data segment starts)
    mov ds, ax                     ; move AX (@data) to DS (data segment)
    MOV     DI, 0FFFFh
    ; FIXME will it work?
    ;mov es, ax                     ; move AX (@data) to ES (extended data segment) 

    MOV     SI, 0081h               ; Set start of arguments ;siac
    MOV     BX, 0                   ; Count index of argument file name
    MOV     CX, -1                  ; Count amount of arguments

ARG_PARSE:
    MOV     AL, byte ptr ES:[SI]    ; Store next char of arguments ;siac

    CMP     AL, 13                  ; End of arguments (newline)
    JE      CHECK_ERRORS            ; Check if right amount of arguments

    CMP     AL, ' '                 ; End of this argument, skip space
    JE      SKIP_SPACE

    CMP     AL, '/'                 ; Check if "/?" is trying to be written
    JE      ERR_TEST

    add si, 1 ;siac                 ; Store this character to appropriate array
    JMP     WRITE

ERR_TEST:
    add si, 1 ;siac
    MOV     AL, byte ptr ES:[SI]    ; Check if next byte is '?' ;siac
    CMP     AL, '?'
    JNE     WRITE_INIT              ; If no, continue with write
ERROR:
    MOV     DX, OFFSET err_msg      ; Error message
    MOV     AH, 09
    INT     21h

    MOV     AX, 4C00H
    INT     21h

NO_OPEN:                            ; Cant open/write file
    MOV     DX, offset cant_open
    MOV     AH, 09
    INT     21h
JMP ERROR

WRITE_INIT:                         ; Fix before write (After ERR_TEST)
    sub si, 1 ;siac
    MOV     AL, byte ptr ES:[SI] ;siac
    add si, 1 ;siac
JMP WRITE

SKIP_SPACE:                         ; Skips the space, shifts to next array
    add si, 1 ;siac
    MOV     AL, byte ptr ES:[SI] ;siac
    CMP     AL, ' '                 ; Skip all the spaces
    JE      SKIP_SPACE
    INC     CX                      ; Sets which array should be used
    MOV     BX, 0                   ; Index of new array is 0
JMP ARG_PARSE

CHECK_ERRORS:                       ; Only continue if there are 3 arguments
    CMP     CX, 1
    JE      CONTINUE

    CMP     CX, 2                   ; In case there is a space before newline
    JE      CONTINUE
JMP ERROR

WRITE:                              ; Write to appropriate parameter
    CMP     CX, 0
    JE      FIRST_PARAM

    CMP     CX, 1
    JE      SECOND_PARAM
JMP ERROR                           ; If there are more arguments, error

FIRST_PARAM:
    MOV     [file_n+BX], AL         ; First param
    INC     BX
JMP ARG_PARSE

SECOND_PARAM:
    MOV     [output_n+BX], AL           ; Second param
    INC     BX
JMP ARG_PARSE

CONTINUE:
    INT 3h                          ; Debug
    CLC                             ; Clear carry

    MOV     DX, OFFSET file_n       ; Atidaryti  input faila
    MOV     AX, 3D00h
    INT     21h

    CALL    check_carry             ; Check if successful

    MOV     in_handle, AX               ; Store handle

    MOV     CX, 0
    MOV     DX, OFFSET output_n     ; Open output file
    MOV     AX, 3C00h
    INT     21h

    CALL    check_carry             ; check if success

    MOV     out_handle, AX      ; save handle

    MOV     AX, 0
    MOV     BX, 0
    MOV     CX, 0
    MOV     DX, 0

PARSE:                              ; The whole algorithm
    ;xor di, di
    CALL    check_read              ; Check if new input has to be read
    MOV     CX, [bytes_read]
    CMP     CX, 0                   ; Check if any bytes left in file
    JLE     EXIT                    ; nothing left in file, quit

    CALL    store_next_byte         ; Get the next byte for opc

    PUSH SI
    call main
    POP SI
    ;m_putsf '     ;'
    call write_proc
JMP PARSE

EXIT:
    MOV     AX, 0
TERMINATE:
    MOV     AH, 4Ch
    INT     21h

; ############################################################################

main proc
_xxx:
    xor dh, dh

_after_clean_dh_xxx:
    ; get 1st octal digit
    inc di
    xor ax, ax
    mov al, byte ptr [data_octal+di]

    cmp al, 0FFh
    je exit_program

    cmp al, 3
    je _3xx
    ja short undefined_1st_octal

    cmp al, 1
    jb _0xx
    je _1xx
    jmp _2xx

    ;jmp short _xxx

undefined_byte:
    inc di
    inc di
    inc di
    jmp short undefined

undefined_1st_octal:
    inc di
    inc di
    jmp short undefined

undefined_2nd_octal:
    inc di
    jmp short undefined

undefined:
    mov si, 10
    m_putsf '; UNDEFINED'
    ;jmp short _xxx
    ret

exit_program:
    m_exit0

; ============================================================
;  _0XX
; ============================================================
_0xx:
    ; get 2nd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

    ; get 3rd octal digit
    inc di ; SI now also points to 3rd octal
    mov bl, byte ptr [data_octal+di]

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
    jb short __00_0123_add_reg_rm
    je _004_add_acc_imm_byte
    jmp _005_add_acc_imm_word

; ------------------------------------------------------------
__00_0123_add_reg_rm:
    mov si, 4
    m_putsf 'ADD '
    call p_op_0dw_reg_rm
    ret ; jmp _xxx

; -------------------------------------------------------------
_004_add_acc_imm_byte:
    mov si, 8
    m_putsf 'ADD AL, '
    call p_print_next_byte
    ret ; jmp _xxx

; -------------------------------------------------------------
_005_add_acc_imm_word:
    mov si, 8
    m_putsf 'ADD AX, '
    call p_print_next_word
    ret ; jmp _xxx

; ------------------------------------------------------------
;  _01X
; ------------------------------------------------------------
_01x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 4
    jb short __01_0123_or_reg_rm
    je _014_or_acc_imm_byte
    jmp _015_or_acc_imm_word

; ------------------------------------------------------------
__01_0123_or_reg_rm:
    mov si, 3
    m_putsf 'OR '
    call p_op_0dw_reg_rm
    ret ; jmp _xxx

; -------------------------------------------------------------
_014_or_acc_imm_byte:
    mov si, 7
    m_putsf 'OR AL, '
    call p_print_next_byte
    ret ; jmp _xxx

; -------------------------------------------------------------
_015_or_acc_imm_word:
    mov si, 7
    m_putsf 'OR AX, '
    call p_print_next_word
    ret ; jmp _xxx

; ------------------------------------------------------------
;  _02X
; ------------------------------------------------------------
_02x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 4
    jb short __02_0123_adc_reg_rm
    je _024_adc_acc_imm_byte
    jmp _025_adc_acc_imm_word
    
; ------------------------------------------------------------
__02_0123_adc_reg_rm:
    mov si, 4
    m_putsf 'ADC '
    call p_op_0dw_reg_rm
    ret ; jmp _xxx

; ------------------------------------------------------------
_024_adc_acc_imm_byte:
    mov si, 8
    m_putsf 'ADC AL, '
    call p_print_next_byte
    ret ; jmp _xxx

; -------------------------------------------------------------
_025_adc_acc_imm_word:
    mov si, 8
    m_putsf 'ADC AX, '
    call p_print_next_word
    ret ; jmp _xxx

; ------------------------------------------------------------
;  _03X
; ------------------------------------------------------------
_03x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 4
    jb short __03_0123_sbb_reg_rm
    je _034_sbb_acc_imm_byte
    jmp _035_sbb_acc_imm_word

; ------------------------------------------------------------
__03_0123_sbb_reg_rm:
    mov si, 4
    m_putsf 'SBB '
    call p_op_0dw_reg_rm
    ret ; jmp _xxx

; -------------------------------------------------------------
_034_sbb_acc_imm_byte:
    mov si, 8
    m_putsf 'SBB AL, '
    call p_print_next_byte
    ret ; jmp _xxx

; -------------------------------------------------------------
_035_sbb_acc_imm_word:
    mov si, 8
    m_putsf 'SBB AX, '
    call p_print_next_word
    ret ; jmp _xxx

; ------------------------------------------------------------
;  _04X
; ------------------------------------------------------------
_04x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 4
    jb short __04_0123_and_reg_rm
    je _044_and_acc_imm_byte
    jmp _045_and_acc_imm_word

; ------------------------------------------------------------
__04_0123_and_reg_rm:
    mov si, 4
    m_putsf 'AND '
    call p_op_0dw_reg_rm
    ret ; jmp _xxx

; -------------------------------------------------------------
_044_and_acc_imm_byte:
    mov si, 8
    m_putsf 'AND AL, '
    call p_print_next_byte
    ret ; jmp _xxx

; -------------------------------------------------------------
_045_and_acc_imm_word:
    mov si, 8
    m_putsf 'AND AX, '
    call p_print_next_word
    ret ; jmp _xxx

; ------------------------------------------------------------
;  _05X
; ------------------------------------------------------------
_05x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 4
    jb short __05_0123_sub_reg_rm
    je _054_sub_acc_imm_byte
    jmp _055_sub_acc_imm_word

; ------------------------------------------------------------
__05_0123_sub_reg_rm:
    mov si, 4
    m_putsf 'SUB '
    call p_op_0dw_reg_rm
    ret ; jmp _xxx

; -------------------------------------------------------------
_054_sub_acc_imm_byte:
    mov si, 8
    m_putsf 'SUB AL, '
    call p_print_next_byte
    ret ; jmp _xxx

; -------------------------------------------------------------
_055_sub_acc_imm_word:
    mov si, 8
    m_putsf 'SUB AX, '
    call p_print_next_word
    ret ; jmp _xxx

; ------------------------------------------------------------
;  _06X
; ------------------------------------------------------------
_06x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 4
    jb short __06_0123_xor_reg_rm
    je _064_xor_acc_imm_byte
    jmp _065_xor_acc_imm_word

; ------------------------------------------------------------
__06_0123_xor_reg_rm:
    mov si, 4
    m_putsf 'XOR '
    call p_op_0dw_reg_rm
    ret ; jmp _xxx

; -------------------------------------------------------------
_064_xor_acc_imm_byte:
    mov si, 8
    m_putsf 'XOR AL, '
    call p_print_next_byte
    ret ; jmp _xxx

; -------------------------------------------------------------
_065_xor_acc_imm_word:
    mov si, 8
    m_putsf 'XOR AX, '
    call p_print_next_word
    ret ; jmp _xxx

; ------------------------------------------------------------
;  _07X
; ------------------------------------------------------------
_07x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 4
    jb short __07_0123_cmp_reg_rm
    je _074_cmp_acc_imm_byte
    jmp _075_cmp_acc_imm_word

; ------------------------------------------------------------
__07_0123_cmp_reg_rm:
    mov si, 4
    m_putsf 'CMP '
    call p_op_0dw_reg_rm
    ret ; jmp _xxx

; -------------------------------------------------------------
_074_cmp_acc_imm_byte:
    mov si, 8
    m_putsf 'CMP AL, '
    call p_print_next_byte
    ret ; jmp _xxx

; -------------------------------------------------------------
_075_cmp_acc_imm_word:
    mov si, 8
    m_putsf 'CMP AX, '
    call p_print_next_word
    ret ; jmp _xxx

; ------------------------------------------------------------
;  _0X6
; ------------------------------------------------------------
_0x6_push_seg:
    ; 2nd octal digit is already in AL
    ; it is one of {0,1,2,3}
    ;
    ; SI already points to the last octal digit read

    mov si, 5
    m_putsf 'PUSH '

    mov bl, al 
    shl bl, 1 ; times 2
    m_print_reg SR16

    ret ; jmp _xxx

; TODO
_0x6_seg_change_prefix:
    ; 2nd octal digit is already in AL
    ; AL is one of {4,5,6,7}
    ;
    ; SI already points to the last octal digit read
    sub al, 3
    mov dh, al

    jmp _after_clean_dh_xxx

; ------------------------------------------------------------
;  _0X7
; ------------------------------------------------------------
_0x7_pop_seg:
    ; 2nd octal digit is already in AL
    ; it is one of {0,1,2,3}

    mov si, 4
    m_putsf 'POP '

    mov bl, al 
    shl bl, 1 ; times 2
    m_print_reg SR16

    ret ; jmp _xxx

; -------------------------------------------------------------
_047_add_sub_adjust:
    mov si, 3
    m_putsf 'DAA'
    ret ; jmp _xxx

; -------------------------------------------------------------
_057_add_sub_adjust:
    mov si, 3
    m_putsf 'DAS'
    ret ; jmp _xxx

; -------------------------------------------------------------
_067_add_sub_adjust:
    mov si, 3
    m_putsf 'AAA'
    ret ; jmp _xxx

; -------------------------------------------------------------
_077_add_sub_adjust:
    mov si, 3
    m_putsf 'AAS'
    ret ; jmp _xxx

; ============================================================
;  _1XX
; ============================================================
_1xx:
    ; get 2nd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

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

; ------------------------------------------------------------
;  _10X
; ------------------------------------------------------------
_10x_inc_reg_word:
    ; get 3rd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

    cmp al, 7
    ja undefined

    mov si, 4
    m_putsf 'INC '

    mov bl, al
    shl bl, 1 ; times 2
    m_print_reg Rw

    ret ; jmp _xxx

; ------------------------------------------------------------
;  _11X
; ------------------------------------------------------------
_11_dec_reg_word:
    ; get 3rd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

    cmp al, 7
    ja undefined

    mov si, 4
    m_putsf 'DEC '

    mov bl, al
    shl bl, 1; times 2. bl = 4
    m_print_reg Rw

    ret ; jmp _xxx

; ------------------------------------------------------------
;  _12X
; ------------------------------------------------------------
_12_push_reg_word:
    ; get 3rd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

    cmp al, 7
    ja undefined

    mov si, 5
    m_putsf 'PUSH '

    mov bl, al
    shl bl, 1; times 2. bl = 4
    m_print_reg Rw

    ret ; jmp _xxx

; ------------------------------------------------------------
;  _13X
; ------------------------------------------------------------
_13_pop_reg_word:
    ; get 3rd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

    cmp al, 7
    ja undefined

    mov si, 4
    m_putsf 'POP '

    mov bl, al
    shl bl, 1; times 2. bl = 4
    m_print_reg Rw

    ret ; jmp _xxx

; ------------------------------------------------------------
;  _16X
; ------------------------------------------------------------
_16x:
    ; get 3rd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

    inc di ; SI now points to the first 
           ; octal digit of the offset

    cmp al, 7
    je _167_ja_near
    ja undefined

    cmp al, 3
    jb short __16_012
    je _163_jae_near
    jmp short __16_456

    __16_012:
        cmp al, 1
        jb short _160_jo_near
        je _161_jno_near
        jmp _162_jb_near

    __16_456:
        cmp al, 5
        jb _164_je_near
        je _165_jne_near
        jmp _166_jbe_near

; ------------------------------------------------------------
_160_jo_near:
    mov si, 3
    m_putsf 'JO '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; ------------------------------------------------------------
_161_jno_near:
    mov si, 4
    m_putsf 'JNO '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; ------------------------------------------------------------
_162_jb_near:
    mov si, 3
    m_putsf 'JB '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; ------------------------------------------------------------
_163_jae_near:
    mov si, 4
    m_putsf 'JAE '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; ------------------------------------------------------------
_164_je_near:
    mov si, 3
    m_putsf 'JE '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; ------------------------------------------------------------
_165_jne_near:
    mov si, 4
    m_putsf 'JNE '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; ------------------------------------------------------------
_166_jbe_near:
    mov si, 4
    m_putsf 'JBE '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; ------------------------------------------------------------
_167_ja_near:
    mov si, 3
    m_putsf 'JA '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; ------------------------------------------------------------
;  _17X
; ------------------------------------------------------------
_17x:
    ; get 3rd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

    inc di ; SI now points to the first 
           ; octal digit of the offset

    cmp al, 7
    je _177_jg_near
    ja undefined

    cmp al, 3
    jb short __17_012
    je _173_jnp_near
    jmp short __17_456

    __17_012:
        cmp al, 1
        jb short _170_js_near
        je _171_jns_near
        jmp _172_jp_near

    __17_456:
        cmp al, 5
        jb _174_jl_near
        je _175_jge_near
        jmp _176_jle_near

; ------------------------------------------------------------
_170_js_near:
    mov si, 3
    m_putsf 'JS '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; ------------------------------------------------------------
_171_jns_near:
    mov si, 4
    m_putsf 'JNS '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; ------------------------------------------------------------
_172_jp_near:
    mov si, 3
    m_putsf 'JP '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; ------------------------------------------------------------
_173_jnp_near:
    mov si, 4
    m_putsf 'JNP '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; ------------------------------------------------------------
_174_jl_near:
    mov si, 3
    m_putsf 'JL '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; ------------------------------------------------------------
_175_jge_near:
    mov si, 4
    m_putsf 'JGE '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; ------------------------------------------------------------
_176_jle_near:
    mov si, 4
    m_putsf 'JLE '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; ------------------------------------------------------------
_177_jg_near:
    mov si, 3
    m_putsf 'JG '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; ============================================================
;  _2XX
; ============================================================
_2xx:
    ; get 2nd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

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
    inc di
    mov al, byte ptr [data_octal+di]

    cmp al, 7
    ja undefined

    cmp al, 4
    jb short __20_0123

    cmp al, 6
    jb _20_45_test_reg_rm
    jmp _20_67_xchg_reg_rm

    __20_0123:
        inc di ; point to 'mod'
        inc di ; point SI to next octal digit after 'mod'
        mov bl, byte ptr [data_octal+di]
        dec di
        dec di ; return SI back
        ; find out which operation is used
        cmp bl, 4
        jb short __20_0123_mod_0123
        je _20_0123_and_rm_imm

        cmp bl, 6
        jb _20_0123_sub_rm_imm
        je _20_0123_xor_rm_imm
        jmp _20_0123_cmp_rm_imm

    __20_0123_mod_0123:
        cmp bl, 2
        jb short __20_0123_mod_01
        je _20_0123_adc_rm_imm
        jmp _20_0123_sbb_rm_imm

    __20_0123_mod_01:
        cmp bl, 1
        jb _20_0123_add_rm_imm
        jmp _20_0123_or_rm_imm

; ------------------------------------------------------------
_20_0123_add_rm_imm:
    mov si, 4
    m_putsf 'ADD '
    call p_op_0sw_rm_imm
    ret ; jmp _xxx

; ------------------------------------------------------------
_20_0123_or_rm_imm:
    mov si, 3
    m_putsf 'OR '
    call p_op_0sw_rm_imm
    ret ; jmp _xxx

; ------------------------------------------------------------
_20_0123_adc_rm_imm:
    mov si, 4
    m_putsf 'ADC '
    call p_op_0sw_rm_imm
    ret ; jmp _xxx

; ------------------------------------------------------------
_20_0123_sbb_rm_imm:
    mov si, 4
    m_putsf 'SBB '
    call p_op_0sw_rm_imm
    ret ; jmp _xxx

; ------------------------------------------------------------
_20_0123_and_rm_imm:
    mov si, 4
    m_putsf 'AND '
    call p_op_0sw_rm_imm
    ret ; jmp _xxx

; ------------------------------------------------------------
_20_0123_sub_rm_imm:
    mov si, 4
    m_putsf 'SUB '
    call p_op_0sw_rm_imm
    ret ; jmp _xxx

; ------------------------------------------------------------
_20_0123_xor_rm_imm:
    mov si, 4
    m_putsf 'XOR '
    call p_op_0sw_rm_imm
    ret ; jmp _xxx

; ------------------------------------------------------------
_20_0123_cmp_rm_imm:
    mov si, 4
    m_putsf 'CMP '
    call p_op_0sw_rm_imm
    ret ; jmp _xxx

; ------------------------------------------------------------
_20_45_test_reg_rm:
    mov si, 5
    m_putsf 'TEST '
    ; AL contains '10w'
    mov dl, al
    and dl, 001b ; will be used for decode procedures

    inc di ; si points to 'mod' now

    call p_decode_reg
    mov si, 2
    m_putsf ', '
    call p_decode_rm

    ; point SI to 'r/m'
    inc di
    inc di

    cmp cl, 0
    je si_in_right_place_L2 ; offset was not used

    ; offset was used
    ; point SI to the last byte read
    ;
    ; cl contains information about how many 
    ; bytes were read as an offset or direct address
    xor ch, ch
    loop_L2:
        inc di
    loop loop_L2

    si_in_right_place_L2:
    ret ; jmp _xxx

; ------------------------------------------------------------
_20_67_xchg_reg_rm:
    mov si, 5
    m_putsf 'XCHG '
    ; AL contains '11w'
    mov dl, al
    and dl, 001b ; will be used for decode procedures

    inc di ; si points to 'mod' now

    call p_decode_reg
    mov si, 2
    m_putsf ', '
    call p_decode_rm

    ; point SI to 'r/m'
    inc di
    inc di

    cmp cl, 0
    je si_in_right_place_L3 ; offset was not used

    ; offset was used
    ; point SI to the last byte read
    ;
    ; cl contains information about how many 
    ; bytes were read as an offset or direct address
    xor ch, ch
    loop_L3:
        inc di
    loop loop_L3

    si_in_right_place_L3:
    ret ; jmp _xxx

; ------------------------------------------------------------
;  _21X
; ------------------------------------------------------------
_21x:
    ; get 3rd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

    cmp al, 7
    je short __217_mod_xxx
    ja undefined

    cmp al, 4
    jb short __21_0123_mov_reg_rm

    cmp al, 5
    je _215_lea_reg_mem
    jmp _21_46_mov_rm_segreg

    __217_mod_xxx:
        inc di ; point to 'mod'
        inc di ; point SI to next octal digit after 'mod'
        mov bl, byte ptr [data_octal+di]
        dec di
        dec di ; return SI back
        ; find out if it's a legit opcode
        cmp bl, 6
        je _217_pop_rm
        jmp undefined_byte

; ------------------------------------------------------------
__21_0123_mov_reg_rm:
    mov si, 4
    m_putsf 'MOV '
    call p_op_0dw_reg_rm
    ret ; jmp _xxx

; ------------------------------------------------------------
_21_46_mov_rm_segreg:
    ; check if 'reg' is not '1xx'
    call store_next_byte
    inc di ; point to 'mod'
    inc di ; point to 'reg'
    mov bl, byte ptr [data_octal+di]
    dec di 
    dec di ; return SI back
    ; find out if it's a legit opcode
    cmp bl, 4 ; reg cannot be '1xx'
    jae undefined_byte

    ; AL containts '1d0'
    cmp al, 6
    je _216_mov_segreg_rm

    _214_mov_rm_segreg:
        mov si, 4
        m_putsf 'MOV '

        mov al, 001
        m_before_decode ; it will put '001' from AL to DL,
                        ; which is what is needed.
                        ; this will tell the decode procedure
                        ; that the operand is a word
        call p_decode_rm
        m_move_index

        mov si, 2
        m_putsf ', '

        ; BL still contains 'reg', which is '0sr'
        shl bl, 1 ; times 2
        m_print_reg SR16

        ret ; jmp _xxx

    _216_mov_segreg_rm:
        ; BL still contains 'reg', which is '0sr'
        ; if 'd'=1, which is the case here, 0sr cannot be
        ; 001, since it cannot be written to 'CS'
        cmp bl, 1 
        je undefined_byte

        mov si, 4
        m_putsf 'MOV '

        shl bl, 1 ; times 2
        m_print_reg SR16

        mov si, 2
        m_putsf ', '

        mov al, 001
        m_before_decode ; it will put '001' from AL to DL,
                        ; which is what is needed.
                        ; this will tell the decode procedure
                        ; that the operand is a word
        call p_decode_rm
        m_move_index

        ret ; jmp _xxx

; ------------------------------------------------------------
_215_lea_reg_mem:
    ; check if mod is not '11'
    inc di ; point to 'mod'
    mov bl, byte ptr [data_octal+di]
    dec di ; return SI back
    ; find out if it's a legit opcode
    cmp bl, 3 ; mod cannot be '11'
    je undefined_byte

    mov si, 4
    m_putsf 'LEA '

    ; AL contains '101'
    MOV AL, 001 ; tell the decode procedures that
                ; the operand will be a word

    m_before_decode ; it will put '001' in DL,
                    ; which is what is needed.
    call p_decode_reg
    mov si, 3
    m_putsf ', d' ; 'd' is for 'dword', since the next 
                 ; operand  must be memory ('word ptr ...')
    call p_decode_rm
    m_move_index

    ret ; jmp _xxx

; ------------------------------------------------------------
_217_pop_rm:
    mov si, 4
    m_putsf 'POP '
    ; AL contains '111'
    m_before_decode ; it will put '001' in DL,
                    ; which is what is needed.
                    ; this will tell the decode procedure
                    ; that the operand is a word
    call p_decode_rm
    m_move_index

    ret ; jmp _xxx

; ------------------------------------------------------------
;  _22X
; ------------------------------------------------------------
_22x_xchg_reg_ax:
    ; get 3rd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

    cmp al, 7
    ja undefined

    cmp al, 0
    je _22x_nop

    mov si, 5
    m_putsf 'XCHG '

    mov bl, al
    shl bl, 1; times 2. bl = 4
    m_print_reg Rw

    mov si, 4
    m_putsf ', AX'
    ret ; jmp _xxx

    _22x_nop:
        mov si, 3
        m_putsf 'NOP'
        ret ; jmp _xxx

; ------------------------------------------------------------
;  _23X
; ------------------------------------------------------------
_23x:
    ; get 3rd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

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
        je _232_call_far_absolute_direct
        jmp _233_wait

    __23_01:
        cmp al, 1
        jb short _230_cbw
        jmp _231_cwd

; -------------------------------------------------------------
_230_cbw:
    mov si, 3
    m_putsf 'CBW'
    ret ; jmp _xxx

; -------------------------------------------------------------
_231_cwd:
    mov si, 3
    m_putsf 'CWD'
    ret ; jmp _xxx

; ------------------------------------------------------------
_232_call_far_absolute_direct:
    mov si, 5
    m_putsf 'CALL '
    add di, 6 ; first print the second word
    call p_print_next_word
    m_putfchar '+'
    sub di, 12 ; then print the first word
    call p_print_next_word
    add di, 6 ; move SI to the last byte read

    ret ; jmp _xxx

; -------------------------------------------------------------
_233_wait:
    mov si, 4
    m_putsf 'WAIT'
    ret ; jmp _xxx

; -------------------------------------------------------------
_234_pushf:
    mov si, 5
    m_putsf 'PUSHF'
    ret ; jmp _xxx

; -------------------------------------------------------------
_235_popf:
    mov si, 4
    m_putsf 'POPF'
    ret ; jmp _xxx

; -------------------------------------------------------------
_236_sahf:
    mov si, 4
    m_putsf 'SAHF'
    ret ; jmp _xxx

; -------------------------------------------------------------
_237_lahf:
    mov si, 4
    m_putsf 'LAHF'
    ret ; jmp _xxx

; ------------------------------------------------------------
;  _24X
; ------------------------------------------------------------
_24x:
    ; get 3rd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

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

; -------------------------------------------------------------
_240_mov_acc_mem_byte:
    mov si, 17
    m_putsf 'MOV AL, byte ptr '
    m_print_sr_prefix_default
    m_putfchar '['

    call p_print_next_word
    m_putfchar ']'
    ret ; jmp _xxx

; -------------------------------------------------------------
_241_mov_acc_mem_word:
    mov si, 17
    m_putsf 'MOV AX, word ptr '
    m_print_sr_prefix_default
    m_putfchar '['

    call p_print_next_word
    m_putfchar ']'
    ret ; jmp _xxx

; -------------------------------------------------------------
_242_mov_mem_acc_byte:
    mov si, 13
    m_putsf 'MOV byte ptr '
    m_print_sr_prefix_default
    m_putfchar '['

    call p_print_next_word
    mov si, 5
    m_putsf '], AL'
    ret ; jmp _xxx

; -------------------------------------------------------------
_243_mov_mem_acc_word:
    mov si, 13
    m_putsf 'MOV word ptr '
    m_print_sr_prefix_default
    m_putfchar '['

    call p_print_next_word
    mov si, 5
    m_putsf '], AX'
    ret ; jmp _xxx

; -------------------------------------------------------------
_244_movsb:
    mov si, 5
    m_putsf 'MOVSB'
    ret ; jmp _xxx

; -------------------------------------------------------------
_245_movsw:
    mov si, 5
    m_putsf 'MOVSW'
    ret ; jmp _xxx

; -------------------------------------------------------------
_246_cmpsb:
    mov si, 5
    m_putsf 'CMPSB'
    ret ; jmp _xxx

; -------------------------------------------------------------
_247_cmpsw:
    mov si, 5
    m_putsf 'CMPSW'
    ret ; jmp _xxx

; ------------------------------------------------------------
;  _25X
; ------------------------------------------------------------
_25x:
    ; get 3rd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

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

; -------------------------------------------------------------
_250_test_acc_imm_byte:
    mov si, 9
    m_putsf 'TEST AL, '
    call p_print_next_byte

    ret ; jmp _xxx

; -------------------------------------------------------------
_251_test_acc_imm_word:
    mov si, 9
    m_putsf 'TEST AX, '
    call p_print_next_word

    ret ; jmp _xxx

; ------------------------------------------------------------
_252_stosb:
    mov si, 5
    m_putsf 'STOSB'
    ret ; jmp _xxx

; ------------------------------------------------------------
_253_stosw:
    mov si, 5
    m_putsf 'STOSW'
    ret ; jmp _xxx

; ------------------------------------------------------------
_254_lodsb:
    mov si, 5
    m_putsf 'LODSB'
    ret ; jmp _xxx

; ------------------------------------------------------------
_255_lodsw:
    mov si, 5
    m_putsf 'LODSW'
    ret ; jmp _xxx

; ------------------------------------------------------------
_256_scasb:
    mov si, 5
    m_putsf 'SCASB'
    ret ; jmp _xxx

; ------------------------------------------------------------
_257_scasw:
    mov si, 5
    m_putsf 'SCASW'
    ret ; jmp _xxx

; ------------------------------------------------------------
;  _26X
; ------------------------------------------------------------
_26x_mov_reg_imm_byte:
    ; get 3rd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

    cmp al, 7
    ja undefined

    mov si, 4
    m_putsf 'MOV '

    mov bl, al
    shl bl, 1 ; times 2
    m_print_reg Rb
    mov si, 2
    m_putsf ', '
    call p_print_next_byte

    ret ; jmp _xxx

; ------------------------------------------------------------
;  _27X
; ------------------------------------------------------------
_27x_mov_reg_imm_word:
    ; get 3rd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

    cmp al, 7
    ja undefined

    mov si, 4
    m_putsf 'MOV '

    mov bl, al
    shl bl, 1; times 2. bl = 4
    m_print_reg Rw
    mov si, 2
    m_putsf ', '
    call p_print_next_word

    ret ; jmp _xxx

; ============================================================
;  _3XX
; ============================================================
_3xx:
    ; get 2nd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

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
    inc di
    mov al, byte ptr [data_octal+di]

    cmp al, 7
    ja undefined

    cmp al, 6
    jb short __30_2345
    jmp _30_67_mov_rm_imm

    __30_2345:
        cmp al, 4
        jb short __30_23

    __30_45_xx:
        inc di ; point to 'mod'
        mov bl, byte ptr [data_octal+di]
        dec di ; return SI back
        ; find out if it's a legit opcode
        cmp bl, 3 ; mod cannot be '11'
        jb short __30_45
        jmp undefined_byte

    __30_45:
        cmp al, 5
        jb _304_les_reg_mem
        jmp _305_lds_reg_mem

    __30_23:
        cmp al, 3
        jb short _302_ret_imm
        jmp _303_ret

; -------------------------------------------------------------
_302_ret_imm:
    mov si, 4
    m_putsf 'RET '
    call p_print_next_word
    ret ; jmp _xxx

; -------------------------------------------------------------
_303_ret:
    mov si, 3
    m_putsf 'RET'
    ret ; jmp _xxx

; ------------------------------------------------------------
_304_les_reg_mem:
    mov si, 4
    m_putsf 'LES '
    ; AL contains '100'
    MOV AL, 001 ; tell the decode procedures that
                ; the operand will be a word
    m_before_decode ; it will put '001' in DL,
                    ; which is what is needed.
    call p_decode_reg
    mov si, 3
    m_putsf ', d' ; 'd' is for 'dword', since the next 
                 ; operand  must be memory ('word ptr ...')
    call p_decode_rm
    m_move_index

    ret ; jmp _xxx

; ------------------------------------------------------------
_305_lds_reg_mem:
    mov si, 4
    m_putsf 'LDS '
    ; AL contains '101'
    MOV AL, 001 ; tell the decode procedures that
                ; the operand will be a word
    m_before_decode ; it will put '001' in DL,
                    ; which is what is needed.
    call p_decode_reg
    mov si, 3
    m_putsf ', d' ; 'd' is for 'dword', since the next 
                 ; operand  must be memory ('word ptr ...')
    call p_decode_rm
    m_move_index

    ret ; jmp _xxx

; ------------------------------------------------------------
_30_67_mov_rm_imm:
    ; TODO check for other code than '000' between mod and r/m !
    mov si, 4
    m_putsf 'MOV '
    ; AL so far contains 3 bits '11w' as an octal number.

    ; keep only the last bit (ie. set 's' to 0)
    and al, 001b

    ; now the following procedure can be called. 
    ; It will think that it is indeed the command
    ; of format '0sw', which is exactly what is needed here.
    ; 's' was set to 0, since there is no 's' bit in test_rm_imm.
    call p_op_0sw_rm_imm
    ret ; jmp _xxx

; ------------------------------------------------------------
;  _31X
; ------------------------------------------------------------
_31x:
    ; get 3rd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

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

; -------------------------------------------------------------
_312_retf_imm:
    mov si, 5
    m_putsf 'RETF '
    call p_print_next_word
    ret ; jmp _xxx

; -------------------------------------------------------------
_313_retf:
    mov si, 4
    m_putsf 'RETF'
    ret ; jmp _xxx

; -------------------------------------------------------------
_314_int3:
    mov si, 5
    m_putsf 'INT 3'
    ret ; jmp _xxx

; -------------------------------------------------------------
_315_int_number:
    mov si, 4
    m_putsf 'INT '
    call p_print_next_byte
    ret ; jmp _xxx

; -------------------------------------------------------------
_316_into:
    mov si, 4
    m_putsf 'INTO'
    ret ; jmp _xxx

; -------------------------------------------------------------
_317_iret:
    mov si, 4
    m_putsf 'IRET'
    ret ; jmp _xxx

; ------------------------------------------------------------
;  _32X
; ------------------------------------------------------------
_32x:
    ; get 3rd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

    cmp al, 7
    ja undefined

    cmp al, 4
    jb short __32_0123

    cmp al, 6
    jb __32_45
    je undefined ; _326
    jmp _327_xlat

    __32_0123:
        inc di ; point to 'mod'
        inc di ; point SI to next octal digit after 'mod'
        mov bl, byte ptr [data_octal+di]
        dec di
        dec di ; return SI back

        ; find out which operation is used
        cmp bl, 2
        jb short __32_0123_mod_01
        je _32_0123_rcl_rm_times

        cmp bl, 5
        jb short __32_0123_mod_34
        je _32_0123_shr_rm_times

        cmp bl, 7
        je _32_0123_sar_rm_times
        jmp undefined_byte; _32X_0123_mod_6

    __32_0123_mod_01:
        cmp bl, 1
        jb _32_0123_rol_times
        jmp _32_0123_ror_times

    __32_0123_mod_34:
        cmp bl, 4
        jb _32_0123_rcr_times
        jmp _32_0123_shl_times

    __32_45:
        ; check if next byte is part of AAM/AAD opcode
        cmp byte ptr [data_octal+di+1], 0
        jne undefined

        cmp byte ptr [data_octal+di+2], 1
        jne undefined

        cmp byte ptr [data_octal+di+3], 2
        jne undefined

        ; if AAM/AAD is recognized, move index to the 
        ; end of next byte, which is part of the opcode
        inc di
        inc di
        inc di

        cmp al, 4
        je _324_aam
        jmp _325_aad

; -------------------------------------------------------------
_32_0123_rol_times:
    mov si, 4
    m_putsf 'ROL '
    jmp _32_0123_fin

; -------------------------------------------------------------
_32_0123_ror_times:
    mov si, 4
    m_putsf 'ROR '
    jmp _32_0123_fin

; -------------------------------------------------------------
_32_0123_rcl_rm_times:
    mov si, 4
    m_putsf 'RCL '
    jmp _32_0123_fin

; -------------------------------------------------------------
_32_0123_rcr_times:
    mov si, 4
    m_putsf 'RCR '
    jmp _32_0123_fin

; -------------------------------------------------------------
_32_0123_shl_times:
    mov si, 4
    m_putsf 'SHL '
    jmp _32_0123_fin

; -------------------------------------------------------------
_32_0123_shr_rm_times:
    mov si, 4
    m_putsf 'SHR '
    jmp _32_0123_fin

; -------------------------------------------------------------
_32_0123_sar_rm_times:
    mov si, 4
    m_putsf 'SAR '
    jmp _32_0123_fin

; -------------------------------------------------------------
_32_0123_fin:
    m_before_decode
    call p_decode_rm
    m_move_index

    mov si, 2
    m_putsf ', '

    ; AL still contains '0vw'
    cmp al, 2
    jb short times_is_1

    times_is_cl:
        mov si, 2
        m_putsf 'CL'
        ret ; jmp _xxx

    times_is_1:
        m_putfchar '1'
        ret ; jmp _xxx

; -------------------------------------------------------------
_324_aam:
    mov si, 3
    m_putsf 'AAM'
    ret ; jmp _xxx

; -------------------------------------------------------------
_325_aad:
    mov si, 3
    m_putsf 'AAD'
    ret ; jmp _xxx

; -------------------------------------------------------------
_327_xlat:
    mov si, 4
    m_putsf 'XLAT'
    ret ; jmp _xxx

; ------------------------------------------------------------
;  _33X
; ------------------------------------------------------------
_33x:
    mov si, 13
    m_putsf '; <ESC code> '

    inc di ; 3rd octal digit of opcode ('xxx')
    inc di ; points to 'mod'
    mov al, byte ptr [data_octal+di]

    cmp al, 3
    je _33x_3

    mov dl, 002 ; preparing for decode proc
                ; si already points to 'mod'
                ;
                ; DL above 1 means no pointer
                ; directive will be printed
    call p_decode_rm
    m_move_index

    jmp short _33x_end

    _33x_3:
        inc di
        inc di ; point to last byte read
        jmp _33x_end

    _33x_end:
    ret ; jmp _xxx

; ------------------------------------------------------------
;  _34X
; ------------------------------------------------------------
_34x:
    ; get 3rd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

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
        je _342_loop_near
        jmp _343_jcxz_near

    __34_01:
        cmp al, 1
        jb short _340_loopne_near
        jmp _341_loope_near

; ------------------------------------------------------------
_340_loopne_near:
    inc di ; SI now points to the first 
           ; octal digit of the offset
    mov si, 7
    m_putsf 'LOOPNE '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; ------------------------------------------------------------
_341_loope_near:
    inc di ; SI now points to the first 
           ; octal digit of the offset
    mov si, 6
    m_putsf 'LOOPE '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; ------------------------------------------------------------
_342_loop_near:
    inc di ; SI now points to the first 
           ; octal digit of the offset
    mov si, 5
    m_putsf 'LOOP '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; ------------------------------------------------------------
_343_jcxz_near:
    inc di ; SI now points to the first 
           ; octal digit of the offset
    mov si, 5
    m_putsf 'JCXZ '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; -------------------------------------------------------------
_344_in_acc_port_direct_byte:
    mov si, 7
    m_putsf 'IN AL, '
    call p_print_next_byte
    ret ; jmp _xxx

; -------------------------------------------------------------
_345_in_acc_port_direct_word:
    mov si, 7
    m_putsf 'IN AX, '
    call p_print_next_byte
    ret ; jmp _xxx

; -------------------------------------------------------------
_346_out_acc_port_direct_byte:
    mov si, 4
    m_putsf 'OUT ' 
    call p_print_next_byte
    mov si, 4
    m_putsf ', AL'
    ret ; jmp _xxx

; -------------------------------------------------------------
_347_out_acc_port_direct_word:
    mov si, 4
    m_putsf 'OUT ' 
    call p_print_next_byte
    mov si, 4
    m_putsf ', AX'
    ret ; jmp _xxx

; ------------------------------------------------------------
;  _35X
; ------------------------------------------------------------
_35x:
    ; get 3rd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

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
        je _352_jmp_far_absolute_direct
        jmp _353_jmp_short_relative

    __35_01:
        cmp al, 1
        jb short _350_call_near_relative
        jmp _351_jmp_near_relative

; ------------------------------------------------------------
_350_call_near_relative:
    inc di ; SI now points to the first 
           ; octal digit of the offset
    mov si, 5
    m_putsf 'CALL '
    m_octal_word_to_number
    m_print_near_offset_word
    ret ; jmp _xxx

; ------------------------------------------------------------
_351_jmp_near_relative:
    inc di ; SI now points to the first 
           ; octal digit of the offset
    mov si, 4
    m_putsf 'JMP '
    m_octal_word_to_number
    m_print_near_offset_word
    ret ; jmp _xxx

; ------------------------------------------------------------
_352_jmp_far_absolute_direct:
    mov si, 4
    m_putsf 'JMP '
    add di, 6 ; first print the second word
    call p_print_next_word
    m_putfchar ':'
    sub di, 12 ; then print the first word
    call p_print_next_word
    add di, 6 ; move SI to the last byte read

    ret ; jmp _xxx

; ------------------------------------------------------------
_353_jmp_short_relative:
    inc di ; SI now points to the first 
           ; octal digit of the offset
    mov si, 10
    m_putsf 'JMP short '
    m_octal_byte_to_number
    m_print_near_offset_byte
    ret ; jmp _xxx

; -------------------------------------------------------------
_354_in_acc_port_indirect_byte:
    mov si, 9
    m_putsf 'IN AL, DX'
    ret ; jmp _xxx

; -------------------------------------------------------------
_355_in_acc_port_indirect_word:
    mov si, 9
    m_putsf 'IN AX, DX'
    ret ; jmp _xxx

; -------------------------------------------------------------
_356_out_acc_port_indirect_byte:
    mov si, 10
    m_putsf 'OUT DX, AL'
    ret ; jmp _xxx

; -------------------------------------------------------------
_357_out_acc_port_indirect_word:
    mov si, 10
    m_putsf 'OUT DX, AX'
    ret ; jmp _xxx

; ------------------------------------------------------------
;  _36X
; ------------------------------------------------------------
_36x:
    ; get 3rd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

    cmp al, 7
    ja undefined

    cmp al, 6
    jb short __36_012345

    __36_67:
        inc di ; point to 'mod'
        inc di ; point SI to next octal digit after 'mod'
        mov bl, byte ptr [data_octal+di]
        dec di
        dec di ; return SI back
        ; find out which operation is used
        cmp bl, 4
        jb short __36_67_mod_0123
        je _36_67_mul_rm

        cmp bl, 6
        jb _36_67_imul_rm
        je _36_67_div_rm
        jmp _36_67_idiv_rm

    __36_67_mod_0123:
        cmp bl, 2
        jb short __36_67_mod_01
        je _36_67_not_rm
        jmp _36_67_neg_rm

    __36_67_mod_01:
        cmp bl, 1
        jb _36_67_test_rm_imm
        jmp undefined_byte; _36_67_mod_001_rm

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

; -------------------------------------------------------------
_360_lock:
    mov si, 4
    m_putsf 'LOCK'
    ret ; jmp _xxx

; -------------------------------------------------------------
_362_repne:
    mov si, 5
    m_putsf 'REPNE'
    ret ; jmp _xxx

; -------------------------------------------------------------
_36_3_rep:
    mov si, 3
    m_putsf 'REP'
    ret ; jmp _xxx

; -------------------------------------------------------------
_36_4_hlt:
    mov si, 3
    m_putsf 'HLT'
    ret ; jmp _xxx

; -------------------------------------------------------------
_36_5_cmc:
    mov si, 3
    m_putsf 'CMC'
    ret ; jmp _xxx

; -------------------------------------------------------------
_36_67_test_rm_imm:
    mov si, 5
    m_putsf 'TEST '
    ; AL so far contains 3 bits '11w' as an octal number.

    ; keep only the last bit (ie. set 's' to 0)
    and al, 001b

    ; now the following procedure can be called. 
    ; It will think that it is indeed the command
    ; of format '0sw', which is exactly what is needed here.
    ; 's' was set to 0, since there is no 's' bit in test_rm_imm.
    call p_op_0sw_rm_imm
    ret ; jmp _xxx

; -------------------------------------------------------------
_36_67_not_rm:
    mov si, 4
    m_putsf 'NOT '
    ; AL contains '11w'
    m_before_decode
    call p_decode_rm
    m_move_index

    ret ; jmp _xxx

; -------------------------------------------------------------
_36_67_neg_rm:
    mov si, 4
    m_putsf 'NEG '
    ; AL contains '11w'
    m_before_decode
    call p_decode_rm
    m_move_index

    ret ; jmp _xxx

; -------------------------------------------------------------
_36_67_mul_rm:
    mov si, 4
    m_putsf 'MUL '
    ; AL contains '11w'
    m_before_decode
    call p_decode_rm
    m_move_index

    ret ; jmp _xxx

; -------------------------------------------------------------
_36_67_imul_rm:
    mov si, 5
    m_putsf 'IMUL '
    ; AL contains '11w'
    m_before_decode
    call p_decode_rm
    m_move_index

    ret ; jmp _xxx

; -------------------------------------------------------------
_36_67_div_rm:
    mov si, 4
    m_putsf 'DIV '
    ; AL contains '11w'
    m_before_decode
    call p_decode_rm
    m_move_index

    ret ; jmp _xxx

; -------------------------------------------------------------
_36_67_idiv_rm:
    mov si, 5
    m_putsf 'IDIV '
    ; AL contains '11w'
    m_before_decode
    call p_decode_rm
    m_move_index

    ret ; jmp _xxx

; ------------------------------------------------------------
;  _37X
; ------------------------------------------------------------
_37x:
    ; get 3rd octal digit
    inc di
    mov al, byte ptr [data_octal+di]

    cmp al, 7
    ja undefined

    cmp al, 6
    jb __37_012345

    __37_67:
        inc di ; point to 'mod'
        inc di ; point SI to next octal digit after 'mod'
        mov bl, byte ptr [data_octal+di]
        dec di
        dec di ; return SI back
        ; find out which operation is used
        cmp bl, 2
        jb short __37_67_mod_01
        je _377_call_near_absolute_indirect

        cmp bl, 5
        jb short __377_mod_34
        je _377_jmp_far_absolute_indirect

        cmp bl, 7
        jb _377_push_rm
        jmp undefined_byte; _377_mod_111_rm

    __37_67_mod_01:
        cmp bl, 1
        jb _37_67_inc_rm
        jmp _37_67_dec_rm

    __377_mod_34:
        cmp bl, 4
        jb _377_call_far_absolute_indirect
        jmp _377_jmp_near_absolute_indirect

    __37_012345:
        cmp al, 3
        jb short __37_012
        je _37_3_sti

    cmp al, 5
    jb _37_4_cld
    jmp _37_5_std

    __37_012:
        cmp al, 1
        jb _370_clc
        je _371_stc
        jmp _372_cli

; -------------------------------------------------------------
_370_clc:
    mov si, 3
    m_putsf 'CLC'
    ret ; jmp _xxx

; -------------------------------------------------------------
_371_stc:
    mov si, 3
    m_putsf 'STC'
    ret ; jmp _xxx

; -------------------------------------------------------------
_372_cli:
    mov si, 3
    m_putsf 'CLI'
    ret ; jmp _xxx

; -------------------------------------------------------------
_37_3_sti:
    mov si, 3
    m_putsf 'STI'
    ret ; jmp _xxx

; -------------------------------------------------------------
_37_4_cld:
    mov si, 3
    m_putsf 'CLD'
    ret ; jmp _xxx

; -------------------------------------------------------------
_37_5_std:
    mov si, 3
    m_putsf 'STD'
    ret ; jmp _xxx

; -----------------------------------------------------------/
_37_67_inc_rm:
    mov si, 4
    m_putsf 'INC '
    ; AL contains '11w'
    m_before_decode ; it will put '00w' in DL,
                    ; which is what is needed.
                    ; this will tell the decode procedure
                    ; that the operand is byte/word
    call p_decode_rm
    m_move_index

    ret ; jmp _xxx

; -----------------------------------------------------------/
_37_67_dec_rm:
    mov si, 4
    m_putsf 'DEC '
    ; AL contains '11w'
    m_before_decode ; it will put '00w' in DL,
                    ; which is what is needed.
                    ; this will tell the decode procedure
                    ; that the operand is byte/word
    call p_decode_rm
    m_move_index

    ret ; jmp _xxx

; -----------------------------------------------------------/
_377_call_near_absolute_indirect:
    mov si, 5
    m_putsf 'CALL '
    inc di ; points to 'mod'

    mov dl, 002 ; preparing for decode proc
                ; si already points to 'mod'
                ;
                ; DL above 1 means no pointer
                ; directive will be printed
                ; (it will have a default meaning
                ; that the operand is a word)
 
    call p_decode_rm
    m_move_index

    ret ; jmp _xxx

; -----------------------------------------------------------/
_377_call_far_absolute_indirect:
    ; check if mod is not '11'
    inc di ; point to 'mod'
    mov bl, byte ptr [data_octal+di]
    dec di ; return SI back
    ; find out if it's a legit opcode
    cmp bl, 3 ; mod cannot be '11'
    je undefined_byte

    mov si, 5
    m_putsf 'CALL '

    ; AL contains '101'
    MOV AL, 001 ; tell the decode procedures that
                ; the operand will be a word

    m_before_decode ; it will put '001' in DL,
                    ; which is what is needed.

    m_putfchar 'd' ; 'd' is for 'dword', since the next 
                   ; operand must be memory ('word ptr ...')
    call p_decode_rm
    m_move_index

    ret ; jmp _xxx

; -----------------------------------------------------------/
_377_jmp_near_absolute_indirect:
    mov si, 4
    m_putsf 'JMP '
    inc di ; points to 'mod'

    mov dl, 002 ; preparing for decode proc
                ; si already points to 'mod'
                ;
                ; DL above 1 means no pointer
                ; directive will be printed
                ; (it will have a default meaning
                ; that the operand is a word)
 
    call p_decode_rm
    m_move_index

    ret ; jmp _xxx

; -----------------------------------------------------------/
_377_jmp_far_absolute_indirect:
    ; check if mod is not '11'
    inc di ; point to 'mod'
    mov bl, byte ptr [data_octal+di]
    dec di ; return SI back
    ; find out if it's a legit opcode
    cmp bl, 3 ; mod cannot be '11'
    je undefined_byte

    mov si, 4
    m_putsf 'JMP '

    ; AL contains '101'
    MOV AL, 001 ; tell the decode procedures that
                ; the operand will be a word

    m_before_decode ; it will put '001' in DL,
                    ; which is what is needed.

    m_putfchar 'd' ; 'd' is for 'dword', since the next 
                   ; operand must be memory ('word ptr ...')

    call p_decode_rm
    m_move_index

    ret ; jmp _xxx

; -----------------------------------------------------------/
_377_push_rm:
    mov si, 5
    m_putsf 'PUSH '
    ; AL contains '111'
    m_before_decode ; it will put '001' in DL,
                    ; which is what is needed.
                    ; this will tell the decode procedure
                    ; that the operand is a word
    call p_decode_rm
    m_move_index

    ret ; jmp _xxx

; -----------------------------------------------------------/
endp main

end start
