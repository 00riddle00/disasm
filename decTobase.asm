; ============================================================
;  DESCRIPTION
; ============================================================
; Programa: Nr. 32
; Uzduoties salyga: 
    ; Parasykite programa, kuri iveda skaiciu desimtaineje 
    ; sistemoje. Tuomet ivedamas skaicius, kuris simbolizuoja 
    ; sistema. Isspausdina pirmaji skaicių ivestoje sistemoje. 
    ; Pavyzdziui: Jei skaičiai 5 ir 2 isvesti turi 101, jei 
    ; skaiciai 20 ir 16 tai atspausdina 14.
; 10185 -> base 22
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

MAX_INPUT_LEN = 6     ; 5 + <CR>
MAX_BASE_LEN = 3      ; 2 + <CR>
MAX_UNARY_NUM = 1000  ; limit the size of the number to be 
                      ; converted to unary number system

; ============================================================
;  DATA
; ============================================================

.data
    op_input           dw 0
    op_to_base         dw 0

    capacity_input     db MAX_INPUT_LEN
    size_input         db ?
    number_input       db MAX_INPUT_LEN dup (?)

    capacity_to_base   db MAX_BASE_LEN
    size_to_base       db ?
    number_to_base     db MAX_BASE_LEN dup (?)

    base10_multiplier  dw 000Ah  ; =10
    spacing            dw 0004   ; separate 4 digits by space (ex. 'A010 F010')

    sep1               db '=============================================================================$'
    sep2               db '-----------------------------------------------------------------------------$'

; ============================================================
;  CODE
; ============================================================

.code

; ------------------------------------------------
; PROCEDURES
; ------------------------------------------------

; this proc takes a decimal number in ASCII chars 
; and saves the numeric value in one word
;
; arguments need to be put in stack before call (in that order):
;     params[0]: ptr to str (source) (must have a '$' char at the end)
;     params[1]: ptr to int (destination)
;
; a user has to handle these error labels:
;     err_nan
;     err_num_max
; including cleaning the stack after error occurs
proc store_number_in_word
    ; preparing the stack
    push bp                      ; std procedure when proc arguments are 
    mov bp, sp                   ; already put in the stack before call
    ; reading arguments
    mov si, [bp+6]
    mov di, [bp+4]

    ; stack before "push bp"
    ;                 ________________
    ;           bp -> |______________|
    ;                 |_number_input_|
    ;                 |___op_input___|
    ;           sp -> |__return_addr_|
    ;                 |______________|
    ;                 |              |
    ;
    ; stack after "push bp"
    ;                 ________________
    ; bp -> old bp -> |______________|
    ;                 |_number_input_|
    ;                 |___op_input___|
    ;                 |__return_addr_|
    ;           sp -> |___old_bp_____|
    ;                 |              |
    ;
    ; stack after "mov bp, sp"
    ;
    ;       old bp -> |______________|
    ;                 |_number_input_| <- bp + 6
    ;                 |___op_input___| <- bp + 4
    ;                 |__return_addr_|
    ;     bp -> sp -> |___old_bp_____|
    ;                 |              |

    ; procedure body
    xor ax, ax  
    xor bx, bx

    read_chr:
        xchg ax, bx              ; store current number in bx
        lodsb                    ; put current digit symbol from ds:[si] to al
        cmp al, '$'
        je short end_of_input
        sub al, '0'
        jc short e1_err_nan
        cmp al, 9
        ja short e1_err_nan

        xchg ax, bx              ; put current number in ax for multiplication
        mul [base10_multiplier]
        add ax, bx               ; update the current number (which is in ax for now)
        jc short e2_err_num_max  ; overflow means the number does not fit in 2 bytes

        jmp short read_chr

    end_of_input:
        xchg ax, bx              ; put the result back in ax
        stosw                    ; saugome AX'o skaiciu zodyje adresu es:[di]
    
        ; clear the stack
        pop bp

        ; stack after "pop bp"
        ;                 ________________
        ; bp -> old bp -> |______________|
        ;                 |_number_input_| 
        ;                 |___op_input___| 
        ;           sp -> |__return_addr_|
        ;                 |              |

        ; return
        ret 4                    ; pop return address (set IP to it) and 
                                 ; pop both function arguments (2 words = 4 bytes) as well
                                 ; (Fortran/Pascal style calling convention)

    e1_err_nan:
        call err_nan
    e2_err_num_max:
        call err_num_max
endp

; ------------------------------------------------/

start:
    mov ax, @data                  ; perkelti data i registra ax
    mov ds, ax                     ; perkelti ax (data) i data segmenta
    mov es, ax                     ; perkelti ax (data) i data segmenta

    ; isvesti programos aprasa
    m_println sep1
    m_putsln 'Programa konvertuoja desimtaini skaiciu i jusu pasirinkta skaiciavimo sistema'
    m_println sep1
    
prompt1:
    ; Isvesti uzklausa nr.1
    m_putsln 'Iveskite skaiciu (max=65535):'
    m_puts '> '

    ; skaityti eilute
    mov dx, offset capacity_input  ; skaityti i buferio offseta 
    mov ah, 0Ah                    ; eilutes skaitymo subprograma
    int 21h                        ; dos'o interuptas

    m_print_nl                     ; cia reikia print newline, nes paskutinis char isvedime buvo
                                   ; <CR>, kuris grazina BIOS kursoriu atgal i eilutes pradzia, ir kadangi 
                                   ; po jo nera <LF>, kursorius nepasislenka viena eilute zemyn. Taigi sekantis
                                   ; outputas uzrasys ant virsaus eilutes pradzioje, panaikindamas senaji output'a

    ; ivesties ilgis
    xor bx, bx
    mov bl, size_input             ; idedam i bl kiek simboliu is viso
    cmp bl, 0                      ; ivesties ilgio validacija
    je err_no_input

    ; write '$' instead of 'CR' symbol
    mov byte ptr [number_input+bx], '$'

    ; passing arguments to the procedure
    ; first argument is passed first to the stack 
    ; (Fortran/Pascal style calling convention)
    mov bp, sp
    push offset number_input
    push offset op_input

    ; stack after "mov bp, sp"
    ;             ________________
    ; bp -> sp -> |______________|
    ;             |              |
    ;
    ; stack after arguments are pushed
    ;             ________________
    ;       bp -> |______________|
    ;             |_number_input_|
    ;       sp -> |___op_input___|
    ;             |______________|
    ;             |______________|
    ;             |              |

    call store_number_in_word       ; the called function will pop the arguments
                                    ; passed to it on return. 
                                    ; (Fortran/Pascal style calling convention)
                                    ; Otherwise, the calling procedure should
                                    ;
                                    ; increase stack pointer neccesary number of
                                    ; bytes after a call (C style calling convention)
     
    ; stack after the procedure has returned
    ;             ________________
    ; bp -> sp -> |______________|
    ;             |              |

prompt2:
    ; Isvesti uzklausa nr.2
    m_println sep2
    m_putsln 'Iveskite pasirinktos skaiciavimo sistemos pagrinda (min=1, max=62):'
    m_puts '> $'

    ; skaityti eilute
    mov dx, offset capacity_to_base  ; skaityti i buferio offseta 
    mov ah, 0Ah                      ; eilutes skaitymo subprograma
    int 21h                          ; dos'o INTeruptas
    m_print_nl

    ; ivesties ilgis
    xor bx, bx
    mov bl, size_to_base             ; idedam i bl kiek simboliu is viso
    cmp bl, 0                        ; ivesties ilgio validacija
    je err_no_input

    m_println sep2

    ; write '$' instead of 'CR' symbol
    mov byte ptr [number_to_base+bx], '$'  

    ; passing arguments to the procedure
    mov bp, sp
    push offset number_to_base
    push offset op_to_base

    call store_number_in_word

    ; validate correct "to base" input
    cmp [op_to_base], 0
    je err_base_zero
    cmp [op_to_base], 62
    ja err_base_max

    xor cx, cx
    mov ax, [op_input]
    mov bx, [op_to_base]

    cmp bx, 1
    jne short conversion

; in case base = 1
unary_conversion:
    cmp [op_input], MAX_UNARY_NUM
    ja err_unary_max

    m_puts 'Rezultatas: '
    mov cx, [op_input]           ; how many '1's to print
    inc cx                       ; plus additional '1' to mark zero
    mov ah, 2
print_one:
    mov dl, '1'
    mov al, cl
    div byte ptr spacing
    cmp ah, 0                    ; check if space needs to be printed (test if mod(cx,spacing) == 0)
    je short print_space
    mov ah, 2
    int 21h
    loop print_one
    jmp exit_program
print_one_no_space:              ; this extra block is needed so that CX gets
    m_putchar '1'                ; decremented if space was the previous character
    loop print_one
    jmp exit_program
print_space:
    m_putspace
    jmp short print_one_no_space
 
jmp short exit_program

conversion:
    inc cl
    
    xor dx, dx                   ; isvalyti dx, nes cia bus liekana po div
    div bx                       ; div ax/bx, ir liekana padedama dx
    push dx                      ; padeti skaitmeni
    
    cmp ax, 0                    ; jei jau skaicius isdalintas
    jz short converted           ; tai eiti i pabaiga

    xchg ax, dx
    mov al, cl
    div byte ptr spacing
    cmp ah, 0                    ; check if space needs to be pushed to stack (test if mod(cx,spacing) == 0)
    jne short convert

    push 99                      ; let 99 here mean 'space'
    jmp short convert            ; kitu atveju imti kita skaitmeni

convert:                         ; this extra block is used to put the remainder
    xchg ax, dx                  ; back to AX before next iteration 
    jmp short conversion

converted:
    m_puts 'Rezultatas: '

print_result:
    mov ah, 2                    ; atspausdinti skaitmenis
    pop dx
    cmp dx, 99                   ; in case of 'space'
    je short process_space

    cmp dx, 10
    jb short process_number
    cmp dx, 36
    jb short process_uppercase

    process_lowercase:
        sub dx, 35
        add dx, 60h
        jmp short print_symbol

    process_uppercase:
        sub dx, 9
        add dx, 40h
        jmp short print_symbol

    process_number:
        add dx, '0'
        jmp short print_symbol

    process_space:
        mov dx, 20h
        inc cl
        jmp short print_symbol

    print_symbol:
        int 21h
        dec cl
        jnz print_result

exit_program:
    m_exit0

err_no_input:
    m_println sep2
    m_putsln '[ERROR] Iveskite bent viena simboli!'
    m_println sep2
    jmp prompt1

err_base_zero:
    m_println sep2
    m_putsln '[ERROR] Ivestas pagrindas mazesnis uz 1!'
    m_println sep2
    jmp prompt1

err_base_max:
    m_println sep2
    m_putsln '[ERROR] Ivestas pagrindas didesnis uz 62!'
    m_println sep2
    jmp prompt1

err_unary_max:
    m_println sep2
    m_putsln '[ERROR] Skaicius per didelis vienetainei sistemai!'
    m_println sep2
    jmp prompt1

; Handling labels of the procedure
err_nan:
    m_println sep2
    m_putsln '[ERROR] Skaicius per didelis vienetainei sistemai!'
    m_println sep2
    jmp prompt1

err_num_max:
    m_println sep2
    m_putsln '[ERROR] Ivestas skaicius per didelis! (netelpa zodyje)'
    m_println sep2
    jmp prompt1

end start
