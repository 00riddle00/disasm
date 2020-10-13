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
; Atliko: Tomas Giedraitis
; ============================================================

; ============================================================
;  MACROS
; ============================================================
; print string
print macro string               
    push dx
    push ax
    mov ah, 09                      
    mov dx, offset string
    int 21h
    pop ax
    pop dx
endm

; print newline
print_nl macro
    push dx
    push ax
    mov ah, 02
    mov dl, 13
    int 21h
    mov dl, 10
    int 21h
    pop ax
    pop dx
endm

; print string with newline
printl macro string               
    print string
    print_nl
endm

; print char
putchar macro char
   push dx
   push ax
   mov dl, offset char
   mov ah, 02
   int 21h
   pop ax
   pop dx
endm

exit macro
    MOV ax, 4c00h                   
    INT 21h                         
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

BUFSIZE	= 255
MAX_INPUT_LEN = 11 ; 10 + eilutes pabaiga
MAX_BASE_LEN = 4   ;  2 + eilutes pabaiga

; ============================================================
;  DATA
; ============================================================

.data
    sep1        db '=============================================================================$'
    sep2        db '-----------------------------------------------------------------------------$'

    desc         db 'Programa konvertuoja desimtaini skaiciu i jusu pasirinkta skaiciavimo sistema$'
    prompt1_msg  db 'Iveskite simboliu eilute:', 13, 10, '> $'
    prompt2_msg  db 'Iveskite pasirinktos skaiciavimo sistemos pagrinda (min=1, max=36):', 13, 10, '> $'
    errmsg_len   db '[ERROR] Iveskite bent viena simboli!$'
    errmsg_num   db '[ERROR] Kiekvienas ivestas simbolis turi buti skaicius!$'
    errmsg_min   db '[ERROR] Ivestas pagrindas mazesnis uz 1!$'
    errmsg_max   db '[ERROR] Ivestas pagrindas didesnis uz 36!$'
    result       db 'Rezultatas: $'

    capacity_in   db MAX_INPUT_LEN
    size_in       db ?
    data_in       db MAX_INPUT_LEN dup (?)

    capacity_base db MAX_BASE_LEN
    size_base     db ?
    data_base     db MAX_BASE_LEN dup (?)

    op_in	      dw 0
    op_base	      db 0

    result_inverted db 16 dup ('$')

; ============================================================
;  CODE
; ============================================================

.code

; ------------------------------------------------
; PROCEDURES
; ------------------------------------------------

PROC print_new_line
    MOV ah, 02h
    MOV dl, 13
    INT 21h
    MOV dl, 10
    INT 21h
    RET
print_new_line ENDP	

; ------------------------------------------------/

start:
    MOV ax, @data                   ; perkelti data i registra ax
    MOV ds, ax                      ; perkelti ax (data) i data segmenta
    MOV es, ax                      ; perkelti ax (data) i data segmenta

    ; Isvesti programos aprasa
    printl sep1
    printl desc
    printl sep1
    
prompt1:
    ; Isvesti uzklausa nr.1
    print prompt1_msg

    ; skaityti eilute
    MOV dx, offset capacity_in      ; skaityti i buferio offseta 
    MOV ah, 0Ah                     ; eilutes skaitymo subprograma
    INT 21h                         ; dos'o INTeruptas

    ; ivesties ilgis
    MOV cl, size_in                 ; idedam i cl kiek simboliu is viso
    cmp cl, 0                       ; ivesties ilgio validacija

    jne @1
    jmp err_len
    @1:
    printl sep2
    xor ch, ch                      ; isvalome ch, nes cx (su jau esama cl reiksme) bus 
                                    ; ---naudojamas "loop" komandoje
conv_in:
    MOV si, offset data_in          ; priskirti source index'ui buferio koordinates
    MOV di, offset op_in            ; priskirti destination index'ui busima konversijos rezultata
	xor	ax, ax		; accumulator
	xor	bx, bx		; number
next_chr_in:
	lodsb               ; imti is ds:si stringo dali ir dedame i al 
	sub	al, '0'			; normalize ASCII to number
	jc	err_num
	cmp	al, 9
	ja	err_num

	;lea	bx, [bx+bx*4]	; bx = bx * 5
	;lea	bx, [ax+bx*2]	; bx = (bx * 2) + digit
	loop next_chr_in

	mov ax, bx			; move number to eax
    mov ax, 65
	stosw				; store value and increment pointer

prompt2:
    ; Isvesti uzklausa nr.2
    print prompt2_msg

    ; skaityti eilute
    MOV dx, offset capacity_base    ; skaityti i buferio offseta 
    MOV ah, 0Ah                     ; eilutes skaitymo subprograma
    INT 21h                         ; dos'o INTeruptas

    ; ivesties ilgis
    MOV cl, size_base               ; idedam i cl kiek simboliu is viso
    cmp cl, 0                       ; ivesties ilgio validacija
    je err_len
    cmp cl, 2                       ; ivesties ilgio validacija
    ja err_max
    printl sep2
     
    MOV si, offset data_base        ; priskirti source index'ui buferio koordinates
    xor ch, ch                      ; isvalome ch, nes cx (su jau esama cl reiksme) bus 
                                    ; ---naudojamas "loop" komandoje

conv_base:
    MOV si, offset data_base          ; priskirti source index'ui buferio koordinates
    MOV di, offset op_base            ; priskirti destination index'ui busima konversijos rezultata
	xor	ax, ax		; accumulator
	xor	bx, bx		; number
next_chr_base:
	lodsb               ; imti is ds:si stringo dali ir dedame i al 
	sub	al, '0'			; normalize ASCII to number
	jc	err_num
	cmp	al, 9
	jg	err_num
	;lea	bx, [bx+bx*4]	; ebx = ebx * 5
	;lea	bx, [ax+bx*2]	; ebx = (ebx * 2) + digit
	loop next_chr_base

	mov ax, bx			; move number to eax
    mov ax, 8
	stosb				; store value and increment pointer

; isvesti: input
print result
printl op_in

; isvesti: base
print result
printl op_base

exit

; division
mov di, offset result_inverted
xor dx, dx
xor cx, cx
xor bh, bh
MOV ax, [op_in]
MOV bl, [op_base]
division:
   DIV BX
   xchg AX, DX
   stosb
   xchg AX, DX
   cmp AX, 0
   je print_result
   jmp division

print_result:


err_len:
    printl sep2
    printl errmsg_len
    printl sep2
    JMP prompt1

err_num:
    printl sep2
    printl errmsg_num
    printl sep2
    JMP prompt1

err_max:
    printl sep2
    printl errmsg_max
    printl sep2
    JMP prompt1
     
exit
     
end start
