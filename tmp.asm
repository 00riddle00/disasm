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

.model small     ;one code segment one data segment
.stack 100h
jumps

MAX_INPUT_LEN = 11 ; 10 + eilutes pabaiga
MAX_BASE_LEN = 3   ;  2 + eilutes pabaiga

; Macro Definition for printing string

print macro string               
mov ah,09h                      
mov dx,offset string
int 21h
endm print                            
     
.data
    newline      db 0Dh, 0Ah, '$'
    line1        db '=============================================================================', 0Dh, 0Ah, '$'
    line2        db '-----------------------------------------------------------------------------', 0Dh, 0Ah, '$'

    desc         db 'Programa konvertuoja desimtaini skaiciu i jusu pasirinkta skaiciavimo sistema', 0Dh, 0Ah, '$'
    prompt1_msg  db 'Iveskite simboliu eilute:', 0Dh, 0Ah, '> $'
    prompt2_msg  db 'Iveskite pasirinktos skaiciavimo sistemos pagrinda (min=1, max=36):', 0Dh, 0Ah, '> $'
    errmsg_len   db '[ERROR] Iveskite bent viena simboli!', 0Dh, 0Ah, '$'
    errmsg_num   db '[ERROR] Kiekvienas ivestas simbolis turi buti skaicius!', 0Dh, 0Ah, '$'
    errmsg_min   db '[ERROR] Ivestas pagrindas mazesnis uz 1!', 0Dh, 0Ah, '$'
    errmsg_max   db '[ERROR] Ivestas pagrindas didesnis uz 36!', 0Dh, 0Ah, '$'
    result       db 0Dh, 0Ah, 'Rezultatas:', 0Dh, 0Ah, '$'

    capacity_in   db MAX_INPUT_LEN
    size_in       db ?
    data_in       db MAX_INPUT_LEN dup (?)

    capacity_base db MAX_BASE_LEN
    size_base     db ?
    data_base     db MAX_BASE_LEN dup (?)

    op_in	      dw 0
    op_base	      db 0

    result_inverted db 16 dup ('$')

.code

start:
    ; Isvesti programos aprasa
    MOV ah, 09h
    MOV dx, offset line1
    int 21h
    jmp eof
    print newline

    MOV ax, @data                   ; perkelti data i registra ax
    MOV ds, ax                      ; perkelti ax (data) i data segmenta
    MOV es, ax                      ; perkelti ax (data) i data segmenta

    MOV ah, 40h
    MOV bx, 1
    MOV cx, 2
    mov dx, offset newline
    int 21h

    jmp eof

    ; Isvesti programos aprasa
    MOV ah, 09h
    MOV dx, offset line1
    int 21h
    MOV dx, offset desc
    int 21h
    MOV dx, offset line1
    int 21h
    
prompt1:
    ; Isvesti uzklausa nr.1
    MOV ah, 09h
    MOV dx, offset prompt1_msg
    int 21h

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
    MOV ah, 09h
    MOV dx, offset prompt2_msg
    int 21h

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
     
    ; isvesti: rezultatas
    ;MOV ah, 09h
    ;MOV dx, offset result
    ;int 21h
    
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
MOV ah, 09h
MOV dx, offset result
int 21h
MOV ah, 09h
MOV dx, offset op_in
int 21h

; isvesti: base
MOV ah, 09h
MOV dx, offset result
int 21h
MOV ah, 09h
MOV dx, offset op_base
int 21h

jmp eof

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
    MOV ah, 09h
    MOV dx, offset line2
    INT 21h
    MOV dx, offset errmsg_len
    INT 21h
    MOV dx, offset line2
    INT 21h
    JMP prompt1

err_num:
    MOV ah, 09h
    MOV dx, offset line2
    INT 21h
    MOV dx, offset errmsg_num
    INT 21h
    MOV dx, offset line2
    INT 21h
    JMP prompt1

err_max:
    MOV ah, 09h
    MOV dx, offset line2
    INT 21h
    MOV dx, offset errmsg_max
    INT 21h
    MOV dx, offset line2
    INT 21h
    JMP prompt1
     
eof:
    MOV ax, 4c00h                   ; griztame i dos'a
    INT 21h                         ; dos'o INTeruptas
     
end start
