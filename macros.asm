; =================================
;  CONSTANTS
; =================================
C_BUFFSIZE = 255

; =================================
;  MACROS
; =================================

; ---------------------
;  user input
; ---------------------

; get string from user
m_gets macro buffer
   push ax dx
   mov ah, 0Ah 
   mov dx, offset buffer
   int 21h
   pop dx ax
endm

; gets bit from a byte by index
m_getbit macro  byte, index
; Changes AX !!
; Pirmas argumentas:  baitas (adresas/reiksme/konstanta)
; Antras argumentas:  kelinta bita norime suzinoti (bitai skaiciuojami nuo nulinio - jauniausio)
; Rezultas: al - nurodyto bito reiksme
   push cx
   mov cl, index            ; bito numeris
   mov al, byte ptr byte     ; krauname baita
   shr al, cl                  ; stumiame nurodyta bita i pradzia
   and al, 01                  ; atmetame kitus bitus
   pop cx
endm

; ---------------------
;  compare
; ---------------------

; place the bigger of two words in ax
get_big macro word1, word2
; Changes AX !!
local @@exit
    mov ax, [word1]
    cmp ax, [word2]
    jg  @@exit
    mov ax, [word2]
@@exit:
endm 

; ---------------------
;  exit
; ---------------------

m_exit0 macro
    mov ax, 4c00h                   
    int 21h                         
endm

m_exit1 macro
    mov ax, 4c01h                   
    int 21h                         
endm
 
; ---------------------
;  write to STDOUT
; ---------------------

; print string
m_print macro string               
    push ax dx
    mov ah, 09                      
    mov dx, offset string
    int 21h
    pop dx ax
endm

; print newline
m_print_nl macro
    push ax dx
    mov ah, 02
    mov dl, 13
    int 21h
    mov dl, 10
    int 21h
    pop dx ax
endm

; print string with newline
m_println macro string               
    m_print string
    m_print_nl
endm

; print string (immediate): ex. m_puts "hello!"
m_puts macro string
local @@start, @@data
      push ax dx
      push ds
      jmp short @@start     ; string is being stored
@@data db string,'$'        ; in the code segment
@@start:                    ; so skip over it
      mov  ax,cs
      mov  ds,ax            ; set DS to code segment
      mov  ah,9
      lea  dx, [@@data]
      int  21h
      pop  ds               ; restore registers
      pop dx ax
endm 

; print immediate string with newline
m_putsln macro string               
    m_puts string
    m_print_nl
endm

; print char
m_putchar macro char
   push ax dx
   mov dl, char
   mov ah, 02
   int 21h
   pop dx ax
endm

m_putdigit macro digit
   push ax dx
   mov dl, digit               
   add dl, '0'                 
   mov ah, 02                  
   int 21h
   pop dx ax
endm

m_putspace macro
    m_putchar 20h
endm

; ---------------------
;  write to FILE
; ---------------------

; print string
m_printf macro string               
    push ax bx cx dx
    mov ah, 40h
    mov bx, out_handle
    mov cx, si
    mov dx, offset string
    int 21h
    xor si, si
    pop dx cx bx ax
endm

; print newline
m_printf_nl macro
local @@start, @@data
      push ax bx cx dx
      push ds
      jmp short @@start     ; string is being stored
@@data db 0Dh, 0Ah           ; in the code segment
@@start:                    ; so skip over it
      mov bx, out_handle
      mov ax, cs
      mov ds, ax            ; set DS to code segment
      mov ah, 40h
      mov cx, 2            ; how many bytes to write
      lea dx, [@@data]
      int 21h
      pop ds                ; restore registers
      pop dx cx bx ax
endm 

; print string with newline
m_printfln macro string               
    m_printf string
    m_printf_nl
endm

m_putsf macro string
local @@start, @@data
      m_puts string ; used for testing
      push ax bx cx dx
      push ds
      jmp short @@start     ; string is being stored
@@data db string            ; in the code segment
@@start:                    ; so skip over it
      mov bx, out_handle
      mov ax, cs
      mov ds, ax            ; set DS to code segment
      mov ah, 40h
      mov cx, si            ; how many bytes to write
      lea dx, [@@data]
      int 21h
      xor si, si
      pop ds                ; restore registers
      pop dx cx bx ax
endm 

; print immediate string with newline
m_putsfln macro string               
    m_putsf string
    m_printf_nl
endm

; print char
m_putfchar macro char
   push ax cx dx
   mov dl, char
   mov ah, 40h
   mov bx, out_handle
   mov cx, 1
   int 21h
   pop dx cx ax
endm

m_putfdigit macro digit
   push ax dx
   mov dl, digit               
   add dl, '0'                 
   mov ah, 02                  
   int 21h
   pop dx ax
endm

m_putfspace macro
    m_putfchar 20h
endm

