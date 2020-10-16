; =================================
;  CONSTANTS
; =================================
C_BUFFSIZE = 255

; =================================
;  MACROS
; =================================

; ---------------------
;  stack
; ---------------------

m_push_axdx macro
    push ax
    push dx
endm

m_pop_dxax macro
    pop dx
    pop ax
endm

; ---------------------
;  user input
; ---------------------

; get string from user
m_gets macro buffer
   m_push_axdx
   mov ah, 0Ah 
   mov dx, offset buffer
   int 21h
   m_pop_dxax
endm

; ---------------------
;  user input
; ---------------------

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
;  printing
; ---------------------

; print string
m_print macro string               
    m_push_axdx
    mov ah, 09                      
    mov dx, offset string
    int 21h
    m_pop_dxax
endm

; print newline
m_print_nl macro
    m_push_axdx
    mov ah, 02
    mov dl, 13
    int 21h
    mov dl, 10
    int 21h
    m_pop_dxax
endm

; print string with newline
m_println macro string               
    m_print string
    m_print_nl
endm

; print string (immediate): ex. printi "hello!"
m_puts macro string
local @@start, @@data
      m_push_axdx
      push ds
      jmp  @@start     ; string is being stored
@@data db string,'$'   ; in the code segment
@@start:               ; so, skip over it
      mov  ax,cs
      mov  ds,ax       ;set ds to code segment
      mov  ah,9
      lea  dx, [@@data]
      int  21h
      pop  ds          ;restore registers
      m_pop_dxax
endm 

; print immediate string with newline
m_putsln macro string               
    m_puts string
    m_print_nl
endm

; print char
m_putchar macro char
   m_push_axdx
   mov dl, char
   mov ah, 02
   int 21h
   m_pop_dxax
endm

m_putdigit macro digit
   m_push_axdx
   mov dl, digit               
   add dl, '0'                 
   mov ah, 02                  
   int 21h
   m_pop_dxax
endm

m_putspace macro
    m_putchar 20h
endm

; ---------------------
;  other
; ---------------------

; place the bigger of two words in ax
get_big macro word1, word2
; Changes AX !!
LOCAL @@exit
    mov ax, [word1]
    cmp ax, [word2]
    jg  @@exit
    mov ax, [word2]
@@exit:
ENDM 

m_exit0 macro
    MOV ax, 4c00h                   
    INT 21h                         
endm

m_exit1 macro
    MOV ax, 4c01h                   
    INT 21h                         
endm
 
