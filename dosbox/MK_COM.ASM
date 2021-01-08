; Name:             mk_com.asm
; Assemble:         tasm mk_com
; Link:             tlink /t mk_com
; Run in DOSBox:    mk_com.com
; pagamina COM mini programa

; in order to create COM file:
; * Use TINY memory model
; * The size of executable can not exceed single segment (64kb)
; * All calls and label must be NEAR
; * Set entry point offset to 100h, the STARTUPCODE directive takes care for you automatically when TINY memory model has been used
; * You can’t have a predefined stack segment

; to be able to use Turbo Debugger source level debugging with COM file:
; * TASM /zi mk_com.asm
; * TLINK /v mk_com.obj ; makes EXE file
; * TDSTRIP -s -c mk_com.exe mk_com.com  ; TDSTRIP is used to generate COM file out of EXE file
;
; else, just assemble it simply,without debug info:
; * TASM mk_com.asm
; * TLINK /t mk_com.obj

.model tiny
    
.code
org 100h 
begin:
    jmp main

main proc            ; <=== Entry point (main function)
    mov dl, 'A'
    mov ah, 02h
    int 21h

    mov ax,4c00h
    int 21h
main endp            ; <=== End function
    
end begin            ; <=== End program

