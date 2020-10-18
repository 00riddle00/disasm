;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
procParseUInt16:
   locals
   ; Išskiria iš buferio, kurio adresas DX'e sveiką skaičių int16 tipo
   ; Rezultatas patalpinamas AX'e. BX'e - adresas, kur buvo sustota (pvz. tarpas)  
   ; SVARBU: skaitomas skaičius turi būti korektiškas
   push dx
   push cx
   push si
   push di
   mov bx, dx
   mov ax, 0
   mov si, 0              ; number of digits 
   mov cl, 0              ; 0 - if nonnegative, 1 - otherwise
   ; eating spaces:
   @@leading_spaces:
      cmp byte ptr [bx], ' '
      jne @@next1
      inc bx
      jmp @@leading_spaces
    
   @@next1:
      cmp byte ptr [bx], 0          ; the end of the string?
      jne @@next2
      jmp @@endParsing
   @@next2:
   @@digits:
      cmp byte ptr [bx], '0'          
      jb  @@lessThanNumeric
      cmp byte ptr [bx], '9'          
      jbe  @@updateAX
      @@lessThanNumeric: 
         jmp @@endParsing
      @@updateAX:
         mov dx, 10
         mul dx
         mov dh, 0 
         mov dl, byte ptr [bx]
         sub dl, '0'
         add ax, dx
         inc si
      inc bx 
      jmp @@digits
   @@endParsing:
      cmp si, 0                   ; empty string?
      je @@setErrorAndReturn
      clc
      cmp cl, 1
      je @@negateAndReturn
      jmp @@return
   
   @@negateAndReturn:
      neg ax
      jmp @@return
          
   @@setErrorAndReturn:
      stc

   @@return:        
   pop di
   pop si
   pop cx
   pop dx
   ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
procParseInt16:
   locals
   ; Išskiria iš buferio, kurio adresas DX'e sveiką skaičių int16 tipo
   ; Rezultatas patalpinamas AX'e. BX'e - adresas, kur buvo sustota (pvz. taepas)  
   ; SVARBU: skaitomas skaičius turi būti korektiškas
   push dx
   push cx
   push si
   push di
   mov bx, dx
   mov ax, 0
   mov si, 0              ; number of digits 
   mov cl, 0              ; 0 - if nonnegative, 1 - otherwise
   ; eating spaces:
   @@leading_spaces:
      cmp byte ptr [bx], ' '
      jne @@next1
      inc bx
      jmp @@leading_spaces
    
   @@next1:
      cmp byte ptr [bx], 0          ; the end of the string?
      jne @@next2
      jmp @@endParsing
   @@next2:
      cmp byte ptr [bx], '-'   ; the minus
      jne @@digits
      mov cl, 1            ; negative number
      inc bx
   
   @@digits:
      cmp byte ptr [bx], '0'          
      jb  @@lessThanNumeric
      cmp byte ptr [bx], '9'          
      jbe  @@updateAX
      @@lessThanNumeric: 
         jmp @@endParsing
      @@updateAX:
         mov dx, 10
         mul dx
         mov dh, 0 
         mov dl, [bx]
         sub dl, '0'
         add ax, dx
         inc si
      inc bx 
      jmp @@digits
   @@endParsing:
      cmp si, 0                   ; empty string?
      je @@setErrorAndReturn
      clc
      cmp cl, 1
      je @@negateAndReturn
      jmp @@return
   
   @@negateAndReturn:
      neg ax
      jmp @@return
          
   @@setErrorAndReturn:
      stc

   @@return:        
   pop di
   pop si
   pop cx
   pop dx
   ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
procInt16ToStr:
   locals
   ; Konvertuoja reikšmę iš AX į ASCIIZ eilutę (10-nėje sistemoje)
   ; AX - int16 (nuo -32768 iki 32767), kurį reikia konvertuoti; 
   ; DX  - adresas, kur bus patalipntas rezultatas
   ;      
   push di    
   push si
   push cx
   push bx
   push dx
   mov bx, dx
   mov cx, 0     
   mov si, 0         
   cmp ax, 0
   jge @@next
     mov cl, 1
     mov byte ptr [bx], '-'
     inc bx
     neg ax

  
  @@next:
     mov dx, 0
     mov di, 10
     div di
     add dl, '0'
     mov byte ptr [bx], dl
     inc bx
     inc si
     cmp ax, 0
     je @@setSign
     jmp @@next
    
  @@setSign:
     


  @@reverse:
  ;   inc bx
     mov byte ptr [bx], 0             ; asciiz
     dec bx

     pop dx
     push dx
     mov di, dx 
    
     cmp cl, 1
     jne @@Ok
     inc di
     
     @@Ok:
     mov ax, si
     shr ax, 1
     mov cx, ax
     cmp cx, 0
     je @@return
     
     @@loopByDigits:
        mov al, [di]
        mov ah, byte ptr [bx]
        mov [di], ah
        mov byte ptr [bx], al
        dec bx
        inc di
        loop @@loopByDigits

  @@return: 
  pop dx
  pop bx
  pop cx
  pop si
  pop di
  ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
procUInt16ToStr:
    locals
   ; Konvertuoja reikšmę iš AX į ASCIIZ eilutę (10-nėje sistemoje)
   ; AX - int16 (nuo -32768 iki 32767), kurį reikia konvertuoti; 
   ; DX  - adresas, kur bus patalipntas rezultatas
   ;      
   push di    
   push si
   push cx
   push bx
   push dx
   mov bx, dx
   mov cx, 0     
   mov si, 0         

  @@next:
     mov dx, 0
     mov di, 10
     div di
     add dl, '0'
     mov byte ptr [bx], dl
     inc bx
     inc si
     cmp ax, 0
     je @@setSign
     jmp @@next
    
  @@setSign:
     


  @@reverse:
  ;   inc bx
     mov byte ptr [bx], 0             ; asciiz
     dec bx

     pop dx
     push dx
     mov di, dx 
    
     cmp cl, 1
     jne @@Ok
     inc di
     
     @@Ok:
     mov ax, si
     shr ax, 1
     mov cx, ax
     cmp cx, 0
     je @@return
     
     @@loopByDigits:
        mov al, [di]
        mov ah, byte ptr [bx]
        mov [di], ah
        mov byte ptr [bx], al
        dec bx
        inc di
        loop @@loopByDigits

  @@return: 
  pop dx
  pop bx
  pop cx
  pop si
  pop di
  ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

procGetStr:
    locals
   ; skaito  eilutę iš klaviatūros ir padaro ją ASCIIZ
   ; įvestis:  dx - buferio adresas; al - ilgiausios galimos sekos ilgis; 
   ; išvestis: dx - asciiz sekos adresas; 
   ; CF yra 1, jeigu buvo klaidų
   push bx
   push cx
   push dx
   mov bx, dx
   mov byte ptr [bx], al
   mov ah, 0ah
   int 21h
   inc bx
   mov ch, 0
   mov cl, byte ptr [bx]
   inc bx
   @@loopBySymbols:
       mov al, byte ptr [bx]
       mov byte ptr [bx-2], al
       inc bx
       loop @@loopBySymbols 
   mov byte ptr [bx-2], 0
   pop dx
   pop cx
   pop bx
   ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
procGetInt16:
    locals
   ; skaito  sveiką skaičių  iš klaviatūros ir gražina jį AX'e (nuo -32768 iki 32767)
   jmp @@kodas
 @@buferis:
      db '                      '
 @@kodas:
   push bx
   push cx
   push dx
   mov dx, offset @@buferis 
   mov al, 10
   call procGetStr
   call procParseInt16  
   pop dx
   pop cx
   pop bx
   ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
procGetUInt16:
    locals
   ; skaito beženklį sveiką skaičių  iš klaviatūros ir gražina jį AX'e (nuo 0 iki 65535)
   jmp @@kodas
 @@buferis:
      db '                      '
 @@kodas:
   push bx
   push cx
   push dx
   mov dx, offset @@buferis 
   mov al, 10
   call procGetStr
   call procParseUInt16  
   pop dx
   pop cx
   pop bx
   ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
procPutStr:
    locals
   ; spausdina asciiz eilutę į stdout 
   ; DX - asciiz eilutė;
   push bx
   push dx 
   push ax
   mov bx, dx
   @@loopBySymbols:
      mov dl, byte ptr [bx]
      cmp dl, 0
      je @@return
        mov ah, 02h
        int 21h
        inc bx
      jmp @@loopBySymbols
	
   @@return:
      pop ax
      pop dx
      pop bx
      ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
procPutInt16:
    locals
   ; spausdina int16 ant ekrano
   ; input:  ax - sveikas skaičius (nuo -32768 iki 32767)

   jmp @@kodas
 @@buferis: 
   db 00,00,00,00,00,00,00,00,00,00,00,00 
 @@kodas:
   push bx
   push dx 
   push ax
   mov dx, offset @@buferis
   call procInt16ToStr
   call procPutStr 
   pop ax
   pop dx
   pop bx
   ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
procPutUInt16:
    locals
   ; spausdina int16 ant ekrano
   ; input:  ax - beženklis sveikas skaičius (nuo 0 iki 65535)

   jmp @@kodas
 @@buferis: 
   db 00,00,00,00,00,00,00,00,00,00,00,00 
 @@kodas:
   push bx
   push dx 
   push ax
   mov dx, offset @@buferis
   call procUInt16ToStr
   call procPutStr 
   pop ax
   pop dx
   pop bx
   ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
procNewLine:
    locals
   ; prints \n 
   jmp @@begin
   @@localData:
   db 0Dh, 0Ah, 00h
   
   @@begin: 
  
   push dx 
   push ax
   mov dx, offset @@localData
   call procPutStr
   pop ax
   pop dx
   ret 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
procPutHexWord:
    locals
; Spausdina 16-tainį žodį, kuris paduodamas AX'e
;

   jmp @@begin
   @@localData:
   db '0x     ', 0
   
   @@begin: 
   push dx 
   push ax
   push cx
   push bx
   push si
   mov bx, ax
   mov cx, 4
   mov si, 0
   @@loop4:   
      mov dx, bx
      and dh, 0F0h
      rept 4 
          shr dh, 1
      endm
      mov dl, dh 
      add dl, '0'
      cmp dl, '9'
      jle @@print
         sub  dl, '0'
         add  dl, ('A'-10)
      @@print:
      mov byte ptr [@@localData + si + 2], dl
      rept 4
          shl bx, 1
      endm
      inc si 
      loop @@loop4
   
   mov dx, offset @@localData
   call procPutStr
   pop si
   pop bx
   pop cx
   pop ax
   pop dx
   ret 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
procPutHexByte:
    locals
; Spausdina 16-tainį baitą, kuris paduodamas AL'e
;

   jmp @@begin
   @@localData:
   db '0x     ', 0
   
   @@begin: 
   push dx 
   push ax
   push cx
   push bx
   push si
   mov bx, ax
   mov cx, 2
   mov si, 0
   @@loop4:   
      mov dx, bx
      and dh, 0F0h
      rept 4
          shr dh, 1
      endm
      mov dl, dh 
      add dl, '0'
      cmp dl, '9'
      jle @@print
         sub  dl, '0'
         add  dl, ('A'-10)
      @@print:
      mov byte ptr [@@localData + si + 2], dl
      rept 4
          shl bx, 1
      endm
      inc si 
      loop @@loop4
   
   mov dx, offset @@localData + 2
   call procPutStr
   pop si
   pop bx
   pop cx
   pop ax
   pop dx
   ret 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   Paprasta grafika
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

procSetGraphicsMode:
    locals
; Nustato grafinį režimą, 320x200x256
   push ax 
   mov ah, 00
   mov al, 13h
   int 10h
   pop ax
   ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
procSetTextMode:
    locals
; Nustato tekstinį režimą, 80x25
   push ax 
   mov ah, 00
   mov al, 03h
   int 10h
   pop ax
   ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
procPutPixel:
    locals
;  deda nurodytos spalvos pikselį nurodytoje vietoje
;  cl - color
;  si - x
;  di - y
   push ax 
   push cx
   push si
   push di
   push dx
   push es
   mov ax, di
   mov dx, 320
   mul dx
   add si, ax
   mov ax, 0A000h
   mov es, ax 
   mov byte [es:si], cl
   pop es
   pop dx
   pop di
   pop si
   pop cx
   pop ax
   ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


