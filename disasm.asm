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

   data_octal  db 2, 6, 4,  0, 1, 1            ; 0100: B409    | MOV AH, 011
               db 2, 7, 2,  3, 3, 6,  0, 0, 1  ; 0102: BADE01  | MOV DX, 001336
              ;db 3, 1, 5,  0, 4, 1            ; 0105: CD21    | INT 21
               db 1, 4, 5                      ; 01F9: 65      | UNDEFINED
               db 0, 1, 6                      ; 01FA: 0E      | PUSH CS
               db 0, 1, 1                      ; 0???: 09      | 011
               db 0, 3, 6                      ; 0???: 1E      | PUSH DS
               db 0, 0, 7                      ; 0???: 07      | POP ES
               db 0, 2, 7                      ; 0???: 17      | POP SS 
               db 0, 2, 5                      ; 0???: 15      | 025
               db 3, 2, 6                      ; 0???: D6      | UNDEFINED
               db 2, 4, 4                      ; 0???: ??      | MOVSB
               db 2, 4, 7                      ; 0???: ??      | CMPSW
               db 2, 5, 2                      ; 0???: ??      | STOSB
               db 2, 5, 5                      ; 0???: ??      | LODSW
               db 2, 5, 7                      ; 0???: ??      | SCASW
               db 2, 5, 0,  0, 4, 9            ; 0???: ????    | TEST AL, 049
               db 2, 5, 1,  0, 0, 1,  3, 3, 6  ; 0???: ??????  | TEST AX, 336001
               db 0, 4, 7                      ; 0???: ??      | DAA
               db 0, 7, 7                      ; 0???: ??      | AAS
               db 1, 0, 6                      ; 0???: ??      | INC SI
               db 1, 1, 3                      ; 0???: ??      | DEC BX
               db 1, 2, 0                      ; 0???: ??      | PUSH AX
               db 1, 3, 1                      ; 0???: ??      | POP CX
               db 2, 2, 0                      ; 0???: ??      | NOP
               db 2, 2, 5                      ; 0???: ??      | XCHG BP, AX
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
; ...
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
; -----------------------------------------------------------/

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

    cmp al, 3
    jb short __00_012
    je _003
    jmp short __00_45

    __00_012:
        cmp al, 1
        jb short _000
        je _001
        jmp _002

    __00_45:
        cmp al, 5
        je _005
        jmp _004

; ------------------------------------------------------------
_000:
    m_putsln '000'
    jmp _xxx

; ------------------------------------------------------------
_001:
    m_putsln '001'
    jmp _xxx

; ------------------------------------------------------------
_002:
    m_putsln '002'
    jmp _xxx

; ------------------------------------------------------------
_003:
    m_putsln '003'
    jmp _xxx

; ------------------------------------------------------------
_004:
    m_putsln '004'
    jmp _xxx

; ------------------------------------------------------------
_005:
    m_putsln '005'
    jmp _xxx

; ------------------------------------------------------------
;  _01X
; ------------------------------------------------------------
_01x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 3
    jb short __01_012
    je _013
    jmp short __01_45

    __01_012:
        cmp al, 1
        jb short _010
        je _011
        jmp _012

    __01_45:
        cmp al, 5
        je _015
        jmp _014

; ------------------------------------------------------------
_010:
    m_putsln '010'
    jmp _xxx

; ------------------------------------------------------------
_011:
    m_putsln '011'
    jmp _xxx

; ------------------------------------------------------------
_012:
    m_putsln '012'
    jmp _xxx

; ------------------------------------------------------------
_013:
    m_putsln '013'
    jmp _xxx

; ------------------------------------------------------------
_014:
    m_putsln '014'
    jmp _xxx

; ------------------------------------------------------------
_015:
    m_putsln '015'
    jmp _xxx

; ------------------------------------------------------------
;  _02X
; ------------------------------------------------------------
_02x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 3
    jb short __02_012
    je _023
    jmp short __02_45

    __02_012:
        cmp al, 1
        jb short _020
        je _021
        jmp _022

    __02_45:
        cmp al, 5
        je _025
        jmp _024

; ------------------------------------------------------------
_020:
    m_putsln '020'
    jmp _xxx

; ------------------------------------------------------------
_021:
    m_putsln '021'
    jmp _xxx

; ------------------------------------------------------------
_022:
    m_putsln '022'
    jmp _xxx

; ------------------------------------------------------------
_023:
    m_putsln '023'
    jmp _xxx

; ------------------------------------------------------------
_024:
    m_putsln '024'
    jmp _xxx

; ------------------------------------------------------------
_025:
    m_putsln '025'
    jmp _xxx

; ------------------------------------------------------------
;  _03X
; ------------------------------------------------------------
_03x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 3
    jb short __03_012
    je _033
    jmp short __03_45

    __03_012:
        cmp al, 1
        jb short _030
        je _031
        jmp _032

    __03_45:
        cmp al, 5
        je _035
        jmp _034

; ------------------------------------------------------------
_030:
    m_putsln '030'
    jmp _xxx

; ------------------------------------------------------------
_031:
    m_putsln '031'
    jmp _xxx

; ------------------------------------------------------------
_032:
    m_putsln '032'
    jmp _xxx

; ------------------------------------------------------------
_033:
    m_putsln '033'
    jmp _xxx

; ------------------------------------------------------------
_034:
    m_putsln '034'
    jmp _xxx

; ------------------------------------------------------------
_035:
    m_putsln '035'
    jmp _xxx

; ------------------------------------------------------------
;  _04X
; ------------------------------------------------------------
_04x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 3
    jb short __04_012
    je _043
    jmp short __04_45

    __04_012:
        cmp al, 1
        jb short _040
        je _041
        jmp _042

    __04_45:
        cmp al, 5
        je _045
        jmp _044

; ------------------------------------------------------------
_040:
    m_putsln '040'
    jmp _xxx

; ------------------------------------------------------------
_041:
    m_putsln '041'
    jmp _xxx

; ------------------------------------------------------------
_042:
    m_putsln '042'
    jmp _xxx

; ------------------------------------------------------------
_043:
    m_putsln '043'
    jmp _xxx

; ------------------------------------------------------------
_044:
    m_putsln '044'
    jmp _xxx

; ------------------------------------------------------------
_045:
    m_putsln '045'
    jmp _xxx

; ------------------------------------------------------------
;  _05X
; ------------------------------------------------------------
_05x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 3
    jb short __05_012
    je _053
    jmp short __05_45

    __05_012:
        cmp al, 1
        jb short _050
        je _051
        jmp _052

    __05_45:
        cmp al, 5
        je _055
        jmp _054

; ------------------------------------------------------------
_050:
    m_putsln '050'
    jmp _xxx

; ------------------------------------------------------------
_051:
    m_putsln '051'
    jmp _xxx

; ------------------------------------------------------------
_052:
    m_putsln '052'
    jmp _xxx

; ------------------------------------------------------------
_053:
    m_putsln '053'
    jmp _xxx

; ------------------------------------------------------------
_054:
    m_putsln '054'
    jmp _xxx

; ------------------------------------------------------------
_055:
    m_putsln '055'
    jmp _xxx

; ------------------------------------------------------------
;  _06X
; ------------------------------------------------------------
_06x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 3
    jb short __06_012
    je _063
    jmp short __06_45

    __06_012:
        cmp al, 1
        jb short _060
        je _061
        jmp _062

    __06_45:
        cmp al, 5
        je _065
        jmp _064

; ------------------------------------------------------------
_060:
    m_putsln '060'
    jmp _xxx

; ------------------------------------------------------------
_061:
    m_putsln '061'
    jmp _xxx

; ------------------------------------------------------------
_062:
    m_putsln '062'
    jmp _xxx

; ------------------------------------------------------------
_063:
    m_putsln '063'
    jmp _xxx

; ------------------------------------------------------------
_064:
    m_putsln '064'
    jmp _xxx

; ------------------------------------------------------------
_065:
    m_putsln '065'
    jmp _xxx

; ------------------------------------------------------------
;  _07X
; ------------------------------------------------------------
_07x:
    ; 3rd octal digit is already in BL
    mov al, bl

    cmp al, 3
    jb short __07_012
    je _073
    jmp short __07_45

    __07_012:
        cmp al, 1
        jb short _070
        je _071
        jmp _072

    __07_45:
        cmp al, 5
        je _075
        jmp _074

; ------------------------------------------------------------
_070:
    m_putsln '070'
    jmp _xxx

; ------------------------------------------------------------
_071:
    m_putsln '071'
    jmp _xxx

; ------------------------------------------------------------
_072:
    m_putsln '072'
    jmp _xxx

; ------------------------------------------------------------
_073:
    m_putsln '073'
    jmp _xxx

; ------------------------------------------------------------
_074:
    m_putsln '074'
    jmp _xxx

; ------------------------------------------------------------
_075:
    m_putsln '075'
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

    ; ------- print segment register name --------
    mov dl, byte ptr [SR+bx+1]
    mov ah, 02h
    int 21h

    mov dl, byte ptr [SR+bx]
    mov ah, 02h
    int 21h
    ; -------------------------------------------/

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

    ; ------- print segment register name --------
    mov dl, byte ptr [SR+bx+1]
    mov ah, 02h
    int 21h

    mov dl, byte ptr [SR+bx]
    mov ah, 02h
    int 21h
    ; -------------------------------------------/

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

    ; --------- print word register name ---------
    mov dl, byte ptr [Rw+bx+1]
    mov ah, 02h
    int 21h

    mov dl, byte ptr [Rw+bx]
    mov ah, 02h
    int 21h
    ; -------------------------------------------/

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

    ; --------- print word register name ---------
    mov dl, byte ptr [Rw+bx+1]
    mov ah, 02h
    int 21h

    mov dl, byte ptr [Rw+bx]
    mov ah, 02h
    int 21h
    ; -------------------------------------------/

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

    ; --------- print word register name ---------
    mov dl, byte ptr [Rw+bx+1]
    mov ah, 02h
    int 21h

    mov dl, byte ptr [Rw+bx]
    mov ah, 02h
    int 21h
    ; -------------------------------------------/

    m_print_nl

    jmp _xxx

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

    ; --------- print word register name ---------
    mov dl, byte ptr [Rw+bx+1]
    mov ah, 02h
    int 21h

    mov dl, byte ptr [Rw+bx]
    mov ah, 02h
    int 21h
    ; -------------------------------------------/

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

    ; --------- print word register name ---------
    mov dl, byte ptr [Rw+bx+1]
    mov ah, 02h
    int 21h

    mov dl, byte ptr [Rw+bx]
    mov ah, 02h
    int 21h
    ; -------------------------------------------/

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
    je _237
    ja undefined

    cmp al, 3
    jb short __23_012
    je _233
    jmp short __23_456

    __23_012:
        cmp al, 1
        jb short _230
        je _231
        jmp _232

    __23_456:
        cmp al, 5
        jb _234
        je _235
        jmp _236

; ------------------------------------------------------------
_230:
    m_putsln '230'
    jmp _xxx

; ------------------------------------------------------------
_231:
    m_putsln '231'
    jmp _xxx

; ------------------------------------------------------------
_232:
    m_putsln '232'
    jmp _xxx

; ------------------------------------------------------------
_233:
    m_putsln '233'
    jmp _xxx

; ------------------------------------------------------------
_234:
    m_putsln '234'
    jmp _xxx

; ------------------------------------------------------------
_235:
    m_putsln '235'
    jmp _xxx

; ------------------------------------------------------------
_236:
    m_putsln '236'
    jmp _xxx

; ------------------------------------------------------------
_237:
    m_putsln '237'
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
        jb _240_mov_acc_mem_byte
        jmp _241_mov_acc_mem_word

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

; ************************************************************
_240_mov_acc_mem_byte:
    m_putsln '_240_mov_acc_mem_byte'
    jmp _xxx

; ************************************************************
_241_mov_acc_mem_word:
    m_putsln '_241_mov_acc_mem_word'
    jmp _xxx

; ************************************************************
_242_mov_mem_acc_byte:
    m_putsln '_242_mov_mem_acc_byte'
    jmp _xxx

; ************************************************************
_243_mov_mem_acc_word:
    m_putsln '_243_mov_mem_acc_word'
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
    m_puts 'TEST '

    ; ------- print byte accumulator name --------
    mov dl, byte ptr [Rb+1]
    mov ah, 02h
    int 21h

    mov dl, byte ptr [Rb]
    mov ah, 02h
    int 21h
    ; -------------------------------------------/

    m_puts ', '

    ; --------- print next byte -----------------
    inc si
    mov dl, byte ptr [data_octal+si]
    add dl, 30h
    mov ah, 02h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si]
    add dl, 30h
    mov ah, 02h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si]
    add dl, 30h
    mov ah, 02h
    int 21h
    ; -------------------------------------------/

    m_print_nl

    jmp _xxx

; ************************************************************
_251_test_acc_imm_word:
    m_puts 'TEST '

    ; ------- print word accumulator name --------
    mov dl, byte ptr [Rw+1]
    mov ah, 02h
    int 21h

    mov dl, byte ptr [Rw]
    mov ah, 02h
    int 21h
    ; -------------------------------------------/

    m_puts ', '

    ; --------- print next word -----------------
    inc si
    mov dl, byte ptr [data_octal+si+3]
    add dl, 30h
    mov ah, 02h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si+3]
    add dl, 30h
    mov ah, 02h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si+3]
    add dl, 30h
    mov ah, 02h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si-3]
    add dl, 30h
    mov ah, 02h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si-3]
    add dl, 30h
    mov ah, 02h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si-3]
    add dl, 30h
    mov ah, 02h
    int 21h
    ; -------------------------------------------/

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

    ; ------- print byte register name ----------
    mov dl, byte ptr [Rb+bx+1]
    mov ah, 02h
    int 21h

    mov dl, byte ptr [Rb+bx]
    mov ah, 02h
    int 21h
    ; -------------------------------------------/

    m_puts ', '

    ; --------- print next byte -----------------
    inc si
    mov dl, byte ptr [data_octal+si]
    add dl, 30h
    mov ah, 02h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si]
    add dl, 30h
    mov ah, 02h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si]
    add dl, 30h
    mov ah, 02h
    int 21h
    ; -------------------------------------------/

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

    ; --------- print word register name ---------
    mov dl, byte ptr [Rw+bx+1]
    mov ah, 02h
    int 21h

    mov dl, byte ptr [Rw+bx]
    mov ah, 02h
    int 21h
    ; -------------------------------------------/

    m_puts ', '

    ; --------- print next word -----------------
    inc si
    mov dl, byte ptr [data_octal+si+3]
    add dl, 30h
    mov ah, 02h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si+3]
    add dl, 30h
    mov ah, 02h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si+3]
    add dl, 30h
    mov ah, 02h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si-3]
    add dl, 30h
    mov ah, 02h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si-3]
    add dl, 30h
    mov ah, 02h
    int 21h

    inc si
    mov dl, byte ptr [data_octal+si-3]
    add dl, 30h
    mov ah, 02h
    int 21h
    ; -------------------------------------------/

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
    je _307
    ja undefined

    cmp al, 3
    jb short __30_012
    je _303
    jmp short __30_456

    __30_012:
        cmp al, 2
        je _302
        jmp undefined ; _300, _301

    __30_456:
        cmp al, 5
        jb _304
        je _305
        jmp _306

; ------------------------------------------------------------
_302:
    m_putsln '302'
    jmp _xxx

; ------------------------------------------------------------
_303:
    m_putsln '303'
    jmp _xxx

; ------------------------------------------------------------
_304:
    m_putsln '304'
    jmp _xxx

; ------------------------------------------------------------
_305:
    m_putsln '305'
    jmp _xxx

; ------------------------------------------------------------
_306:
    m_putsln '306'
    jmp _xxx

; ------------------------------------------------------------
_307:
    m_putsln '307'
    jmp _xxx

; ------------------------------------------------------------
;  _31X
; ------------------------------------------------------------
_31x:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    je _317
    ja undefined

    cmp al, 3
    jb short __31_012
    je _313
    jmp short __31_456

    __31_012:
        cmp al, 2
        je _312
        jmp undefined ; _310, _311

    __31_456:
        cmp al, 5
        jb _314
        je _315
        jmp _316

; ------------------------------------------------------------
_312:
    m_putsln '312'
    jmp _xxx

; ------------------------------------------------------------
_313:
    m_putsln '313'
    jmp _xxx

; ------------------------------------------------------------
_314:
    m_putsln '314'
    jmp _xxx

; ------------------------------------------------------------
_315:
    m_putsln '315'
    jmp _xxx

; ------------------------------------------------------------
_316:
    m_putsln '316'
    jmp _xxx

; ------------------------------------------------------------
_317:
    m_putsln '317'
    jmp _xxx

; ------------------------------------------------------------
;  _32X
; ------------------------------------------------------------
_32x:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    je _327
    ja undefined

    cmp al, 3
    jb short __32_012
    je _323
    jmp short __32_456

    __32_012:
        cmp al, 1
        jb short _320
        je _321
        jmp _322

    __32_456:
        cmp al, 5
        jb _324
        je _325
        jmp undefined ; _326

; ------------------------------------------------------------
_320:
    m_putsln '320'
    jmp _xxx

; ------------------------------------------------------------
_321:
    m_putsln '321'
    jmp _xxx

; ------------------------------------------------------------
_322:
    m_putsln '322'
    jmp _xxx

; ------------------------------------------------------------
_323:
    m_putsln '323'
    jmp _xxx

; ------------------------------------------------------------
_324:
    m_putsln '324'
    jmp _xxx

; ------------------------------------------------------------
_325:
    m_putsln '325'
    jmp _xxx

; ------------------------------------------------------------
_327:
    m_putsln '327'
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
    je _347
    ja undefined

    cmp al, 3
    jb short __34_012
    je _343
    jmp short __34_456

    __34_012:
        cmp al, 1
        jb short _340
        je _341
        jmp _342

    __34_456:
        cmp al, 5
        jb _344
        je _345
        jmp _346

; ------------------------------------------------------------
_340:
    m_putsln '340'
    jmp _xxx

; ------------------------------------------------------------
_341:
    m_putsln '341'
    jmp _xxx

; ------------------------------------------------------------
_342:
    m_putsln '342'
    jmp _xxx

; ------------------------------------------------------------
_343:
    m_putsln '343'
    jmp _xxx

; ------------------------------------------------------------
_344:
    m_putsln '344'
    jmp _xxx

; ------------------------------------------------------------
_345:
    m_putsln '345'
    jmp _xxx

; ------------------------------------------------------------
_346:
    m_putsln '346'
    jmp _xxx

; ------------------------------------------------------------
_347:
    m_putsln '347'
    jmp _xxx

; ------------------------------------------------------------
;  _35X
; ------------------------------------------------------------
_35x:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    je _357
    ja undefined

    cmp al, 3
    jb short __35_012
    je _353
    jmp short __35_456

    __35_012:
        cmp al, 1
        jb short _350
        je _351
        jmp _352

    __35_456:
        cmp al, 5
        jb _354
        je _355
        jmp _356

; ------------------------------------------------------------
_350:
    m_putsln '350'
    jmp _xxx

; ------------------------------------------------------------
_351:
    m_putsln '351'
    jmp _xxx

; ------------------------------------------------------------
_352:
    m_putsln '352'
    jmp _xxx

; ------------------------------------------------------------
_353:
    m_putsln '353'
    jmp _xxx

; ------------------------------------------------------------
_354:
    m_putsln '354'
    jmp _xxx

; ------------------------------------------------------------
_355:
    m_putsln '355'
    jmp _xxx

; ------------------------------------------------------------
_356:
    m_putsln '356'
    jmp _xxx

; ------------------------------------------------------------
_357:
    m_putsln '357'
    jmp _xxx

; ------------------------------------------------------------
;  _36X
; ------------------------------------------------------------
_36x:
    ; get 3rd octal digit
    inc si
    mov al, byte ptr [data_octal+si]

    cmp al, 7
    je _367
    ja undefined

    cmp al, 3
    jb short __36_012
    je _363
    jmp short __36_456

    __36_012:
        cmp al, 1
        jb _360
        ja _362
        jmp undefined ; _361

    __36_456:
        cmp al, 5
        jb _364
        je _365
        jmp _366

; ------------------------------------------------------------
_360:
    m_putsln '360'
    jmp _xxx

; ------------------------------------------------------------
_361:
    m_putsln '361'
    jmp _xxx

; ------------------------------------------------------------
_362:
    m_putsln '362'
    jmp _xxx

; ------------------------------------------------------------
_363:
    m_putsln '363'
    jmp _xxx

; ------------------------------------------------------------
_364:
    m_putsln '364'
    jmp _xxx

; ------------------------------------------------------------
_365:
    m_putsln '365'
    jmp _xxx

; ------------------------------------------------------------
_366:
    m_putsln '366'
    jmp _xxx

; ------------------------------------------------------------
_367:
    m_putsln '367'
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
