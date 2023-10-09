; Source https://z80code.amstrad.info/edit/LF689PCzX9yeCazyd

;  IMPOSSABALL
;  Disarked & Commented by Siko
; Some data is duplicated and should be removed from the source
; Some code is not yet disassembled
; 
; Main adresses
; -----------------------------------------------------------------------
; Original : label           : desc
; #0100    :                 :
; #0900    : font            : (1x8, sored in mode 2) 
; #1700    : checkboard_data : (40x32x4, stored in "mode 2")
; #6f03    : waitvbl
; #7086    : draw_checkboard : draw in 3 buffers
; #7278    : buf2screen:	 : copy from buffer to screen
; #7C5C    : draw_text		 :
; #7e78    : title_screen    : start title screen
; #90AA    : player data 	 : (lives...)
; #912D    : main_loop		 : Game Main loop
; #B086    : menu_tiles      : for drawing menu (used once, then overwritten)

;
; * Interrupts are not used
; * Mode 1

; Entry point
run start

buffer_zone1: 	equ #Ad04 ; buffering for pre rendering
buffer_zone2: 	equ #B124
buffer_zone3: 	equ #B962
work_buffer: 	equ #be00
labbf12: 		equ #bf12
dposy: 			equ #bf12
dposx: 			equ #bf13
timer_cnt: 		equ #bf5d ; (2 bytes)
num_cyl:		equ #bf69
labBF48 		equ #BF48
labBF47 		equ #BF47
labBF70 		equ #BF70


org #6f00	; original entry point => not used anymore 
jp start

waitvbl:
lab6F03:
	push af
    push bc
.loopNC
	call lab6F12
    jr nc,.loopNC
.loopC
	call lab6F12
    jr c,.loopC
    pop bc
    pop af
    ret 
    
lab6F12 
    ld b,#f5
    in a,(c)
    rra 
    ret 

;6f18
db #CD,#FB,#6F,#C2,#81,#92,#3A,#50 
db #7E,#87,#6F,#26,#00,#11,#67,#6F,#19,#5E,#23,#56,#21,#0D,#BF,#06 ; #6f20 ~.o&..go.^#V!...
db #05,#1A,#CD,#8E,#6F,#36,#00,#28,#01,#34,#23,#13,#10,#F3,#21,#0D ; #6f30 ....o6.(.4#...!.
db #BF,#7E,#23,#96,#23,#ED,#44,#32,#13,#BF,#7E,#23,#96,#ED,#44,#32 ; #6f40 .~#.#.D2..~#..D2
db #12,#BF,#C9,#43,#3B,#24,#1B,#3F,#22,#1B,#3C,#43,#26,#4A,#4B,#49 ; #6f50 ...C;$.?".<C&JKI
db #48,#4C,#32,#33,#31,#30,#34,#53,#6F,#58,#6F,#5D,#6F,#62,#6F,#3E ; #6f60 HL23104SoXo]obo>
db #2C,#3A,#45,#33

LAB6F74: 
db #21,#6F,#6F,#06,#05,#7E,#CD,#8E,#6F,#28,#05,#23 ; #6f70 ,:E3!oo..~..o(.#
db #10,#F7,#37,#C9,#B7,#C9


; masks
lab6F86:
	db #01,#02,#04,#08,#10,#20,#40,#80

LAB6F8E:
	push bc
    push hl
    push de
    ld d,a
    and 7
    ld e,a
    ld a,d
    ld d,0
    ld hl,lab6F86
lab6F9B add hl,de
    ld d,(hl)
    rrca 
    rrca 
    rrca 
    and 15
    ld e,a
    cp 10
    jr nc,lab6FDA

    ld bc,#f40e
    out (c),c
lab6FAC ld b,#f6
    in a,(c)
    and 48
    ld c,a
    or 192
    out (c),a
    out (c),c
    inc b
    ld a,146
    out (c),a
    push bc
    set 6,c
    ld b,#f6
    ld a,c
    or e
    out (c),a
    ld b,#f4
    in a,(c)
    cpl 
    ld e,a
    and d
    pop bc
    push af
    ld a,130
    out (c),a
    dec b
    out (c),c
    pop af
    jr lab6FDB
    
lab6FDA xor a
lab6FDB or a
    ld a,e
    pop de
    pop hl
    pop bc
    ret 


lab6FE1 ld b,0
lab6FE3 
	push bc
    ld a,b
    call lab6F8E
    pop bc
    or a
    ret nz
    ld a,b
    add a,8
    ld b,a
    cp 80
    jr nz,lab6FE3
    xor a
    ret 
    
LAB6FF5:
    call lab6FE1
    jr nz,lab6FF5
    ret 
    
lab6FFB ld a,(lab90AF)
    or a
    ret z
    ld a,66
    jr lab6F8E
    
lab7004 push hl
lab7005 ld hl,#0082
lab7008 dec hl
    ld a,h
    or l
    jr nz,lab7008
    djnz lab7005
    pop hl
    ret 
lab7011 or a
    ret z
    jp m,lab7019
    ld a,1
    ret 
lab7019 ld a,255
    ret 
lab701C bit 7,b
    jr z,lab702C
    or a
    jp m,lab7028
    cp c
    ret c
    ld a,c
    ret 
lab7028 cp b
    ret nc
    ld a,b
    ret 
lab702C cp b
    jr nc,lab7031
    ld a,b
    ret 
lab7031 cp c
    ret c
    ld a,c
    ret 

; clear buffer #be00-be40
lab7035 
	ld b,64
    ld hl,work_buffer
lab703A ld (hl),0
    inc hl
    djnz lab703A
    xor a				; ok....
    ld (#BE40),a
    ret 
     
lab7044:
	call lab8E74
    call waitvbl
    call lab8FD1
    call buf2screen
    ret 

org #7051
; Screen addresses
lab7051:
  dw #c05a,#c85a,#d05a,#d85a,#e05a,#e85a,#f05a,#f85a
  dw #c5aa,#cdaa,#d5aa,#ddaa,#e5aa,#edaa,#f5aa,#fdaa 
  
org #7071
lab7071:
    ld hl,#7051
    ld c,16
    ld a,255
lab7078 
    ld e,(hl)
    inc hl
    ld d,(hl)
    inc hl
    
    ld b,60
lab707E 
    ld (de),a
    inc de
    djnz lab707E
    dec c
    jr nz,lab7078
    ret 



; Draw Checkboard (top and bottom) in a buffer
draw_checkboard:
lab7086 
    
    ld (#BE41),sp    
    ld hl,buffer_zone3
    ld de,#B102
    
    exx
    ld ix,work_buffer
    ld de,checkboard_data
    ld b,32      ; 32 lines 

lab709A:
    ld a,(ix+1)
    ld c,a
    rra 
    and 3
    ld h,a
    add a,a
    add a,a
    add a,h
    ld h,a
    ld a,c
    rra 
    rra 
    rra 
    and 7
    neg
    add a,8
    ld l,a
    add hl,de
    ld sp,hl
    ld a,c
    rra 
    jp c,lab716E
    
    exx
   
repeat 16
    pop bc
    ld a,c
    ld (hl),a
    ld (de),a
    inc hl
    inc de
    ld a,b
    ld (hl),a
    ld (de),a
    inc hl
    inc de
rend
  
    inc hl
    inc de
    
    jp lab7244

; same as above, but with an additional  rra before writing data
lab716E:
    dec hl
    ld a,(hl)
    exx
	rra 
    
repeat 16    
    pop bc
    ld a,c
    rra 
    ld (hl),a
    ld (de),a
    inc hl
    inc de
    ld a,b
    rra 
    ld (hl),a
    ld (de),a
    inc hl
    inc de
rend

	inc hl
    inc de


lab7244:
    ex de,hl
    ld bc,-66 ; #ffbe
    add hl,bc
    ex de,hl
    exx

    ld hl,40
    add hl,de
    ex de,hl
    inc ix
    inc ix
    dec b
    jp nz,#709A   
    
    
; Clear buffer for the middle of the screen 64*16*2
    ld sp,buffer_zone3
    ld de,0 ; Pattern for filling the background, try #0155 for example :) could do some parallax her :)
    ld b,64
lab7260 
    push de
    push de
    push de
    push de
    push de
    push de
    push de
    push de
    push de
    push de
    push de
    push de
    push de
    push de
    push de
    push de
    dec sp 
    djnz lab7260
    
    ld sp,(#BE40+1)
    ret     

; Copy buffer to screen, with some *heavy* processing
buf2screen:
lab7278:
    ld hl,buffer_zone1
    ld ix,#0800
    
    ; first 32 lines
    ld a,32
    call lab7291

	; 64 middle lines
    ld hl,buffer_zone2
    ld a,64
    call lab7396
    
    ; last 32 lines
    ld hl,buffer_zone3+2
    ld a,32

lab7291 
    ld (#BE41),sp
    ld sp,hl
    ld b,240
    exx
    ld b,a
lab729A exx
    ld l,(ix+0)
    ld h,(ix+1)
    inc ix
    inc ix
    inc hl
    inc hl

repeat 15
    pop de
    ld a,e
    and b
    ld (hl),a
    inc l
    xor e
    ld (hl),a
    inc hl
    ld a,d
    and b
    ld (hl),a
    inc l
    xor d
    ld (hl),a
    inc hl
rend
  
    inc sp
    inc sp
    inc sp
    exx
    dec b
    jp nz,lab729A
    exx
    ld sp,(#BE41)
    ret 
   
lab7396:
	ld (#BE41),sp
    ld sp,hl
    ld b,240
    exx
    ld b,a
.loop:
    exx
    ld l,(ix+0)
    ld h,(ix+1)
    inc ix
    inc ix
    inc hl
    inc hl
    
repeat 15    
    pop de
    ld a,e
    and b
    cpl 
    ld (hl),a
    inc l
    xor e
    ld (hl),a
    inc hl
    ld a,d
    and b
    cpl 
    ld (hl),a
    inc l
    xor d
    ld (hl),a
    inc hl
rend

    
    inc sp
    inc sp
    inc sp
    exx
    dec b
    jp nz,.loop
    exx
    ld sp,(#BE41)
    ret 



lab74B9:
db #B7,#C8,#ED,#73,#41,#BE,#4F 
db #3A,#40,#BE,#81,#32,#40,#BE,#79,#B7,#F2,#CE,#74,#ED,#44,#3D,#1E ; #74c0 :@..2@.y...t.D=.
db #00,#CB,#3F,#CB,#1B,#CB,#3F,#CB,#1B,#57,#DD,#21,#00,#15,#DD,#19 ; #74d0 ..?...?..W.!....
db #31,#00,#BE,#3E,#20,#CB,#79,#01,#00,#20,#20,#1C,#E1,#DD,#5E,#00 ; #74e0 1..> .y..  ...^.
db #DD,#56,#01,#19,#B7,#ED,#42,#30,#01,#09,#E5,#33,#33,#DD,#23,#DD ; #74f0 .V....B0...33.#.
db #23,#04,#3D,#C2,#EC,#74,#18,#19,#E1,#DD,#5E,#00,#DD,#56,#01,#B7 ; #7500 #.=..t....^..V..
db #ED,#52,#30,#01,#09,#E5,#33,#33,#DD,#23,#DD,#23,#04,#3D,#C2,#08 ; #7510 .R0...33.#.#.=..
db #75,#ED,#7B,#41,#BE,#C9,#22,#15,#BF,#32,#17,#BF,#79,#FE,#0D,#D0 ; #7520 u.{A.."..2..y...
db #FE,#07,#20,#05,#3A,#A2,#90,#B7,#C8,#7D,#D6,#08,#1F,#1F,#1F,#1F ; #7530 .. .:....}......
db #E6,#03,#87,#5F,#3A,#17,#BF,#FE,#10,#28,#04,#FE,#20,#20,#01,#1C ; #7540 ..._:....(..  ..
db #79,#87,#87,#87,#83,#5F,#16,#00,#21,#02,#01,#19,#7E,#FE,#FF,#C8 ; #7550 y...._..!...~...
db #6F,#5F,#26,#00,#16,#00,#29,#29,#29,#B7,#ED,#52,#ED,#5B,#00,#01 ; #7560 o_&...)))..R.[..
db #19,#E5,#DD,#E1,#3A,#17,#BF,#FE,#20,#3E,#00,#38,#01,#3C,#32,#14 ; #7570 ....:... >.8.<2.
db #BF,#2A,#15,#BF,#AF,#CD,#75,#79,#DD,#5E,#03,#16,#00,#B7,#ED,#52 ; #7580 .*....uy.^.....R
db #3A,#14,#BF,#B7,#20,#05,#DD,#7E,#01,#18,#03,#DD,#7E,#02,#CD,#D2 ; #7590 :... ..~....~...
db #79,#D8,#E5,#DD,#7E,#06,#FE,#10,#38,#0E,#FE,#1B,#30,#0A,#3A,#A2 ; #75a0 y...~...8...0.:.
db #90,#B7,#DD,#7E,#06,#28,#01,#3C,#87,#87,#87,#80,#6F,#26,#00,#5D ; #75b0 ...~.(.<....o&.]
db #54,#29,#19,#11,#00,#2B,#19,#5E,#23,#56,#23,#46,#21,#A0,#2D,#19 ; #75c0 T)...+.^#V#F!.-.
db #DD,#5E,#04,#DD,#56,#05,#D9,#E1,#D9,#DD,#4E,#00,#ED,#73,#41,#BE ; #75d0 .^..V.....N..sA.
db #78,#D9,#FE,#04,#20,#10,#01,#1E,#00,#3A,#14,#BF,#B7,#28,#03,#01 ; #75e0 x... ....:...(..
db #DC,#FF,#D9,#C3,#7E,#78,#FE,#03,#20,#10,#01,#1F,#00,#3A,#14,#BF ; #75f0 ....~x.. ....:..
db #B7,#28,#03,#01,#DD,#FF,#D9,#C3,#47,#78,#01,#20,#00,#3A,#14,#BF ; #7600 .(......Gx. .:..
db #B7,#28,#03,#01,#DE,#FF,#D9,#C3,#17,#78,#01,#03,#01,#01,#01,#02 ; #7610 .(.......x......
db #01,#01,#01,#01,#03,#01,#01,#01,#01,#04,#01,#01,#01,#01,#01,#03 ; #7620 
db #01,#01,#01,#01,#01,#02,#02,#02,#01,#01,#01,#01,#02,#03,#02,#01 ; #7630 
db #01,#01,#01,#02,#04,#02,#01,#01,#01,#01,#01,#02,#03,#02,#01,#01 ; #7640 
db #01,#01,#01,#01,#02,#04,#02,#01,#01,#01,#01,#01,#01,#02,#05,#02 ; #7650 
db #01,#01,#01,#01,#01,#01,#02,#06,#02,#01,#01,#01,#01,#01,#01,#01 ; #7660 
db #02,#05,#02,#01,#01,#01,#01,#01,#01,#01,#01,#02,#06,#02,#01,#01 ; #7670 
db #01,#01,#01,#01,#01,#01,#02,#07,#02,#01,#01,#01,#01,#01,#01,#01 ; #7680 
db #01,#01,#02,#06,#02,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#02 ; #7690 
db #07,#02,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#03,#06,#03,#01 ; #76a0 
db #01,#01,#01,#01,#00,#03,#03,#05,#08,#05,#0D,#05,#12,#07,#19,#07 ; #76b0 
db #20,#07,#27,#07,#2E,#09,#37,#09,#40,#09,#49,#09,#52,#0B,#5D,#0B ; #76c0  .'...7.@.I.R.].
db #68,#0B,#73,#0D,#80,#0D,#8D,#0D,#0D,#0D,#0D,#0D,#0D,#0D,#0D,#0D ; #76d0 h.s.............
db #0D,#0E,#0E,#0E,#0E,#0E,#0E,#0E,#0E,#0F,#0F,#0F,#0F,#0F,#0F,#0F ; #76e0 
db #10,#10,#10,#10,#10,#10,#11,#11,#11,#11,#11,#12,#12,#12,#12,#13 ; #76f0 
db #13,#13,#13,#14,#14,#14,#15,#15,#15,#16,#16,#16,#02,#02,#02,#03 ; #7700 
db #03,#03,#04,#04,#04,#05,#05,#05,#06,#06,#06,#07,#07,#07,#00,#E5 ; #7710 
db #26,#00,#CD,#75,#79,#11,#F8,#FF,#19,#C1,#E5,#59,#16,#00,#21,#D2 ; #7720 &..uy......Y..!.
db #76,#19,#56,#CB,#3A,#92,#57,#3A,#1E,#77,#E6,#03,#82,#D9,#E1,#CD ; #7730 v.V.:.W:.w......
db #D2,#79,#D9,#7E,#21,#B4,#76,#D6,#05,#4F,#3A,#1E,#77,#B7,#28,#0D ; #7740 .y.~!.v..O:.w.(.
db #FE,#03,#28,#09,#FE,#04,#C8,#87,#87,#91,#ED,#44,#4F,#79,#87,#F5 ; #7750 ..(........DOy..
db #4F,#06,#00,#09,#7E,#2C,#4E,#6F,#26,#00,#11,#1A,#76,#19,#F1,#E5 ; #7760 O...~,No&...v...
db #6F,#26,#00,#11,#00,#69,#19,#5E,#2C,#56,#21,#00,#69,#19,#D1,#FE ; #7770 o&...i.^,V!.i...
db #18,#DA,#0E,#78,#C3,#74,#78,#00,#00,#01,#02,#02,#03,#03,#03,#04 ; #7780 ...x.tx.........
db #04,#04,#04,#05,#05,#05,#05,#06,#06,#06,#06,#06,#07,#07,#07,#07 ; #7790 
db #07,#32,#87,#77,#3A,#A5,#90,#B7,#C0,#7D,#5F,#08,#16,#00,#21,#D2 ; #77a0 .2.w:....}_...!.
db #76,#19,#7E,#D6,#05,#4F,#3A,#87,#77,#D6,#06,#0F,#E6,#1F,#FE,#18 ; #77b0 v.~..O:.w.......
db #38,#02,#3E,#18,#C6,#88,#6F,#3E,#00,#CE,#77,#67,#79,#96,#4F,#06 ; #77c0 8.>...o>..wgy.O.
db #00,#21,#0C,#77,#09,#7E,#C5,#F5,#08,#26,#00,#6F,#7C,#CD,#75,#79 ; #77d0 .!.w.~...&.o|.uy
db #47,#F1,#F5,#CB,#3F,#ED,#44,#80,#CD,#D2,#79,#11,#42,#00,#19,#EB ; #77e0 G...?.D...y.B...
db #1B,#F1,#08,#C1,#79,#87,#4F,#06,#00,#21,#24,#69,#09,#7E,#2C,#66 ; #77f0 ....y.O..!$i.~,f
db #6F,#01,#00,#69,#09,#08,#FE,#06,#DA,#B1,#78,#C3,#CE,#78,#ED,#73 ; #7800 o..i......x..x.s
db #41,#BE,#D9,#01,#20,#00,#D9,#1A,#13,#B7,#28,#12,#47,#F9,#D9,#D1 ; #7810 A... .....(.G...
db #7E,#B3,#AA,#77,#23,#D1,#7E,#B3,#AA,#77,#09,#D9,#10,#EF,#7D,#C6 ; #7820 ~..w#.~..w....}.
db #04,#6F,#30,#01,#24,#0D,#C2,#17,#78,#ED,#7B,#41,#BE,#C9,#ED,#73 ; #7830 .o0.$...x.{A...s
db #41,#BE,#D9,#01,#1F,#00,#D9,#1A,#13,#B7,#28,#18,#47,#F9,#D9,#D1 ; #7840 A.........(.G...
db #7E,#B3,#AA,#77,#23,#D1,#7E,#B3,#AA,#77,#23,#D1,#7E,#B3,#AA,#77 ; #7850 ~..w#.~..w#.~..w
db #09,#D9,#10,#E9,#7D,#C6,#06,#6F,#30,#01,#24,#0D,#C2,#47,#78,#ED ; #7860 ....}..o0.$..Gx.
db #7B,#41,#BE,#C9

lab7874 ld (#BE41),sp
    exx
    ld bc,#1E
    dec hl
    exx
lab787E ld a,(de)
    inc de
    or a
    jr z,lab78A1
    ld b,a
lab7884 ld sp,hl
    exx
    pop de
    ld a,(hl)
    or e
    xor d
    ld (hl),a
    inc hl
lab788C pop de
    ld a,(hl)
    or e
    xor d
    ld (hl),a
    inc hl
    pop de
    ld a,(hl)
    or e
    xor d
    ld (hl),a
    inc hl
    pop de
    ld a,(hl)
    or e
    xor d
    ld (hl),a
    add hl,bc
    exx
    djnz lab7884
lab78A1 ld a,l
    add a,8
    ld l,a
    jr nc,lab78A8
    inc h
lab78A8 dec c
    jp nz,lab787E
    ld sp,(#BE41)
    ret 


lab78B1:
    ld (#BE41),sp
    ld sp,hl
    ex de,hl
    ld bc,#20
lab78BA 
    ex af,af'
    pop de
    ld a,(hl)
    and e
    ld (hl),a
    inc hl
    ld a,(hl)
    and d
    ld (hl),a
    add hl,bc
    ex af,af'
    dec a
    jp nz,lab78BA
    ld sp,(#BE41)
    ret 
    
    
lab78CE ld (#BE41),sp
    ld sp,hl
    ex de,hl
    dec hl
    ld bc,#1E
lab78D8 ex af,af'
    
    pop de
    ld a,(hl)
    and e
    ld (hl),a
    inc hl
    ld a,(hl)
    and d
    ld (hl),a
    
    inc hl
    
    pop de
    ld a,(hl)
    and e
    ld (hl),a
    inc hl
    ld a,(hl)
    and d
    ld (hl),a
    
    add hl,bc
    ex af,af'
    dec a
    jp nz,lab78D8
    ld sp,(#BE41)
    ret 

lab78F5
db #00,#01,#02,#01,#04,#01,#06,#01,#08,#01,#0A ; #78f0 .{A.............
db #01,#0D,#01,#0F,#01,#11,#01,#13,#01,#16,#01,#18,#01,#1A,#01,#1D ; #7900 
db #01,#1F,#01,#22,#01,#25,#01,#27,#01,#2A,#01,#2D,#01,#2F,#01,#32 ; #7910 ...".%.'.*.-./.2
db #01,#35,#01,#38,#01,#3B,#01,#3E,#01,#41,#01,#44,#01,#48,#01,#4B ; #7920 .5.8.;.>.A.D.H.K
db #01,#4E,#01,#52,#01,#55,#01,#59,#01,#5D,#01,#60,#01,#64,#01,#68 ; #7930 .N.R.U.Y.].`.d.h
db #01,#6C,#01,#70,#01,#74,#01,#79,#01,#7D,#01,#82,#01,#86,#01,#8B ; #7940 .l.p.t.y.}......
db #01,#90,#01,#95,#01,#9A,#01,#9F,#01,#A4,#01,#AA,#01,#AF,#01,#B5 ; #7950 
db #01,#BB,#01,#C1,#01,#C7,#01,#CE,#01,#D4,#01,#DB,#01,#E2,#01,#E9 ; #7960 
db #01,#F0,#01,#F8,#01,#E5,#D6,#20,#ED,#44,#CB,#25,#26,#00,#11,#F5 ; #7970 ....... .D.%&...
db #78,#19,#5E,#23,#56,#21,#00,#00,#B7,#28,#19,#4F,#CB,#7F,#28,#02 ; #7980 x.^#V!...(.O..(.
db #ED,#44,#87,#87,#06,#06,#29,#87,#30,#01,#19,#10,#F9,#CB,#79,#7C ; #7990 .D....).0.....y|
db #28,#02,#ED,#44,#C6,#40,#E1,#F5,#7C,#4F,#21,#00,#00,#B7,#28,#1C ; #79a0 (..D.@..|O!...(.
db #F2,#B5,#79,#ED,#44,#87,#06,#07,#29,#87,#30,#01,#19,#10,#F9,#6C ; #79b0 ..y.D...).0....l
db #26,#00,#CB,#79,#28,#06,#EB,#62,#6A,#B7,#ED,#52

lab79CC ld de,#80
    add hl,de
    pop af
    ret 
    
lab79D2:
    ld c,a
    bit 7,h
    jr nz,lab79E2
    ld a,h
    or a
    jr nz,lab7A12
    ld a,l
    cp 247
    jr nc,lab7A12
    jr lab79EC
lab79E2 ld a,h
    cp 255
    jr nz,lab7A12
    ld a,l
    cp 241
    jr c,lab7A12
lab79EC ld a,l
    and 7
    ld b,a
    sra h
    rr l
    sra h
    rr l
    sra h
    rr l
    ld de,#AD03
    add hl,de
    ld a,c
    ex de,hl
    ld l,a
    ld h,0
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    add a,l
    ld l,a
    jr nc,lab7A0F
    inc h
lab7A0F add hl,de
    or a
    ret 
lab7a12:
	scf
    ret 

lab7a14:
db #03,#02,#01,#FF,#FE,#FD,#06,#05,#04,#02,#01,#FF ; #7a10 ..7.............
db #FE,#FC,#FB,#FA,#08,#07,#06,#06,#04,#02,#01,#FF,#FE,#FC,#FA,#FA ; #7a20 
db #F9,#F8,#08,#08,#08,#07,#06,#05,#04,#03,#01,#FF,#FD,#FC,#FB,#FA ; #7a30 
db #F9,#F8,#F8,#F8,#14,#7A,#1A,#7A,#24,#7A,#32,#7A,#06,#0A,#0E,#12 ; #7a40 .....z.z$z2z....
db #3E,#06,#32,#1B,#BF,#21,#D0,#FF,#22,#18,#BF,#3E,#20,#32,#1A,#BF ; #7a50 >.2..!.."..> 2..
db #AF,#32,#8C,#7A,#32,#1C,#BF,#32,#1D,#BF,#32,#2A,#BF,#32,#26,#BF ; #7a60 .2.z2..2..2*.2&.
db #2A,#44,#7A,#22,#28,#BF,#3A,#4C,#7A,#32,#27,#BF,#21,#00,#00,#22 ; #7a70 *Dz"(.:Lz2'.!.."
db #A3,#90,#AF,#32,#A5,#90,#3E,#FF,#32,#2C,#BF,#C9,#00,#21,#1C,#BF ; #7a80 ...2..>.2,...!..
db #3A,#8C,#7A,#B7,#20,#10,#3A,#13,#BF,#B7,#28,#0A,#86,#06,#FA,#0E ; #7a90 :.z. .:...(.....
db #06,#CD,#1C,#70,#18,#07,#7E,#CD,#11,#70,#ED,#44,#86,#77,#21,#1D ; #7aa0 ...p..~..p.D.w!.
db #BF,#3A,#8C,#7A,#B7,#20,#12,#3A,#12,#BF,#ED,#44,#B7,#28,#0A,#86 ; #7ab0 .:.z. .:...D.(..
db #06,#FC,#0E,#04,#CD,#1C,#70,#18,#07,#7E,#CD,#11,#70,#ED,#44,#86 ; #7ac0 ......p..~..p.D.
db #77,#3A,#1C,#BF,#47,#3A,#1D,#BF,#B0,#20,#03,#32,#8C,#7A,#2A,#18 ; #7ad0 w:..G:... .2.z*.
db #BF,#CB,#7C,#28,#0A,#7D,#FE,#A0,#30,#05,#3E,#06,#32,#1C,#BF,#C9 ; #7ae0 ..|(.}..0.>.2...
db #21,#26,#BF,#5E,#34,#7E,#23,#BE,#38,#02,#2B,#35,#16,#00,#2A,#28 ; #7af0 !&.^4~#.8.+5..*(
db #BF,#19,#7E,#C9,#2A,#18,#BF,#3A,#1C,#BF,#5F,#87,#9F,#57,#19,#22 ; #7b00 ..~.*..:.._..W."
db #22,#BF,#21,#1A,#BF,#3A,#1D,#BF,#86,#06,#06,#0E,#39,#CD,#1C,#70 ; #7b10 ".!..:......9..p
db #32,#24,#BF,#23,#3A,#1E,#BF,#86,#F2,#2D,#7B,#3E,#06,#06,#06,#0E ; #7b20 2$.#:....-{>....
db #39,#CD,#1C,#70,#32,#25,#BF,#C9,#3A,#47,#BF,#FE,#01,#28,#0C,#FE ; #7b30 9..p2%..:G...(..
db #FF,#C0,#3A,#27,#BF,#CB,#3F,#32,#26,#BF,#C9,#AF,#32,#26,#BF,#3A ; #7b40 ..:'..?2&...2&.:
db #11,#BF,#21,#2A,#BF,#B7,#28,#0A,#7E,#3C,#FE,#04,#38,#09,#3E,#03 ; #7b50 ..!*..(.~<..8.>.
db #18,#05,#7E,#B7,#28,#01,#3D,#77,#5F,#16,#00,#21,#4C,#7A,#19,#7E ; #7b60 ..~.(.=w_..!Lz.~
db #32,#27,#BF,#21,#44,#7A,#19,#19,#5E,#23,#56,#ED,#53,#28,#BF,#C9 ; #7b70 2'.!Dz..^#V.S(..
db #3A,#2C,#BF,#FE,#FF,#28,#26,#3A,#2B,#BF,#21,#1B,#BF,#BE,#20,#1D ; #7b80 :,...(&:+.!... .
db #21,#2C,#BF,#35,#C0,#AF,#32,#1C,#BF,#32,#1D,#BF,#36,#FF,#3A,#1B ; #7b90 !,.5..2..2..6.:.
db #BF,#FE,#20,#3E,#01,#38,#02,#3E,#FF,#32,#47,#BF,#C9,#3A,#1B,#BF ; #7ba0 .. >.8.>.2G..:..
db #32,#2B,#BF,#3E,#03,#32,#2C,#BF,#C9,#3A,#A5,#90,#B7,#C0,#AF,#32 ; #7bb0 2+.>.2,..:.....2
db #48,#BF,#32,#47,#BF,#CD,#F0,#7A,#32,#1E,#BF,#21,#1C,#BF,#11,#1F ; #7bc0 H.2G...z2..!....
db #BF,#01,#03,#00,#ED,#B0,#CD,#04,#7B,#CD,#E7,#88,#30,#06,#CD,#82 ; #7bd0 ........{...0...
db #89,#CD,#13,#8B,#21,#22,#BF,#11,#18,#BF,#01,#04,#00,#ED,#B0,#21 ; #7be0 ....!".........!
db #1D,#BF,#3A,#1A,#BF,#FE,#06,#20,#06,#CB,#7E,#28,#02,#36,#00,#23 ; #7bf0 ..:.... ..~(.6.#
db #3A,#1A,#BF,#FE,#39,#20,#06,#CB,#7E,#20,#02,#36,#00,#3A,#1B,#BF ; #7c00 :...9 ..~ .6.:..
db #FE,#06,#3E,#01,#28,#09,#3A,#1B,#BF,#FE,#39,#3E,#FF,#20,#03,#32 ; #7c10 ..>.(.:...9>. .2
db #47,#BF,#CD,#8D,#7A,#CD,#80,#7B,#CD,#38,#7B,#C9,#00,#00,#50,#00 ; #7c20 G...z..{.8{...P.
db #A0,#00,#F0,#00,#40,#01,#90,#01,#E0,#01,#30,#02,#80,#02,#D0,#02 ; #7c30 ....@.....0.....
db #20,#03,#70,#03,#C0,#03,#10,#04,#60,#04,#B0,#04,#00,#05,#50,#05 ; #7c40  .p.....`.....P.
db #A0,#05,#F0,#05,#40,#06,#90,#06,#E0,#06,#30,#07

; Dessin texte
draw_text:
LAB7C5C: 
    push af
    push bc
    push de
    push hl
    cp 32
    jr nc,lab7C6F
    cp 4
    jp nc,lab7D29
    ld (#BF32),a
    jp lab7D29
lab7C6F cp 192
    jr c,lab7C7B
    sub 192
    ld (#BF2D),a
    jp lab7D29
lab7C7B cp 160
    jr c,lab7CA0
    sub 160
    ld c,a
    ld a,(#BF2D)
    ld b,a
    ld hl,#7C2C
    ld e,b
    ld d,0
    add hl,de
    add hl,de
    ld e,(hl)
    inc hl
    ld d,(hl)
    ld hl,#c008
    add hl,de
    ld e,c
    ld d,0
    add hl,de
    add hl,de
    ld (#BF2E),hl
    jp lab7D29
lab7CA0 sub 32
    ld l,a
    ld h,0
    ld de,font
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,de
    ld de,(#BF2E)

	; Draw Character    
    ld b,8
    ld a,(#BF32)
    or a
    jr z,lab7CC1
    dec a
    jr z,lab7CD8
    dec a
    jr z,lab7CFA
    
    jp lab7D11

lab7CC1 ld a,(hl)
    rrca 
    rrca 
    rrca 
    rrca 
    and 15
    ld (de),a
    inc e
    ld a,(hl)
    and 15
    ld (de),a
    dec e
    inc l
    ld a,d
    add a,8
    ld d,a
    djnz lab7CC1
    jr lab7D26

lab7CD8 ld a,(hl)
    rrca 
    rrca 
    rrca 
    rrca 
    push af
    and 15
    ld c,a
    ld a,(hl)
    and 240
    or c
    ld (de),a
    inc e
    pop af
    and 240
    ld c,a
    ld a,(hl)
    and 15
    or c
    ld (de),a
    dec e
    inc l

    ld a,d
    add a,8
    ld d,a
    djnz lab7CD8
    jr lab7D26
    
lab7CFA ld a,(hl)
    and 240
    ld (de),a
    inc e
    ld a,(hl)
    rlca 
    rlca 
    rlca 
    rlca 
    and 240
    ld (de),a
    dec e
    inc l
    ld a,d
    add a,8
    ld d,a
    djnz lab7CFA
    jr lab7D26


lab7D11 
    
    ld a,(hl)
    or 15
    ld (de),a
    inc e
    
    ld a,(hl)
    rlca 
    rlca 
    rlca 
    rlca 
    or 15
    ld (de),a
    dec e
    inc l
    
    ld a,d
    add a,8
    ld d,a
    djnz lab7D11

lab7D26 call lab7D92
lab7D29 pop hl
    pop de
    pop bc
    pop af
    ret 


LAB7D2E: 
	db #7E,#23 
db #FE,#0D,#C8,#CD,#5C,#7C,#18,#F6,#00,#05,#0A,#0E,#C0,#06,#C0,#0F 
db #01,#07,#0B,#10,#01,#07,#0C,#11,#02,#08,#0D,#12,#03,#09,#0C,#11 
db #03,#09,#0B,#11,#04,#05,#C0,#0F,#03,#07,#0B,#11,#03,#07,#0C,#11 
db #C0,#C0,#C0,#C0

lab7D64 db #FE,#20,#28,#04,#D6,#30,#18,#02,#3E,#0A,#6F,#26 
        db #00,#11,#38,#7D,#29,#29,#19,#EB,#2A,#2E,#BF,#CD,#9B,#7D,#01,#50 
        db #00,#09,#22,#2E,#BF,#CD,#9B,#7D,#01,#B0,#FF,#09,#22,#2E,#BF,#CD 
        db #92,#7D

lab7d92:
	db #2A,#2E,#BF,#23,#23,#22,#2E,#BF,#C9,#CD,#9E,#7D,#1A,#C6 
    db #60,#13,#C3,#5C,#7C

lab7DA5 db #00,#00,#00,#00,#00,#00
lab7dab: 
     db 0

lab7dac: 
	push af
    ld a,2
    call draw_text
    ld de,draw_text
    jr lab7DC0   
    
lab7DB7:
	push af
    ld a,0
    call draw_text
    ld de,lab7D64
lab7DC0 pop af
    ld (lab7E1D+1),de
    ld (lab7DAB),a
    ld a,b
    add a,192
    call draw_text
    ld a,c
    add a,160
    call draw_text
    ld de,lab7DA5
    ld b,6
    ld a,32
lab7DDB ld (de),a
    inc de
    djnz lab7DDB
    ld b,3
    ld de,lab7DA5
    jr lab7DE9
lab7DE6 inc de
    inc de
    dec hl
lab7DE9 ld a,(hl)
    and 255
    jr nz,lab7DF1
    djnz lab7DE6
    inc b
lab7DF1 and 240
    jr z,lab7DFC
    ld a,48
lab7DF7 rld 
    ld (de),a
    rrd 
lab7DFC inc de
    ld a,48
    rrd 
    ld (de),a
    rld 
    inc de
    dec hl
    djnz lab7DF7
    ld hl,lab7DA5
    ld a,(lab7DAB)
    ld b,a
    sub 6
    neg
    or a
    jr z,lab7E1A
lab7E16 inc hl
    dec a
    jr nz,lab7E16
lab7E1A ld a,(hl)
    push bc
    push hl
lab7E1D call draw_text
    pop hl
    pop bc
    inc hl
    djnz lab7E1A
    ret 
LAB7E26:
	db #21,#51,#70,#0E,#10,#3E,#00,#5E,#23,#56 ; #7e20 ..#...!Qp..>.^#V
db #23,#06,#3C,#12,#13,#10,#FC,#0D,#20,#F3,#21,#00,#08,#0E,#80,#5E ; #7e30 #.<..... .!....^
db #23,#56,#23,#13,#13,#06,#3C,#12,#13,#10,#FC,#0D,#20,#F1,#C9

LAB7E4F: db #01,0,0

lab7E52: 
	ld hl,lab81BA
    jp lab7D2E	

LAB7E58:
	call lab7E52
    ld hl,txtplay
    call lab7D2E
    
    ld a,(lab7E4F+2)
    or a
    ret z
    ld hl,lab8208
    call lab7D2E
    ld a,(lab90AC)
    add a,49
    jp draw_text
 
lab7e74: 
    db #3F,#26,#4C,#34

lab7E78:
title_screen:
	call lab90B4
    call lab7E26
    ld a,7
    ld (lab7E4F),a
    call lab7E58
    call lab8062
    call lab6FF5
    call lab903E
    ld a,1
    ld (lab7E4F),a
lab7E94 call lab7E58

	; wait 4 frames
	ld b,4
lab7E99 push bc
    call waitvbl
    pop bc
    djnz lab7E99

	ld hl,lab7E4F
    inc (hl)
    ld a,(hl)
    cp 8
    jr nz,lab7ECE
    ld (hl),1
    call lab6F74
    jr nc,lab7EB5
    ld a,1
    ld (lab7E4F+2),a
lab7EB5 ld a,(lab7E4F+2)
    or a
    jr z,lab7ECE
    ld a,36
    call lab6F8E
    jr z,lab7ECE
    ld hl,lab90AC
    inc (hl)
    ld a,(#0C00)
    cp (hl)
    jr nz,lab7ECE
    ld (hl),0

lab7ECE ld hl,lab7E74
    ld b,4
lab7ED3 ld a,(hl)
    inc hl
    call lab6F8E
    jr nz,lab7EDF
    djnz lab7ED3
    jp lab7E94

lab7EDF ld a,4
    sub b
    ld (lab7E4f+1),a
    ret 

lab7EE6:
 db #F6,#7E,#06,#7F,#18,#7F,#2A,#7F,#3B,#7F,#4B,#7F,#5A,#7F,#65,#7F

lab7EF6:
 db "  NICE AND EASY",#0D,
 db "PLAYING WITH FIRE",#0D

org #7f18
lab7f18:
 db "GETTING "
db #54,#48,#45,#20,#4B,#4E,#41,#43,#4B,#0D,#20,#41,#20,#54,#52,#49 ; #7f20 THE KNACK. A TRI
db #41,#4C,#20,#4F,#46,#20,#54,#49,#4D,#45,#0D,#20,#20,#45,#56,#49 ; #7f30 AL OF TIME.  EVI
db #46,#20,#52,#4F,#44,#49,#52,#52,#4F,#43,#0D,#20,#20,#20,#54,#48 ; #7f40 F RODIRROC.   TH
db #52,#45,#45,#20,#54,#4F,#20,#47,#4F,#0D,#20,#20,#20,#20,#20,#20 ; #7f50 REE TO GO.      
db #55,#55,#47,#48,#0D,#20,#20,#20,#45,#4E,#44,#4F,#46,#41,#4E,#45 ; #7f60 UUGH.   ENDOFANE
db #52,#41,#0D

org #7f73
lab7f73: db #00

lab7F74:
	ld (lab7F73),a
    call lab90B4
lab7F7A call lab7E26
lab7F7D call lab7E52
lab7F80 ld hl,lab8219	; text
lab7F83 call lab7D2E
lab7F86 ld a,(lab90AB)
lab7F89 add a,49
lab7F8B call draw_text
    call lab7D2E
    ld a,(lab90AB)
    add a,a
lab7F95 ld l,a
    ld h,0
    ld de,lab7EE6
    add hl,de
    ld e,(hl)
    inc hl
    ld d,(hl)
lab7F9F ex de,hl
    call lab7D2E
lab7FA3 ld hl,lab822F
    call lab7D2E
lab7FA9 ld a,(num_lives)
lab7FAC add a,48
lab7FAE call draw_text
    ld e,0
    ld d,1
lab7FB5 call lab6FF5
    ld b,25
lab7FBA 
	push bc
    call waitvbl
    call waitvbl
    ld a,(lab7F73)
lab7FC4 or a
    jr z,lab7FD0
    push de
    ld a,e
    call lab8F1C
lab7FCC pop de
    ld a,e
    add a,d
    ld e,a
lab7FD0 push de
    call lab6FE1
    pop de
    pop bc
    jr nz,lab7FDA
    djnz lab7FBA
lab7FDA jp lab8DBC


db #F5,#CD,#B4 ; #7fdd ...o.. .........
db #90,#CD,#26,#7E,#CD,#52,#7E,#21,#3F,#82,#CD,#2E,#7D,#F1,#B7,#28 ; #7fe0 ..&~.R~!?...}..(
db #41,#21,#5D,#82,#CD,#2E,#7D,#3A,#AA,#90,#B7,#28,#3B,#FE,#0A,#38 ; #7ff0 A!]...}:...(;..8
db #02,#3E,#09,#F5,#21,#70,#82,#CD,#2E,#7D,#F1,#F5,#C6,#30,#CD,#5C ; #8000 .>..!p...}...0.\
db #7C,#CD,#2E,#7D,#F1,#87,#F5,#C6,#14,#CD,#1C,#8F,#11,#00,#01,#CD ; #8010 |..}............
db #5C,#83,#CD,#04,#83,#06,#04,#CD,#03,#6F,#10,#FB,#F1,#3D,#20,#E6 ; #8020 \........o...= .
db #18,#06,#21,#4C,#82,#CD,#2E,#7D,#3E,#01,#32,#73,#7F,#1E,#19,#16 ; #8030 ..!L...}>.2s....
db #FF,#C3,#B5,#7F,#00,#00

lab8046: db #01,#4A,#4D,#50,#00,#50,#00,#5D,#20,#20 ; #8040 .JMP.P.]  
db #00,#30,#00,#5D,#20,#20,#00,#20,#00,#5D,#20,#20,#00,#10,#00,#5D ; #8050 .0.]  . .]  ...]
db #20,#20
LAB8062:
	ld hl,lab8046
    ld a,0
lab8067 push af
    ld e,a
    add a,8
    ld b,a
    ld c,9
    ld a,2
lab8070 call draw_text
    push hl
    ld a,6
lab8076 call lab7DAC
    ld a,178
lab807B call draw_text
    pop hl
    inc hl
lab8080 ld b,3
lab8082 ld a,(hl)
    call draw_text
    inc hl
    djnz lab8082
    inc hl
    inc hl
    pop af
    inc a
    cp 5
    jr nz,lab8067
    ret 
    
db #00,#00,#00,#20,#20,#20,#FF,#00,#21,#63,#BF,#11,#92,#80 ; #8090 .....   ..!c....
db #01,#03,#00,#ED,#B0,#3E,#41,#12,#13,#3E,#20,#12,#13,#12,#CD,#C0 ; #80a0 .....>A..> .....
db #80,#D0,#32,#99,#80,#FE,#04,#3F,#D8,#CD,#C0,#80,#38,#FB,#37,#C9 ; #80b0 ..2....?....8.7.
db #21,#44,#80,#DD,#21,#92,#80,#06,#05,#7E,#23,#DD,#96,#00,#27,#7E ; #80c0 !D..!....~#...'~
db #23,#DD,#9E,#01,#27,#7E,#23,#DD,#9E,#02,#27,#30,#18,#2B,#2B,#2B ; #80d0 #...'~#...'0.+++
db #0E,#06,#56,#DD,#7E,#00,#77,#DD,#72,#00,#DD,#23,#23,#0D,#20,#F2 ; #80e0 ..V.~.w.r..##. .
db #3E,#05,#90,#37,#C9,#23,#23,#23,#10,#CF,#B7,#C9,#00,#00,#00,#00 ; #80f0 >..7.###........
db #CD,#9A,#80,#D0,#CD,#B4,#90,#CD,#26,#7E,#CD,#52,#7E,#CD,#62,#80 ; #8100 ........&~.R~.b.
db #3A,#99,#80,#6F,#87,#85,#87,#6F,#26,#00,#11,#47,#80,#19,#22,#FD ; #8110 :..o...o&..G..".
db #80,#EB,#3E,#03,#32,#FF,#80,#21,#95,#82,#CD,#2E,#7D,#CD,#F5,#6F ; #8120 ..>.2..!....}..o
db #AF,#32,#FC,#80,#06,#0A,#C5,#CD,#03,#6F,#C1,#10,#F9,#3E,#02,#CD ; #8130 .2.......o...>..
db #5C,#7C,#3A,#99,#80,#C6,#C8,#CD,#5C,#7C,#3E,#B2,#CD,#5C,#7C,#2A ; #8140 \|:.....\|>..\|*
db #FD,#80,#06,#03,#7E,#CD,#5C,#7C,#23,#10,#F9,#D5,#CD,#18,#6F,#D1 ; #8150 ....~.\|#.....o.
db #3A,#11,#BF,#B7,#28,#16,#3A,#FC,#80,#B7,#20,#C8,#3E,#01,#32,#FC ; #8160 :...(.:... .>.2.
db #80,#13,#21,#FF,#80,#35,#C8,#3E,#41,#12,#18,#B8,#AF,#32,#FC,#80 ; #8170 ..!..5.>A....2..
db #3A,#13,#BF,#B7,#28,#AE,#F2,#93,#81,#1A,#3D,#FE,#41,#30,#0C,#3E ; #8180 :...(.....=.A0.>
db #5A,#18,#08,#1A,#3C,#FE,#5B,#38,#02,#3E,#41,#12,#C3,#34,#81,#3E ; #8190 Z...<.[8.>A..4.>
db #12,#CD,#8E,#6F,#C8,#21,#D6,#82,#CD,#2E,#7D,#CD,#BC,#8D,#CD,#18 ; #81a0 ...o.!....}.....
db #6F,#3A,#11,#BF,#B7,#28,#F7,#C3,#04,#83
lab81ba:
	db #C3,#A9,#01,#21,#03,#22 
db #23,#24,#25,#26,#27,#28,#29,#2A,#2B,#2C,#2D,#01,#2E,#C4,#A9,#01 ; #81c0 #$%&'()*+,-.....
db #2F,#03,#3A,#3B,#3C,#3D,#3E,#73,#74,#75,#76,#77,#78,#79,#01,#7A ; #81d0 /.:;<=>stuvwxy.z
db #C5,#A9,#01,#7B,#7C,#7C,#7C,#7C,#7C,#7C,#7C,#7C,#7C,#7C,#7C,#7C ; #81e0 ...{||||||||||||
db #7D,#0D


txtplay:
lab81F2: db #CF,#A7,#00,"PRESS FIRE TO PLAY",#0D
txtstart:
LAB8208: db #D1,#A9,#02,"START LEVEL  ",#0D
txtlevel:
lab8219: db #C8,#A8,#02,"LEVEL NUMBER  ",#0D
LAB822A: db #CA,#A7,#02,#0D
txtlives:
LAB822F: db #CC,#A9,#00,"LIVES LEFT  ",#0D
txtgameover:
LAB823F: db #C9,#AB,#02,"GAME OVER",#0D
txtnolives:
LAB824C: db #CB,#A9,#00,"NO LIVES LEFT",#0D
txtoot:
LAB825D: db #CB,#A8,#00,"RAN OUT OF TIME",#0D
txtbonus:
LAB8270: 
db #CD,#A8,#00,#4C,#49,#56,#45,#53,#20,#4C,#45,#46,#54,#20,#42,#4F ; #8270 ...LIVES LEFT BO
db #4E,#55,#53,#CF,#A9,#00,#0D,#20,#58,#20,#32,#30,#30,#20,#50,#4F ; #8280 NUS.... X 200 PO
db "INTS",#0D 

txtname:
lab8295: db #CF,#A5,#00,"PLEASE ENTER YOUR NAME"
lab82ae: db #D0,#A6, #00,"LEFT?RIGHT TO SELECT"
		 db #D1,#A9,#00, "FIRE TO ENTER",#0D

txtpause:
lab82d5: db #D5,#AA,#00," PAUSE MODE "
         db #D6,#AA,#00,"FIRE TO PLAY",#0D

lab82f5:
db #04,#15,#02,#6B,#BF,#0A,#15,#06,#65,#BF,#18 ; ...k....e..

db #15,#03,#5F,#BF

lab8304: 
	ld ix,lab82F5
    call lab830E
    call lab830E
lab830E ld c,(ix+0)
lab8311 ld b,(ix+1)
lab8314 
	ld a,(ix+2)
    ld l,(ix+3)
    ld h,(ix+4)
    push ix
    call lab7DB7
lab8322 pop ix
    ld de,5
    add ix,de
    ret 
lab832A:

db #CD,#E5,#8E,#2A,#5D,#BF ; #8320 .}...........*].
db #7D,#D6,#01,#27,#6F,#FE,#99,#7C,#20,#04,#D6,#01,#27,#67,#FE,#99 ; #8330 }..'o..| ...'g..
db #20,#03,#21,#00,#00,#22,#5D,#BF,#C9,#2A,#5D,#BF,#7D,#C6,#20,#27 ; #8340  .!.."]..*].}. '
db #6F,#7C,#CE,#00,#27,#67,#22,#5D,#BF,#C3,#04,#83,#21,#63,#BF,#7B ; #8350 o|..'g"]....!c.{
db #86,#27,#77,#23,#7A,#46,#8E,#27,#77,#4F,#23,#7E,#CE,#00,#27,#77 ; #8360 .'w#zF.'wO#~..'w
db #30,#0B,#3E,#99,#32,#63,#BF,#32,#64,#BF,#32,#65,#BF,#78,#E6,#F0 ; #8370 0.>.2c.2d.2e.x..
db #FE,#40,#20,#08,#79,#E6,#F0,#FE,#50,#C0,#18,#07,#FE,#40,#C0,#79 ; #8380 .@ .y...P....@.y
db #E6,#F0,#C0,#3A,#AA,#90,#FE,#09,#C8,#3C,#32,#AA,#90,#C9,#2A,#5D ; #8390 ...:.....<2...*]
db #BF,#7C,#B5,#28,#1C,#CD,#2D,#83,#11,#10,#00,#CD,#5C,#83,#3A,#63 ; #83a0 .|.(..-.....\.:c
db #BF,#E6,#F0,#0F,#0F,#0F,#0F,#C6,#0A,#CD,#1C,#8F,#CD,#04,#83,#18 ; #83b0 
db #DD,#CD,#BC,#8D,#06,#64,#C5,#CD,#03,#6F,#C1,#10,#F9,#C9,#05,#AF ; #83c0 .....d...o......
db #32,#70,#BF,#3A,#6F,#BF,#B7,#28,#10,#21,#CE,#83,#35,#20,#0A,#36 ; #83d0 2p.:o..(.!..5 .6
db #05,#CD,#2A,#83,#3E,#01,#32,#70,#BF,#3A,#48,#BF,#CB,#4F,#11,#00 ; #83e0 ..*.>.2p.:H..O..
db #01,#C4,#5C,#83,#2A,#18,#BF,#CB,#7C,#20,#32,#CB,#3C,#CB,#1D,#CB ; #83f0 ..\.*...| 2.<...
db #3C,#CB,#1D,#ED,#5B,#A3,#90,#CB,#3A,#CB,#1B,#CB,#3A,#CB,#1B,#B7 ; #8400 <...[...:...:...
db #ED,#52,#38,#19,#7D,#B7,#28,#15,#ED,#5B,#18,#BF,#ED,#53,#A3,#90 ; #8410 .R8.}.(..[...S..
db #AF,#C6,#01,#27,#2D,#20,#FA,#5F,#16,#00,#CD,#5C,#83,#3E,#01,#32 ; #8420 ...'- ._...\.>.2
db #70,#BF



lab8432 ld a,(labBF48)
    bit 1,a
    jr z,lab8447
    ld a,(num_cyl)
    sub 1
    daa
    ld (num_cyl),a
    ld a,1
    ld (labBF70),a
lab8447 ld a,(labBF70)
    or a
    ret z
    jp lab8304

lab844F:
db #00,#03,#00,#05,#00,#05,#00,#03,#00,#05,#00,#06,#00,#06,#00

lab845e:
	db #06,#87 ; #8450 
db #6F,#26,#00,#11,#4F,#84,#19,#5E,#23,#56,#ED,#53,#5D,#BF,#C3,#04 ; #8460 o&..O..^#V.S]...
db #83,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #8470 
db #00,#2A,#18,#BF,#11,#00,#FF,#CB,#7C,#20,#10,#7C,#B7,#20,#05,#7D ; #8480 .*......| .|. .}
db #FE,#80,#38,#07,#01,#80,#FF,#09,#54,#5C,#1C,#ED,#53,#43,#BF,#7B ; #8490 ..8.....T\..SC.{
db #CD,#AA,#84,#E5,#7A,#CD,#AA,#84,#D1,#C9,#B7,#FA,#C9

db #84,#21,#40 ; #84a0 ....z.........!@
db #BF,#BE,#30,#15,#4F,#3A,#41,#BF,#81,#4F,#06,#00,#21,#29,#0C,#09 ; #84b0 ..0.O:A..O..!)..
db #09,#4E,#2C,#46,#21,#69,#0C,#09,#C9

db #21,#71,#84,#C9

lab84CD:
	db #21,#00,#00 
	db #22,#49,#BF,#21,#69,#0C,#7E,#23,#FE,#FF,#C8,#B7,#28,#F8,#47,#7E 
	db #FE,#04,#30,#04,#36,#04,#18,#06,#FE,#06,#20,#02,#36,#07,#23,#23 
	db #10,#ED,#18,#E2

lab84f4:
	db #3A,#AB,#90,#CD,#53,#85,#3A,#AB,#90,#5F,#3A,#00 ; #84f0 ....:...S.:.._:.
db #0C,#3D,#BB,#30,#01,#5F,#16,#00,#21,#01,#0C,#19,#7E,#32,#41,#BF ; #8500 .=.0._..!...~2A.
db #47,#21,#09,#0C,#19,#7E,#32,#40,#BF,#80,#3D,#32,#42,#BF,#21,#11 ; #8510 G!...~2@..=2B.!.
db #0C,#19,#46,#AF,#C6,#01,#27,#10,#FB,#32,#69,#BF,#C9,#21,#AB,#90 ; #8520 ..F...'..2i..!..
db #34,#3A,#00,#0C,#BE,#C0,#36,#00,#C9,#6F,#26,#00,#11,#19,#0C,#29 ; #8530 4:....6..o&....)
db #19,#5E,#23,#56,#21,#00,#0C,#19,#E5,#DD,#E1,#DD,#46,#00,#DD,#23 ; #8540 .^#V!.......F..#
db #78,#B7,#C9,#CD,#39,#85,#C8,#DD,#7E,#02,#DD,#77,#05,#DD,#6E,#00 ; #8550 x...9...~..w..n.
db #DD,#66,#01,#11,#00,#0C,#19,#23,#FE,#FF,#20,#06,#DD,#7E,#04,#77 ; #8560 .f.....#.. ..~.w
db #18,#04,#DD,#7E,#03,#77,#11,#06,#00,#DD,#19,#10,#DA,#C9,#47,#3A ; #8570 ...~.w........G:
db #A5,#90,#B7,#C0,#78,#CD,#39,#85,#C8,#CD,#94,#85,#11,#06,#00,#DD ; #8580 ....x.9.........
db #19,#10,#F6,#C9,#C5,#DD,#6E,#00,#DD,#66,#01,#11,#00,#0C,#19,#23 ; #8590 ......n..f.....#
db #DD,#7E,#05,#FE,#FF,#3E,#FC,#28,#02,#3E,#04,#86,#77,#DD,#BE,#03 ; #85a0 .~...>.(.>..w...
db #28,#02,#30,#0A,#DD,#7E,#03,#77,#DD,#36,#05,#01,#18,#0D,#DD,#BE ; #85b0 (.0..~.w.6......
db #04,#38,#08,#DD,#7E,#04,#77,#DD,#36,#05,#FF,#C1,#C9,#3A,#1A,#BF ; #85c0 .8..~.w.6....:..
db #6F,#3A,#1B,#BF,#CD,#A1,#77,#21,#45,#BE,#22,#75,#BF,#AF,#32,#44 ; #85d0 o:....w!E."u..2D
db #BE,#CD,#5B,#86,#32,#73,#BF,#CD,#81,#84,#AF,#32,#74,#BF,#3E,#08 ; #85e0 ..[.2s.....2t.>.
db #32,#71,#BF,#3E,#00,#32,#72,#BF,#CD,#97,#86,#D5,#3A,#44,#BF,#57 ; #85f0 2q.>.2r.....:D.W
db #1E,#00,#CD,#B2,#86,#E3,#3A,#43,#BF,#57,#1E,#00,#CD,#B2,#86,#EB ; #8600 ......:C.W......
db #E1,#3A,#74,#BF,#3C,#32,#74,#BF,#3A,#72,#BF,#FE,#20,#28,#17,#B7 ; #8610 .:t.<2t.:r.. (..
db #28,#08,#FE,#10,#28,#08,#3E,#20,#18,#06,#3E,#10,#18,#02,#3E,#30 ; #8620 (...(.> ..>...>0
db #32,#72,#BF,#C3,#F8,#85,#3A,#71,#BF,#C6,#10,#32,#71,#BF,#FE,#48 ; #8630 2r....:q...2q..H
db #C2,#F3,#85,#CD,#97,#86,#C9,#00,#01,#03,#02,#04,#05,#07,#06,#08 ; #8640 
db #09,#0B,#0A,#0C,#0D,#0F,#0E,#10,#10,#10,#10,#3A,#1A,#BF,#06,#00 ; #8650 ...........:....
db #FE,#08,#38,#10,#04,#FE,#18,#38,#0B,#04,#FE,#28,#38,#06,#04,#FE ; #8660 ..8....8...(8...
db #38,#38,#01,#04,#CB,#20,#CB,#20,#3A,#1B,#BF,#0E,#00,#FE,#16,#38 ; #8670 88... . :......8
db #0B,#0C,#FE,#26,#38,#06,#0C,#FE,#2A,#38,#01,#0C,#78,#81,#4F,#06 ; #8680 ...&8...*8..x.O.
db #00,#21,#47,#86,#09,#7E,#C9,#3A,#73,#BF,#47,#3A,#74,#BF,#B8,#C0 ; #8690 .!G..~.:s.G:t...
db #D5,#E5,#3A,#1A,#BF,#6F,#26,#00,#3A,#1B,#BF,#CD,#1F,#77,#E1,#D1 ; #86a0 ..:..o&.:....w..
db #C9,#00,#FD,#E5,#D5,#FD,#2A,#75,#BF,#7A,#32,#B1,#86,#EB,#ED,#4B ; #86b0 ......*u.z2....K
db #18,#BF,#B7,#ED,#42,#EB,#7E,#23,#B7,#CA,#5E,#87,#E5,#DD,#E1,#F5 ; #86c0 ....B.~#..^.....
db #DD,#E5,#D5,#DD,#6E,#01,#26,#00,#19,#7C,#B7,#20,#0B,#7D,#FE,#80 ; #86d0 ....n.&..|. .}..
db #30,#6D,#FE,#14,#38,#0F,#18,#48,#FE,#FF,#20,#63,#7D,#FE,#81,#38 ; #86e0 0m..8..H.. c}..8
db #5E,#FE,#EC,#38,#3B,#3A,#44,#BE,#FE,#14,#30,#34,#3C,#32,#44,#BE ; #86f0 ^..8;:D...04<2D.
db #DD,#E5,#C1,#FD,#71,#00,#FD,#70,#01,#DD,#7E,#01,#FD,#77,#02,#3A ; #8700 ....q..p..~..w.:
db #B1,#86,#FD,#77,#03,#3A,#71,#BF,#FD,#77,#04,#3A,#72,#BF,#FD,#77 ; #8710 ...w.:q..w.:r..w
db #05,#DD,#7E,#00,#FD,#77,#06,#FD,#36,#07,#00,#11,#0A,#00,#FD,#19 ; #8720 ..~..w..6.......
db #65,#3A,#71,#BF,#6F,#3A,#72,#BF,#DD,#4E,#00,#F5,#79,#FE,#09,#38 ; #8730 e:q.o:r..N..y..8
db #0A,#FE,#0C,#30,#06,#3A,#8C,#8C,#DD,#77,#00,#F1,#CD,#26,#75,#D1 ; #8740 ...0.:...w...&u.
db #DD,#E1,#F1,#DD,#23,#DD,#23,#3D,#C2,#CF,#86,#DD,#E5,#E1,#FD,#22 ; #8750 ....#.#=......."
db #75,#BF,#D1,#FD,#E1,#C9,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#80,#87 ; #8760 u...............
db #9B,#87,#B6,#87,#B6,#87,#D1,#87,#EC,#87,#08,#88,#28,#88,#53,#88 ; #8770 ............(.S.
db #51,#64,#79,#90,#90,#90,#90,#90,#90,#90,#90,#90,#90,#90,#90,#90 ; #8780 Qdy.............
db #90,#90,#90,#90,#90,#90,#90,#90,#79,#64,#51,#64,#64,#64,#64,#64 ; #8790 ........ydQddddd
db #64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#79,#79,#90,#90,#90,#90 ; #87a0 ddddddddddyy....
db #79,#64,#51,#31,#24,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #87b0 ydQ1$...........
db #00,#90,#40,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #87c0 ..@.............
db #00,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64 ; #87d0 .ddddddddddddddd
db #64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64 ; #87e0 dddddddddddddddd
db #64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#51,#40,#31 ; #87f0 dddddddddddddQ@1
db #00,#00,#00,#00,#00,#00,#00,#00,#64,#64,#64,#64,#64,#64,#64,#64 ; #8800 ........dddddddd
db #64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64 ; #8810 dddddddddddddddd
db #64,#64,#64,#64,#51,#40,#31,#00,#64,#64,#64,#64,#64,#64,#64,#64 ; #8820 ddddQ@1.dddddddd
db #64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64 ; #8830 dddddddddddddddd
db #64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#51 ; #8840 dddddddddddddddQ
db #40,#31,#00,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64 ; #8850 @1.ddddddddddddd
db #64,#64,#64,#51,#51,#51,#51,#40,#40,#31,#31,#24,#00,#00,#00,#00 ; #8860 dddQQQQ@@11$....
db #00,#F5,#26,#1B,#DD,#7E,#06,#FE,#0A,#20,#04,#26,#1F,#18,#06,#FE ; #8870 ..&..~... .&....
db #0B,#20,#02,#26,#2A,#F1,#DD,#96,#05,#C6,#05,#FA,#91,#88,#BC,#38 ; #8880 . .&*..........8
db #02,#AF,#C9,#4F,#06,#00,#DD,#7E,#05,#FE,#20,#38,#04,#7C,#91,#3D ; #8890 ...O...~.. 8.|.=
db #4F,#DD,#7E,#06,#FE,#0D,#30,#E9,#5F,#16,#00,#21,#66,#87,#19,#19 ; #88a0 O.~...0._..!f...
db #5E,#23,#56,#7A,#A3,#FE,#FF,#28,#D8,#EB,#09,#7E,#B7,#C9,#00,#01 ; #88b0 ^#Vz...(...~....
db #04,#09,#10,#19,#24,#31,#40,#51,#64,#79,#7A,#CD,#D5,#88,#57,#7B ; #88c0 ....$1@Qdyz...W{
db #CD,#D5,#88,#82,#C9,#CD,#49,#8A,#FE,#0B,#38,#02,#3E,#0B,#4F,#06 ; #88d0 ......I...8.>.O.
db #00,#21,#BE,#88,#09,#7E,#C9,#DD,#21,#45,#BE,#0E,#00,#3A,#44,#BE ; #88e0 .!...~..!E...:D.
db #B7,#C8,#47,#C5,#CD,#34,#89,#C1,#3E,#00,#30,#02,#3C,#0C,#DD,#77 ; #88f0 ..G..4..>.0.<..w
db #07,#11,#0A,#00,#DD,#19,#10,#EB,#79,#B7,#C8,#37,#C9,#3A,#40,#BF ; #8900 ........y..7.:@.
db #57,#1E,#00,#2A,#18,#BF,#CB,#7C,#20,#18,#B7,#ED,#52,#38,#13,#3A ; #8910 W..*...| ...R8.:
db #69,#BF,#B7,#28,#0B,#ED,#53,#18,#BF,#3E,#FA,#32,#1C,#BF,#18,#02 ; #8920 i..(..S..>.2....
db #37,#C9,#B7,#C9,#DD,#7E,#06,#FE,#04,#3F,#D0,#FE,#0D,#D0,#2A,#22 ; #8930 7....~...?....*"
db #BF,#DD,#5E,#02,#DD,#56,#03,#B7,#ED,#52,#CB,#7C,#28,#07,#EB,#21 ; #8940 ..^..V...R.|(..!
db #00,#00,#B7,#ED,#52,#7C,#B7,#C0,#7D,#FE,#0C,#D0,#57,#3A,#24,#BF ; #8950 ....R|..}...W:$.
db #DD,#96,#04,#F2,#68,#89,#ED,#44,#FE,#0C,#D0,#5F,#CD,#CA,#88,#47 ; #8960 ....h..D..._...G
db #C5,#3A,#25,#BF,#CD,#71,#88,#5F,#C1,#28,#05,#78,#BB,#D8,#37,#C8 ; #8970 .:%..q._.(.x..7.
db #B7,#C9,#CD,#FC,#89,#21,#1C,#BF,#7E,#23,#B6,#23,#B6,#28,#6C,#21 ; #8980 .....!..~#.#.(l!
db #18,#BF,#11,#22,#BF,#01,#04,#00,#ED,#B0,#AF,#32,#1E,#BF,#3A,#1E ; #8990 ...".......2..:.
db #BF,#32,#5C,#BF,#3A,#4C,#BF,#32,#1C,#BF,#3A,#4E,#BF,#32,#1D,#BF ; #89a0 .2\.:L.2..:N.2..
db #3A,#50,#BF,#32,#1E,#BF,#21,#22,#BF,#11,#58,#BF,#01,#04,#00,#ED ; #89b0 :P.2..!"..X.....
db #B0,#CD,#04,#7B,#CD,#E7,#88,#38,#21,#CD,#F1,#8A,#21,#57,#BF,#35 ; #89c0 ...{...8!...!W.5
db #CB,#7E,#28,#CA,#21,#18,#BF,#11,#22,#BF,#01,#04,#00,#ED,#B0,#AF ; #89d0 .~(.!...".......
db #32,#1C,#BF,#32,#1D,#BF,#32,#1E,#BF,#C9,#21,#58,#BF,#11,#22,#BF ; #89e0 2..2..2...!X..".
db #01,#04,#00,#ED,#B0,#3A,#5C,#BF,#32,#1E,#BF,#C9,#3A,#1C,#BF,#CD ; #89f0 .....:\.2...:...
db #49,#8A,#57,#3A,#1D,#BF,#CD,#49,#8A,#BA,#38,#01,#57,#3A,#1E,#BF ; #8a00 I.W:...I..8.W:..
db #CD,#49,#8A,#BA,#30,#01,#7A,#3D,#87,#87,#87,#32,#57,#BF,#3A,#1C ; #8a10 .I..0.z=...2W.:.
db #BF,#CD,#4E,#8A,#ED,#53,#51,#BF,#3A,#1D,#BF,#CD,#4E,#8A,#ED,#53 ; #8a20 ..N..SQ.:...N..S
db #53,#BF,#3A,#1E,#BF,#CD,#4E,#8A,#ED,#53,#55,#BF,#21,#00,#00,#22 ; #8a30 S.:...N..SU.!.."
db #4B,#BF,#22,#4D,#BF,#22,#4F,#BF,#C9,#B7,#F0,#ED,#44,#C9,#11,#00 ; #8a40 K."M."O.....D...
db #00,#B7,#C8,#F5,#CD,#49,#8A,#3D,#21,#57,#BF,#B6,#87,#5F,#16,#00 ; #8a50 .....I.=!W..._..
db #21,#71,#8A,#19,#5E,#23,#56,#F1,#F0,#21,#00,#00,#B7,#ED,#52,#EB ; #8a60 !q..^#V..!....R.
db #C9,#00,#01,#00,#02,#00,#03,#00,#04,#00,#05,#00,#06,#00,#07,#00 ; #8a70 
db #08,#80,#00,#00,#01,#80,#01,#00,#02,#80,#02,#00,#03,#80,#03,#00 ; #8a80 
db #04,#55,#00,#AB,#00,#00,#01,#55,#01,#AB,#01,#00,#02,#55,#02,#AB ; #8a90 .U.....U.....U..
db #02,#40,#00,#80,#00,#C0,#00,#00,#01,#40,#01,#80,#01,#C0,#01,#00 ; #8aa0 .@.......@......
db #02,#33,#00,#66,#00,#9A,#00,#CD,#00,#00,#01,#33,#01,#66,#01,#9A ; #8ab0 .3.f.......3.f..
db #01,#2B,#00,#55,#00,#80,#00,#AB,#00,#D5,#00,#00,#01,#2B,#01,#55 ; #8ac0 .+.U.........+.U
db #01,#25,#00,#49,#00,#6E,#00,#92,#00,#B7,#00,#DB,#00,#00,#01,#25 ; #8ad0 .%.I.n.........%
db #01,#20,#00,#40,#00,#60,#00,#80,#00,#A0,#00,#C0,#00,#E0,#00,#00 ; #8ae0 . .@.`..........
db #01,#2A,#4B,#BF,#ED,#5B,#51,#BF,#19,#22,#4B,#BF,#2A,#4D,#BF,#ED ; #8af0 .*K..[Q.."K.*M..
db #5B,#53,#BF,#19,#22,#4D,#BF,#2A,#4F,#BF,#ED,#5B,#55,#BF,#19,#22 ; #8b00 [S.."M.*O..[U.."
db #4F,#BF,#C9,#AF,#32,#45,#BF,#32,#46,#BF,#DD,#21,#45,#BE,#3A,#44 ; #8b10 O...2E.2F..!E.:D
db #BE,#B7,#C8,#F5,#DD,#CB,#07,#46,#C4,#41,#8B,#11,#0A,#00,#F1,#DD ; #8b20 .......F.A......
db #19,#3D,#20,#EF,#21,#48,#BF,#CB,#66,#C4,#3C,#8C,#C9,#16,#FB,#0A ; #8b30 .= .!H..f.<.....
db #06,#3A,#25,#BF,#57,#06,#04,#0E,#00,#21,#3D,#8B,#CB,#39,#DD,#7E ; #8b40 .:%.W....!=..9.~
db #05,#86,#F2,#56,#8B,#AF,#5F,#7A,#BB,#38,#02,#CB,#D9,#23,#10,#EC ; #8b50 ...V.._z.8...#..
db #DD,#7E,#06,#FE,#0D,#D0,#87,#5F,#16,#00,#21,#76,#8B,#19,#5E,#23 ; #8b60 .~....._..!v..^#
db #56,#D5,#21,#48,#BF,#C9,#75,#8B,#75,#8B,#75,#8B,#75,#8B,#90,#8B ; #8b70 V.!H..u.u.u.u...
db #DF,#8B,#FC,#8B,#01,#8C,#1B,#8C,#1E,#8C,#1E,#8C,#1E,#8C,#1E,#8C ; #8b80 
db #DD,#7E,#05,#FE,#20,#30,#1F,#CB,#41,#28,#38,#ED,#5B,#49,#BF,#7A ; #8b90 .~.. 0..A(8.[I.z
db #B3,#20,#0C,#DD,#5E,#00,#DD,#56,#01,#ED,#53,#49,#BF,#CB,#CE,#3E ; #8ba0 . ..^..V..SI...>
db #01,#32,#47,#BF,#18,#1D,#CB,#49,#20,#19,#ED,#5B,#49,#BF,#7A,#B3 ; #8bb0 .2G....I ..[I.z.
db #20,#0C,#DD,#5E,#00,#DD,#56,#01,#ED,#53,#49,#BF,#CB,#CE,#3E,#FF ; #8bc0  ..^..V..SI...>.
db #32,#47,#BF,#CB,#41,#20,#07,#CB,#49,#28,#03,#CD,#21,#8C,#C9,#DD ; #8bd0 2G..A ..I(..!...
db #7E,#05,#FE,#20,#30,#0B,#CB,#51,#28,#03,#CB,#C6,#C9,#CD,#21,#8C ; #8be0 ~.. 0..Q(.....!.
db #C9,#CB,#59,#20,#03,#CB,#C6,#C9,#CD,#21,#8C,#C9,#CB,#D6,#CB,#C6 ; #8bf0 ..Y .....!......
db #C9,#CB,#DE,#DD,#5E,#00,#DD,#56,#01,#3E,#06,#12,#DD,#7E,#05,#FE ; #8c00 ....^..V.>...~..
db #20,#3E,#01,#38,#02,#3E,#FF,#32,#47,#BF,#C9,#C3,#21,#8C,#CB,#C6 ; #8c10  >.8.>.2G...!...
db #C9,#CB,#E6,#3A,#22,#BF,#DD,#96,#02,#57,#3A,#24,#BF,#DD,#96,#04 ; #8c20 ...:"....W:$....
db #5F,#21,#45,#BF,#7E,#82,#77,#23,#7E,#83,#77,#C9,#06,#32,#ED,#5B ; #8c30 _!E.~.w#~.w..2.[
db #45,#BF,#7B,#B2,#28,#29,#C5,#D5,#CD,#CA,#88,#D1,#C1,#B8,#38,#1F ; #8c40 E.{.()........8.
db #28,#1D,#7B,#CD,#49,#8A,#CB,#3F,#CB,#7B,#28,#02,#ED,#44,#5F,#7A ; #8c50 (.{.I..?.{(..D_z
db #CD,#49,#8A,#CB,#3F,#CB,#7A,#28,#02,#ED,#44,#57,#C3,#42,#8C,#ED ; #8c60 .I..?.z(..DW.B..
db #53,#1C,#BF,#3E,#01,#32,#8C,#7A,#C9,#2A,#49,#BF,#7C,#B5,#C8,#35 ; #8c70 S..>.2.z.*I.|..5
db #C0,#21,#00,#00,#22,#49,#BF,#C9,#04,#00,#8D,#8C,#09,#09,#09,#09 ; #8c80 .!.."I..........
db #09,#0A,#0B,#0B,#0A,#21,#88,#8C,#35,#C0,#36,#04,#2A,#8A,#8C,#7E ; #8c90 .....!..5.6.*..~
db #32,#8C,#8C,#23,#3A,#89,#8C,#3C,#FE,#08,#20,#04,#AF,#21,#8D,#8C ; #8ca0 2..#:..<.. ..!..
db #22,#8A,#8C,#32,#89,#8C,#C9


lab8cb7: 
  db #FF,#80,#FF,#00,#FF,#01,#00,#00,#FF 
  db #87,#FF,#FF,#FF,#E1,#00,#00,#FC,#84,#00,#00,#3F,#21,#00,#00,#FF 
  db #87,#FF,#FF,#FF,#E1,#00,#00,#FF,#80,#FF,#00,#FF,#01,#00,#00

lab8cdf:
buf40:
  ds 40
  
lab8d07:
  db #02,#01,#0A,#01,#02

lab8d0c:
  db #FF,#80,#F0,#10 
  db #00,#00,#00,#00,#FF,#9F,#F0,#90,#00,#00,#00,#00,#FF,#80,#F0,#10
  db #00,#00,#00,#00
lab8D24:
  db #01,#06,#01

; copy data (40 bytes)  .. ?? 
; a=??
lab8D27 
	push af
    ld a,(lab8D56)
    ld hl,lab8D0C		; not 40 bytes
    or a
    jr z,lab8D34
    ld hl,lab8CB7
lab8D34 
	ld de,buf40
    ld bc,40
    ldir
    pop af
    ld c,a
    or a
    ret z
    
lab8D40 ld hl,buf40    
    ld b,20
    or a
    ex af,af'
    or a
    
lab8D48 ex af,af'
    rr (hl)
    inc hl
    ex af,af'
    rr (hl)
    inc hl
    djnz lab8D48
    
    dec c
    jr nz,lab8D40
    ret 
    
lab8D56 db 0

lab8d57:
    ld (lab8D56),a
    ld hl,#fffa
    call lab8DA0
    jr nc,lab8D6D
    ld a,(#BF40)
    ld h,a
    dec h
    ld l,250
    call lab8DA0
    ret c
lab8D6D push hl
    ld a,b
    call lab8D27
    exx
    pop hl
    ld bc,#1E
    exx
    ld de,lab8D24
    ld c,3
    
    ld a,(lab8D56)
    or a
    jr z,lab8D88
    
    ld de,lab8D07
    ld c,5
lab8D88:
    ld hl,lab8CDF
    ld b,8
lab8D8D push bc
    push de
    push hl
    call lab8D99
lab8D93 pop hl
    pop de
    pop bc
    djnz lab8D8D
    ret 
lab8D99 ld (#BE41),sp
    jp lab787E
lab8DA0 ld de,(#BF18)
    or a
    sbc hl,de
    ld a,(lab8D56)
    or a
    jr z,lab8DB2
    add hl,hl
    ld a,0
    jr lab8DB4
lab8DB2 ld a,32
lab8DB4 ld de,#80
    add hl,de
    call lab79D2
    ret 


lab8DBC
	ld hl,#BF38
    ld b,8
lab8DC1 ld (hl),0
    inc hl
    djnz lab8DC1
    
    ld a,255
    ld (#BF34),a
    ld a,7
    ld c,28
    call set_psg_reg

; reg8,9,a <=0
lab8DD2:
    ld a,8
    ld c,0
    call set_psg_reg
    inc a
    call set_psg_reg
    inc a
    call set_psg_reg
    ld a,40
    ld (lab9013),a
    ret 

; Table pour saut
tab_jp1:
lab8de7:
	dw lab8E3a,lab8E3f,lab8E49,lab8E4e,lab8E56,lab8E62,lab8E69,lab8E72

lab8DF7:
	db #07,#8E,#0E,#8E,#13,#8E,#2D,#8E,#32 ; #8df0 .b.i.r.......-.2
db #8E,#36,#8E,#38,#8E,#2D,#8E,#06,#82,#83,#84,#85,#86,#87,#04,#14 ; #8e00 .6.8.-..........
db #16,#18,#1A,#19,#12,#16,#16,#19,#1E,#1E,#1D,#19,#19,#16,#12,#12 ; #8e10 
db #14,#16,#16,#17,#19,#19,#1B,#1E,#1E,#1D,#19,#19,#1E,#04,#0A,#08 ; #8e20 
db #06,#04,#03,#14,#19,#14,#01,#80,#01,#0F

lab8e3a: 
 	ld a,(#90A5)
    rra 
    ret 

lab8e3f: 
	ld a,(#90A6)
    cpl 
    ld hl,#BF6F
    and (hl)
    rra 
    ret 
    
lab8e49:
	ld a,(#90A6)
    rra 
    ret 
    
lab8e4e: 
	ld hl,(#BF49)
    ld a,h
    or l
    ret z
    scf
    ret 
    
lab8e56:
	ld a,(#BF48)
    scf
    bit 2,a
    ret nz
    bit 3,a
    ret nz
    or a
    ret 
lab8e62:
    ld a,(#BF47)
    or a
    ret z
    scf
    ret 
lab8e69:
    ld a,(#BF48)
    scf
    bit 4,a
    ret nz
    or a
    ret 
lab8e72:
	or a
    ret    

; Parcours la table tab_jp1 et appelle toutes les routines
lab8e74:
	ld ix,tab_jp1
    ld b,8
    ld de,#BF38
lab8E7D:
	ld l,(ix+0)
    ld h,(ix+1)
    call jphl    
    jr nc,lab8E90
    
    ld a,(de)
    or a
    jr nz,lab8E96
    inc a
    ld (de),a
    jr lab8E96    
lab8E90
	ld a,(de)
    or a
    jr z,lab8E96
    xor a
    ld (de),a
lab8E96 inc ix
    inc ix
    inc de
    djnz lab8E7D
    
    ld a,(#BF34)
    or a
    jp p,lab8EA6
    ld a,8
lab8EA6 or a
    jr z,lab8ED0
    ld b,a
    ld hl,#BF38
    ld de,lab8DF7
    ld c,0
lab8EB2 ld a,(hl)
    cp 1
    jr z,lab8EBF
    inc hl
    inc de
    inc de
    inc c
    djnz lab8EB2
    jr lab8ED0
lab8EBF ld a,c
    ld (#BF34),a
    ex de,hl
    ld e,(hl)
    inc hl
    ld d,(hl)
    ld a,(de)
    inc de
    ld (#BF37),a
    ld (#BF35),de
lab8ED0 ld hl,#BF38
    ld b,8
lab8ED5 ld a,(hl)
    or a
    jr z,lab8EDB
    ld (hl),2
lab8EDB inc hl
    djnz lab8ED5
    ret    

jphl:
	lab8EDF jp (hl)


lab8EE0 
    ld a,31
    jp lab9014
lab8EE5 ld a,0
    jp lab9014

lab8EEA add a,a
    ld b,a
    ld a,r
    xor b
    and 31
    call lab9013+1
    ret 
    ld b,6
    jp lab7004

; Audio?
; set PSG REG = A, value = C
set_psg_reg:
lab8EFA:    
    push af
    push bc
    ld b,244
    out (c),a
    ld b,246
    in a,(c) ; ? 
    or 192
    out (c),a
    and 63
    out (c),a
    ld b,244
    out (c),c
    ld b,246
    ld c,a
    or 128
    out (c),a
    out (c),c
    pop bc
    pop af
    ret 


lab8f1c:
    push af
    ld a,8
    ld c,12
    call set_psg_reg
    inc a
    ld c,10
    call set_psg_reg
    pop af
    ld e,a
    ld d,0
    ld hl,lab8F89
    add hl,de
    add hl,de
    ld a,0
    ld c,(hl)
    inc hl
    call set_psg_reg
    inc a
    ld c,(hl)
    call set_psg_reg
    ld de,#ffcf
    add hl,de
    ld a,2
    ld c,(hl)
    inc hl
    call set_psg_reg
    inc a
    ld c,(hl)
    call set_psg_reg
    ld a,(#BF33)
    or a
    ret z
lab8F54 ld b,80
    jp lab7004
    
db #47,#06,#ED,#05,#98,#05,#47 
db #05,#FC,#04,#B4,#04,#70,#04,#31,#04,#F4,#03,#BC,#03,#86,#03,#53 ; #8f60 .....p.1.......S
db #03,#24,#03,#F6,#02,#CC,#02,#A4,#02,#7E,#02,#5A,#02,#38,#02,#18 ; #8f70 .$.......~.Z.8..
db #02,#FA,#01,#DE,#01,#C3,#01,#AA,#01,

; notes?
lab8f89:
  db #92,#01,#7B,#01,#66,#01,#52 
  db #01,#3F,#01,#2D,#01,#1C,#01,#0C,#01,#FD,#00,#EF,#00,#E1,#00,#D5 
  db #00,#C9,#00,#BE,#00,#B3,#00,#A9,#00,#9F,#00,#96,#00,#8E,#00,#86 
  db #00,#7F,#00,#77,#00,#71,#00,#6A,#00,#64,#00,#5F,#00,#59,#00,#54 
  db #00,#50,#00,#4B,#00,#47,#00,#43,#00,#3F,#00,#3C,#00,#38,#00,#35 
  db #00

lab8FD1: 
    ld a,(#BF34)
    or a
    jp p,lab8FDF
    ld a,7
    ld c,28
    jp lab8DD2
lab8FDF ld hl,(#BF35)
    ld a,(hl)
    inc hl
    ld (#BF35),hl
    cp 128
    jr z,lab8FEF
    cp 129
    jr nz,lab8FF6
lab8FEF sub 128
    call lab8EE0
    jr lab9004
lab8FF6 cp 130
    jr c,lab9001
    sub 130
    call lab8EEA
lab8FFF jr lab9004
lab9001 call lab8F1C


lab9004: 
  	ld hl,#BF37
    dec (hl)
    ret nz
    ld a,255
    ld (#BF34),a
    ret 
    
lab900F: db #00,#00,#00,#00 
lab9013: db #28
 
 lab9014: 
 	ld c,a
    ld a,6
    call set_psg_reg
    
    ld a,10
    ld c,0
    call set_psg_reg
	ld a,(lab9013)
    ld c,a
    ld a,11
    call set_psg_reg
    inc a
    ld c,0
    call set_psg_reg
    inc a
    ld c,15
    call set_psg_reg
    ld a,10
    ld c,16
    call set_psg_reg
    ret 

LAB903E:
  db #CD,#BC ; #9030 <.....>.........
  db #8D,#3E,#28,#32,#13,#90,#3E,#01,#32,#33,#BF,#21,#01,#05,#22,#0F ; #9040 .>(2..>.23.!..".
  db #90,#3A,#00,#05,#32,#11,#90,#AF,#32,#12,#90,#18,#07,#AF,#32,#33 ; #9050 .:..2...2.....23
  db #BF,#C3,#BC,#8D,#CD,#E1,#6F,#20,#F4,#2A,#0F,#90,#3A,#12,#90,#B7 ; #9060 ......o .*..:...
  db #28,#13,#CB,#7E,#3E,#08,#C4,#14,#90,#7E,#E6,#7F,#FE,#24,#30,#05 ; #9070 (..~>....~...$0.
  db #CD,#1C,#8F,#18,#03,#CD,#54,#8F,#21,#12,#90,#34,#7E,#FE,#02,#20 ; #9080 ......T.!..4~.. 
  db #D3,#36,#00,#2A,#0F,#90,#23,#22,#0F,#90,#21,#11,#90,#35,#28,#A6 ; #9090 .6.*..#"..!..5(.
  db #18,#C2,#00,#00,#00,#00,#00,#00


lab90A8: db #00,#00

lab90AA: 
num_lives: db #00

lab90AB: db #00
lab90AC: db #00,#00,#00
lab90aF: db #00 

; palette black white white red (lvl1)
lab90b0: db #14
lab90b1: db #0B
lab90b2: db #0B
lab90b3: db #1C


lab90b4:
	ld a,#0a
    jr lab90BA

lab90B8:
	ld a,#0b

lab90BA:

    ld (lab90B1),a
; set palette
    ld b,#7f
    ld d,#40 ; set color
    ld c,0
    out (c),c
    ld a,(lab90B0)
    or d
    out (c),a
    inc c
    out (c),c
    ld a,(lab90B1)
    or d
    out (c),a
    inc c
    out (c),c
    ld a,(lab90B2)
    or d
    out (c),a
    inc c
    out (c),c
    ld a,(lab90B3)
    or d
    out (c),a
    
    ; border
    ld c,16
    out (c),c
    ld a,(lab90B0)
    or d
    out (c),a
lab90F0 ret 

lab90F1 
set_col3:
	ld b,#7f
    ld c,3
    out (c),c
    or #40
    out (c),a
    ret 


start_menu:

	lab90FC ld (lab90A8),sp
    ld hl,work_buffer
    ld de,work_buffer+1
    ld bc,#01FF
    ld (hl),0
    ldir

lab910D 
	; Title screen
	xor a
    ld (lab90AF),a        
    call title_screen
    
    ; Start Game
    ld a,1
    ld (lab90AF),a
    ld a,4			; Lives
    ld (num_lives),a

	ld a,(lab90AC)
    ld (lab90AB),a
    ld hl,0
    ld (#BF63),hl
    ld (#BF65),hl   

main_loop:    
lab912D 
	call lab84CD
    call lab84F4
    ld a,(lab90AB)
    call lab845E+1
    xor a
    ld (#BF6F),a
    ld a,1
    jr lab9146
lab9141 xor a
    ld (#BF6F),a
    xor a
lab9146 call lab7F74
    call lab9174
    jr c,lab9156
    call #839E
    call #852D
    jr lab912D
    
lab9156:
	ld hl,(timer_cnt)
    ld a,h
    or l
    ld a,1
    jr z,lab916C
	; Time out
	ld hl,num_lives
    dec (hl)
    ld a,0
    jr z,lab916C
    call #8349
    jr lab9141
lab916C call #7FDD
    call #8100
    jr lab910D
lab9174 call #7A50
    xor a
    ld (#771E),a
    call #7E26
    call #7035
    call #8DBC
    xor a
    ld (#BF48),a
    xor a
    ld (#90A6),a
    call #7071
    call #90B8
lab9192 ld hl,#90A2
    ld a,(hl)
    cpl 
    ld (hl),a
    call draw_checkboard
    ld a,0
    call lab8D57
   
lab91A0 call #85CD
    ld a,1
    call lab8D57
    call #8C95
    call lab7044	; wait vbl+render to screen 
    call lab8FD1
    call #819F
    ld a,(lab90AB)
    call #857E
    call #6F18
    ld hl,(#BF18)
    ld (#90AD),hl
    call #7BB9
    ld a,(#BF19)
    add a,a
    jr c,lab91DA
    ld a,(#90A5)
    ld hl,#90A6
    or (hl)
    jr nz,lab91DA
    ld a,1
    ld (#BF6F),a
lab91DA call #8C79
    ld a,(#BF48)
    bit 3,a
    call nz,#8349
    ld a,(#90A5)
    or a
    jr z,lab91FF
    ld a,(#90A7)
    cp 4
    jr c,lab9207
    add a,a
    ld b,a
    ld a,(lab90B3)
    add a,b
    and 31
    call lab90F1
    jr lab920A
lab91FF ld a,(#90A6)
    or a
    jr z,lab922D
    jr lab9207
lab9207 call lab90B8
lab920A ld a,1
    ld (#7A8C),a
    ld a,(#771E)
lab9212 or a
    jr z,lab921D
    cp 4
    jr z,lab921D
    inc a
    ld (#771E),a
lab921D ld hl,#90A7
    dec (hl)
    jr nz,lab924E
    call lab90B8
    ld a,(#90A6)
    or a
    ret nz
    scf
    ret 
    
lab922D ld a,(labBF48)
    bit 0,a
    jr nz,lab923B
    ld hl,(#BF5D)
    ld a,h
    or l
    jr nz,lab924E
lab923B ld a,1
    ld (#90A5),a
    xor a
    ld (#BF6F),a
    ld a,1
    ld (#771E),a
    ld a,9
    ld (#90A7),a
lab924E call #83CF
    ld a,(#90A6)
    or a
    jr nz,lab9270
    call #890D
    jp nc,lab9270
    xor a
    ld (#BF6F),a
    ld a,1
    ld (#90A6),a
    ld a,20
    ld (#90A7),a
    ld a,1
    ld (#7A8C),a
lab9270 ld hl,(#90AD)
    ld de,(#BF18)
    or a
    sbc hl,de
    ld a,l
    call lab74B9
    jp lab9192
lab9281:

db #3E,#17,#CD,#8E,#6F,#28,#10,#3E,#15,#CD,#8E,#6F,#28,#09,#06 
db #7F,#3E,#89,#ED,#79,#C3,#00,#00

lab9298:
ld sp,(lab90A8)
    call lab8304
    jp lab90FC
    ld a,l
    ld hl,lab822F
    call lab7D2E

	ld a,(num_lives)
    add a,48
    call lab7C5C
    ld e,0
    ld d,1
    call lab6FF5
    ld b,25
lab92BA push bc
    call waitvbl
    call waitvbl
    ld a,(lab7F73)
    or a
    jr z,lab92D0
    push de
    ld a,e
    call lab8F1C
    pop de
    ld a,e
    add a,d
    ld e,a
lab92D0 push de
    call lab6FE1
    pop de
    pop bc
    jr nz,lab92DA
    djnz lab92BA
lab92DA jp lab8DBC

lab92DD:
db #F5,#CD,#B4 ; #92d0 ...o.. .........
db #90,#CD,#26,#7E,#CD,#52,#7E,#21,#3F,#82,#CD,#2E,#7D,#F1,#B7,#28 ; #92e0 ..&~.R~!?...}..(
db #41,#21,#5D,#82,#CD,#2E,#7D,#3A,#AA,#90,#B7,#28,#3B,#FE,#0A,#38 ; #92f0 A!]...}:...(;..8
db #02,#3E,#09,#F5,#21,#70,#82,#CD,#2E,#7D,#F1,#F5,#C6,#30,#CD,#5C ; #9300 .>..!p...}...0.\
db #7C,#CD,#2E,#7D,#F1,#87,#F5,#C6,#14,#CD,#1C,#8F,#11,#00,#01,#CD ; #9310 |..}............
db #5C,#83,#CD,#04,#83,#06,#04,#CD,#03,#6F,#10,#FB,#F1,#3D,#20,#E6 ; #9320 \........o...= .
db #18,#06,#21,#4C,#82,#CD,#2E,#7D,#3E,#01,#32,#73,#7F,#1E,#19,#16 ; #9330 ..!L...}>.2s....
db #FF,#C3,#B5,#7F,#00,#00,#01,#4A,#4D,#50,#00,#50,#00,#5D,#20,#20 ; #9340 .......JMP.P.]  
db #00,#30,#00,#5D,#20,#20,#00,#20,#00,#5D,#20,#20,#00,#10,#00,#5D ; #9350 .0.]  . .]  ...]
db #20,#20,#21,#46,#80,#3E,#00,#F5,#5F,#C6,#08,#47,#0E,#09,#3E,#02 ; #9360   !F.>.._..G..>.
db #CD,#5C,#7C,#E5,#3E,#06,#CD,#AC,#7D,#3E,#B2,#CD,#5C,#7C,#E1,#23 ; #9370 .\|.>...}>..\|.#
db #06,#03,#7E,#CD,#5C,#7C,#23,#10,#F9,#23,#23,#F1,#3C,#FE,#05,#20 ; #9380 ..~.\|#..##.<.. 
db #D6,#C9,#00,#00,#00,#20,#20,#20,#FF,#00,#21,#63,#BF,#11,#92,#80 ; #9390 .....   ..!c....
db #01,#03,#00,#ED,#B0,#3E,#41,#12,#13,#3E,#20,#12,#13,#12,#CD,#C0 ; #93a0 .....>A..> .....
db #80,#D0,#32,#99,#80,#FE,#04,#3F,#D8,#CD,#C0,#80,#38,#FB,#37,#C9 ; #93b0 ..2....?....8.7.
db #21,#44,#80,#DD,#21,#92,#80,#06,#05,#7E,#23,#DD,#96,#00,#27,#7E ; #93c0 !D..!....~#...'~
db #23,#DD,#9E,#01,#27,#7E,#23,#DD,#9E,#02,#27,#30,#18,#2B,#2B,#2B ; #93d0 #...'~#...'0.+++
db #0E,#06,#56,#DD,#7E,#00,#77,#DD,#72,#00,#DD,#23,#23,#0D,#20,#F2 ; #93e0 ..V.~.w.r..##. .
db #3E,#05,#90,#37,#C9,#23,#23,#23,#10,#CF,#B7,#C9,#00,#00,#00,#00 ; #93f0 >..7.###........
db #CD,#9A,#80,#D0,#CD,#B4,#90,#CD,#26,#7E,#CD,#52,#7E,#CD,#62,#80 ; #9400 ........&~.R~.b.
db #3A,#99,#80,#6F,#87,#85,#87,#6F,#26,#00,#11,#47,#80,#19,#22,#FD ; #9410 :..o...o&..G..".
db #80,#EB,#3E,#03,#32,#FF,#80,#21,#95,#82,#CD,#2E,#7D,#CD,#F5,#6F ; #9420 ..>.2..!....}..o
db #AF,#32,#FC,#80,#06,#0A,#C5,#CD,#03,#6F,#C1,#10,#F9,#3E,#02,#CD ; #9430 .2.......o...>..
db #5C,#7C,#3A,#99,#80,#C6,#C8,#CD,#5C,#7C,#3E,#B2,#CD,#5C,#7C,#2A ; #9440 \|:.....\|>..\|*
db #FD,#80,#06,#03,#7E,#CD,#5C,#7C,#23,#10,#F9,#D5,#CD,#18,#6F,#D1 ; #9450 ....~.\|#.....o.
db #3A,#11,#BF,#B7,#28,#16,#3A,#FC,#80,#B7,#20,#C8,#3E,#01,#32,#FC ; #9460 :...(.:... .>.2.
db #80,#13,#21,#FF,#80,#35,#C8,#3E,#41,#12,#18,#B8,#AF,#32,#FC,#80 ; #9470 ..!..5.>A....2..
db #3A,#13,#BF,#B7,#28,#AE,#F2,#93,#81,#1A,#3D,#FE,#41,#30,#0C,#3E ; #9480 :...(.....=.A0.>
db #5A,#18,#08,#1A,#3C,#FE,#5B,#38,#02,#3E,#41,#12,#C3,#34,#81,#3E ; #9490 Z...<.[8.>A..4.>
db #12,#CD,#8E,#6F,#C8,#21,#D6,#82,#CD,#2E,#7D,#CD,#BC,#8D,#CD,#18 ; #94a0 ...o.!....}.....
db #6F,#3A,#11,#BF,#B7,#28,#F7,#C3,#04,#83,#C3,#A9,#01,#21,#03,#22 ; #94b0 o:...(.......!."
db #23,#24,#25,#26,#27,#28,#29,#2A,#2B,#2C,#2D,#01,#2E,#C4,#A9,#01 ; #94c0 #$%&'()*+,-.....
db #2F,#03,#3A,#3B,#3C,#3D,#3E,#73,#74,#75,#76,#77,#78,#79,#01,#7A ; #94d0 /.:;<=>stuvwxy.z
db #C5,#A9,#01,#7B,#7C,#7C,#7C,#7C,#7C,#7C,#7C,#7C,#7C,#7C,#7C,#7C ; #94e0 ...{||||||||||||
; Duplicated data ?
db #7D,#0D,#CF,#A7,#00,#50,#52,#45,#53,#53,#20,#46,#49,#52,#45,#20 ; #94f0 }....PRESS FIRE 
db #54,#4F,#20,#50,#4C,#41,#59,#0D,#D1,#A9,#02,#53,#54,#41,#52,#54 ; #9500 TO PLAY....START
db #20,#4C,#45,#56,#45,#4C,#20,#20,#0D,#C8,#A8,#02,#4C,#45,#56,#45 ; #9510  LEVEL  ....LEVE
db #4C,#20,#4E,#55,#4D,#42,#45,#52,#20,#20,#0D,#CA,#A7,#02,#0D,#CC ; #9520 L NUMBER  ......
db #A9,#00,#4C,#49,#56,#45,#53,#20,#4C,#45,#46,#54,#20,#20,#0D,#C9 ; #9530 ..LIVES LEFT  ..
db #AB,#02,#47,#41,#4D,#45,#20,#4F,#56,#45,#52,#0D,#CB,#A9,#00,#4E ; #9540 ..GAME OVER....N
db #4F,#20,#4C,#49,#56,#45,#53,#20,#4C,#45,#46,#54,#0D,#CB,#A8,#00 ; #9550 O LIVES LEFT....
db #52,#41,#4E,#20,#4F,#55,#54,#20,#4F,#46,#20,#54,#49,#4D,#45,#0D ; #9560 RAN OUT OF TIME.
db #CD,#A8,#00,#4C,#49,#56,#45,#53,#20,#4C,#45,#46,#54,#20,#42,#4F ; #9570 ...LIVES LEFT BO
db #4E,#55,#53,#CF,#A9,#00,#0D,#20,#58,#20,#32,#30,#30,#20,#50,#4F ; #9580 NUS.... X 200 PO
db #49,#4E,#54,#53,#0D,#CF,#A5,#00,#50,#4C,#45,#41,#53,#45,#20,#45 ; #9590 INTS....PLEASE E
db #4E,#54,#45,#52,#20,#59,#4F,#55,#52,#20,#4E,#41,#4D,#45,#D0,#A6 ; #95a0 NTER YOUR NAME..
db #00,#4C,#45,#46,#54,#3F,#52,#49,#47,#48,#54,#20,#54,#4F,#20,#53 ; #95b0 .LEFT?RIGHT TO S
db #45,#4C,#45,#43,#54,#D1,#A9,#00,#46,#49,#52,#45,#20,#54,#4F,#20 ; #95c0 ELECT...FIRE TO 
db #45,#4E,#54,#45,#52,#0D,#D5,#AA,#00,#20,#50,#41,#55,#53,#45,#20 ; #95d0 ENTER.... PAUSE 
db #4D,#4F,#44,#45,#20,#D6,#AA,#00,#46,#49,#52,#45,#20,#54,#4F,#20 ; #95e0 MODE ...FIRE TO 
db #50,#4C,#41,#59,#0D,#04,#15,#02,#6B,#BF,#0A,#15,#06,#65,#BF,#18 ; #95f0 PLAY....k....e..
db #15,#03,#5F,#BF,#DD,#21,#F5,#82,#CD,#0E,#83,#CD,#0E,#83,#DD,#4E ; #9600 .._..!.........N
db #00,#DD,#46,#01,#DD,#7E,#02,#DD,#6E,#03,#DD,#66,#04,#DD,#E5,#CD ; #9610 ..F..~..n..f....
db #B7,#7D,#DD,#E1,#11,#05,#00,#DD,#19,#C9,#CD,#E5,#8E,#2A,#5D,#BF ; #9620 .}...........*].
db #7D,#D6,#01,#27,#6F,#FE,#99,#7C,#20,#04,#D6,#01,#27,#67,#FE,#99 ; #9630 }..'o..| ...'g..
db #20,#03,#21,#00,#00,#22,#5D,#BF,#C9,#2A,#5D,#BF,#7D,#C6,#20,#27 ; #9640  .!.."]..*].}. '
db #6F,#7C,#CE,#00,#27,#67,#22,#5D,#BF,#C3,#04,#83,#21,#63,#BF,#7B ; #9650 o|..'g"]....!c.{
db #86,#27,#77,#23,#7A,#46,#8E,#27,#77,#4F,#23,#7E,#CE,#00,#27,#77 ; #9660 .'w#zF.'wO#~..'w
db #30,#0B,#3E,#99,#32,#63,#BF,#32,#64,#BF,#32,#65,#BF,#78,#E6,#F0 ; #9670 0.>.2c.2d.2e.x..
db #FE,#40,#20,#08,#79,#E6,#F0,#FE,#50,#C0,#18,#07,#FE,#40,#C0,#79 ; #9680 .@ .y...P....@.y
db #E6,#F0,#C0,#3A,#AA,#90,#FE,#09,#C8,#3C,#32,#AA,#90,#C9,#2A,#5D ; #9690 ...:.....<2...*]
db #BF,#7C,#B5,#28,#1C,#CD,#2D,#83,#11,#10,#00,#CD,#5C,#83,#3A,#63 ; #96a0 .|.(..-.....\.:c
db #BF,#E6,#F0,#0F,#0F,#0F,#0F,#C6,#0A,#CD,#1C,#8F,#CD,#04,#83,#18 ; #96b0 
db #DD,#CD,#BC,#8D,#06,#64,#C5,#CD,#03,#6F,#C1,#10,#F9,#C9,#05,#AF ; #96c0 .....d...o......
db #32,#70,#BF,#3A,#6F,#BF,#B7,#28,#10,#21,#CE,#83,#35,#20,#0A,#36 ; #96d0 2p.:o..(.!..5 .6
db #05,#CD,#2A,#83,#3E,#01,#32,#70,#BF,#3A,#48,#BF,#CB,#4F,#11,#00 ; #96e0 ..*.>.2p.:H..O..
db #01,#C4,#5C,#83,#2A,#18,#BF,#CB,#7C,#20,#32,#CB,#3C,#CB,#1D,#CB ; #96f0 ..\.*...| 2.<...
db #3C,#CB,#1D,#ED,#5B,#A3,#90,#CB,#3A,#CB,#1B,#CB,#3A,#CB,#1B,#B7 ; #9700 <...[...:...:...
db #ED,#52,#38,#19,#7D,#B7,#28,#15,#ED,#5B,#18,#BF,#ED,#53,#A3,#90 ; #9710 .R8.}.(..[...S..
db #AF,#C6,#01,#27,#2D,#20,#FA,#5F,#16,#00,#CD,#5C,#83,#3E,#01,#32 ; #9720 ...'- ._...\.>.2
db #70,#BF,#3A,#48,#BF,#CB,#4F,#28,#0E,#3A,#69,#BF,#D6,#01,#27,#32 ; #9730 p.:H..O(.:i...'2
db #69,#BF,#3E,#01,#32,#70,#BF,#3A,#70,#BF,#B7,#C8,#C3,#04,#83,#00 ; #9740 i.>.2p.:p.......
db #03,#00,#05,#00,#05,#00,#03,#00,#05,#00,#06,#00,#06,#00,#06,#87 ; #9750 
db #6F,#26,#00,#11,#4F,#84,#19,#5E,#23,#56,#ED,#53,#5D,#BF,#C3,#04 ; #9760 o&..O..^#V.S]...
db #83,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #9770 
db #00,#2A,#18,#BF,#11,#00,#FF,#CB,#7C,#20,#10,#7C,#B7,#20,#05,#7D ; #9780 .*......| .|. .}
db #FE,#80,#38,#07,#01,#80,#FF,#09,#54,#5C,#1C,#ED,#53,#43,#BF,#7B ; #9790 ..8.....T\..SC.{
db #CD,#AA,#84,#E5,#7A,#CD,#AA,#84,#D1,#C9,#B7,#FA,#C9,#84,#21,#40 ; #97a0 ....z.........!@
db #BF,#BE,#30,#15,#4F,#3A,#41,#BF,#81,#4F,#06,#00,#21,#29,#0C,#09 ; #97b0 ..0.O:A..O..!)..
db #09,#4E,#2C,#46,#21,#69,#0C,#09,#C9,#21,#71,#84,#C9,#21,#00,#00 ; #97c0 .N,F!i...!q..!..
db #22,#49,#BF,#21,#69,#0C,#7E,#23,#FE,#FF,#C8,#B7,#28,#F8,#47,#7E ; #97d0 "I.!i.~#....(.G~
db #FE,#04,#30,#04,#36,#04,#18,#06,#FE,#06,#20,#02,#36,#07,#23,#23 ; #97e0 ..0.6..... .6.##
db #10,#ED,#18,#E2,#3A,#AB,#90,#CD,#53,#85,#3A,#AB,#90,#5F,#3A,#00 ; #97f0 ....:...S.:.._:.
db #0C,#3D,#BB,#30,#01,#5F,#16,#00,#21,#01,#0C,#19,#7E,#32,#41,#BF ; #9800 .=.0._..!...~2A.
db #47,#21,#09,#0C,#19,#7E,#32,#40,#BF,#80,#3D,#32,#42,#BF,#21,#11 ; #9810 G!...~2@..=2B.!.
db #0C,#19,#46,#AF,#C6,#01,#27,#10,#FB,#32,#69,#BF,#C9,#21,#AB,#90 ; #9820 ..F...'..2i..!..
db #34,#3A,#00,#0C,#BE,#C0,#36,#00,#C9,#6F,#26,#00,#11,#19,#0C,#29 ; #9830 4:....6..o&....)
db #19,#5E,#23,#56,#21,#00,#0C,#19,#E5,#DD,#E1,#DD,#46,#00,#DD,#23 ; #9840 .^#V!.......F..#
db #78,#B7,#C9,#CD,#39,#85,#C8,#DD,#7E,#02,#DD,#77,#05,#DD,#6E,#00 ; #9850 x...9...~..w..n.
db #DD,#66,#01,#11,#00,#0C,#19,#23,#FE,#FF,#20,#06,#DD,#7E,#04,#77 ; #9860 .f.....#.. ..~.w
db #18,#04,#DD,#7E,#03,#77,#11,#06,#00,#DD,#19,#10,#DA,#C9,#47,#3A ; #9870 ...~.w........G:
db #A5,#90,#B7,#C0,#78,#CD,#39,#85,#C8,#CD,#94,#85,#11,#06,#00,#DD ; #9880 ....x.9.........
db #19,#10,#F6,#C9,#C5,#DD,#6E,#00,#DD,#66,#01,#11,#00,#0C,#19,#23 ; #9890 ......n..f.....#
db #DD,#7E,#05,#FE,#FF,#3E,#FC,#28,#02,#3E,#04,#86,#77,#DD,#BE,#03 ; #98a0 .~...>.(.>..w...
db #28,#02,#30,#0A,#DD,#7E,#03,#77,#DD,#36,#05,#01,#18,#0D,#DD,#BE ; #98b0 (.0..~.w.6......
db #04,#38,#08,#DD,#7E,#04,#77,#DD,#36,#05,#FF,#C1,#C9,#3A,#1A,#BF ; #98c0 .8..~.w.6....:..
db #6F,#3A,#1B,#BF,#CD,#A1,#77,#21,#45,#BE,#22,#75,#BF,#AF,#32,#44 ; #98d0 o:....w!E."u..2D
db #BE,#CD,#5B,#86,#32,#73,#BF,#CD,#81,#84,#AF,#32,#74,#BF,#3E,#08 ; #98e0 ..[.2s.....2t.>.
db #32,#71,#BF,#3E,#00,#32,#72,#BF,#CD,#97,#86,#D5,#3A,#44,#BF,#57 ; #98f0 2q.>.2r.....:D.W
db #1E,#00,#CD,#B2,#86,#E3,#3A,#43,#BF,#57,#1E,#00,#CD,#B2,#86,#EB ; #9900 ......:C.W......
db #E1,#3A,#74,#BF,#3C,#32,#74,#BF,#3A,#72,#BF,#FE,#20,#28,#17,#B7 ; #9910 .:t.<2t.:r.. (..
db #28,#08,#FE,#10,#28,#08,#3E,#20,#18,#06,#3E,#10,#18,#02,#3E,#30 ; #9920 (...(.> ..>...>0
db #32,#72,#BF,#C3,#F8,#85,#3A,#71,#BF,#C6,#10,#32,#71,#BF,#FE,#48 ; #9930 2r....:q...2q..H
db #C2,#F3,#85,#CD,#97,#86,#C9,#00,#01,#03,#02,#04,#05,#07,#06,#08 ; #9940 
db #09,#0B,#0A,#0C,#0D,#0F,#0E,#10,#10,#10,#10,#3A,#1A,#BF,#06,#00 ; #9950 ...........:....
db #FE,#08,#38,#10,#04,#FE,#18,#38,#0B,#04,#FE,#28,#38,#06,#04,#FE ; #9960 ..8....8...(8...
db #38,#38,#01,#04,#CB,#20,#CB,#20,#3A,#1B,#BF,#0E,#00,#FE,#16,#38 ; #9970 88... . :......8
db #0B,#0C,#FE,#26,#38,#06,#0C,#FE,#2A,#38,#01,#0C,#78,#81,#4F,#06 ; #9980 ...&8...*8..x.O.
db #00,#21,#47,#86,#09,#7E,#C9,#3A,#73,#BF,#47,#3A,#74,#BF,#B8,#C0 ; #9990 .!G..~.:s.G:t...
db #D5,#E5,#3A,#1A,#BF,#6F,#26,#00,#3A,#1B,#BF,#CD,#1F,#77,#E1,#D1 ; #99a0 ..:..o&.:....w..
db #C9,#00,#FD,#E5,#D5,#FD,#2A,#75,#BF,#7A,#32,#B1,#86,#EB,#ED,#4B ; #99b0 ......*u.z2....K
db #18,#BF,#B7,#ED,#42,#EB,#7E,#23,#B7,#CA,#5E,#87,#E5,#DD,#E1,#F5 ; #99c0 ....B.~#..^.....
db #DD,#E5,#D5,#DD,#6E,#01,#26,#00,#19,#7C,#B7,#20,#0B,#7D,#FE,#80 ; #99d0 ....n.&..|. .}..
db #30,#6D,#FE,#14,#38,#0F,#18,#48,#FE,#FF,#20,#63,#7D,#FE,#81,#38 ; #99e0 0m..8..H.. c}..8
db #5E,#FE,#EC,#38,#3B,#3A,#44,#BE,#FE,#14,#30,#34,#3C,#32,#44,#BE ; #99f0 ^..8;:D...04<2D.
db #DD,#E5,#C1,#FD,#71,#00,#FD,#70,#01,#DD,#7E,#01,#FD,#77,#02,#3A ; #9a00 ....q..p..~..w.:
db #B1,#86,#FD,#77,#03,#3A,#71,#BF,#FD,#77,#04,#3A,#72,#BF,#FD,#77 ; #9a10 ...w.:q..w.:r..w
db #05,#DD,#7E,#00,#FD,#77,#06,#FD,#36,#07,#00,#11,#0A,#00,#FD,#19 ; #9a20 ..~..w..6.......
db #65,#3A,#71,#BF,#6F,#3A,#72,#BF,#DD,#4E,#00,#F5,#79,#FE,#09,#38 ; #9a30 e:q.o:r..N..y..8
db #0A,#FE,#0C,#30,#06,#3A,#8C,#8C,#DD,#77,#00,#F1,#CD,#26,#75,#D1 ; #9a40 ...0.:...w...&u.
db #DD,#E1,#F1,#DD,#23,#DD,#23,#3D,#C2,#CF,#86,#DD,#E5,#E1,#FD,#22 ; #9a50 ....#.#=......."
db #75,#BF,#D1,#FD,#E1,#C9,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#80,#87 ; #9a60 u...............
db #9B,#87,#B6,#87,#B6,#87,#D1,#87,#EC,#87,#08,#88,#28,#88,#53,#88 ; #9a70 ............(.S.
db #51,#64,#79,#90,#90,#90,#90,#90,#90,#90,#90,#90,#90,#90,#90,#90 ; #9a80 Qdy.............
db #90,#90,#90,#90,#90,#90,#90,#90,#79,#64,#51,#64,#64,#64,#64,#64 ; #9a90 ........ydQddddd
db #64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#79,#79,#90,#90,#90,#90 ; #9aa0 ddddddddddyy....
db #79,#64,#51,#31,#24,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #9ab0 ydQ1$...........
db #00,#90,#40,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #9ac0 ..@.............
db #00,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64 ; #9ad0 .ddddddddddddddd
db #64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64 ; #9ae0 dddddddddddddddd
db #64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#51,#40,#31 ; #9af0 dddddddddddddQ@1
db #00,#00,#00,#00,#00,#00,#00,#00,#64,#64,#64,#64,#64,#64,#64,#64 ; #9b00 ........dddddddd
db #64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64 ; #9b10 dddddddddddddddd
db #64,#64,#64,#64,#51,#40,#31,#00,#64,#64,#64,#64,#64,#64,#64,#64 ; #9b20 ddddQ@1.dddddddd
db #64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64 ; #9b30 dddddddddddddddd
db #64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#51 ; #9b40 dddddddddddddddQ
db #40,#31,#00,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64,#64 ; #9b50 @1.ddddddddddddd
db #64,#64,#64,#51,#51,#51,#51,#40,#40,#31,#31,#24,#00,#00,#00,#00 ; #9b60 dddQQQQ@@11$....
db #00,#F5,#26,#1B,#DD,#7E,#06,#FE,#0A,#20,#04,#26,#1F,#18,#06,#FE ; #9b70 ..&..~... .&....
db #0B,#20,#02,#26,#2A,#F1,#DD,#96,#05,#C6,#05,#FA,#91,#88,#BC,#38 ; #9b80 . .&*..........8
db #02,#AF,#C9,#4F,#06,#00,#DD,#7E,#05,#FE,#20,#38,#04,#7C,#91,#3D ; #9b90 ...O...~.. 8.|.=
db #4F,#DD,#7E,#06,#FE,#0D,#30,#E9,#5F,#16,#00,#21,#66,#87,#19,#19 ; #9ba0 O.~...0._..!f...
db #5E,#23,#56,#7A,#A3,#FE,#FF,#28,#D8,#EB,#09,#7E,#B7,#C9,#00,#01 ; #9bb0 ^#Vz...(...~....
db #04,#09,#10,#19,#24,#31,#40,#51,#64,#79,#7A,#CD,#D5,#88,#57,#7B ; #9bc0 ....$1@Qdyz...W{
db #CD,#D5,#88,#82,#C9,#CD,#49,#8A,#FE,#0B,#38,#02,#3E,#0B,#4F,#06 ; #9bd0 ......I...8.>.O.
db #00,#21,#BE,#88,#09,#7E,#C9,#DD,#21,#45,#BE,#0E,#00,#3A,#44,#BE ; #9be0 .!...~..!E...:D.
db #B7,#C8,#47,#C5,#CD,#34,#89,#C1,#3E,#00,#30,#02,#3C,#0C,#DD,#77 ; #9bf0 ..G..4..>.0.<..w
db #07,#11,#0A,#00,#DD,#19,#10,#EB,#79,#B7,#C8,#37,#C9,#3A,#40,#BF ; #9c00 ........y..7.:@.
db #57,#1E,#00,#2A,#18,#BF,#CB,#7C,#20,#18,#B7,#ED,#52,#38,#13,#3A ; #9c10 W..*...| ...R8.:
db #69,#BF,#B7,#28,#0B,#ED,#53,#18,#BF,#3E,#FA,#32,#1C,#BF,#18,#02 ; #9c20 i..(..S..>.2....
db #37,#C9,#B7,#C9,#DD,#7E,#06,#FE,#04,#3F,#D0,#FE,#0D,#D0,#2A,#22 ; #9c30 7....~...?....*"
db #BF,#DD,#5E,#02,#DD,#56,#03,#B7,#ED,#52,#CB,#7C,#28,#07,#EB,#21 ; #9c40 ..^..V...R.|(..!
db #00,#00,#B7,#ED,#52,#7C,#B7,#C0,#7D,#FE,#0C,#D0,#57,#3A,#24,#BF ; #9c50 ....R|..}...W:$.
db #DD,#96,#04,#F2,#68,#89,#ED,#44,#FE,#0C,#D0,#5F,#CD,#CA,#88,#47 ; #9c60 ....h..D..._...G
db #C5,#3A,#25,#BF,#CD,#71,#88,#5F,#C1,#28,#05,#78,#BB,#D8,#37,#C8 ; #9c70 .:%..q._.(.x..7.
db #B7,#C9,#CD,#FC,#89,#21,#1C,#BF,#7E,#23,#B6,#23,#B6,#28,#6C,#21 ; #9c80 .....!..~#.#.(l!
db #18,#BF,#11,#22,#BF,#01,#04,#00,#ED,#B0,#AF,#32,#1E,#BF,#3A,#1E ; #9c90 ...".......2..:.
db #BF,#32,#5C,#BF,#3A,#4C,#BF,#32,#1C,#BF,#3A,#4E,#BF,#32,#1D,#BF ; #9ca0 .2\.:L.2..:N.2..
db #3A,#50,#BF,#32,#1E,#BF,#21,#22,#BF,#11,#58,#BF,#01,#04,#00,#ED ; #9cb0 :P.2..!"..X.....
db #B0,#CD,#04,#7B,#CD,#E7,#88,#38,#21,#CD,#F1,#8A,#21,#57,#BF,#35 ; #9cc0 ...{...8!...!W.5
db #CB,#7E,#28,#CA,#21,#18,#BF,#11,#22,#BF,#01,#04,#00,#ED,#B0,#AF ; #9cd0 .~(.!...".......
db #32,#1C,#BF,#32,#1D,#BF,#32,#1E,#BF,#C9,#21,#58,#BF,#11,#22,#BF ; #9ce0 2..2..2...!X..".
db #01,#04,#00,#ED,#B0,#3A,#5C,#BF,#32,#1E,#BF,#C9,#3A,#1C,#BF,#CD ; #9cf0 .....:\.2...:...
db #49,#8A,#57,#3A,#1D,#BF,#CD,#49,#8A,#BA,#38,#01,#57,#3A,#1E,#BF ; #9d00 I.W:...I..8.W:..
db #CD,#49,#8A,#BA,#30,#01,#7A,#3D,#87,#87,#87,#32,#57,#BF,#3A,#1C ; #9d10 .I..0.z=...2W.:.
db #BF,#CD,#4E,#8A,#ED,#53,#51,#BF,#3A,#1D,#BF,#CD,#4E,#8A,#ED,#53 ; #9d20 ..N..SQ.:...N..S
db #53,#BF,#3A,#1E,#BF,#CD,#4E,#8A,#ED,#53,#55,#BF,#21,#00,#00,#22 ; #9d30 S.:...N..SU.!.."
db #4B,#BF,#22,#4D,#BF,#22,#4F,#BF,#C9,#B7,#F0,#ED,#44,#C9,#11,#00 ; #9d40 K."M."O.....D...
db #00,#B7,#C8,#F5,#CD,#49,#8A,#3D,#21,#57,#BF,#B6,#87,#5F,#16,#00 ; #9d50 .....I.=!W..._..
db #21,#71,#8A,#19,#5E,#23,#56,#F1,#F0,#21,#00,#00,#B7,#ED,#52,#EB ; #9d60 !q..^#V..!....R.
db #C9,#00,#01,#00,#02,#00,#03,#00,#04,#00,#05,#00,#06,#00,#07,#00 ; #9d70 
db #08,#80,#00,#00,#01,#80,#01,#00,#02,#80,#02,#00,#03,#80,#03,#00 ; #9d80 
db #04,#55,#00,#AB,#00,#00,#01,#55,#01,#AB,#01,#00,#02,#55,#02,#AB ; #9d90 .U.....U.....U..
db #02,#40,#00,#80,#00,#C0,#00,#00,#01,#40,#01,#80,#01,#C0,#01,#00 ; #9da0 .@.......@......
db #02,#33,#00,#66,#00,#9A,#00,#CD,#00,#00,#01,#33,#01,#66,#01,#9A ; #9db0 .3.f.......3.f..
db #01,#2B,#00,#55,#00,#80,#00,#AB,#00,#D5,#00,#00,#01,#2B,#01,#55 ; #9dc0 .+.U.........+.U
db #01,#25,#00,#49,#00,#6E,#00,#92,#00,#B7,#00,#DB,#00,#00,#01,#25 ; #9dd0 .%.I.n.........%
db #01,#20,#00,#40,#00,#60,#00,#80,#00,#A0,#00,#C0,#00,#E0,#00,#00 ; #9de0 . .@.`..........
db #01,#2A,#4B,#BF,#ED,#5B,#51,#BF,#19,#22,#4B,#BF,#2A,#4D,#BF,#ED ; #9df0 .*K..[Q.."K.*M..
db #5B,#53,#BF,#19,#22,#4D,#BF,#2A,#4F,#BF,#ED,#5B,#55,#BF,#19,#22 ; #9e00 [S.."M.*O..[U.."
db #4F,#BF,#C9,#AF,#32,#45,#BF,#32,#46,#BF,#DD,#21,#45,#BE,#3A,#44 ; #9e10 O...2E.2F..!E.:D
db #BE,#B7,#C8,#F5,#DD,#CB,#07,#46,#C4,#41,#8B,#11,#0A,#00,#F1,#DD ; #9e20 .......F.A......
db #19,#3D,#20,#EF,#21,#48,#BF,#CB,#66,#C4,#3C,#8C,#C9,#16,#FB,#0A ; #9e30 .= .!H..f.<.....
db #06,#3A,#25,#BF,#57,#06,#04,#0E,#00,#21,#3D,#8B,#CB,#39,#DD,#7E ; #9e40 .:%.W....!=..9.~
db #05,#86,#F2,#56,#8B,#AF,#5F,#7A,#BB,#38,#02,#CB,#D9,#23,#10,#EC ; #9e50 ...V.._z.8...#..
db #DD,#7E,#06,#FE,#0D,#D0,#87,#5F,#16,#00,#21,#76,#8B,#19,#5E,#23 ; #9e60 .~....._..!v..^#
db #56,#D5,#21,#48,#BF,#C9,#75,#8B,#75,#8B,#75,#8B,#75,#8B,#90,#8B ; #9e70 V.!H..u.u.u.u...
db #DF,#8B,#FC,#8B,#01,#8C,#1B,#8C,#1E,#8C,#1E,#8C,#1E,#8C,#1E,#8C ; #9e80 
db #DD,#7E,#05,#FE,#20,#30,#1F,#CB,#41,#28,#38,#ED,#5B,#49,#BF,#7A ; #9e90 .~.. 0..A(8.[I.z
db #B3,#20,#0C,#DD,#5E,#00,#DD,#56,#01,#ED,#53,#49,#BF,#CB,#CE,#3E ; #9ea0 . ..^..V..SI...>
db #01,#32,#47,#BF,#18,#1D,#CB,#49,#20,#19,#ED,#5B,#49,#BF,#7A,#B3 ; #9eb0 .2G....I ..[I.z.
db #20,#0C,#DD,#5E,#00,#DD,#56,#01,#ED,#53,#49,#BF,#CB,#CE,#3E,#FF ; #9ec0  ..^..V..SI...>.
db #32,#47,#BF,#CB,#41,#20,#07,#CB,#49,#28,#03,#CD,#21,#8C,#C9,#DD ; #9ed0 2G..A ..I(..!...
db #7E,#05,#FE,#20,#30,#0B,#CB,#51,#28,#03,#CB,#C6,#C9,#CD,#21,#8C ; #9ee0 ~.. 0..Q(.....!.
db #C9,#CB,#59,#20,#03,#CB,#C6,#C9,#CD,#21,#8C,#C9,#CB,#D6,#CB,#C6 ; #9ef0 ..Y .....!......
db #C9,#CB,#DE,#DD,#5E,#00,#DD,#56,#01,#3E,#06,#12,#DD,#7E,#05,#FE ; #9f00 ....^..V.>...~..
db #20,#3E,#01,#38,#02,#3E,#FF,#32,#47,#BF,#C9,#C3,#21,#8C,#CB,#C6 ; #9f10  >.8.>.2G...!...
db #C9,#CB,#E6,#3A,#22,#BF,#DD,#96,#02,#57,#3A,#24,#BF,#DD,#96,#04 ; #9f20 ...:"....W:$....
db #5F,#21,#45,#BF,#7E,#82,#77,#23,#7E,#83,#77,#C9,#06,#32,#ED,#5B ; #9f30 _!E.~.w#~.w..2.[
db #45,#BF,#7B,#B2,#28,#29,#C5,#D5,#CD,#CA,#88,#D1,#C1,#B8,#38,#1F ; #9f40 E.{.()........8.
db #28,#1D,#7B,#CD,#49,#8A,#CB,#3F,#CB,#7B,#28,#02,#ED,#44,#5F,#7A ; #9f50 (.{.I..?.{(..D_z
db #CD,#49,#8A,#CB,#3F,#CB,#7A,#28,#02,#ED,#44,#57,#C3,#42,#8C,#ED ; #9f60 .I..?.z(..DW.B..
db #53,#1C,#BF,#3E,#01,#32,#8C,#7A,#C9,#2A,#49,#BF,#7C,#B5,#C8,#35 ; #9f70 S..>.2.z.*I.|..5
db #C0,#21,#00,#00,#22,#49,#BF,#C9,#04,#00,#8D,#8C,#09,#09,#09,#09 ; #9f80 .!.."I..........
db #09,#0A,#0B,#0B,#0A,#21,#88,#8C,#35,#C0,#36,#04,#2A,#8A,#8C,#7E ; #9f90 .....!..5.6.*..~
db #32,#8C,#8C,#23,#3A,#89,#8C,#3C,#FE,#08,#20,#04,#AF,#21,#8D,#8C ; #9fa0 2..#:..<.. ..!..
db #22,#8A,#8C,#32,#89,#8C,#C9,#FF,#80,#FF,#00,#FF,#01,#00,#00,#FF ; #9fb0 "..2............
db #87,#FF,#FF,#FF,#E1,#00,#00,#FC,#84,#00,#00,#3F,#21,#00,#00,#FF ; #9fc0 ...........?!...
db #87,#FF,#FF,#FF,#E1,#00,#00,#FF,#80,#FF,#00,#FF,#01,#00,#00,#00 ; #9fd0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #9fe0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #9ff0 
db #00,#00,#00,#00,#00,#00,#00,#02,#01,#0A,#01,#02,#FF,#80,#F0,#10 ; #a000 
db #00,#00,#00,#00,#FF,#9F,#F0,#90,#00,#00,#00,#00,#FF,#80,#F0,#10 ; #a010 
db #00,#00,#00,#00,#01,#06,#01,#F5,#3A,#56,#8D,#21,#0C,#8D,#B7,#28 ; #a020 ........:V.!...(
db #03,#21,#B7,#8C,#11,#DF,#8C,#01,#28,#00,#ED,#B0,#F1,#4F,#B7,#C8 ; #a030 .!......(....O..
db #21,#DF,#8C,#06,#14,#B7,#08,#B7,#08,#CB,#1E,#23,#08,#CB,#1E,#23 ; #a040 !..........#...#
db #10,#F6,#0D,#20,#EB,#C9,#00,#32,#56,#8D,#21,#FA,#FF,#CD,#A0,#8D ; #a050 ... ...2V.!.....
db #30,#0B,#3A,#40,#BF,#67,#25,#2E,#FA,#CD,#A0,#8D,#D8,#E5,#78,#CD ; #a060 0.:@.g%.......x.
db #27,#8D,#D9,#E1,#01,#1E,#00,#D9,#11,#24,#8D,#0E,#03,#3A,#56,#8D ; #a070 '........$...:V.
db #B7,#28,#05,#11,#07,#8D,#0E,#05,#21,#DF,#8C,#06,#08,#C5,#D5,#E5 ; #a080 .(......!.......
db #CD,#99,#8D,#E1,#D1,#C1,#10,#F5,#C9,#ED,#73,#41,#BE,#C3,#7E,#78 ; #a090 ..........sA..~x
db #ED,#5B,#18,#BF,#B7,#ED,#52,#3A,#56,#8D,#B7,#28,#05,#29,#3E,#00 ; #a0a0 .[....R:V..(.)>.
db #18,#02,#3E,#20,#11,#80,#00,#19,#CD,#D2,#79,#C9,#21,#38,#BF,#06 ; #a0b0 ..> ......y.!8..
db #08,#36,#00,#23,#10,#FB,#3E,#FF,#32,#34,#BF,#3E,#07,#0E,#1C,#CD ; #a0c0 .6.#..>.24.>....
db #FA,#8E,#3E,#08,#0E,#00,#CD,#FA,#8E,#3C,#CD,#FA,#8E,#3C,#CD,#FA ; #a0d0 ..>......<...<..
db #8E,#3E,#28,#32,#13,#90,#C9,#3A,#8E,#3F,#8E,#49,#8E,#4E,#8E,#56 ; #a0e0 .>(2...:.?.I.N.V
db #8E,#62,#8E,#69,#8E,#72,#8E,#07,#8E,#0E,#8E,#13,#8E,#2D,#8E,#32 ; #a0f0 .b.i.r.......-.2
db #8E,#36,#8E,#38,#8E,#2D,#8E,#06,#82,#83,#84,#85,#86,#87,#04,#14 ; #a100 .6.8.-..........
db #16,#18,#1A,#19,#12,#16,#16,#19,#1E,#1E,#1D,#19,#19,#16,#12,#12 ; #a110 
db #14,#16,#16,#17,#19,#19,#1B,#1E,#1E,#1D,#19,#19,#1E,#04,#0A,#08 ; #a120 
db #06,#04,#03,#14,#19,#14,#01,#80,#01,#0F,#3A,#A5,#90,#1F,#C9,#3A ; #a130 ..........:....:
db #A6,#90,#2F,#21,#6F,#BF,#A6,#1F,#C9,#3A,#A6,#90,#1F,#C9,#2A,#49 ; #a140 ../!o....:....*I
db #BF,#7C,#B5,#C8,#37,#C9,#3A,#48,#BF,#37,#CB,#57,#C0,#CB,#5F,#C0 ; #a150 .|..7.:H.7.W.._.
db #B7,#C9,#3A,#47,#BF,#B7,#C8,#37,#C9,#3A,#48,#BF,#37,#CB,#67,#C0 ; #a160 ..:G...7.:H.7.g.
db #B7,#C9,#B7,#C9,#DD,#21,#E7,#8D,#06,#08,#11,#38,#BF,#DD,#6E,#00 ; #a170 .....!.....8..n.
db #DD,#66,#01,#CD,#DF,#8E,#30,#08,#1A,#B7,#20,#0A,#3C,#12,#18,#06 ; #a180 .f....0... .<...
db #1A,#B7,#28,#02,#AF,#12,#DD,#23,#DD,#23,#13,#10,#E0,#3A,#34,#BF ; #a190 ..(....#.#...:4.
db #B7,#F2,#A6,#8E,#3E,#08,#B7,#28,#27,#47,#21,#38,#BF,#11,#F7,#8D ; #a1a0 ....>..('G!8....
db #0E,#00,#7E,#FE,#01,#28,#08,#23,#13,#13,#0C,#10,#F5,#18,#11,#79 ; #a1b0 ..~..(.#.......y
db #32,#34,#BF,#EB,#5E,#23,#56,#1A,#13,#32,#37,#BF,#ED,#53,#35,#BF ; #a1c0 24..^#V..27..S5.
db #21,#38,#BF,#06,#08,#7E,#B7,#28,#02,#36,#02,#23,#10,#F7,#C9,#E9 ; #a1d0 !8...~.(.6.#....
db #3E,#1F,#C3,#14,#90,#3E,#00,#C3,#14,#90,#87,#47,#ED,#5F,#A8,#E6 ; #a1e0 >....>.....G._..
db #1F,#CD,#14,#90,#C9,#06,#06,#C3,#04,#70,#F5,#C5,#06,#F4,#ED,#79 ; #a1f0 .........p.....y
db #06,#F6,#ED,#78,#F6,#C0,#ED,#79,#E6,#3F,#ED,#79,#06,#F4,#ED,#49 ; #a200 ...x...y.?.y...I
db #06,#F6,#4F,#F6,#80,#ED,#79,#ED,#49,#C1,#F1,#C9,#F5,#3E,#08,#0E ; #a210 ..O...y.I....>..
db #0C,#CD,#FA,#8E,#3C,#0E,#0A,#CD,#FA,#8E,#F1,#5F,#16,#00,#21,#89 ; #a220 ....<......_..!.
db #8F,#19,#19,#3E,#00,#4E,#23,#CD,#FA,#8E,#3C,#4E,#CD,#FA,#8E,#11 ; #a230 ...>.N#...<N....
db #CF,#FF,#19,#3E,#02,#4E,#23,#CD,#FA,#8E,#3C,#4E,#CD,#FA,#8E,#3A ; #a240 ...>.N#...<N...:
db #33,#BF,#B7,#C8,#06,#50,#C3,#04,#70,#47,#06,#ED,#05,#98,#05,#47 ; #a250 3....P..pG.....G
db #05,#FC,#04,#B4,#04,#70,#04,#31,#04,#F4,#03,#BC,#03,#86,#03,#53 ; #a260 .....p.1.......S
db #03,#24,#03,#F6,#02,#CC,#02,#A4,#02,#7E,#02,#5A,#02,#38,#02,#18 ; #a270 .$.......~.Z.8..
db #02,#FA,#01,#DE,#01,#C3,#01,#AA,#01,#92,#01,#7B,#01,#66,#01,#52 ; #a280 ...........{.f.R
db #01,#3F,#01,#2D,#01,#1C,#01,#0C,#01,#FD,#00,#EF,#00,#E1,#00,#D5 ; #a290 .?.-............
db #00,#C9,#00,#BE,#00,#B3,#00,#A9,#00,#9F,#00,#96,#00,#8E,#00,#86 ; #a2a0 
db #00,#7F,#00,#77,#00,#71,#00,#6A,#00,#64,#00,#5F,#00,#59,#00,#54 ; #a2b0 ...w.q.j.d._.Y.T
db #00,#50,#00,#4B,#00,#47,#00,#43,#00,#3F,#00,#3C,#00,#38,#00,#35 ; #a2c0 .P.K.G.C.?.<.8.5
db #00,#3A,#34,#BF,#B7,#F2,#DF,#8F,#3E,#07,#0E,#1C,#C3,#D2,#8D,#2A ; #a2d0 .:4.....>......*
db #35,#BF,#7E,#23,#22,#35,#BF,#FE,#80,#28,#04,#FE,#81,#20,#07,#D6 ; #a2e0 5.~#"5...(... ..
db #80,#CD,#E0,#8E,#18,#0E,#FE,#82,#38,#07,#D6,#82,#CD,#EA,#8E,#18 ; #a2f0 ........8.......
db #03,#CD,#1C,#8F,#21,#37,#BF,#35,#C0,#3E,#FF,#32,#34,#BF,#C9,#00 ; #a300 ....!7.5.>.24...
db #00,#00,#00,#28,#4F,#3E,#06,#CD,#FA,#8E,#3E,#0A,#0E,#00,#CD,#FA ; #a310 ...(O>....>.....
db #8E,#3A,#13,#90,#4F,#3E,#0B,#CD,#FA,#8E,#3C,#0E,#00,#CD,#FA,#8E ; #a320 .:..O>....<.....
db #3C,#0E,#0F,#CD,#FA,#8E,#3E,#0A,#0E,#10,#CD,#FA,#8E,#C9,#CD,#BC ; #a330 <.....>.........
db #8D,#3E,#28,#32,#13,#90,#3E,#01,#32,#33,#BF,#21,#01,#05,#22,#0F ; #a340 .>(2..>.23.!..".
db #90,#3A,#00,#05,#32,#11,#90,#AF,#32,#12,#90,#18,#07,#AF,#32,#33 ; #a350 .:..2...2.....23
db #BF,#C3,#BC,#8D,#CD,#E1,#6F,#20,#F4,#2A,#0F,#90,#3A,#12,#90,#B7 ; #a360 ......o .*..:...
db #28,#13,#CB,#7E,#3E,#08,#C4,#14,#90,#7E,#E6,#7F,#FE,#24,#30,#05 ; #a370 (..~>....~...$0.
db #CD,#1C,#8F,#18,#03,#CD,#54,#8F,#21,#12,#90,#34,#7E,#FE,#02,#20 ; #a380 ......T.!..4~.. 
db #D3,#36,#00,#2A,#0F,#90,#23,#22,#0F,#90,#21,#11,#90,#35,#28,#A6 ; #a390 .6.*..#"..!..5(.
db #18,#C2,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #a3a0 
db #14,#0B,#0B,#1C,#3E,#0A,#18,#02,#3E,#0B,#32,#B1,#90,#06,#7F,#16 ; #a3b0 ....>...>.2.....
db #40,#0E,#00,#ED,#49,#3A,#B0,#90,#B2,#ED,#79,#0C,#ED,#49,#3A,#B1 ; #a3c0 @...I:....y..I:.
db #90,#B2,#ED,#79,#0C,#ED,#49,#3A,#B2,#90,#B2,#ED,#79,#0C,#ED,#49 ; #a3d0 ...y..I:....y..I
db #3A,#B3,#90,#B2,#ED,#79,#0E,#10,#ED,#49,#3A,#B0,#90,#B2,#ED,#79 ; #a3e0 :....y...I:....y
db #C9,#06,#7F,#0E,#03,#ED,#49,#F6,#40,#ED,#79,#C9,#ED,#73,#A8,#90 ; #a3f0 ......I.@.y..s..
db #21,#00,#BE,#11,#01,#BE,#01,#FF,#01,#36,#00,#ED,#B0,#AF,#32,#AF ; #a400 !........6....2.
db #90,#CD,#78,#7E,#3E,#01,#32,#AF,#90,#3E,#04,#32,#AA,#90,#3A,#AC ; #a410 ..x~>.2..>.2..:.
db #90,#32,#AB,#90,#21,#00,#00,#22,#63,#BF,#22,#65,#BF,#CD,#CD,#84 ; #a420 .2..!.."c."e....
db #CD,#F4,#84,#3A,#AB,#90,#CD,#5F,#84,#AF,#32,#6F,#BF,#3E,#01,#18 ; #a430 ...:..._..2o.>..
db #05,#AF,#32,#6F,#BF,#AF,#CD,#74,#7F,#CD,#74,#91,#38,#08,#CD,#9E ; #a440 ..2o...t..t.8...
db #83,#CD,#2D,#85,#18,#D7,#2A,#5D,#BF,#7C,#B5,#3E,#01,#28,#0D,#21 ; #a450 ..-...*].|.>.(.!
db #AA,#90,#35,#3E,#00,#28,#05,#CD,#49,#83,#18,#D5,#CD,#DD,#7F,#CD ; #a460 ..5>.(..I.......
db #00,#81,#18,#99,#CD,#50,#7A,#AF,#32,#1E,#77,#CD,#26,#7E,#CD,#35 ; #a470 .....Pz.2.w.&~.5
db #70,#CD,#BC,#8D,#AF,#32,#48,#BF,#AF,#32,#A6,#90,#CD,#71,#70,#CD ; #a480 p....2H..2...qp.
db #B8,#90,#21,#A2,#90,#7E,#2F,#77,#CD,#86,#70,#3E,#00,#CD,#57,#8D ; #a490 ..!..~/w..p>..W.
db #CD,#CD,#85,#3E,#01,#CD,#57,#8D,#CD,#95,#8C,#CD,#44,#70,#CD,#D1 ; #a4a0 ...>..W.....Dp..
db #8F,#CD,#9F,#81,#3A,#AB,#90,#CD,#7E,#85,#CD,#18,#6F,#2A,#18,#BF ; #a4b0 ....:...~...o*..
db #22,#AD,#90,#CD,#B9,#7B,#3A,#19,#BF,#87,#38,#0E,#3A,#A5,#90,#21 ; #a4c0 "....{:...8.:..!
db #A6,#90,#B6,#20,#05,#3E,#01,#32,#6F,#BF,#CD,#79,#8C,#3A,#48,#BF ; #a4d0 ... .>.2o..y.:H.
db #CB,#5F,#C4,#49,#83,#3A,#A5,#90,#B7,#28,#14,#3A,#A7,#90,#FE,#04 ; #a4e0 ._.I.:...(.:....
db #38,#15,#87,#47,#3A,#B3,#90,#80,#E6,#1F,#CD,#F1,#90,#18,#0B,#3A ; #a4f0 8..G:..........:
db #A6,#90,#B7,#28,#28,#18,#00,#CD,#B8,#90,#3E,#01,#32,#8C,#7A,#3A ; #a500 ...((.....>.2.z:
db #1E,#77,#B7,#28,#08,#FE,#04,#28,#04,#3C,#32,#1E,#77,#21,#A7,#90 ; #a510 .w.(...(.<2.w!..
db #35,#20,#2B,#CD,#B8,#90,#3A,#A6,#90,#B7,#C0,#37,#C9,#3A,#48,#BF ; #a520 5 +...:....7.:H.
db #CB,#47,#20,#07,#2A,#5D,#BF,#7C,#B5,#20,#13,#3E,#01,#32,#A5,#90 ; #a530 .G .*].|. .>.2..
db #AF,#32,#6F,#BF,#3E,#01,#32,#1E,#77,#3E,#09,#32,#A7,#90,#CD,#CF ; #a540 .2o.>.2.w>.2....
db #83,#3A,#A6,#90,#B7,#20,#19,#CD,#0D,#89,#D2,#70,#92,#AF,#32,#6F ; #a550 .:... .....p..2o
db #BF,#3E,#01,#32,#A6,#90,#3E,#14,#32,#A7,#90,#3E,#01,#32,#8C,#7A ; #a560 .>.2..>.2..>.2.z
db #2A,#AD,#90,#ED,#5B,#18,#BF,#B7,#ED,#52,#7D,#CD,#B9,#74,#C3,#92 ; #a570 *...[....R}..t..
db #91,#3E,#17,#CD,#8E,#6F,#28,#10,#3E,#15,#CD,#8E,#6F,#28,#09,#06 ; #a580 .>...o(.>...o(..
db #7F,#3E,#89,#ED,#79,#C3,#00,#00,#ED,#7B,#A8,#90,#CD,#04,#83,#C3 ; #a590 .>..y....{......
db #FC,#90

start:
	di
    ld sp,#ACFE
    ld b,#7f
    ld a,141
    out (c),a
	
	call draw_menu
	jp start_menu
   

org #a670
/*
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#18,#3C,#7E,#FF ; #a670 .............<~.
db #18,#18,#18,#18,#18,#18,#18,#18,#FF,#7E,#3C,#18,#10,#30,#70,#FF ; #a680 .........~<..0p.
db #FF,#70,#30,#10,#08,#0C,#0E,#FF,#FF,#0E,#0C,#08,#00,#00,#18,#3C ; #a690 .p0............<
db #7E,#FF,#FF,#00,#00,#00,#FF,#FF,#7E,#3C,#18,#00,#80,#E0,#F8,#FE ; #a6a0 ~.......~<......
db #F8,#E0,#80,#00,#02,#0E,#3E,#FE,#3E,#0E,#02,#00,#38,#38,#92,#7C ; #a6b0 ......>.>...88.|
db #10,#28,#28,#28,#38,#38,#10,#FE,#10,#28,#44,#82,#38,#38,#12,#7C ; #a6c0 .(((88...(D.88.|
db #90,#28,#24,#22,#38,#38,#90,#7C,#12,#28,#48,#88,#00,#3C,#18,#3C ; #a6d0 .($"88.|.(H..<.<
db #3C,#3C,#18,#00,#3C,#FF,#FF,#18,#0C,#18,#30,#18,#18,#3C,#7E,#18 ; #a6e0 <<..<.....0..<~.
db #18,#7E,#3C,#18,#00,#24,#66,#FF,#66,#24,#00,#00,#00,#00,#07,#00 ; #a6f0 .~<..$f.f$......
db #00,#00,#00,#10,#A9,#FF,#F2,#BF,#FF,#00,#49,#4D,#50,#53,#42,#41 ; #a700 ..........IMPSBA
db #4C,#45,#42,#41,#53,#01,#00,#00,#30,#12,#13,#14,#15,#16,#17,#00 ; #a710 LEBAS...0.......
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#B0,#00,#00,#FF,#00,#00,#00 ; #a720 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #a730 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #a740 
db #02,#7C,#96,#7C,#96,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #a750 .|.|............
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#70,#01,#00,#50,#57,#00 ; #a760 ..........p..PW.
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #a770 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #a780 
db #00,#00,#00,#00,#00,#50,#57,#00,#BF,#01,#00,#00,#00,#00,#00,#00 ; #a790 .....PW.........
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #a7a0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #a7b0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #a7c0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #a7d0 
db #00,#00,#00,#00,#02,#00,#20,#00,#50,#03,#02,#00,#24,#01,#20,#01 ; #a7e0 ...... .P...$. .
db #00,#00,#00,#00,#00,#00,#00,#00,#80,#41,#40,#42,#43,#46,#4B,#4F ; #a7f0 .........A@BCFKO
db #54,#58,#5A,#41,#41,#54,#46,#5A,#41,#04,#01,#1C,#00,#00,#00,#1C ; #a800 TXZAATFZA.......
db #01,#1A,#1A,#1D,#00,#00,#0E,#00,#0F,#01,#1F,#09,#0D,#27,#4C,#27 ; #a810 .............'L'
db #20,#73,#65,#6C,#65,#63,#74,#73,#20,#6C,#65,#76,#65,#6C,#20,#5B ; #a820  selects level [
db #59,#2F,#4E,#5D,#F6,#01,#04,#17,#B0,#0B,#12,#50,#2D,#12,#E0,#B0 ; #a830 Y/N].......P-...
db #43,#F0,#C1,#F4,#33,#84,#E0,#FA,#29,#EB,#40,#55,#9B,#88,#EE,#C2 ; #a840 C...3...).@U....
db #4B,#42,#1F,#59,#4E,#59,#00,#67,#AC,#32,#A0,#4C,#51,#54,#A3,#15 ; #a850 KB.YNY.g.2.LQT..
db #0A,#68,#CC,#2C,#CF,#E5,#A4,#CF,#50,#A5,#CF,#57,#A5,#CF,#A0,#A5 ; #a860 .h.,....P..W....
db #CF,#18,#A6,#CF,#07,#A6,#CF,#03,#A6,#CF,#FE,#A4,#CF,#7F,#A5,#CF ; #a870 
db #99,#A5,#CF,#C6,#A5,#CF,#53,#A6,#CF,#92,#A6,#30,#CD,#07,#00,#00 ; #a880 ......S....0....
db #24,#00,#03,#07,#00,#B3,#00,#3F,#00,#C0,#00,#10,#00,#00,#00,#C1 ; #a890 $......?........
db #09,#2A,#52,#E5,#02,#04,#05,#FF,#00,#4A,#80,#80,#80,#80,#80,#80 ; #a8a0 .*R......J......
db #80,#80,#80,#80,#80,#80,#80,#80,#80,#FF,#FF,#FF,#00,#00,#00,#00 ; #a8b0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #a8c0 
db #24,#00,#03,#07,#00,#AA,#00,#3F,#00,#C0,#00,#10,#00,#02,#00,#41 ; #a8d0 $......?.......A
db #09,#2A,#52,#E5,#02,#04,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #a8e0 .*R.............
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #a8f0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #a900 
db #00,#00,#02,#00,#00,#00,#00,#00,#30,#A9,#90,#A8,#A9,#A8,#B9,#A8 ; #a910 ........0.......
db #00,#00,#00,#00,#00,#00,#00,#00,#30,#A9,#D0,#A8,#E9,#A8,#F9,#A8 ; #a920 ........0.......
db #00,#49,#4D,#50,#53,#42,#41,#4C,#45,#42,#41,#53,#00,#00,#00,#80 ; #a930 .IMPSBALEBAS....
db #02,#03,#04,#05,#06,#07,#08,#09,#0A,#0B,#0C,#0D,#0E,#0F,#10,#11 ; #a940 
db #00,#49,#4D,#50,#53,#42,#41,#4C,#45,#42,#41,#53,#01,#00,#00,#30 ; #a950 .IMPSBALEBAS...0
db #12,#13,#14,#15,#16,#17,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #a960 
db #E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5 ; #a970 
db #E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5 ; #a980 
db #E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5 ; #a990 
db #E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5,#E5 ; #a9a0 

db #E4,#74,#FC,#21,#81,#4A,#30,#13,#99,#BB,#77,#ED,#11,#30,#B4,#DD ; #a9b0 .t.!.J0...w..0..
db #53,#29,#1F,#0B,#8C,#AC,#4A,#D9,#81,#43,#05,#F0,#F0,#88,#EE,#1E ; #a9c0 S)....J..C......
db #5A,#E7,#D0,#44,#52,#9D,#A9,#E5,#EF,#66,#A0,#10,#94,#E3,#52,#77 ; #a9d0 Z..DR....f....Rw
db #EE,#0D,#34,#19,#A6,#16,#3F,#12,#11,#B5,#A7,#00,#0F,#3F,#32,#CE ; #a9e0 ..4...?......?2.
db #22,#AC,#04,#F5,#83,#C5,#82,#21,#56,#26,#21,#11,#D5,#27,#92,#CB ; #a9f0 "......!V&!..'..
db #99,#50,#AB,#55,#28,#0C,#0B,#EF,#07,#70,#F4,#8C,#C5,#E0,#99,#DD ; #aa00 .P.U(....p......
db #88,#D3,#44,#77,#95,#0E,#0F,#58,#28,#04,#30,#44,#CB,#A0,#13,#14 ; #aa10 ..Dw...X(.0D....
db #01,#DA,#0A,#42,#23,#58,#06,#9A,#CB,#32,#88,#B2,#74,#40,#43,#3C ; #aa20 ...B#X...2..t@C<
db #25,#B9,#8F,#1D,#AB,#99,#CC,#EE,#77,#33,#FA,#88,#A1,#54,#70,#C6 ; #aa30 %.......w3...Tp.
db #41,#11,#25,#C4,#33,#FF,#86,#EA,#E0,#C0,#E1,#70,#CC,#8A,#C7,#5C ; #aa40 A.%.3......p...\
db #77,#A8,#74,#EE,#BB,#AA,#4F,#66,#91,#77,#88,#5A,#30,#70,#1A,#6A ; #aa50 w.t...Of.w.Z0p.j
db #07,#20,#F0,#43,#E0,#05,#00,#11,#30,#68,#41,#9A,#94,#A3,#40,#00 ; #aa60 . .C....0hA...@.
db #CC,#0D,#FF,#6C,#00,#00,#A5,#A2,#95,#09,#40,#95,#88,#44,#B3,#2A ; #aa70 ...l......@..D.*
db #A0,#66,#1D,#81,#8D,#04,#18,#99,#50,#99,#D0,#4C,#8D,#22,#AF,#F5 ; #aa80 .f......P..L."..
db #C5,#E5,#46,#48,#CD,#32,#BC,#E1,#C1,#F1,#23,#3C,#10,#F1,#46,#48 ; #aa90 ..FH.2....#<..FH
db #C3,#38,#BC,#01,#06,#BC,#ED,#49,#04,#ED,#71,#C9,#ED,#49,#04,#ED ; #aaa0 .8.....I..q..I..
db #61,#05,#0C,#ED,#49,#04,#ED,#69,#05,#C9,#E6,#AA,#C8,#4F,#0F,#B1 ; #aab0 a...I..i.....O..
db #4F,#7C,#CD,#05,#58,#71,#2C,#71,#C6,#08,#67,#71,#2D,#71,#C6,#08 ; #aac0 O|..Xq,q..gq-q..
db #67,#C9,#01,#00,#7C,#F0,#91,#A9,#3E,#E8,#F9,#C6,#4E,#E8,#58,#78 ; #aad0 g...|...>...N.Xx
db #EC,#0C,#F2,#0F,#21,#C6,#14,#9B,#64,#49,#7F,#63,#1C,#3A,#46,#7E ; #aae0 ....!...dI.c.:F~
db #62,#E0,#2B,#1C,#5C,#F8,#48,#A7,#30,#41,#32,#44,#98,#12,#88,#C3 ; #aaf0 b.+.\.H.0A2D....
db #FC,#BF,#0A,#92,#3A,#9A,#61,#83,#C9,#90,#87,#41,#C9,#48,#61,#C6 ; #ab00 ....:.a....A.Ha.
db #9C,#C2,#50,#13,#46,#69,#6C,#D6,#FC,#E0,#F6,#21,#57,#03,#3C,#64 ; #ab10 ..P.Fil....!W.<d
db #E8,#0F,#A5,#88,#A2,#CC,#90,#C0,#00,#A1,#42,#23,#13,#25,#01,#30 ; #ab20 ..........B#.%.0
db #02,#00,#20,#00,#50,#03,#02,#00,#24,#01,#20,#01,#00,#00,#00,#00 ; #ab30 .. .P...$. .....
db #00,#00,#00,#00,#80,#41,#40,#42,#43,#46,#4B,#4F,#54,#58,#5A,#41 ; #ab40 .....A@BCFKOTXZA
db #41,#54,#46,#5A,#41,#04,#01,#1C,#00,#00,#00,#1C,#01,#1A,#1A,#1D ; #ab50 ATFZA...........
db #00,#00,#0E,#00,#0F,#01,#1F,#09,#0D,#27,#4C,#27,#20,#73,#65,#6C ; #ab60 .........'L' sel
db #65,#63,#74,#73,#20,#6C,#65,#76,#65,#6C,#20,#5B,#59,#2F,#4E,#5D ; #ab70 ects level [Y/N]
db #F6,#01,#04,#17,#B0,#0B,#12,#50,#2D,#12,#E0,#B0,#43,#F0,#C1,#F4 ; #ab80 .......P-...C...
db #33,#84,#E0,#FA,#29,#EB,#40,#55,#9B,#88,#EE,#C2,#4B,#42,#1F,#59 ; #ab90 3...).@U....KB.Y
db #4E,#59,#00,#67,#AC,#32,#A0,#4C,#51,#54,#A3,#15,#0A,#68,#CC,#2C ; #aba0 NY.g.2.LQT...h.,
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #abb0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #abc0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #abd0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #abe0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #abf0 
db #00,#00,#00,#00,#00,#00,#00,#00,#01,#84,#01,#FF,#00,#00,#00,#00 ; #ac00 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #ac10 
db #00,#08,#26,#C9,#FD,#00,#00,#00,#00,#00,#00,#00,#00,#08,#26,#C9 ; #ac20 ..&...........&.
db #FD,#00,#00,#00,#00,#00,#00,#00,#00,#08,#26,#C9,#FD,#00,#00,#00 ; #ac30 ..........&.....
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#02,#26,#C9,#FD,#00 ; #ac40 ............&...
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#04,#26,#C9 ; #ac50 ..............&.
db #FD,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#08 ; #ac60 
db #26,#C9,#FD,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #ac70 &...............
db #00,#10,#26,#C9,#FD,#00,#00,#00,#00,#00,#72,#75,#6E,#22,#69,#6D ; #ac80 ..&.......run"im
db #70,#73,#62,#61,#6C,#65,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #ac90 psbale..........
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #aca0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #acb0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #acc0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #acd0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #ace0 
db #00,#00,#B6,#B5,#BE,#B5,#57,#B0,#47,#17,#85,#B3,#C5,#A5,#00,#00 ; #acf0 ......W.G.......
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #ad00 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #ad10 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #ad20 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #ad30 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #ad40 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #ad50 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #ad60 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #ad70 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#3F,#00 ; #ad80 ..............?.
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #ad90 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #ada0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #adb0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #adc0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #add0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #ade0 
db #00,#00,#00,#05,#05,#05,#05,#05,#05,#05,#05,#05,#05,#05,#05,#05 ; #adf0 
db #05,#05,#05,#05,#05,#05,#05,#05,#05,#05,#05,#05,#05,#00,#00,#00 ; #ae00 
db #00,#00,#00,#00,#00,#00,#00,#6F,#01,#70,#AE,#73,#01,#72,#01,#00 ; #ae10 .......o.p.s.r..
db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#50,#57,#00,#00,#00,#00 ; #ae20 ..........PW....
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #ae30 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #ae40 
db #00,#00,#00,#00,#00,#90,#01,#FF,#78,#01,#FE,#BF,#0D,#00,#7B,#A6 ; #ae50 ........x.....{.
db #FB,#A6,#40,#00,#6F,#01,#C0,#58,#C0,#58,#C0,#58,#C0,#58,#00,#00 ; #ae60 ..@.o..X.X.X.X..
db #43,#41,#54,#D2,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #ae70 CAT.............
*/

; this code is called once, then this area will be used as a drawing buffer (#ac00 -#bd00) 
org #b000
draw_menu:
labB000 jp labB008

labB003 out (c),c
    out (c),a
    ret 
labB008 di
    ld b,127
    ld a,84
    ld c,0
    call labB003
    ld c,1
    call labB003
    ld c,2
    call labB003
    ld c,3
    call labB003
    ld c,16
    call labB003

    ; clear screen
    ld hl,#C000
    ld de,#c001
    ld bc,#3FFF
    ld (hl),0
    ldir
    
    
    ld de,menu_tiles
    ld b,0
labB038 ld c,8
labB03A ld a,(de)
    push de
    ld l,a
    ld h,0
    add hl,hl ; A*16
    add hl,hl
    add hl,hl
    add hl,hl
    ld de,labB386
    add hl,de
    push hl
    push bc
	; draw 1st Column	
	call labB067
    pop bc
    pop hl
    ld de,8
    add hl,de
    inc c
    push bc
    ; draw 2nd Column
    call labB067
    pop bc
    inc c
    pop de
    inc de
    ld a,c
    cp 72
    jr nz,labB03A
    inc b
    ld a,b
    cp 24
    jr nz,labB038
labB066 ret 

; draw 1 column of a char/tile
labB067 push hl
    ld l,b
    ld h,0
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    ld e,l
    ld d,h
    add hl,hl
    add hl,hl
    add hl,de
    ld e,c
    ld d,192
    add hl,de
    ex de,hl
    pop hl

	
    ld b,8
labB07C ld a,(hl)
    ld (de),a
    inc hl
    ld a,d
    add a,8
    ld d,a
    djnz labB07C
    ret 


org #b086

labB086:
menu_tiles:
; Tiles pour 'caracteres' l'ecran titre . Speccy inside!
db #00,#01,#02,#03,#04,#05,#06,#07,#08,#08,#08,#08,#08,#08,#08,#08	; 1ere ligne
db #08,#08,#08,#08,#08,#08,#08,#01,#09,#0A,#0B,#0C,#0D,#0E,#08,#0f

; comme des bourrins! juste pour dessiner le cadre a gauche et a droite!
repeat 18
db #10,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11
db #11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#12
rend 

db #13,#14,#14,#14,#14,#14,#14,#14,#14,#14,#14,#14,#14,#14,#14,#14
db #14,#14,#14,#14,#14,#14,#14,#14,#14,#14,#14,#14,#14,#14,#14,#15

; hud
db #11,#16,#17,#17,#17,#17,#17,#17,#18,#16,#17,#17,#17,#17,#17,#17
db #17,#17,#17,#17,#17,#17,#18,#16,#17,#17,#17,#17,#17,#17,#18,#11

db #19,#1A,#1B,#1C,#11,#11,#11,#11,#1D,#1A,#11,#11,#11,#11,#11,#11
db #11,#11,#11,#11,#11,#11,#1D,#1A,#11,#11,#11,#11,#11,#11,#1D,#1E

db #1F,#1A,#20,#21,#11,#11,#11,#11,#1D,#1A,#11,#11,#11,#11,#11,#11
db #11,#11,#11,#11,#11,#11,#1D,#1A,#11,#11,#11,#11,#11,#11,#1D,#22

db #23,#24,#25,#26,#27,#28,#29,#2A,#2B,#24,#2C,#2C,#2C,#2C,#2D,#2E
db #2F,#30,#2C,#2C,#2C,#2C,#2B,#24,#2C,#31,#32,#33,#34,#2C,#2B,#23

; Characteres pour  16 bytes/char, Column1/column2
labB386:
db #00,#01,#03,#07,#0F,#0F,#0E,#0E,#0F,#0F,#0F,#0C,#0B,#07,#0F,#0F
db #0F,#0F,#0F,#00,#0F,#0F,#0F,#00,#0E,#0E ; #b390 
db #0E,#00,#0E,#0E,#0E,#00,#0F,#0B,#0B,#08,#0B,#0B,#0F,#00,#0E,#0A ; #b3a0 
db #0A,#02,#0A,#0A,#0E,#00,#0F,#08,#0B,#08,#0B,#08,#0F,#00,#0E,#02 ; #b3b0 
db #0E,#02,#0E,#02,#0E,#00,#0F,#0A,#0A,#0A,#0A,#08,#0F,#00,#0E,#0A ; #b3c0 
db #0A,#0A,#0A,#02,#0E,#00,#0F,#08,#0B,#08,#0F,#08,#0F,#00,#0E,#02 ; #b3d0 
db #0E,#02,#0A,#02,#0E,#00,#0F,#08,#0B,#0B,#0B,#08,#0F,#00,#0E,#02 ; #b3e0 
db #0A,#0A,#0A,#02,#0E,#00,#0F,#0B,#09,#0A,#0B,#0B,#0F,#00,#0E,#0A ; #b3f0 
db #0A,#0A,#02,#0A,#0E,#00,#0F,#0F,#0F,#00,#0F,#0F,#0F,#00,#0F,#0F ; #b400 
db #0F,#00,#0F,#0F,#0F,#00,#0F,#0C,#0F,#0F,#0F,#0C,#0F,#00,#0F,#00 ; #b410 
db #09,#09,#09,#01,#0F,#00,#0F,#00,#0F,#00,#02,#02,#0F,#00,#0F,#00 ; #b420 
db #0F,#00,#04,#04,#0F,#00,#0F,#00,#0F,#08,#09,#09,#0F,#00,#0F,#01 ; #b430 
db #09,#01,#0F,#0F,#0F,#00,#0F,#09,#09,#09,#09,#09,#0F,#00,#0F,#00 ; #b440 
db #02,#00,#0E,#0E,#0F,#00,#0F,#04,#04,#04,#04,#04,#0F,#00,#0F,#01 ; #b450 
db #09,#01,#09,#01,#0F,#00,#0F,#00,#03,#00,#02,#00,#0F,#00,#0E,#06 ; #b460 
db #0E,#06,#06,#06,#0E,#00,#0F,#0F,#0F,#03,#0D,#0E,#0F,#0F,#00,#08 ; #b470 
db #0C,#0E,#0F,#0F,#07,#07,#0E,#0E,#0E,#0E,#0E,#0E,#0E,#0E,#0E,#0E ; #b480 
db #0E,#0E,#0E,#0E,#0E,#0E
; Char #11: space 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00
db #07,#07,#07,#07,#07,#07,#07,#07,#07,#07 
db #07,#07,#07,#07,#07,#07,#0E,#0E,#0F,#0F,#07,#03,#01,#00,#0F,#0F 
db #07,#0B,#0C,#0F,#0F,#0F,#00,#0F,#0F,#0F,#00,#0F,#0F,#0F,#00,#0F 
db #0F,#0F,#00,#0F,#0F,#0F,#0F,#0F,#0E,#0D,#03,#0F,#0F,#0F,#07,#07 
db #0F,#0F,#0E,#0C,#08,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 
db #00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#00,#0F,#00,#00 
db #00,#00,#00,#00,#00,#0F,#00,#00,#00,#00,#00,#00,#00,#08,#00,#00 
db #00,#00,#00,#00,#00,#00,#FF,#77,#33,#11,#00,#00,#00,#00,#FF,#FF 
db #FF,#FF,#FF,#77,#33,#11,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#EF,#EF 
db #EF,#EF,#EF,#EF,#EF,#EF,#00,#00,#01,#02,#01,#02,#03,#03,#00,#07 
db #08,#01,#0B,#07,#08,#0F,#00,#0E,#07,#0F,#0F,#0E,#01,#0F,#00,#00 
db #08,#0C,#08,#04,#0C,#04,#7F,#7F,#7F,#7F,#7F,#7F,#7F,#7F,#FF,#FF 
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#EE,#CC,#88,#FF,#EE 
db #CC,#88,#00,#00,#00,#00,#00,#00,#00,#00,#00,#11,#33,#77,#00,#11 
db #33,#77,#FF,#FF,#FF,#FF,#03,#03,#03,#03,#03,#02,#01,#00,#0F,#0F 
db #0F,#0F,#0F,#0F,#0A,#07,#0F,#0F,#0E,#0E,#0C,#08,#01,#0E,#04,#04 
db #04,#04,#04,#04,#08,#00,#00,#88,#CC,#EE,#FF,#FF,#FF,#FF,#00,#00 
db #00,#00,#00,#88,#CC,#EE,#FF,#00,#00,#00,#00,#00,#00,#00,#FF,#00 
db #00,#00,#00,#00,#00,#00,#FF,#00,#00,#00,#00,#00,#00,#00,#EF,#01 
db #00,#00,#00,#00,#00,#00,#00,#0F,#00,#0F,#0D,#0C,#0D,#0F,#00,#0F 
db #00,#06,#06,#07,#01,#01,#00,#0F,#00,#0D,#0D,#0D,#09,#09,#00,#0F 
db #00,#08,#08,#08,#0B,#0F,#00,#0F,#00,#06,#06,#06,#06,#06,#00,#0F 
db #00,#0F,#0D,#0D,#0D,#0D,#00,#0F,#00,#0B,#0B,#0B,#0B,#0B,#00,#0F 
db #00,#0C,#06,#06,#06,#0C,#00,#0F,#00,#0F,#0D,#0F,#0C,#0F,#00,#0F 
db #00,#0B,#0B,#0B,#03,#0B,#00,#0F,#00,#0E,#06,#00,#00,#00,#00,#0F 
db #00,#0F,#0C,#0F,#03,#0F,#7F,#08,#00,#00,#00,#00,#00,#00,#FF,#00 
db #00,#00,#00,#00,#00,#00,#00,#0F,#00,#00,#00,#00,#00,#00,#00,#0F 
db #00,#00,#00,#00,#00,#00,#00,#0F,#00,#07,#06,#07,#00,#07,#00,#0F 
db #00,#0D,#01,#0D,#0D,#0D,#00,#0F,#00,#0F,#0B,#08,#0B,#0F,#00,#0F 
db #00,#07,#06,#06,#06,#07,#00,#0F,#00,#0D,#0D,#0D,#0D,#0D,#00,#0F 
db #00,#0F,#0B,#08,#08,#08,#00,#0F,#00,#07,#06,#07,#06,#07,#00,#0F 
db #00,#0C,#0C,#0C,#00,#0C,#00,#0F,#00,#03,#00,#00,#00,#00,#00,#0F 
db #00,#0F,#0C,#0C,#0C,#0C,#00,#0F,#00,#07,#03,#03,#03,#07,#00,#0F 
db #00,#0B,#03,#03,#03,#0B,#00,#0F,#00,#0F,#06,#06,#06,#06,#00,#0F 
db #00,#0D,#0D,#0D,#0D,#0D,#00,#0F,#00,#0F,#0B,#0F,#08,#0F,#00,#0F ; #b6c0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #b6d0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #b6e0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #b6f0 
db #00,#18,#27,#00,#02,#F0,#00,#92,#13,#00,#00,#00,#00,#00,#00,#18 ; #b700 ..'.............
db #27,#00,#02,#F0,#00,#92,#13,#00,#00,#00,#00,#00,#00,#18,#27,#00 ; #b710 '.............'.
db #02,#F0,#00,#92,#13,#00,#00,#00,#00,#00,#00,#18,#27,#00,#02,#F0 ; #b720 ............'...
db #00,#92,#13,#00,#F0,#FF,#7C,#A6,#33,#CC,#00,#CC,#00,#CC,#00,#CC ; #b730 ......|.3.......
db #00,#CC,#00,#CC,#33,#CC,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #b740 ....3...........
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#1F,#09,#0D,#1A,#00,#00,#00 ; #b750 
db #00,#00,#00,#80,#13,#15,#81,#35,#13,#80,#97,#12,#80,#86,#12,#81 ; #b760 .......5........
db #E9,#0A,#81,#40,#19,#00,#59,#14,#80,#E1,#14,#80,#19,#15,#80,#1E ; #b770 ...@..Y.........
db #15,#80,#23,#15,#80,#28,#15,#80,#4F,#15,#80,#3F,#15,#81,#AB,#12 ; #b780 ..#..(..O..?....
db #81,#A6,#12,#80,#5E,#15,#80,#99,#15,#80,#8F,#15,#80,#78,#15,#80 ; #b790 ....^........x..
db #65,#15,#80,#52,#14,#81,#EC,#14,#81,#55,#0C,#80,#C6,#12,#89,#0D ; #b7a0 e..R.....U......
db #15,#84,#01,#15,#00,#EB,#14,#83,#F1,#14,#82,#FA,#14,#80,#39,#15 ; #b7b0 ..............9.
db #82,#47,#15,#01,#00,#00,#C0,#C3,#74,#0C,#00,#00,#00,#00,#00,#00 ; #b7c0 .G......t.......
db #00,#00,#0A,#0A,#04,#04,#13,#0C,#0B,#0C,#17,#0E,#13,#0A,#0B,#1F ; #b7d0 
db #07,#12,#19,#04,#17,#04,#04,#13,#0C,#0B,#0C,#17,#0E,#13,#0A,#0B ; #b7e0 
db #1F,#07,#12,#19,#0A,#07,#00,#00,#06,#00,#00,#00,#00,#00,#81,#61 ; #b7f0 ...............a

db #0D,#00,#00,#00,#0A,#A0,#5E,#A1,#5C,#A2,#7B,#A3,#23,#A6,#40,#AB ; #b800 ......^.\.{.#.@.
db #7C,#AC,#7D,#AD,#7E,#AE,#5D,#AF,#5B,#00,#00,#00,#00,#00,#00,#00 ; #b810 |.}.~.].[.......
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#FB ; #b820 
db #B7,#00,#E2,#BF,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #b830 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #b840 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #b850 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #b860 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #b870 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #b880 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#8D,#03,#CB,#01,#9F,#07 ; #b890 
db #01,#7F,#D4,#B7,#F8,#B7,#7C,#0D,#11,#02,#FD,#B7,#2F,#01,#01,#7F ; #b8a0 ......|...../...
db #44,#FF,#88,#B6,#70,#2D,#00,#00,#00,#F9,#B7,#00,#00,#00,#00,#06 ; #b8b0 D...p-..........
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #b8c0 
db #00,#00,#00,#FC,#A6,#00,#00,#06,#C0,#00,#00,#00,#00,#00,#00,#00 ; #b8d0 

db #00,#00,#00,#00,#00,#00,#00,#00,#00,#A7,#00,#00,#00,#00,#00,#00 ; #b8e0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #b8f0 


db #C3,#5F,#BA,#C3,#66,#BA,#C3,#51,#BA,#C3,#58,#BA,#C3,#70,#BA,#C3 ; #b900 ._..f..Q..X..p..
db #79,#BA,#C3,#9D,#BA,#C3,#7E,#BA,#C3,#87,#BA,#C3,#A1,#BA,#C3,#A7 ; #b910 y.....~.........
db #BA,#3A,#C1,#B8,#B7,#C8,#E5,#F3,#18,#06,#21,#BF,#B8,#36,#01,#C9 ; #b920 .:........!..6..
db #2A,#C0,#B8,#7C,#B7,#28,#07,#23,#23,#23,#3A,#C2,#B8,#BE,#E1,#FB ; #b930 *..|.(.###:.....
db #C9,#F3,#08,#38,#33,#D9,#79,#37,#FB,#08,#F3,#F5,#CB,#91,#ED,#49 ; #b940 ...83.y7.......I
db #CD,#B1,#00,#B7,#08,#4F,#06,#7F,#3A,#31,#B8,#B7,#28,#14,#FA,#72 ; #b950 .....O..:1..(..r
db #B9,#79,#E6,#0C,#F5,#CB,#91,#D9,#CD,#0A,#01,#D9,#E1,#79,#E6,#F3 ; #b960 .y...........y..
db #B4,#4F,#ED,#49,#D9,#F1,#FB,#C9,#08,#E1,#F5,#CB,#D1,#ED,#49,#CD ; #b970 .O.I..........I.
db #3B,#00,#18,#CF,#F3,#E5,#D9,#D1,#18,#06,#F3,#D9,#E1,#5E,#23,#56 ; #b980 ;............^#V
db #08,#7A,#CB,#BA,#CB,#B2,#07,#07,#07,#07,#A9,#E6,#0C,#A9,#C5,#CD ; #b990 .z..............
db #B0,#B9,#F3,#D9,#08,#79,#C1,#E6,#03,#CB,#89,#CB,#81,#B1,#18,#01 ; #b9a0 .....y..........
db #D5,#4F,#ED,#49,#B7,#08,#D9,#FB,#C9,#F3,#08,#79,#E5,#D9,#D1,#18 ; #b9b0 .O.I.......y....
db #15,#F3,#E5,#D9,#E1,#18,#09,#F3,#D9,#E1,#5E,#23,#56,#23,#E5,#EB ; #b9c0 ..........^#V#..
db #5E,#23,#56,#23,#08,#7E,#FE,#FC,#30,#BE,#06,#DF,#ED,#79,#21,#D6 ; #b9d0 ^#V#.~..0....y!.
db #B8,#46,#77,#C5,#FD,#E5,#FE,#10,#30,#0F,#87,#C6,#DA,#6F,#CE,#B8 ; #b9e0 .Fw.....0....o..
db #95,#67,#7E,#23,#66,#6F,#E5,#FD,#E1,#06,#7F,#79,#CB,#D7,#CB,#9F ; #b9f0 .g~#fo.....y....
db #CD,#B0,#B9,#FD,#E1,#F3,#D9,#08,#59,#C1,#78,#06,#DF,#ED,#79,#32 ; #ba00 ........Y.x...y2
db #D6,#B8,#06,#7F,#7B,#18,#90,#F3,#E5,#D9,#D1,#18,#08,#F3,#D9,#E1 ; #ba10 ....{...........
db #5E,#23,#56,#23,#E5,#08,#7A,#CB,#FA,#CB,#F2,#E6,#C0,#07,#07,#21 ; #ba20 ^#V#..z........!
db #D9,#B8,#86,#18,#A5,#F3,#D9,#E1,#5E,#23,#56,#CB,#91,#ED,#49,#ED ; #ba30 ........^#V...I.
db #53,#46,#BA,#D9,#FB,#CD,#45,#33,#F3,#D9,#CB,#D1,#ED,#49,#D9,#FB ; #ba40 SF....E3.....I..
db #C9,#F3,#D9,#79,#CB,#91,#18,#13,#F3,#D9,#79,#CB,#D1,#18,#0C,#F3 ; #ba50 ...y......y.....
db #D9,#79,#CB,#99,#18,#05,#F3,#D9,#79,#CB,#D9,#ED,#49,#D9,#FB,#C9 ; #ba60 .y......y...I...
db #F3,#D9,#A9,#E6,#0C,#A9,#4F,#18,#F2,#CD,#5F,#BA,#18,#0F,#CD,#79 ; #ba70 ......O..._....y
db #BA,#3A,#00,#C0,#2A,#01,#C0,#F5,#78,#CD,#70,#BA,#F1,#E5,#F3,#06 ; #ba80 .:..*...x.p.....
db #DF,#ED,#49,#21,#D6,#B8,#46,#71,#48,#47,#FB,#E1,#C9,#3A,#D6,#B8 ; #ba90 ..I!..FqHG...:..
db #C9,#CD,#AD,#BA,#ED,#B0,#C9,#CD,#AD,#BA,#ED,#B8,#C9,#F3,#D9,#E1 ; #baa0 
db #C5,#CB,#D1,#CB,#D9,#ED,#49,#CD,#C2,#BA,#F3,#D9,#C1,#ED,#49,#D9 ; #bab0 ......I.......I.
db #FB,#C9,#E5,#D9,#FB,#C9,#F3,#D9,#59,#CB,#D3,#CB,#DB,#ED,#59,#D9 ; #bac0 ........Y.....Y.
db #7E,#D9,#ED,#49,#D9,#FB,#C9,#D9,#79,#F6,#0C,#ED,#79,#DD,#7E,#00 ; #bad0 ~..I....y...y.~.
db #ED,#49,#D9,#C9,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #bae0 .I..............
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #baf0 
db #CF,#5C,#9B,#CF,#98,#9B,#CF,#BF,#9B,#CF,#C5,#9B,#CF,#FA,#9B,#CF ; #bb00 .\..............
db #46,#9C,#CF,#B3,#9C,#CF,#04,#9C,#CF,#DB,#9C,#CF,#E1,#9C,#CF,#45 ; #bb10 F..............E
db #9E,#CF,#38,#9D,#CF,#E5,#9D,#CF,#D8,#9E,#CF,#C4,#9E,#CF,#DD,#9E ; #bb20 ..8.............
db #CF,#C9,#9E,#CF,#E2,#9E,#CF,#CE,#9E,#CF,#34,#9E,#CF,#2F,#9E,#CF ; #bb30 ..........4../..
db #F6,#9D,#CF,#F2,#9D,#CF,#FA,#9D,#CF,#0B,#9E,#CF,#19,#9E,#CF,#74 ; #bb40 ...............t
db #90,#CF,#84,#90,#CF,#59,#94,#CF,#52,#94,#CF,#FE,#93,#CF,#35,#93 ; #bb50 .....Y..R.....5.
db #CF,#AC,#93,#CF,#A8,#93,#CF,#08,#92,#CF,#52,#92,#CF,#4F,#95,#CF ; #bb60 ..........R..O..
db #5A,#91,#CF,#65,#91,#CF,#70,#91,#CF,#7C,#91,#CF,#86,#92,#CF,#97 ; #bb70 Z..e..p..|......
db #92,#CF,#76,#92,#CF,#7E,#92,#CF,#CA,#91,#CF,#65,#92,#CF,#65,#92 ; #bb80 ..v..~.....e..e.
db #CF,#A6,#92,#CF,#BA,#92,#CF,#AB,#92,#CF,#C0,#92,#CF,#C6,#92,#CF ; #bb90 
db #7B,#93,#CF,#88,#93,#CF,#D4,#92,#CF,#F2,#92,#CF,#FE,#92,#CF,#2B ; #bba0 {..............+
db #93,#CF,#D4,#94,#CF,#E4,#90,#CF,#03,#91,#CF,#A8,#95,#CF,#D7,#95 ; #bbb0 
db #CF,#FE,#95,#CF,#FB,#95,#CF,#06,#96,#CF,#0E,#96,#CF,#1C,#96,#CF ; #bbc0 
db #A5,#96,#CF,#EA,#96,#CF,#17,#97,#CF,#2D,#97,#CF,#36,#97,#CF,#67 ; #bbd0 .........-..6..g
db #97,#CF,#75,#97,#CF,#6E,#97,#CF,#7A,#97,#CF,#83,#97,#CF,#80,#97 ; #bbe0 ..u..n..z.......
db #CF,#97,#97,#CF,#94,#97,#CF,#A9,#97,#CF,#A6,#97,#CF,#40,#99,#CF ; #bbf0 .............@..
db #BF,#8A,#CF,#D0,#8A,#CF,#37,#8B,#CF,#3C,#8B,#CF,#56,#8B,#CF,#E9 ; #bc00 ......7..<..V...
db #8A,#CF,#0C,#8B,#CF,#17,#8B,#CF,#5D,#8B,#CF,#6A,#8B,#CF,#AF,#8B ; #bc10 ........]..j....
db #CF,#05,#8C,#CF,#11,#8C,#CF,#1F,#8C,#CF,#39,#8C,#CF,#8E,#8C,#CF ; #bc20 ..........9.....
db #A7,#8C,#CF,#F2,#8C,#CF,#1A,#8D,#CF,#F7,#8C,#CF,#1F,#8D,#CF,#EA ; #bc30 
db #8C,#CF,#EE,#8C,#CF,#B9,#8D,#CF,#BD,#8D,#CF,#E5,#8D,#CF,#00,#8E ; #bc40 
db #CF,#44,#8E,#CF,#F9,#8E,#CF,#2A,#8F,#CF,#55,#8C,#CF,#74,#8C,#CF ; #bc50 .D.....*..U..t..
db #93,#8F,#CF,#9B,#8F,#CF,#BC,#A4,#CF,#CE,#A4,#CF,#E1,#A4,#CF,#BB ; #bc60 
db #AB,#CF,#BF,#AB,#CF,#C1,#AB,#DF,#8B,#A8,#DF,#8B,#A8,#DF,#8B,#A8 ; #bc70 
db #DF,#8B,#A8,#DF,#8B,#A8,#DF,#8B,#A8,#DF,#8B,#A8,#DF,#8B,#A8,#DF ; #bc80 
db #8B,#A8,#DF,#8B,#A8,#DF,#8B,#A8,#DF,#8B,#A8,#DF,#8B,#A8,#CF,#AF ; #bc90 
db #A9,#CF,#A6,#A9,#CF,#C1,#A9,#CF,#E9,#9F,#CF,#14,#A1,#CF,#CE,#A1 ; #bca0 
db #CF,#EB,#A1,#CF,#AC,#A1,#CF,#50,#A0,#CF,#6B,#A0,#CF,#95,#A4,#CF ; #bcb0 .......P..k.....
db #9A,#A4,#CF,#A6,#A4,#CF,#AB,#A4,#CF,#5C,#80,#CF,#26,#83,#CF,#30 ; #bcc0 .........\..&..0
db #83,#CF,#A0,#82,#CF,#B1,#82,#CF,#63,#81,#CF,#6A,#81,#CF,#70,#81 ; #bcd0 ........c..j..p.
db #CF,#76,#81,#CF,#7D,#81,#CF,#83,#81,#CF,#B3,#81,#CF,#C5,#81,#CF ; #bce0 .v..}...........
db #D2,#81,#CF,#E2,#81,#CF,#27,#82,#CF,#84,#82,#CF,#55,#82,#CF,#19 ; #bcf0 ......'.....U...
db #82,#CF,#76,#82,#CF,#94,#82,#CF,#9A,#82,#CF,#8D,#82,#CF,#99,#80 ; #bd00 ..v.............
db #CF,#A3,#80,#CF,#ED,#85,#CF,#1C,#86,#CF,#B4,#87,#CF,#76,#87,#CF ; #bd10 .............v..
db #C0,#87,#CF,#86,#87,#CF,#8C,#87,#CF,#E0,#87,#CF,#1B,#88,#CF,#58 ; #bd20 ...............X
db #88,#CF,#44,#88,#CF,#63,#88,#CF,#BD,#88,#CF,#3C,#9D,#CF,#FE,#9B ; #bd30 ..D..c.....<....
db #CF,#60,#94,#CF,#EC,#95,#CF,#D5,#99,#CF,#B0,#97,#CF,#AC,#97,#CF ; #bd40 .`..............
db #2A,#96,#CF,#D9,#99,#CF,#45,#8B,#CF,#0C,#88,#CF,#97,#83,#CF,#02 ; #bd50 *.....E.........
db #AC,#EF,#91,#2F,#EF,#9F,#2F,#EF,#C8,#2F,#EF,#D9,#2F,#EF,#01,#30 ; #bd60 .../../../../..0
db #EF,#14,#30,#EF,#55,#30,#EF,#5F,#30,#EF,#C6,#30,#EF,#A2,#34,#EF ; #bd70 ..0.U0._0..0..4.
db #59,#31,#EF,#9E,#34,#EF,#77,#35,#EF,#04,#36,#EF,#88,#31,#EF,#DF ; #bd80 Y1..4.w5..6..1..
db #36,#EF,#31,#37,#EF,#27,#37,#EF,#45,#33,#EF,#73,#2F,#EF,#AC,#32 ; #bd90 6.17.'7.E3.s/..2
db #EF,#AF,#32,#EF,#B6,#31,#EF,#B1,#31,#EF,#2F,#32,#EF,#53,#33,#EF ; #bda0 ..2..1..1./2.S3.
db #49,#33,#EF,#C8,#33,#EF,#D8,#33,#EF,#D1,#2F,#EF,#36,#31,#EF,#43 ; #bdb0 I3..3..3../.61.C
db #31,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#C3,#5F,#12 ; #bdc0 1............._.
db #C3,#5F,#12,#C3,#4B,#13,#C3,#BE,#13,#C3,#0A,#14,#C3,#86,#17,#C3 ; #bdd0 ._..K...........
db #9A,#17,#C3,#B4,#17,#C3,#8A,#0C,#C3,#71,#0C,#C3,#17,#0B,#C3,#B8 ; #bde0 .........q......
db #1D,#C3,#35,#08,#C3,#40,#1D,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #bdf0 ..5..@..........





org #100
db #6A,#01,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#00,#10,#01,#11,#02,#12 ; #100 j...............
db #03,#13,#04,#14,#05,#15,#06,#16,#07,#17,#08,#18,#09,#19,#0A,#1A ; #110 
db #0B,#1B,#0C,#1C,#0D,#1D,#0E,#1E,#0F,#1F,#20,#24,#21,#25,#22,#26 ; #120 .......... $!%"&
db #23,#27,#28,#FF,#29,#FF,#2A,#FF,#2B,#FF,#28,#FF,#29,#FF,#2A,#FF ; #130 #'(.).*.+.(.).*.
db #2B,#FF,#2C,#FF,#2D,#FF,#2E,#FF,#2F,#FF,#30,#FF,#33,#FF,#36,#FF ; #140 +.,.-.../.0.3.6.
db #39,#FF,#31,#FF,#34,#FF,#37,#FF,#3A,#FF,#32,#FF,#35,#FF,#38,#FF ; #150 9.1.4.7.:.2.5.8.
db #3B,#FF,#3C,#FF,#3D,#FF,#3E,#FF,#3F,#FF,#07,#5C,#24,#06,#2A,#03 ; #160 ;.<.=.>.?..\$.*.
db #00,#08,#5F,#21,#07,#2F,#03,#01,#0A,#64,#1C,#09,#34,#03,#02,#0D ; #170 .._!./...d..4...
db #6C,#14,#0B,#3A,#03,#03,#07,#58,#28,#06,#43,#03,#00,#08,#5B,#25 ; #180 l..:...X(.C...[%
db #07,#48,#03,#01,#0A,#5F,#21,#09,#4D,#03,#02,#0D,#66,#1A,#0B,#53 ; #190 .H..._!.M...f..S
db #03,#03,#07,#54,#2C,#06,#5C,#03,#00,#08,#57,#29,#07,#61,#03,#01 ; #1a0 ...T,.\...W).a..
db #0A,#5A,#26,#09,#66,#03,#02,#0D,#5F,#21,#0B,#6C,#03,#03,#07,#50 ; #1b0 .Z&.f..._!.l...P
db #30,#06,#75,#03,#00,#08,#53,#2D,#07,#7A,#03,#01,#0A,#55,#2B,#09 ; #1c0 0.u...S-.z...U+.
db #7F,#03,#02,#0D,#59,#27,#0B,#85,#03,#03,#04,#4C,#34,#06,#91,#03 ; #1d0 ....Y'.....L4...
db #04,#04,#4E,#32,#07,#94,#03,#05,#05,#50,#30,#09,#97,#03,#06,#05 ; #1e0 ..N2.....P0.....
db #55,#2B,#0B,#9B,#03,#07,#04,#48,#38,#06,#9F,#03,#04,#04,#4A,#36 ; #1f0 U+.....H8.....J6
db #07,#A2,#03,#05,#05,#4B,#35,#09,#A5,#03,#06,#05,#4E,#32,#0B,#A9 ; #200 .....K5.....N2..
db #03,#07,#04,#44,#3C,#06,#AD,#03,#04,#04,#44,#3C,#07,#B0,#03,#05 ; #210 ...D<.....D<....
db #05,#45,#3B,#09,#B3,#03,#06,#05,#47,#39,#0B,#B7,#03,#07,#04,#40 ; #220 .E;.....G9.....@
db #40,#06,#BB,#03,#04,#04,#40,#40,#07,#BE,#03,#05,#05,#40,#40,#09 ; #230 @.....@@.....@@.
db #C1,#03,#06,#05,#40,#40,#0B,#C5,#03,#07,#0C,#50,#2F,#06,#C9,#03 ; #240 ....@@.....P/...
db #08,#0E,#53,#2D,#07,#D4,#03,#09,#0F,#56,#29,#09,#E0,#03,#0A,#12 ; #250 ..S-.....V).....
db #5B,#23,#0B,#ED,#03,#0B,#0C,#40,#40,#06,#C9,#03,#08,#0E,#40,#40 ; #260 [#.....@@.....@@
db #07,#D4,#03,#09,#0F,#40,#40,#09,#E0,#03,#0A,#12,#40,#40,#0B,#ED ; #270 .....@@.....@@..
db #03,#0B,#05,#60,#1F,#06,#FC,#03,#0C,#06,#65,#1A,#07,#FC,#03,#0D ; #280 ...`......e.....
db #08,#6B,#14,#09,#FC,#03,#0E,#0B,#74,#0B,#0B,#FC,#03,#0F,#0C,#52 ; #290 .k......t......R
db #2D,#06,#92,#04,#08,#0E,#54,#2B,#07,#9E,#04,#09,#0F,#57,#28,#09 ; #2a0 -.....T+.....W(.
db #AC,#04,#0A,#12,#5C,#23,#0B,#BB,#04,#0B,#0C,#57,#28,#05,#FC,#03 ; #2b0 ....\#.....W(...
db #10,#0C,#4B,#34,#05,#10,#04,#10,#0C,#40,#3F,#05,#1C,#04,#10,#0F ; #2c0 ..K4.....@?.....
db #5A,#25,#05,#FC,#03,#12,#0F,#4D,#32,#05,#28,#04,#12,#0F,#40,#3F ; #2d0 Z%.....M2.(...@?
db #05,#37,#04,#12,#12,#5E,#21,#05,#FC,#03,#14,#12,#4F,#30,#05,#46 ; #2e0 .7...^!.....O0.F
db #04,#14,#12,#40,#3F,#05,#58,#04,#14,#14,#67,#18,#08,#FC,#03,#16 ; #2f0 ...@?.X...g.....
db #14,#53,#2C,#08,#6A,#04,#16,#14,#40,#3F,#08,#7E,#04,#16,#0C,#51 ; #300 .S,.j...@?.~...Q
db #2E,#07,#CD,#04,#18,#0C,#53,#2C,#07,#D9,#04,#18,#0E,#55,#2A,#09 ; #310 ......S,.....U*.
db #D9,#04,#1A,#0E,#56,#29,#09,#E7,#04,#1A,#01,#01,#01,#01,#03,#01 ; #320 ....V)..........
db #01,#01,#01,#04,#01,#01,#01,#01,#01,#05,#01,#01,#01,#01,#01,#01 ; #330 
db #01,#06,#01,#01,#01,#01,#01,#07,#01,#01,#01,#01,#08,#01,#01,#01 ; #340 
db #01,#01,#0A,#01,#01,#01,#01,#01,#01,#01,#0C,#01,#01,#01,#01,#01 ; #350 
db #0B,#01,#01,#01,#01,#0C,#01,#01,#01,#01,#01,#0F,#01,#01,#01,#01 ; #360 
db #01,#01,#01,#13,#01,#01,#01,#01,#01,#0F,#01,#01,#01,#01,#10,#01 ; #370 
db #01,#01,#01,#01,#14,#01,#01,#01,#01,#01,#01,#01,#19,#01,#01,#01 ; #380 
db #01,#01,#04,#01,#01,#05,#01,#01,#06,#01,#01,#01,#07,#01,#01,#01 ; #390 
db #08,#01,#01,#09,#01,#01,#0B,#01,#01,#01,#0E,#01,#01,#01,#0C,#01 ; #3a0 
db #01,#0F,#01,#01,#11,#01,#01,#01,#15,#01,#01,#01,#10,#01,#01,#13 ; #3b0 
db #01,#01,#16,#01,#01,#01,#1C,#01,#01,#01,#01,#01,#01,#01,#01,#01 ; #3c0 
db #01,#01,#01,#09,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#0A ; #3d0 
db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#0C,#01,#01,#01 ; #3e0 
db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#0F,#01,#01,#01,#01 ; #3f0 
db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01 ; #400 
db #02,#02,#02,#02,#02,#03,#02,#02,#02,#02,#02,#01,#03,#03,#03,#03 ; #410 
db #03,#04,#03,#03,#03,#03,#03,#01,#02,#02,#02,#02,#02,#02,#02,#02 ; #420 
db #02,#02,#02,#02,#02,#01,#01,#03,#03,#03,#03,#03,#03,#03,#03,#03 ; #430 
db #03,#03,#03,#02,#02,#01,#02,#02,#02,#02,#02,#02,#02,#02,#02,#02 ; #440 
db #02,#02,#02,#02,#02,#01,#01,#01,#03,#03,#03,#03,#03,#03,#03,#03 ; #450 
db #03,#03,#03,#03,#03,#02,#02,#02,#02,#01,#02,#02,#02,#02,#02,#02 ; #460 
db #02,#02,#02,#03,#02,#02,#02,#02,#02,#02,#02,#02,#02,#01,#03,#03 ; #470 
db #03,#03,#03,#03,#03,#03,#03,#04,#03,#03,#03,#03,#03,#03,#03,#03 ; #480 
db #03,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#11,#01,#00,#00 ; #490 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#14,#01,#01,#00,#00,#00,#00 ; #4a0 
db #00,#00,#00,#00,#00,#00,#00,#00,#17,#01,#01,#00,#00,#00,#00,#00 ; #4b0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#1C,#01,#01,#01,#01,#01,#02 ; #4c0 
db #02,#02,#01,#01,#02,#02,#02,#01,#01,#02,#02,#02,#02,#02,#02,#02 ; #4d0 
db #02,#02,#02,#02,#02,#02,#02,#02,#03,#03,#03,#03,#03,#02,#02,#03 ; #4e0 
db #03,#03,#03,#03,#02,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #4f0 
db #C0,#95,#24,#A4,#95,#13,#95,#9C,#24,#98,#24,#95,#24,#91,#24,#A4 ; #500 ..$.....$.$.$.$.
db #8E,#10,#91,#93,#15,#97,#18,#9A,#1C,#95,#24,#A4,#95,#13,#95,#9C ; #510 ..........$.....
db #24,#98,#24,#95,#24,#91,#24,#A4,#8E,#10,#91,#93,#15,#97,#18,#9A ; #520 $.$.$.$.........
db #1C,#95,#1C,#9A,#95,#10,#8C,#90,#17,#95,#10,#8C,#09,#93,#1A,#98 ; #530 
db #93,#0F,#8C,#91,#18,#96,#11,#8D,#0A,#95,#1C,#9A,#95,#10,#8C,#90 ; #540 
db #17,#95,#10,#8C,#09,#85,#0C,#89,#90,#0C,#93,#90,#17,#93,#1A,#97 ; #550 
db #13,#95,#24,#A4,#95,#13,#95,#9C,#24,#98,#24,#95,#24,#91,#24,#A4 ; #560 ..$.....$.$.$.$.
db #8E,#10,#91,#93,#15,#97,#18,#9A,#1C,#95,#24,#A4,#95,#13,#95,#9C ; #570 ..........$.....
db #24,#98,#24,#95,#24,#91,#24,#A4,#8E,#10,#91,#93,#15,#97,#18,#9A ; #580 $.$.$.$.........
db #1C,#95,#1C,#9A,#95,#10,#8C,#90,#17,#95,#10,#8C,#09,#93,#1A,#98 ; #590 
db #93,#0F,#8C,#91,#18,#96,#11,#8D,#0A,#95,#1C,#9A,#95,#10,#8C,#90 ; #5a0 
db #17,#95,#10,#8C,#09,#85,#0C,#89,#90,#0C,#93,#90,#17,#93,#1A,#97 ; #5b0 
db #13,#95,#15,#95,#95,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #5c0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #5d0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #5e0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #5f0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #600 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #610 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #620 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #630 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #640 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #650 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #660 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #670 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #680 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #690 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #6a0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #6b0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #6c0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #6d0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #6e0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #6f0 
db #00,#80,#40,#C0,#20,#A0,#60,#E0,#10,#90,#50,#D0,#30,#B0,#70,#F0 ; #700 ..@. .`...P.0.p.
db #08,#88,#48,#C8,#28,#A8,#68,#E8,#18,#98,#58,#D8,#38,#B8,#78,#F8 ; #710 ..H.(.h...X.8.x.
db #04,#84,#44,#C4,#24,#A4,#64,#E4,#14,#94,#54,#D4,#34,#B4,#74,#F4 ; #720 ..D.$.d...T.4.t.
db #0C,#8C,#4C,#CC,#2C,#AC,#6C,#EC,#1C,#9C,#5C,#DC,#3C,#BC,#7C,#FC ; #730 ..L.,.l...\.<.|.
db #02,#82,#42,#C2,#22,#A2,#62,#E2,#12,#92,#52,#D2,#32,#B2,#72,#F2 ; #740 ..B.".b...R.2.r.
db #0A,#8A,#4A,#CA,#2A,#AA,#6A,#EA,#1A,#9A,#5A,#DA,#3A,#BA,#7A,#FA ; #750 ..J.*.j...Z.:.z.
db #06,#86,#46,#C6,#26,#A6,#66,#E6,#16,#96,#56,#D6,#36,#B6,#76,#F6 ; #760 ..F.&.f...V.6.v.
db #0E,#8E,#4E,#CE,#2E,#AE,#6E,#EE,#1E,#9E,#5E,#DE,#3E,#BE,#7E,#FE ; #770 ..N...n...^.>.~.
db #01,#81,#41,#C1,#21,#A1,#61,#E1,#11,#91,#51,#D1,#31,#B1,#71,#F1 ; #780 ..A.!.a...Q.1.q.
db #09,#89,#49,#C9,#29,#A9,#69,#E9,#19,#99,#59,#D9,#39,#B9,#79,#F9 ; #790 ..I.).i...Y.9.y.
db #05,#85,#45,#C5,#25,#A5,#65,#E5,#15,#95,#55,#D5,#35,#B5,#75,#F5 ; #7a0 ..E.%.e...U.5.u.
db #0D,#8D,#4D,#CD,#2D,#AD,#6D,#ED,#1D,#9D,#5D,#DD,#3D,#BD,#7D,#FD ; #7b0 ..M.-.m...].=.}.
db #03,#83,#43,#C3,#23,#A3,#63,#E3,#13,#93,#53,#D3,#33,#B3,#73,#F3 ; #7c0 ..C.#.c...S.3.s.
db #0B,#8B,#4B,#CB,#2B,#AB,#6B,#EB,#1B,#9B,#5B,#DB,#3B,#BB,#7B,#FB ; #7d0 ..K.+.k...[.;.{.
db #07,#87,#47,#C7,#27,#A7,#67,#E7,#17,#97,#57,#D7,#37,#B7,#77,#F7 ; #7e0 ..G.'.g...W.7.w.
db #0F,#8F,#4F,#CF,#2F,#AF,#6F,#EF,#1F,#9F,#5F,#DF,#3F,#BF,#7F,#FF ; #7f0 ..O./.o..._.?...
db #A8,#C0,#A8,#C8,#A8,#D0,#A8,#D8,#A8,#E0,#A8,#E8,#A8,#F0,#A8,#F8 ; #800 
db #F8,#C0,#F8,#C8,#F8,#D0,#F8,#D8,#F8,#E0,#F8,#E8,#F8,#F0,#F8,#F8 ; #810 
db #48,#C1,#48,#C9,#48,#D1,#48,#D9,#48,#E1,#48,#E9,#48,#F1,#48,#F9 ; #820 H.H.H.H.H.H.H.H.
db #98,#C1,#98,#C9,#98,#D1,#98,#D9,#98,#E1,#98,#E9,#98,#F1,#98,#F9 ; #830 
db #E8,#C1,#E8,#C9,#E8,#D1,#E8,#D9,#E8,#E1,#E8,#E9,#E8,#F1,#E8,#F9 ; #840 
db #38,#C2,#38,#CA,#38,#D2,#38,#DA,#38,#E2,#38,#EA,#38,#F2,#38,#FA ; #850 8.8.8.8.8.8.8.8.
db #88,#C2,#88,#CA,#88,#D2,#88,#DA,#88,#E2,#88,#EA,#88,#F2,#88,#FA ; #860 
db #D8,#C2,#D8,#CA,#D8,#D2,#D8,#DA,#D8,#E2,#D8,#EA,#D8,#F2,#D8,#FA ; #870 
db #28,#C3,#28,#CB,#28,#D3,#28,#DB,#28,#E3,#28,#EB,#28,#F3,#28,#FB ; #880 (.(.(.(.(.(.(.(.
db #78,#C3,#78,#CB,#78,#D3,#78,#DB,#78,#E3,#78,#EB,#78,#F3,#78,#FB ; #890 x.x.x.x.x.x.x.x.
db #C8,#C3,#C8,#CB,#C8,#D3,#C8,#DB,#C8,#E3,#C8,#EB,#C8,#F3,#C8,#FB ; #8a0 
db #18,#C4,#18,#CC,#18,#D4,#18,#DC,#18,#E4,#18,#EC,#18,#F4,#18,#FC ; #8b0 
db #68,#C4,#68,#CC,#68,#D4,#68,#DC,#68,#E4,#68,#EC,#68,#F4,#68,#FC ; #8c0 h.h.h.h.h.h.h.h.
db #B8,#C4,#B8,#CC,#B8,#D4,#B8,#DC,#B8,#E4,#B8,#EC,#B8,#F4,#B8,#FC ; #8d0 
db #08,#C5,#08,#CD,#08,#D5,#08,#DD,#08,#E5,#08,#ED,#08,#F5,#08,#FD ; #8e0 
db #58,#C5,#58,#CD,#58,#D5,#58,#DD,#58,#E5,#58,#ED,#58,#F5,#58,#FD ; #8f0 X.X.X.X.X.X.X.X.

font:
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01,#01,#03,#03,#03 ; #900 
db #FF,#FF,#F3,#F3,#F3,#E6,#E6,#E6,#FF,#FF,#1C,#18,#18,#10,#10,#04 ; #910 
db #FF,#FF,#60,#60,#66,#CF,#CE,#C0,#FF,#F0,#C0,#00,#00,#02,#00,#10 ; #920 
db #FF,#FF,#38,#10,#4B,#0F,#07,#04,#FF,#FF,#3C,#18,#99,#F3,#F3,#F0 ; #930 
db #FF,#FF,#1F,#0E,#CE,#FC,#FC,#78,#FF,#FF,#1E,#0E,#4E,#CC,#CC,#0C ; #940 
db #FF,#FF,#07,#03,#73,#E7,#E7,#0E,#FF,#FF,#C7,#83,#93,#33,#33,#03 ; #950 
db #FF,#FF,#9F,#9F,#9F,#3F,#3F,#3F,#FF,#FF,#CF,#CF,#CF,#9F,#9F,#9F ; #960 
db #F8,#F8,#F0,#F6,#F0,#EC,#E0,#EC,#07,#07,#07,#0F,#0F,#0F,#1F,#1F ; #970 
; '0'
db #00,#7C,#FE,#C6,#C6,#C6,#FE,#7C,#00,#38,#38,#18,#18,#18,#18,#18 ; #980 
db #00,#7C,#FE,#06,#7C,#C0,#FE,#FE,#00,#7C,#FE,#06,#3C,#06,#FE,#7C ; #990 
db #00,#3C,#7C,#EC,#CC,#FE,#FE,#0C,#00,#FE,#FE,#C0,#FC,#06,#FE,#7C ; #9a0 
db #00,#7C,#FE,#C0,#FC,#C6,#FE,#7C,#00,#FE,#FE,#06,#0C,#0C,#18,#18 ; #9b0 
db #00,#7C,#FE,#C6,#7C,#C6,#FE,#7C,#00,#7C,#FE,#C6,#7E,#06,#FE,#7C ; #9c0 

db #CC,#CC,#CC,#99,#99,#99,#FF,#FF,#81,#89,#89,#93,#93,#B3,#FF,#FF ; #9d0 
db #80,#9E,#9F,#3F,#3F,#3F,#FF,#FF,#00,#40,#00,#00,#80,#C0,#F0,#FF ; #9e0 
db #04,#06,#0E,#0C,#10,#21,#FF,#FF,#00,#06,#0E,#1C,#38,#70,#E0,#C0 ; #9f0 
; @A
db #00,#7C,#FE,#D6,#DE,#C0,#FE,#7C,#00,#7C,#FE,#C6,#FE,#C6,#C6,#C6 ; #a00 
db #00,#FC,#FE,#C6,#FC,#C6,#FE,#FC,#00,#7C,#FE,#C6,#C0,#C6,#FE,#7C ; #a10 
db #00,#FC,#FE,#C6,#C6,#C6,#FE,#FC,#00,#FE,#FE,#C2,#F8,#C2,#FE,#FE ; #a20 
db #00,#FE,#FE,#C2,#F8,#C0,#C0,#C0,#00,#7C,#FE,#C0,#DE,#C6,#FE,#7C ; #a30 
db #00,#C6,#C6,#C6,#FE,#C6,#C6,#C6,#00,#FE,#FE,#38,#38,#38,#FE,#FE ; #a40 
db #00,#FE,#FE,#38,#38,#38,#F8,#70,#00,#C6,#CE,#FC,#F8,#DC,#CE,#C6 ; #a50 
db #00,#C0,#C0,#C0,#C0,#C2,#FE,#FE,#00,#C6,#EE,#FE,#D6,#D6,#C6,#C6 ; #a60 
db #00,#C6,#E6,#F6,#FE,#DE,#CE,#C6,#00,#7C,#FE,#C6,#C6,#C6,#FE,#7C ; #a70 
db #00,#FC,#FE,#C6,#FC,#C0,#C0,#C0,#00,#7C,#FE,#C6,#C6,#CA,#F4,#7A ; #a80 
db #00,#FC,#FE,#C6,#FC,#C6,#C6,#C6,#00,#7C,#FE,#C0,#7C,#06,#FE,#7C ; #a90 
db #00,#FE,#FE,#38,#38,#38,#38,#38,#00,#C6,#C6,#C6,#C6,#C6,#FE,#7C ; #aa0 
db #00,#C6,#C6,#C6,#EE,#7C,#38,#10,#00,#C6,#C6,#C6,#D6,#D6,#FE,#6C ; #ab0 
db #00,#C6,#EE,#7C,#38,#7C,#EE,#C6,#00,#C6,#C6,#EE,#7C,#38,#38,#38 ; #ac0 
; Z
db #00,#FE,#FE,#1C,#38,#70,#FE,#FE
; <-
db #00,#10,#30,#7E,#7E,#30,#10,#00 
db #00,#00,#00,#C6,#6C,#38,#10,#00,#00,#18,#3C,#7E,#7E,#3C,#18,#00 ; #ae0 
db #00,#00,#10,#38,#6C,#C6,#00,#00,#00,#00,#00,#00,#00,#7E,#7E,#00 ; #af0 
; digits
db #00,#0F,#27,#30,#30,#30,#30,#20,#00,#0F,#07,#00,#00,#00,#00,#07 ; #b00 
db #00,#00,#20,#30,#30,#30,#30,#27,#00,#0F,#27,#30,#30,#30,#30,#27 ; #b10 
db #00,#0F,#07,#00,#00,#00,#00,#00,#00,#F0,#E4,#0C,#0C,#0C,#0C,#04 ; #b20 
db #00,#00,#04,#0C,#0C,#0C,#0C,#04,#00,#F0,#E4,#0C,#0C,#0C,#0C,#E4 ; #b30 
db #00,#00,#04,#0C,#0C,#0C,#0C,#E4,#00,#F0,#E0,#00,#00,#00,#00,#E0 ; #b40 
db #00,#20,#30,#30,#30,#30,#27,#0F,#0F,#27,#30,#30,#30,#30,#27,#0F ; #b50 
db #0F,#07,#00,#00,#00,#00,#07,#0F,#0F,#07,#00,#00,#00,#00,#00,#00 ; #b60 
db #00,#04,#0C,#0C,#0C,#0C,#E4,#F0,#00,#04,#0C,#0C,#0C,#0C,#04,#00 ; #b70 
db #F0,#E0,#00,#00,#00,#00,#E0,#F0,#F0,#E4,#0C,#0C,#0C,#0C,#E4,#F0 ; #b80 
db #F0,#E4,#0C,#0C,#0C,#0C,#04,#00,#78,#7F,#7F,#4E,#C0,#E0,#FF,#FF ; #b90 ........x..N....
db #30,#33,#33,#27,#67,#E7,#FF,#FF,#18,#99,#99,#33,#30,#30,#FF,#FF ; #ba0 033'g......300..
db #0C,#CC,#CC,#99,#19,#39,#FF,#FF,#06,#E6,#E6,#CC,#CC,#CC,#FF,#FF ; #bb0 .....9..........
db #7F,#7F,#7F,#FE,#06,#06,#FF,#FF,#3F,#3F,#3F,#7F,#03,#03,#FF,#FF ; #bc0 ........???.....
db #C0,#D8,#C0,#B0,#80,#B0,#00,#60,#00,#01,#00,#07,#00,#00,#00,#00 ; #bd0 .......`........
db #00,#FF,#00,#FF,#00,#00,#00,#00,#00,#C0,#00,#C0,#00,#00,#00,#00 ; #be0 
db #29,#0D,#04,#BA,#19,#F2,#73,#65,#6E,#64,#62,#79,#74,#65,#28,#6C ; #bf0 ).....sendbyte(l


db #08,#00,#04,#08,#0C,#10,#14,#18,#1C,#04,#04,#04,#04,#04,#04,#04 ; #c00 
db #04,#0C,#14,#12,#08,#16,#18,#04,#0A,#A4,#06,#E1,#06,#FA,#06,#37 ; #c10 ...............7
db #07,#50,#07,#8D,#07,#EE,#07,#37,#08,#00,#00,#30,#00,#58,#00,#80 ; #c20 .P.....7...0.X..
db #00,#B0,#00,#DC,#00,#10,#01,#58,#01,#88,#01,#AC,#01,#E4,#01,#14 ; #c30 .......X........
db #02,#5C,#02,#8E,#02,#C2,#02,#EA,#02,#22,#03,#52,#03,#9E,#03,#C4 ; #c40 .\.......".R....
db #03,#EA,#03,#1A,#04,#5A,#04,#90,#04,#C0,#04,#F0,#04,#22,#05,#52 ; #c50 .....Z.......".R
db #05,#72,#05,#9E,#05,#C6,#05,#EE,#05,#00,#00,#00,#00,#03,#08,#C8 ; #c60 .r..............
db #04,#D8,#08,#E8,#02,#05,#C8,#05,#E8,#03,#05,#48,#04,#58,#05,#68 ; #c70 ...........H.X.h
db #00,#03,#08,#C8,#04,#D8,#08,#E8,#02,#05,#C8,#05,#E8,#03,#05,#48 ; #c80 ...............H
db #04,#58,#05,#68,#00,#00,#00,#00,#00,#02,#05,#68,#05,#A8,#00,#01 ; #c90 .X.h.......h....
db #0C,#78,#00,#02,#05,#78,#05,#B8,#00,#01,#0C,#78,#00,#02,#05,#88 ; #ca0 .x...x.....x....
db #05,#C8,#00,#01,#0C,#D8,#00,#02,#05,#98,#05,#D8,#00,#01,#0C,#D8 ; #cb0 
db #00,#01,#0C,#98,#00,#01,#04,#58,#00,#01,#05,#F8,#00,#02,#04,#68 ; #cc0 .......X.......h
db #04,#F8,#01,#04,#68,#02,#04,#78,#05,#F8,#01,#04,#78,#01,#04,#F8 ; #cd0 ....h..x....x...
db #00,#01,#04,#88,#00,#01,#0C,#38,#00,#02,#05,#78,#0C,#F8,#00,#02 ; #ce0 .......8...x....
db #05,#68,#05,#88,#00,#03,#05,#68,#05,#88,#0C,#F8,#00,#01,#05,#78 ; #cf0 .h.....h.......x
db #00,#02,#05,#78,#0C,#F8,#00,#02,#05,#68,#05,#88,#00,#03,#05,#68 ; #d00 ...x.....h.....h
db #05,#88,#0C,#F8,#00,#01,#05,#78,#00,#02,#09,#48,#07,#B8,#00,#02 ; #d10 .......x...H....
db #09,#48,#08,#B8,#01,#05,#B8,#01,#09,#48,#00,#01,#09,#48,#00,#01 ; #d20 .H.......H...H..
db #09,#48,#00,#01,#09,#48,#00,#02,#09,#48,#07,#B8,#00,#02,#09,#48 ; #d30 .H...H...H.....H
db #08,#B8,#01,#05,#B8,#01,#09,#A8,#00,#01,#04,#A8,#01,#04,#A8,#02 ; #d40 
db #07,#18,#09,#68,#00,#02,#04,#18,#04,#68,#02,#04,#18,#04,#68,#02 ; #d50 ...h.....h....h.
db #07,#18,#09,#68,#00,#02,#04,#18,#04,#68,#02,#04,#18,#04,#68,#01 ; #d60 ...h.....h....h.
db #09,#A8,#00,#01,#04,#A8,#01,#04,#A8,#02,#05,#38,#08,#D8,#01,#05 ; #d70 ...........8....
db #D8,#02,#08,#38,#05,#D8,#01,#05,#38,#04,#05,#38,#04,#58,#08,#D8 ; #d80 ...8....8..8.X..
db #04,#F8,#01,#05,#D8,#02,#08,#38,#05,#D8,#01,#05,#38,#04,#05,#38 ; #d90 .......8....8..8
db #04,#58,#08,#D8,#04,#F8,#01,#05,#D8,#02,#08,#38,#05,#D8,#01,#05 ; #da0 .X.........8....
db #38,#02,#05,#38,#08,#D8,#01,#05,#D8,#02,#08,#38,#05,#D8,#01,#05 ; #db0 8..8.......8....
db #38,#03,#05,#88,#04,#98,#05,#A8,#00,#01,#05,#98,#00,#03,#05,#88 ; #dc0 8...............
db #04,#98,#05,#A8,#00,#01,#05,#98,#00,#03,#05,#88,#04,#98,#05,#A8 ; #dd0 
db #00,#01,#05,#98,#00,#03,#05,#88,#04,#98,#05,#A8,#00,#01,#05,#98 ; #de0 
db #00,#01,#09,#58,#00,#01,#09,#B8,#00,#01,#08,#88,#01,#05,#88,#01 ; #df0 ...X............
db #05,#88,#00,#01,#08,#88,#01,#05,#88,#01,#05,#88,#00,#01,#09,#38 ; #e00 ...............8
db #00,#01,#09,#98,#00,#04,#04,#18,#05,#28,#04,#38,#09,#98,#00,#00 ; #e10 .........(.8....
db #00,#04,#05,#18,#04,#28,#05,#38,#09,#F8,#00,#01,#04,#C8,#01,#04 ; #e20 .....(.8........
db #C8,#04,#04,#18,#05,#28,#04,#38,#09,#98,#00,#01,#04,#C8,#01,#04 ; #e30 .....(.8........
db #C8,#04,#05,#18,#04,#28,#05,#38,#09,#F8,#00,#00,#00,#00,#00,#00 ; #e40 .....(.8........
db #00,#04,#07,#38,#08,#68,#08,#A8,#09,#F8,#02,#05,#68,#05,#A8,#02 ; #e50 ...8.h......h...
db #04,#68,#04,#A8,#00,#04,#07,#38,#08,#68,#08,#A8,#09,#F8,#02,#05 ; #e60 .h.....8.h......
db #68,#05,#A8,#02,#04,#68,#04,#A8,#00,#00,#00,#00,#00,#01,#05,#78 ; #e70 h....h.........x
db #00,#02,#09,#68,#09,#88,#00,#05,#05,#58,#09,#78,#05,#98,#07,#C8 ; #e80 ...h.....X.x....
db #07,#D8,#00,#03,#08,#58,#08,#98,#04,#D8,#03,#05,#58,#05,#98,#04 ; #e90 .....X......X...
db #D8,#05,#05,#58,#09,#78,#05,#98,#07,#C8,#07,#D8,#00,#03,#08,#58 ; #ea0 ...X.x.........X
db #08,#98,#04,#D8,#03,#05,#58,#05,#98,#04,#D8,#01,#05,#78,#00,#02 ; #eb0 ......X......x..
db #09,#68,#09,#88,#00,#02,#05,#68,#0C,#88,#00,#01,#08,#68,#01,#05 ; #ec0 .h.....h.....h..
db #68,#03,#05,#68,#0C,#88,#07,#D8,#00,#01,#08,#68,#01,#05,#68,#02 ; #ed0 h..h.......h..h.
db #05,#68,#0C,#88,#00,#01,#08,#68,#01,#05,#68,#02,#05,#68,#0C,#88 ; #ee0 .h.....h..h..h..
db #00,#01,#08,#68,#01,#05,#68,#02,#08,#58,#0C,#88,#01,#05,#58,#01 ; #ef0 ...h..h..X....X.
db #05,#58,#00,#02,#08,#58,#0C,#88,#01,#05,#58,#02,#05,#58,#0C,#B8 ; #f00 .X...X....X..X..
db #00,#02,#08,#58,#0C,#88,#01,#05,#58,#02,#05,#58,#0C,#B8,#00,#02 ; #f10 ...X....X..X....
db #08,#58,#0C,#88,#01,#05,#58,#01,#05,#58,#00,#01,#04,#B8,#00,#01 ; #f20 .X....X..X......
db #07,#B8,#00,#02,#04,#08,#0C,#58,#00,#01,#04,#08,#01,#04,#08,#02 ; #f30 .......X........
db #04,#08,#0C,#58,#01,#04,#08,#01,#04,#08,#00,#01,#04,#B8,#00,#01 ; #f40 ...X............
db #07,#B8,#00,#03,#0C,#38,#05,#58,#0C,#78,#00,#01,#08,#58,#01,#05 ; #f50 .....8.X.x...X..
db #58,#03,#0C,#38,#05,#58,#0C,#78,#00,#01,#08,#58,#01,#05,#58,#03 ; #f60 X..8.X.x...X..X.
db #0C,#38,#05,#58,#0C,#78,#00,#01,#08,#58,#01,#05,#58,#03,#0C,#38 ; #f70 .8.X.x...X..X..8
db #05,#58,#0C,#78,#00,#01,#08,#58,#01,#05,#58,#00,#00,#00,#00,#03 ; #f80 .X.x...X..X.....
db #09,#08,#05,#58,#09,#F8,#00,#03,#04,#58,#0C,#98,#0C,#C8,#02,#04 ; #f90 ...X.....X......
db #98,#04,#C8,#03,#09,#08,#05,#58,#09,#F8,#00,#03,#04,#58,#0C,#98 ; #fa0 .......X.....X..
db #0C,#C8,#02,#04,#98,#04,#C8,#00,#00,#00,#00,#02,#08,#C8,#07,#D8 ; #fb0 
db #01,#05,#C8,#01,#05,#E8,#00,#05,#05,#08,#09,#58,#08,#B8,#07,#E8 ; #fc0 ...........X....
db #08,#F8,#02,#05,#B8,#05,#F8,#03,#04,#08,#04,#58,#05,#D8,#01,#04 ; #fd0 ...........X....
db #58,#04,#05,#08,#09,#58,#08,#C8,#07,#D8,#01,#05,#C8,#03,#04,#08 ; #fe0 X....X..........
db #04,#58,#05,#E8,#01,#04,#58,#03,#08,#B8,#07,#E8,#08,#F8,#02,#05 ; #ff0 .X....X.........
db #B8,#05,#F8,#01,#05,#D8,#00,#01,#08,#08,#01,#05,#08,#00,#00,#02 ; #1000 
db #04,#C8,#04,#E8,#00,#01,#09,#B8,#00,#04,#08,#08,#04,#B8,#04,#D8 ; #1010 
db #04,#F8,#01,#05,#08,#01,#09,#B8,#00,#00,#00,#00,#00,#01,#07,#F8 ; #1020 
db #00,#00,#00,#04,#04,#08,#04,#28,#04,#48,#07,#F8,#00,#01,#09,#48 ; #1030 .......(.H.....H
db #00,#03,#04,#18,#04,#38,#07,#F8,#00,#01,#09,#48,#00,#01,#07,#F8 ; #1040 .....8.....H....
db #00,#00,#00,#00,#00,#02,#07,#58,#07,#88,#00,#02,#04,#68,#04,#78 ; #1050 .......X.....h.x
db #01,#04,#78,#02,#04,#68,#04,#78,#01,#04,#68,#02,#04,#68,#04,#78 ; #1060 ..x..h.x..h..h.x
db #01,#04,#68,#02,#04,#68,#04,#78,#01,#04,#78,#00,#00,#02,#07,#58 ; #1070 ..h..h.x..x....X
db #07,#88,#00,#03,#0C,#08,#0C,#28,#0C,#48,#00,#00,#00,#05,#0C,#08 ; #1080 .......(.H......
db #0C,#28,#0C,#48,#04,#B8,#04,#D8,#02,#04,#B8,#04,#D8,#02,#0C,#48 ; #1090 .(.H...........H
db #0C,#F8,#00,#05,#0C,#08,#0C,#28,#0C,#48,#04,#B8,#04,#D8,#02,#04 ; #10a0 .......(.H......
db #B8,#04,#D8,#02,#0C,#48,#0C,#F8,#00,#03,#0C,#08,#0C,#28,#0C,#48 ; #10b0 .....H.......(.H
db #00,#00,#00,#03,#05,#78,#04,#88,#05,#98,#00,#01,#05,#88,#00,#05 ; #10c0 .....x..........
db #07,#38,#05,#88,#04,#98,#05,#A8,#07,#F8,#00,#01,#05,#98,#00,#04 ; #10d0 .8..............
db #07,#48,#05,#98,#04,#A8,#05,#B8,#00,#01,#05,#A8,#00,#03,#05,#A8 ; #10e0 .H..............
db #04,#B8,#05,#C8,#00,#01,#05,#B8,#00,#01,#08,#68,#01,#05,#68,#01 ; #10f0 ...........h..h.
db #08,#68,#01,#05,#68,#01,#08,#68,#01,#05,#68,#01,#08,#68,#01,#05 ; #1100 .h..h..h..h..h..
db #68,#02,#07,#08,#08,#68,#01,#05,#68,#01,#05,#68,#00,#01,#08,#68 ; #1110 h....h..h..h...h
db #01,#05,#68,#01,#08,#68,#01,#05,#68,#03,#07,#48,#05,#68,#05,#88 ; #1120 ..h..h..h..H.h..
db #00,#01,#04,#68,#00,#03,#07,#48,#05,#68,#05,#88,#00,#01,#04,#68 ; #1130 ...h...H.h.....h
db #00,#03,#07,#48,#05,#68,#05,#88,#00,#01,#04,#68,#00,#03,#07,#48 ; #1140 ...H.h.....h...H
db #05,#68,#05,#88,#00,#01,#04,#68,#00,#01,#08,#48,#01,#05,#48,#01 ; #1150 .h.....h...H..H.
db #08,#48,#01,#05,#48,#01,#08,#48,#01,#05,#48,#01,#08,#48,#01,#05 ; #1160 .H..H..H..H..H..
db #48,#03,#05,#48,#09,#D8,#09,#F8,#00,#01,#08,#48,#01,#05,#48,#01 ; #1170 H..H.......H..H.
db #08,#48,#01,#05,#48,#01,#08,#48,#01,#05,#48,#01,#08,#68,#01,#05 ; #1180 .H..H..H..H..h..
db #68,#01,#08,#68,#01,#05,#68,#03,#05,#68,#09,#D8,#09,#F8,#00,#01 ; #1190 h..h..h..h......
db #05,#68,#00,#01,#08,#68,#01,#05,#68,#01,#08,#68,#01,#05,#68,#01 ; #11a0 .h...h..h..h..h.
db #08,#68,#01,#05,#68,#01,#08,#68,#01,#05,#68,#02,#09,#98,#09,#D8 ; #11b0 .h..h..h..h.....
db #00,#00,#00,#02,#09,#98,#09,#D8,#00,#00,#00,#02,#09,#98,#09,#D8 ; #11c0 
db #00,#00,#00,#02,#09,#98,#09,#D8,#00,#00,#00,#00,#00,#00,#00,#02 ; #11d0 
db #09,#A8,#09,#C8,#00,#05,#05,#58,#04,#68,#05,#78,#04,#88,#05,#98 ; #11e0 .......X.h.x....
db #00,#02,#09,#A8,#09,#C8,#00,#05,#05,#68,#04,#78,#05,#88,#04,#98 ; #11f0 .........h.x....
db #05,#A8,#00,#00,#00,#00,#00,#01,#08,#78,#01,#05,#78,#01,#04,#78 ; #1200 .........x..x..x
db #00,#01,#05,#78,#00,#01,#04,#78,#01,#04,#78,#01,#08,#78,#01,#05 ; #1210 ...x...x..x..x..
db #78,#01,#04,#78,#00,#01,#05,#78,#00,#01,#04,#78,#01,#04,#78,#01 ; #1220 x..x...x...x..x.
db #09,#98,#00,#02,#09,#98,#09,#B8,#00,#02,#09,#98,#09,#B8,#00,#01 ; #1230 
db #09,#98,#00,#01,#09,#98,#00,#02,#09,#98,#09,#B8,#00,#02,#09,#98 ; #1240 
db #09,#B8,#00,#01,#09,#98,#00,#02,#05,#68,#05,#88,#00,#03,#08,#58 ; #1250 .........h.....X
db #08,#78,#08,#98,#03,#05,#58,#05,#78,#05,#98,#03,#05,#58,#05,#78 ; #1260 .x....X.x....X.x
db #05,#98,#00,#02,#08,#68,#08,#88,#02,#05,#68,#05,#88,#02,#05,#68 ; #1270 .....h....h....h
db #05,#88,#00,#03,#08,#58,#08,#78,#08,#98,#03,#05,#58,#05,#78,#05 ; #1280 .....X.x....X.x.
db #98,#03,#05,#58,#05,#78,#05,#98,#00,#02,#08,#68,#08,#88,#02,#05 ; #1290 ...X.x.....h....
db #68,#05,#88,#FF,#0A,#A0,#00,#01,#78,#D8,#01,#AA,#00,#01,#78,#D8 ; #12a0 h.......x.....x.
db #01,#B4,#00,#FF,#78,#D8,#FF,#BE,#00,#FF,#78,#D8,#FF,#C2,#00,#FF ; #12b0 ....x.....x.....
db #18,#98,#FF,#E6,#00,#01,#38,#B8,#01,#EC,#00,#FF,#B8,#F8,#FF,#FA ; #12c0 ......8.........
db #00,#FF,#B8,#F8,#FF,#04,#01,#FF,#B8,#F8,#FF,#12,#01,#FF,#B8,#F8 ; #12d0 
db #FF,#04,#46,#01,#FF,#68,#A8,#FF,#52,#01,#01,#68,#A8,#01,#62,#01 ; #12e0 ..F..h..R..h..b.
db #01,#68,#A8,#01,#70,#01,#FF,#68,#A8,#FF,#0A,#F2,#01,#01,#58,#98 ; #12f0 .h..p..h......X.
db #01,#F6,#01,#FF,#78,#B8,#FF,#0E,#02,#01,#38,#78,#01,#12,#02,#FF ; #1300 ....x.....8x....
db #58,#98,#FF,#1C,#02,#01,#98,#F8,#01,#28,#02,#FF,#98,#F8,#FF,#38 ; #1310 X........(.....8
db #02,#01,#98,#F8,#01,#48,#02,#FF,#98,#F8,#FF,#58,#02,#FF,#B8,#F8 ; #1320 .....H.....X....
db #FF,#6C,#02,#FF,#B8,#F8,#FF,#04,#0E,#03,#01,#B8,#F8,#01,#1C,#03 ; #1330 .l..............
db #01,#B8,#F8,#01,#36,#03,#FF,#18,#58,#FF,#42,#03,#FF,#18,#58,#FF ; #1340 ....6...X.B...X.
db #0A,#90,#03,#01,#08,#48,#01,#94,#03,#FF,#58,#F8,#FF,#A4,#03,#01 ; #1350 .....H....X.....
db #08,#48,#01,#A8,#03,#FF,#58,#F8,#FF,#CA,#03,#FF,#18,#58,#FF,#E4 ; #1360 .H....X......X..
db #03,#FF,#18,#58,#FF,#16,#04,#01,#B8,#F8,#01,#26,#04,#01,#B8,#F8 ; #1370 ...X.......&....
db #01,#3E,#04,#FF,#08,#48,#FF,#4A,#04,#FF,#08,#48,#FF,#10,#84,#04 ; #1380 .>...H.J...H....
db #01,#08,#A8,#01,#86,#04,#01,#28,#A8,#01,#88,#04,#01,#48,#A8,#01 ; #1390 .......(.....H..
db #8E,#04,#01,#08,#A8,#01,#90,#04,#01,#28,#A8,#01,#92,#04,#01,#48 ; #13a0 .........(.....H
db #A8,#01,#9E,#04,#01,#48,#E8,#01,#A0,#04,#FF,#58,#F8,#FF,#A4,#04 ; #13b0 .....H.....X....
db #01,#08,#A8,#01,#A6,#04,#01,#28,#A8,#01,#A8,#04,#01,#48,#A8,#01 ; #13c0 .......(.....H..
db #B4,#04,#01,#48,#E8,#01,#B6,#04,#FF,#58,#F8,#FF,#BA,#04,#01,#08 ; #13d0 ...H.....X......
db #A8,#01,#BC,#04,#01,#28,#A8,#01,#BE,#04,#01,#48,#A8,#01,#0C,#74 ; #13e0 .....(.....H...t
db #05,#FF,#58,#D8,#FF,#76,#05,#FF,#58,#F8,#FF,#9A,#05,#FF,#78,#D8 ; #13f0 ..X..v..X.....x.
db #FF,#9C,#05,#FF,#78,#F8,#FF,#BC,#05,#FF,#38,#98,#FF,#BE,#05,#FF ; #1400 ....x.....8.....
db #38,#D8,#FF,#C4,#05,#FF,#38,#98,#FF,#C6,#05,#FF,#38,#D8,#FF,#CC ; #1410 8.....8.....8...
db #05,#FF,#38,#98,#FF,#CE,#05,#FF,#38,#D8,#FF,#D4,#05,#FF,#38,#98 ; #1420 ..8.....8.....8.
db #FF,#D6,#05,#FF,#38,#D8,#FF,#10,#E0,#05,#FF,#28,#A8,#FF,#E2,#05 ; #1430 ....8......(....
db #FF,#28,#C8,#FF,#F2,#05,#FF,#28,#A8,#FF,#F4,#05,#FF,#28,#C8,#FF ; #1440 .(.....(.....(..
db #30,#06,#FF,#38,#98,#FF,#34,#06,#FF,#38,#98,#FF,#36,#06,#FF,#38 ; #1450 0..8..4..8..6..8
db #B8,#FF,#3A,#06,#FF,#38,#98,#FF,#3C,#06,#FF,#38,#B8,#FF,#40,#06 ; #1460 ..:..8..<..8..@.
db #FF,#38,#98,#FF,#44,#06,#FF,#38,#98,#FF,#48,#06,#FF,#38,#98,#FF ; #1470 .8..D..8..H..8..
db #4A,#06,#FF,#38,#B8,#FF,#4E,#06,#FF,#38,#98,#FF,#50,#06,#FF,#38 ; #1480 J..8..N..8..P..8
db #B8,#FF,#54,#06,#FF,#38,#98,#FF,#00,#00,#00,#00,#00,#00,#00,#00 ; #1490 ..T..8..........
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #14a0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #14b0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #14c0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #14d0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #14e0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #14f0 
db #00,#01,#08,#01,#10,#01,#18,#01,#20,#01,#28,#01,#30,#01,#38,#01 ; #1500 ........ .(.0.8.
db #40,#01,#48,#01,#50,#01,#58,#01,#60,#01,#68,#01,#70,#01,#78,#01 ; #1510 @.H.P.X.`.h.p.x.
db #80,#01,#88,#01,#90,#01,#98,#01,#A0,#01,#A8,#01,#B0,#01,#B8,#01 ; #1520 
db #C0,#01,#C8,#01,#D0,#01,#D8,#01,#E0,#01,#E8,#01,#F0,#01,#F8,#01 ; #1530 
db #00,#02,#10,#02,#20,#02,#30,#02,#40,#02,#50,#02,#60,#02,#70,#02 ; #1540 .... .0.@.P.`.p.
db #80,#02,#90,#02,#A0,#02,#B0,#02,#C0,#02,#D0,#02,#E0,#02,#F0,#02 ; #1550 
db #00,#03,#10,#03,#20,#03,#30,#03,#40,#03,#50,#03,#60,#03,#70,#03 ; #1560 .... .0.@.P.`.p.
db #80,#03,#90,#03,#A0,#03,#B0,#03,#C0,#03,#D0,#03,#E0,#03,#F0,#03 ; #1570 
db #00,#03,#18,#03,#30,#03,#48,#03,#60,#03,#78,#03,#90,#03,#A8,#03 ; #1580 ....0.H.`.x.....
db #C0,#03,#D8,#03,#F0,#03,#08,#04,#20,#04,#38,#04,#50,#04,#68,#04 ; #1590 ........ .8.P.h.
db #80,#04,#98,#04,#B0,#04,#C8,#04,#E0,#04,#F8,#04,#10,#05,#28,#05 ; #15a0 ..............(.
db #40,#05,#58,#05,#70,#05,#88,#05,#A0,#05,#B8,#05,#D0,#05,#E8,#05 ; #15b0 @.X.p...........
db #00,#04,#20,#04,#40,#04,#60,#04,#80,#04,#A0,#04,#C0,#04,#E0,#04 ; #15c0 .. .@.`.........
db #00,#05,#20,#05,#40,#05,#60,#05,#80,#05,#A0,#05,#C0,#05,#E0,#05 ; #15d0 .. .@.`.........
db #00,#06,#20,#06,#40,#06,#60,#06,#80,#06,#A0,#06,#C0,#06,#E0,#06 ; #15e0 .. .@.`.........
db #00,#07,#20,#07,#40,#07,#60,#07,#80,#07,#A0,#07,#C0,#07,#E0,#07 ; #15f0 .. .@.`.........
db #00,#05,#28,#05,#50,#05,#78,#05,#A0,#05,#C8,#05,#F0,#05,#18,#06 ; #1600 ..(.P.x.........
db #40,#06,#68,#06,#90,#06,#B8,#06,#E0,#06,#08,#07,#30,#07,#58,#07 ; #1610 @.h.........0.X.
db #80,#07,#A8,#07,#D0,#07,#F8,#07,#20,#08,#48,#08,#70,#08,#98,#08 ; #1620 ........ .H.p...
db #C0,#08,#E8,#08,#10,#09,#38,#09,#60,#09,#88,#09,#B0,#09,#D8,#09 ; #1630 ......8.`.......
db #00,#06,#30,#06,#60,#06,#90,#06,#C0,#06,#F0,#06,#20,#07,#50,#07 ; #1640 ..0.`....... .P.
db #80,#07,#B0,#07,#E0,#07,#10,#08,#40,#08,#70,#08,#A0,#08,#D0,#08 ; #1650 ........@.p.....
db #00,#09,#30,#09,#60,#09,#90,#09,#C0,#09,#F0,#09,#20,#0A,#50,#0A ; #1660 ..0.`....... .P.
db #80,#0A,#B0,#0A,#E0,#0A,#10,#0B,#40,#0B,#70,#0B,#A0,#0B,#D0,#0B ; #1670 ........@.p.....
db #00,#07,#38,#07,#70,#07,#A8,#07,#E0,#07,#18,#08,#50,#08,#88,#08 ; #1680 ..8.p.......P...
db #C0,#08,#F8,#08,#30,#09,#68,#09,#A0,#09,#D8,#09,#10,#0A,#48,#0A ; #1690 ....0.h.......H.
db #80,#0A,#B8,#0A,#F0,#0A,#28,#0B,#60,#0B,#98,#0B,#D0,#0B,#08,#0C ; #16a0 ......(.`.......
db #40,#0C,#78,#0C,#B0,#0C,#E8,#0C,#20,#0D,#58,#0D,#90,#0D,#C8,#0D ; #16b0 @.x..... .X.....
db #00,#08,#40,#08,#80,#08,#C0,#08,#00,#09,#40,#09,#80,#09,#C0,#09 ; #16c0 ..@.......@.....
db #00,#0A,#40,#0A,#80,#0A,#C0,#0A,#00,#0B,#40,#0B,#80,#0B,#C0,#0B ; #16d0 ..@.......@.....
db #00,#0C,#40,#0C,#80,#0C,#C0,#0C,#00,#0D,#40,#0D,#80,#0D,#C0,#0D ; #16e0 ..@.......@.....
db #00,#0E,#40,#0E,#80,#0E,#C0,#0E,#00,#0F,#40,#0F,#80,#0F,#C0,#0F ; #16f0 ..@.......@.....



; Damier x4 (40x32)
org #1700
checkboard_data:
db #00,#00,#FF,#FF,#00,#00,#FF,#FF,#00,#00,#FF,#FF,#00,#00,#FF,#FF ; #1700 
db #00,#00,#FF,#FF,#00,#00,#FF,#FF,#00,#00,#FF,#FF,#00,#00,#FF,#FF ; #1710 
db #00,#00,#FF,#FF,#00,#00,#FF,#FF
db #00,#1F,#FF,#E0,#00,#0F,#FF,#F0 ; #1720 
db #00,#07,#FF,#F8,#00,#03,#FF,#FC,#00,#01,#FF,#FE,#00,#00,#FF,#FF ; #1730 
db #00,#00,#7F,#FF,#80,#00,#3F,#FF,#C0,#00,#1F,#FF,#E0,#00,#0F,#FF ; #1740 ......?.........
db #07,#FF,#FC,#00,#01,#FF,#FF,#00,#00,#7F,#FF,#C0,#00,#1F,#FF,#F0 ; #1750 
db #00,#07,#FF,#FC,#00,#01,#FF,#FF,#00,#00,#7F,#FF,#C0,#00,#1F,#FF ; #1760 
db #F0,#00,#07,#FF,#FC,#00,#01,#FF,#FF,#FF,#80,#00,#1F,#FF,#F0,#00 ; #1770 
db #03,#FF,#FE,#00,#00,#7F,#FF,#C0,#00,#0F,#FF,#F8,#00,#01,#FF,#FF ; #1780 
db #00,#00,#3F,#FF,#E0,#00,#07,#FF,#FC,#00,#00,#FF,#FF,#80,#00,#1F ; #1790 ..?.............
db #FF,#F0,#00,#03,#FF,#FF,#00,#00,#3F,#FF,#F0,#00,#03,#FF,#FF,#00 ; #17a0 ........?.......
db #00,#3F,#FF,#F0,#00,#03,#FF,#FF,#00,#00,#3F,#FF,#F0,#00,#03,#FF ; #17b0 .?........?.....
db #FF,#00,#00,#3F,#FF,#F0,#00,#03,#01,#FF,#FF,#C0,#00,#0F,#FF,#FE ; #17c0 ...?............
db #00,#00,#7F,#FF,#F0,#00,#03,#FF,#FF,#80,#00,#1F,#FF,#FC,#00,#00 ; #17d0 
db #FF,#FF,#E0,#00,#07,#FF,#FF,#00,#00,#3F,#FF,#F8,#00,#01,#FF,#FF ; #17e0 .........?......
db #3F,#FF,#F8,#00,#00,#FF,#FF,#E0,#00,#03,#FF,#FF,#80,#00,#0F,#FF ; #17f0 ?...............
db #FE,#00,#00,#3F,#FF,#F8,#00,#00,#FF,#FF,#E0,#00,#03,#FF,#FF,#80 ; #1800 ...?............
db #00,#0F,#FF,#FE,#00,#00,#3F,#FF,#FF,#FF,#80,#00,#0F,#FF,#FF,#00 ; #1810 ......?.........
db #00,#1F,#FF,#FE,#00,#00,#3F,#FF,#FC,#00,#00,#7F,#FF,#F8,#00,#00 ; #1820 ......?.........
db #FF,#FF,#F0,#00,#01,#FF,#FF,#E0,#00,#03,#FF,#FF,#C0,#00,#07,#FF ; #1830 
db #FF,#F0,#00,#00,#FF,#FF,#F0,#00,#00,#FF,#FF,#F0,#00,#00,#FF,#FF ; #1840 
db #F0,#00,#00,#FF,#FF,#F0,#00,#00,#FF,#FF,#F0,#00,#00,#FF,#FF,#F0 ; #1850 
db #00,#00,#FF,#FF,#F0,#00,#00,#FF,#FF,#00,#00,#0F,#FF,#FF,#80,#00 ; #1860 
db #07,#FF,#FF,#C0,#00,#03,#FF,#FF,#E0,#00,#01,#FF,#FF,#F0,#00,#00 ; #1870 
db #FF,#FF,#F8,#00,#00,#7F,#FF,#FC,#00,#00,#3F,#FF,#FE,#00,#00,#1F ; #1880 ..........?.....
db #E0,#00,#00,#FF,#FF,#F8,#00,#00,#3F,#FF,#FE,#00,#00,#0F,#FF,#FF ; #1890 ........?.......
db #80,#00,#03,#FF,#FF,#E0,#00,#00,#FF,#FF,#F8,#00,#00,#3F,#FF,#FE ; #18a0 .............?..
db #00,#00,#0F,#FF,#FF,#80,#00,#03,#FF,#FF,#F0,#00,#00,#3F,#FF,#FE ; #18b0 .............?..
db #00,#00,#07,#FF,#FF,#C0,#00,#00,#FF,#FF,#F8,#00,#00,#1F,#FF,#FF ; #18c0 
db #00,#00,#03,#FF,#FF,#E0,#00,#00,#7F,#FF,#FC,#00,#00,#0F,#FF,#FF ; #18d0 
db #FF,#FF,#00,#00,#03,#FF,#FF,#F0,#00,#00,#3F,#FF,#FF,#00,#00,#03 ; #18e0 ..........?.....
db #FF,#FF,#F0,#00,#00,#3F,#FF,#FF,#00,#00,#03,#FF,#FF,#F0,#00,#00 ; #18f0 .....?..........
db #3F,#FF,#FF,#00,#00,#03,#FF,#FF,#FF,#F0,#00,#00,#1F,#FF,#FF,#80 ; #1900 ?...............
db #00,#00,#FF,#FF,#FC,#00,#00,#07,#FF,#FF,#E0,#00,#00,#3F,#FF,#FF ; #1910 .............?..
db #00,#00,#01,#FF,#FF,#F8,#00,#00,#0F,#FF,#FF,#C0,#00,#00,#7F,#FF ; #1920 
db #FF,#00,#00,#01,#FF,#FF,#FC,#00,#00,#07,#FF,#FF,#F0,#00,#00,#1F ; #1930 
db #FF,#FF,#C0,#00,#00,#7F,#FF,#FF,#00,#00,#01,#FF,#FF,#FC,#00,#00 ; #1940 
db #07,#FF,#FF,#F0,#00,#00,#1F,#FF,#F0,#00,#00,#0F,#FF,#FF,#E0,#00 ; #1950 
db #00,#1F,#FF,#FF,#C0,#00,#00,#3F,#FF,#FF,#80,#00,#00,#7F,#FF,#FF ; #1960 .......?........
db #00,#00,#00,#FF,#FF,#FE,#00,#00,#01,#FF,#FF,#FC,#00,#00,#03,#FF ; #1970 
db #00,#00,#00,#FF,#FF,#FF,#00,#00,#00,#FF,#FF,#FF,#00,#00,#00,#FF ; #1980 
db #FF,#FF,#00,#00,#00,#FF,#FF,#FF,#00,#00,#00,#FF,#FF,#FF,#00,#00 ; #1990 
db #00,#FF,#FF,#FF,#00,#00,#00,#FF,#00,#00,#07,#FF,#FF,#F8,#00,#00 ; #19a0 
db #03,#FF,#FF,#FC,#00,#00,#01,#FF,#FF,#FE,#00,#00,#00,#FF,#FF,#FF ; #19b0 
db #00,#00,#00,#7F,#FF,#FF,#80,#00,#00,#3F,#FF,#FF,#C0,#00,#00,#1F ; #19c0 .........?......
db #00,#00,#7F,#FF,#FF,#C0,#00,#00,#1F,#FF,#FF,#F0,#00,#00,#07,#FF ; #19d0 
db #FF,#FC,#00,#00,#01,#FF,#FF,#FF,#00,#00,#00,#7F,#FF,#FF,#C0,#00 ; #19e0 
db #00,#1F,#FF,#FF,#F0,#00,#00,#07,#FF,#FC,#00,#00,#01,#FF,#FF,#FF ; #19f0 
db #80,#00,#00,#3F,#FF,#FF,#F0,#00,#00,#07,#FF,#FF,#FE,#00,#00,#00 ; #1a00 ...?............
db #FF,#FF,#FF,#C0,#00,#00,#1F,#FF,#FF,#F8,#00,#00,#03,#FF,#FF,#FF ; #1a10 
db #FF,#C0,#00,#00,#0F,#FF,#FF,#FC,#00,#00,#00,#FF,#FF,#FF,#C0,#00 ; #1a20 
db #00,#0F,#FF,#FF,#FC,#00,#00,#00,#FF,#FF,#FF,#C0,#00,#00,#0F,#FF ; #1a30 
db #FF,#FC,#00,#00,#00,#FF,#FF,#FF,#FE,#00,#00,#00,#7F,#FF,#FF,#F0 ; #1a40 
db #00,#00,#03,#FF,#FF,#FF,#80,#00,#00,#1F,#FF,#FF,#FC,#00,#00,#00 ; #1a50 
db #FF,#FF,#FF,#E0,#00,#00,#07,#FF,#FF,#FF,#00,#00,#00,#3F,#FF,#FF ; #1a60 .............?..
db #E0,#00,#00,#03,#FF,#FF,#FF,#80,#00,#00,#0F,#FF,#FF,#FE,#00,#00 ; #1a70 
db #00,#3F,#FF,#FF,#F8,#00,#00,#00,#FF,#FF,#FF,#E0,#00,#00,#03,#FF ; #1a80 .?..............
db #FF,#FF,#80,#00,#00,#0F,#FF,#FF,#00,#00,#00,#1F,#FF,#FF,#FE,#00 ; #1a90 
db #00,#00,#3F,#FF,#FF,#FC,#00,#00,#00,#7F,#FF,#FF,#F8,#00,#00,#00 ; #1aa0 ..?.............
db #FF,#FF,#FF,#F0,#00,#00,#01,#FF,#FF,#FF,#E0,#00,#00,#03,#FF,#FF ; #1ab0 
db #00,#00,#00,#FF,#FF,#FF,#F0,#00,#00,#00,#FF,#FF,#FF,#F0,#00,#00 ; #1ac0 
db #00,#FF,#FF,#FF,#F0,#00,#00,#00,#FF,#FF,#FF,#F0,#00,#00,#00,#FF ; #1ad0 
db #FF,#FF,#F0,#00,#00,#00,#FF,#FF,#00,#00,#07,#FF,#FF,#FF,#C0,#00 ; #1ae0 
db #00,#03,#FF,#FF,#FF,#E0,#00,#00,#01,#FF,#FF,#FF,#F0,#00,#00,#00 ; #1af0 
db #FF,#FF,#FF,#F8,#00,#00,#00,#7F,#FF,#FF,#FC,#00,#00,#00,#3F,#FF ; #1b00 ..............?.
db #00,#00,#3F,#FF,#FF,#FE,#00,#00,#00,#0F,#FF,#FF,#FF,#80,#00,#00 ; #1b10 ..?.............
db #03,#FF,#FF,#FF,#E0,#00,#00,#00,#FF,#FF,#FF,#F8,#00,#00,#00,#3F ; #1b20 ...............?
db #FF,#FF,#FE,#00,#00,#00,#0F,#FF,#00,#01,#FF,#FF,#FF,#F8,#00,#00 ; #1b30 
db #00,#3F,#FF,#FF,#FF,#00,#00,#00,#07,#FF,#FF,#FF,#E0,#00,#00,#00 ; #1b40 .?..............
db #FF,#FF,#FF,#FC,#00,#00,#00,#1F,#FF,#FF,#FF,#80,#00,#00,#03,#FF ; #1b50 
db #00,#0F,#FF,#FF,#FF,#C0,#00,#00,#00,#FF,#FF,#FF,#FC,#00,#00,#00 ; #1b60 
db #0F,#FF,#FF,#FF,#C0,#00,#00,#00,#FF,#FF,#FF,#FC,#00,#00,#00,#0F ; #1b70 
db #FF,#FF,#FF,#C0,#00,#00,#00,#FF,#00,#7F,#FF,#FF,#FF,#00,#00,#00 ; #1b80 
db #03,#FF,#FF,#FF,#F8,#00,#00,#00,#1F,#FF,#FF,#FF,#C0,#00,#00,#00 ; #1b90 
db #FF,#FF,#FF,#FE,#00,#00,#00,#07,#FF,#FF,#FF,#F0,#00,#00,#00,#3F ; #1ba0 ...............?
db #03,#FF,#FF,#FF,#F8,#00,#00,#00,#0F,#FF,#FF,#FF,#E0,#00,#00,#00 ; #1bb0 
db #3F,#FF,#FF,#FF,#80,#00,#00,#00,#FF,#FF,#FF,#FE,#00,#00,#00,#03 ; #1bc0 ?...............
db #FF,#FF,#FF,#F8,#00,#00,#00,#0F,#1F,#FF,#FF,#FF,#E0,#00,#00,#00 ; #1bd0 
db #3F,#FF,#FF,#FF,#C0,#00,#00,#00,#7F,#FF,#FF,#FF,#80,#00,#00,#00 ; #1be0 ?...............
db #FF,#FF,#FF,#FF,#00,#00,#00,#01,#FF,#FF,#FF,#FE,#00,#00,#00,#03 ; #1bf0 
;2nd damier
db #00,#00,#3F,#FF,#C0,#00,#3F,#FF,#C0,#00,#3F,#FF,#C0,#00,#3F,#FF ; #1c00 ..?...?...?...?.
db #C0,#00,#3F,#FF,#C0,#00,#3F,#FF,#C0,#00,#3F,#FF,#C0,#00,#3F,#FF ; #1c10 ..?...?...?...?.
db #C0,#00,#3F,#FF,#C0,#00,#3F,#FF,#C0,#07,#FF,#F8,#00,#03,#FF,#FC ; #1c20 ..?...?.........
db #00,#01,#FF,#FE,#00,#00,#FF,#FF,#00,#00,#7F,#FF,#80,#00,#3F,#FF ; #1c30 ..............?.
db #C0,#00,#1F,#FF,#E0,#00,#0F,#FF,#F0,#00,#07,#FF,#F8,#00,#03,#FF ; #1c40 
db #C1,#FF,#FF,#00,#00,#7F,#FF,#C0,#00,#1F,#FF,#F0,#00,#07,#FF,#FC ; #1c50 
db #00,#01,#FF,#FF,#00,#00,#7F,#FF,#C0,#00,#1F,#FF,#F0,#00,#07,#FF ; #1c60 
db #FC,#00,#01,#FF,#FF,#00,#00,#7F,#FF,#FF,#E0,#00,#07,#FF,#FC,#00 ; #1c70 
db #00,#FF,#FF,#80,#00,#1F,#FF,#F0,#00,#03,#FF,#FE,#00,#00,#7F,#FF ; #1c80 
db #C0,#00,#0F,#FF,#F8,#00,#01,#FF,#FF,#00,#00,#3F,#FF,#E0,#00,#07 ; #1c90 ...........?....
db #FF,#FC,#00,#00,#FF,#FF,#C0,#00,#0F,#FF,#FC,#00,#00,#FF,#FF,#C0 ; #1ca0 
db #00,#0F,#FF,#FC,#00,#00,#FF,#FF,#C0,#00,#0F,#FF,#FC,#00,#00,#FF ; #1cb0 
db #FF,#C0,#00,#0F,#FF,#FC,#00,#00,#C0,#7F,#FF,#F0,#00,#03,#FF,#FF ; #1cc0 
db #80,#00,#1F,#FF,#FC,#00,#00,#FF,#FF,#E0,#00,#07,#FF,#FF,#00,#00 ; #1cd0 
db #3F,#FF,#F8,#00,#01,#FF,#FF,#C0,#00,#0F,#FF,#FE,#00,#00,#7F,#FF ; #1ce0 ?...............
db #CF,#FF,#FE,#00,#00,#3F,#FF,#F8,#00,#00,#FF,#FF,#E0,#00,#03,#FF ; #1cf0 .....?..........
db #FF,#80,#00,#0F,#FF,#FE,#00,#00,#3F,#FF,#F8,#00,#00,#FF,#FF,#E0 ; #1d00 ........?.......
db #00,#03,#FF,#FF,#80,#00,#0F,#FF,#FF,#FF,#E0,#00,#03,#FF,#FF,#C0 ; #1d10 
db #00,#07,#FF,#FF,#80,#00,#0F,#FF,#FF,#00,#00,#1F,#FF,#FE,#00,#00 ; #1d20 
db #3F,#FF,#FC,#00,#00,#7F,#FF,#F8,#00,#00,#FF,#FF,#F0,#00,#01,#FF ; #1d30 ?...............
db #FF,#FC,#00,#00,#3F,#FF,#FC,#00,#00,#3F,#FF,#FC,#00,#00,#3F,#FF ; #1d40 ....?....?....?.
db #FC,#00,#00,#3F,#FF,#FC,#00,#00,#3F,#FF,#FC,#00,#00,#3F,#FF,#FC ; #1d50 ...?....?....?..
db #00,#00,#3F,#FF,#FC,#00,#00,#3F,#FF,#C0,#00,#03,#FF,#FF,#E0,#00 ; #1d60 ..?....?........
db #01,#FF,#FF,#F0,#00,#00,#FF,#FF,#F8,#00,#00,#7F,#FF,#FC,#00,#00 ; #1d70 
db #3F,#FF,#FE,#00,#00,#1F,#FF,#FF,#00,#00,#0F,#FF,#FF,#80,#00,#07 ; #1d80 ?...............
db #F8,#00,#00,#3F,#FF,#FE,#00,#00,#0F,#FF,#FF,#80,#00,#03,#FF,#FF ; #1d90 ...?............
db #E0,#00,#00,#FF,#FF,#F8,#00,#00,#3F,#FF,#FE,#00,#00,#0F,#FF,#FF ; #1da0 ........?.......
db #80,#00,#03,#FF,#FF,#E0,#00,#00,#FF,#FF,#FC,#00,#00,#0F,#FF,#FF ; #1db0 
db #80,#00,#01,#FF,#FF,#F0,#00,#00,#3F,#FF,#FE,#00,#00,#07,#FF,#FF ; #1dc0 ........?.......
db #C0,#00,#00,#FF,#FF,#F8,#00,#00,#1F,#FF,#FF,#00,#00,#03,#FF,#FF ; #1dd0 
db #FF,#FF,#C0,#00,#00,#FF,#FF,#FC,#00,#00,#0F,#FF,#FF,#C0,#00,#00 ; #1de0 
db #FF,#FF,#FC,#00,#00,#0F,#FF,#FF,#C0,#00,#00,#FF,#FF,#FC,#00,#00 ; #1df0 
db #0F,#FF,#FF,#C0,#00,#00,#FF,#FF,#FF,#FC,#00,#00,#07,#FF,#FF,#E0 ; #1e00 
db #00,#00,#3F,#FF,#FF,#00,#00,#01,#FF,#FF,#F8,#00,#00,#0F,#FF,#FF ; #1e10 ..?.............
db #C0,#00,#00,#7F,#FF,#FE,#00,#00,#03,#FF,#FF,#F0,#00,#00,#1F,#FF ; #1e20 
db #FF,#C0,#00,#00,#7F,#FF,#FF,#00,#00,#01,#FF,#FF,#FC,#00,#00,#07 ; #1e30 
db #FF,#FF,#F0,#00,#00,#1F,#FF,#FF,#C0,#00,#00,#7F,#FF,#FF,#00,#00 ; #1e40 
db #01,#FF,#FF,#FC,#00,#00,#07,#FF,#FC,#00,#00,#03,#FF,#FF,#F8,#00 ; #1e50 
db #00,#07,#FF,#FF,#F0,#00,#00,#0F,#FF,#FF,#E0,#00,#00,#1F,#FF,#FF ; #1e60 
db #C0,#00,#00,#3F,#FF,#FF,#80,#00,#00,#7F,#FF,#FF,#00,#00,#00,#FF ; #1e70 ...?............
db #C0,#00,#00,#3F,#FF,#FF,#C0,#00,#00,#3F,#FF,#FF,#C0,#00,#00,#3F ; #1e80 ...?.....?.....?
db #FF,#FF,#C0,#00,#00,#3F,#FF,#FF,#C0,#00,#00,#3F,#FF,#FF,#C0,#00 ; #1e90 .....?.....?....
db #00,#3F,#FF,#FF,#C0,#00,#00,#3F,#C0,#00,#01,#FF,#FF,#FE,#00,#00 ; #1ea0 .?.....?........
db #00,#FF,#FF,#FF,#00,#00,#00,#7F,#FF,#FF,#80,#00,#00,#3F,#FF,#FF ; #1eb0 .............?..
db #C0,#00,#00,#1F,#FF,#FF,#E0,#00,#00,#0F,#FF,#FF,#F0,#00,#00,#07 ; #1ec0 
db #C0,#00,#1F,#FF,#FF,#F0,#00,#00,#07,#FF,#FF,#FC,#00,#00,#01,#FF ; #1ed0 
db #FF,#FF,#00,#00,#00,#7F,#FF,#FF,#C0,#00,#00,#1F,#FF,#FF,#F0,#00 ; #1ee0 
db #00,#07,#FF,#FF,#FC,#00,#00,#01,#FF,#FF,#00,#00,#00,#7F,#FF,#FF ; #1ef0 
db #E0,#00,#00,#0F,#FF,#FF,#FC,#00,#00,#01,#FF,#FF,#FF,#80,#00,#00 ; #1f00 
db #3F,#FF,#FF,#F0,#00,#00,#07,#FF,#FF,#FE,#00,#00,#00,#FF,#FF,#FF ; #1f10 ?...............
db #FF,#F0,#00,#00,#03,#FF,#FF,#FF,#00,#00,#00,#3F,#FF,#FF,#F0,#00 ; #1f20 ...........?....
db #00,#03,#FF,#FF,#FF,#00,#00,#00,#3F,#FF,#FF,#F0,#00,#00,#03,#FF ; #1f30 ........?.......
db #FF,#FF,#00,#00,#00,#3F,#FF,#FF,#FF,#80,#00,#00,#1F,#FF,#FF,#FC ; #1f40 .....?..........
db #00,#00,#00,#FF,#FF,#FF,#E0,#00,#00,#07,#FF,#FF,#FF,#00,#00,#00 ; #1f50 
db #3F,#FF,#FF,#F8,#00,#00,#01,#FF,#FF,#FF,#C0,#00,#00,#0F,#FF,#FF ; #1f60 ?...............
db #F8,#00,#00,#00,#FF,#FF,#FF,#E0,#00,#00,#03,#FF,#FF,#FF,#80,#00 ; #1f70 
db #00,#0F,#FF,#FF,#FE,#00,#00,#00,#3F,#FF,#FF,#F8,#00,#00,#00,#FF ; #1f80 ........?.......
db #FF,#FF,#E0,#00,#00,#03,#FF,#FF,#C0,#00,#00,#07,#FF,#FF,#FF,#80 ; #1f90 
db #00,#00,#0F,#FF,#FF,#FF,#00,#00,#00,#1F,#FF,#FF,#FE,#00,#00,#00 ; #1fa0 
db #3F,#FF,#FF,#FC,#00,#00,#00,#7F,#FF,#FF,#F8,#00,#00,#00,#FF,#FF ; #1fb0 ?...............
db #C0,#00,#00,#3F,#FF,#FF,#FC,#00,#00,#00,#3F,#FF,#FF,#FC,#00,#00 ; #1fc0 ...?......?.....
db #00,#3F,#FF,#FF,#FC,#00,#00,#00,#3F,#FF,#FF,#FC,#00,#00,#00,#3F ; #1fd0 .?......?......?
db #FF,#FF,#FC,#00,#00,#00,#3F,#FF,#C0,#00,#01,#FF,#FF,#FF,#F0,#00 ; #1fe0 ......?.........
db #00,#00,#FF,#FF,#FF,#F8,#00,#00,#00,#7F,#FF,#FF,#FC,#00,#00,#00 ; #1ff0 
db #3F,#FF,#FF,#FE,#00,#00,#00,#1F,#FF,#FF,#FF,#00,#00,#00,#0F,#FF ; #2000 ?...............
db #C0,#00,#0F,#FF,#FF,#FF,#80,#00,#00,#03,#FF,#FF,#FF,#E0,#00,#00 ; #2010 
db #00,#FF,#FF,#FF,#F8,#00,#00,#00,#3F,#FF,#FF,#FE,#00,#00,#00,#0F ; #2020 ........?.......
db #FF,#FF,#FF,#80,#00,#00,#03,#FF,#C0,#00,#7F,#FF,#FF,#FE,#00,#00 ; #2030 
db #00,#0F,#FF,#FF,#FF,#C0,#00,#00,#01,#FF,#FF,#FF,#F8,#00,#00,#00 ; #2040 
db #3F,#FF,#FF,#FF,#00,#00,#00,#07,#FF,#FF,#FF,#E0,#00,#00,#00,#FF ; #2050 ?...............
db #C0,#03,#FF,#FF,#FF,#F0,#00,#00,#00,#3F,#FF,#FF,#FF,#00,#00,#00 ; #2060 .........?......
db #03,#FF,#FF,#FF,#F0,#00,#00,#00,#3F,#FF,#FF,#FF,#00,#00,#00,#03 ; #2070 ........?.......
db #FF,#FF,#FF,#F0,#00,#00,#00,#3F,#C0,#1F,#FF,#FF,#FF,#C0,#00,#00 ; #2080 .......?........
db #00,#FF,#FF,#FF,#FE,#00,#00,#00,#07,#FF,#FF,#FF,#F0,#00,#00,#00 ; #2090 
db #3F,#FF,#FF,#FF,#80,#00,#00,#01,#FF,#FF,#FF,#FC,#00,#00,#00,#0F ; #20a0 ?...............
db #C0,#FF,#FF,#FF,#FE,#00,#00,#00,#03,#FF,#FF,#FF,#F8,#00,#00,#00 ; #20b0 
db #0F,#FF,#FF,#FF,#E0,#00,#00,#00,#3F,#FF,#FF,#FF,#80,#00,#00,#00 ; #20c0 ........?.......
db #FF,#FF,#FF,#FE,#00,#00,#00,#03,#C7,#FF,#FF,#FF,#F8,#00,#00,#00 ; #20d0 
db #0F,#FF,#FF,#FF,#F0,#00,#00,#00,#1F,#FF,#FF,#FF,#E0,#00,#00,#00 ; #20e0 
db #3F,#FF,#FF,#FF,#C0,#00,#00,#00,#7F,#FF,#FF,#FF,#80,#00,#00,#00 ; #20f0 ?...............
db #70,#00,#0F,#FF,#F0,#00,#0F,#FF,#F0,#00,#0F,#FF,#F0,#00,#0F,#FF ; #2100 p...............
db #F0,#00,#0F,#FF,#F0,#00,#0F,#FF,#F0,#00,#0F,#FF,#F0,#00,#0F,#FF ; #2110 
db #F0,#00,#0F,#FF,#F0,#00,#0F,#FF,#F0,#01,#FF,#FE,#00,#00,#FF,#FF ; #2120 
db #00,#00,#7F,#FF,#80,#00,#3F,#FF,#C0,#00,#1F,#FF,#E0,#00,#0F,#FF ; #2130 ......?.........
db #F0,#00,#07,#FF,#F8,#00,#03,#FF,#FC,#00,#01,#FF,#FE,#00,#00,#FF ; #2140 
db #F0,#7F,#FF,#C0,#00,#1F,#FF,#F0,#00,#07,#FF,#FC,#00,#01,#FF,#FF ; #2150 
db #00,#00,#7F,#FF,#C0,#00,#1F,#FF,#F0,#00,#07,#FF,#FC,#00,#01,#FF ; #2160 
db #FF,#00,#00,#7F,#FF,#C0,#00,#1F,#FF,#FF,#F8,#00,#01,#FF,#FF,#00 ; #2170 
db #00,#3F,#FF,#E0,#00,#07,#FF,#FC,#00,#00,#FF,#FF,#80,#00,#1F,#FF ; #2180 .?..............
db #F0,#00,#03,#FF,#FE,#00,#00,#7F,#FF,#C0,#00,#0F,#FF,#F8,#00,#01 ; #2190 
db #FF,#FF,#00,#00,#3F,#FF,#F0,#00,#03,#FF,#FF,#00,#00,#3F,#FF,#F0 ; #21a0 ....?........?..
db #00,#03,#FF,#FF,#00,#00,#3F,#FF,#F0,#00,#03,#FF,#FF,#00,#00,#3F ; #21b0 ......?........?
db #FF,#F0,#00,#03,#FF,#FF,#00,#00,#30,#1F,#FF,#FC,#00,#00,#FF,#FF ; #21c0 ........0.......
db #E0,#00,#07,#FF,#FF,#00,#00,#3F,#FF,#F8,#00,#01,#FF,#FF,#C0,#00 ; #21d0 .......?........
db #0F,#FF,#FE,#00,#00,#7F,#FF,#F0,#00,#03,#FF,#FF,#80,#00,#1F,#FF ; #21e0 
db #F3,#FF,#FF,#80,#00,#0F,#FF,#FE,#00,#00,#3F,#FF,#F8,#00,#00,#FF ; #21f0 ..........?.....

db #FF,#E0,#00,#03,#FF,#FF,#80,#00,#0F,#FF,#FE,#00,#00,#3F,#FF,#F8 ; #2200 .............?..
db #00,#00,#FF,#FF,#E0,#00,#03,#FF,#FF,#FF,#F8,#00,#00,#FF,#FF,#F0 ; #2210 
db #00,#01,#FF,#FF,#E0,#00,#03,#FF,#FF,#C0,#00,#07,#FF,#FF,#80,#00 ; #2220 
db #0F,#FF,#FF,#00,#00,#1F,#FF,#FE,#00,#00,#3F,#FF,#FC,#00,#00,#7F ; #2230 ..........?.....
db #FF,#FF,#00,#00,#0F,#FF,#FF,#00,#00,#0F,#FF,#FF,#00,#00,#0F,#FF ; #2240 
db #FF,#00,#00,#0F,#FF,#FF,#00,#00,#0F,#FF,#FF,#00,#00,#0F,#FF,#FF ; #2250 
db #00,#00,#0F,#FF,#FF,#00,#00,#0F,#FF,#F0,#00,#00,#FF,#FF,#F8,#00 ; #2260 
db #00,#7F,#FF,#FC,#00,#00,#3F,#FF,#FE,#00,#00,#1F,#FF,#FF,#00,#00 ; #2270 ......?.........
db #0F,#FF,#FF,#80,#00,#07,#FF,#FF,#C0,#00,#03,#FF,#FF,#E0,#00,#01 ; #2280 
db #FE,#00,#00,#0F,#FF,#FF,#80,#00,#03,#FF,#FF,#E0,#00,#00,#FF,#FF ; #2290 
db #F8,#00,#00,#3F,#FF,#FE,#00,#00,#0F,#FF,#FF,#80,#00,#03,#FF,#FF ; #22a0 ...?............
db #E0,#00,#00,#FF,#FF,#F8,#00,#00,#3F,#FF,#FF,#00,#00,#03,#FF,#FF ; #22b0 ........?.......
db #E0,#00,#00,#7F,#FF,#FC,#00,#00,#0F,#FF,#FF,#80,#00,#01,#FF,#FF ; #22c0 
db #F0,#00,#00,#3F,#FF,#FE,#00,#00,#07,#FF,#FF,#C0,#00,#00,#FF,#FF ; #22d0 ...?............
db #FF,#FF,#F0,#00,#00,#3F,#FF,#FF,#00,#00,#03,#FF,#FF,#F0,#00,#00 ; #22e0 .....?..........
db #3F,#FF,#FF,#00,#00,#03,#FF,#FF,#F0,#00,#00,#3F,#FF,#FF,#00,#00 ; #22f0 ?..........?....
db #03,#FF,#FF,#F0,#00,#00,#3F,#FF,#FF,#FF,#00,#00,#01,#FF,#FF,#F8 ; #2300 ......?.........
db #00,#00,#0F,#FF,#FF,#C0,#00,#00,#7F,#FF,#FE,#00,#00,#03,#FF,#FF ; #2310 
db #F0,#00,#00,#1F,#FF,#FF,#80,#00,#00,#FF,#FF,#FC,#00,#00,#07,#FF ; #2320 
db #FF,#F0,#00,#00,#1F,#FF,#FF,#C0,#00,#00,#7F,#FF,#FF,#00,#00,#01 ; #2330 
db #FF,#FF,#FC,#00,#00,#07,#FF,#FF,#F0,#00,#00,#1F,#FF,#FF,#C0,#00 ; #2340 
db #00,#7F,#FF,#FF,#00,#00,#01,#FF,#FF,#00,#00,#00,#FF,#FF,#FE,#00 ; #2350 
db #00,#01,#FF,#FF,#FC,#00,#00,#03,#FF,#FF,#F8,#00,#00,#07,#FF,#FF ; #2360 
db #F0,#00,#00,#0F,#FF,#FF,#E0,#00,#00,#1F,#FF,#FF,#C0,#00,#00,#3F ; #2370 ...............?
db #F0,#00,#00,#0F,#FF,#FF,#F0,#00,#00,#0F,#FF,#FF,#F0,#00,#00,#0F ; #2380 
db #FF,#FF,#F0,#00,#00,#0F,#FF,#FF,#F0,#00,#00,#0F,#FF,#FF,#F0,#00 ; #2390 
db #00,#0F,#FF,#FF,#F0,#00,#00,#0F,#F0,#00,#00,#7F,#FF,#FF,#80,#00 ; #23a0 
db #00,#3F,#FF,#FF,#C0,#00,#00,#1F,#FF,#FF,#E0,#00,#00,#0F,#FF,#FF ; #23b0 .?..............
db #F0,#00,#00,#07,#FF,#FF,#F8,#00,#00,#03,#FF,#FF,#FC,#00,#00,#01 ; #23c0 
db #F0,#00,#07,#FF,#FF,#FC,#00,#00,#01,#FF,#FF,#FF,#00,#00,#00,#7F ; #23d0 
db #FF,#FF,#C0,#00,#00,#1F,#FF,#FF,#F0,#00,#00,#07,#FF,#FF,#FC,#00 ; #23e0 
db #00,#01,#FF,#FF,#FF,#00,#00,#00,#7F,#FF,#C0,#00,#00,#1F,#FF,#FF ; #23f0 
db #F8,#00,#00,#03,#FF,#FF,#FF,#00,#00,#00,#7F,#FF,#FF,#E0,#00,#00 ; #2400 
db #0F,#FF,#FF,#FC,#00,#00,#01,#FF,#FF,#FF,#80,#00,#00,#3F,#FF,#FF ; #2410 .............?..
db #FF,#FC,#00,#00,#00,#FF,#FF,#FF,#C0,#00,#00,#0F,#FF,#FF,#FC,#00 ; #2420 
db #00,#00,#FF,#FF,#FF,#C0,#00,#00,#0F,#FF,#FF,#FC,#00,#00,#00,#FF ; #2430 
db #FF,#FF,#C0,#00,#00,#0F,#FF,#FF,#FF,#E0,#00,#00,#07,#FF,#FF,#FF ; #2440 
db #00,#00,#00,#3F,#FF,#FF,#F8,#00,#00,#01,#FF,#FF,#FF,#C0,#00,#00 ; #2450 ...?............
db #0F,#FF,#FF,#FE,#00,#00,#00,#7F,#FF,#FF,#F0,#00,#00,#03,#FF,#FF ; #2460 
db #FE,#00,#00,#00,#3F,#FF,#FF,#F8,#00,#00,#00,#FF,#FF,#FF,#E0,#00 ; #2470 ....?...........
db #00,#03,#FF,#FF,#FF,#80,#00,#00,#0F,#FF,#FF,#FE,#00,#00,#00,#3F ; #2480 ...............?
db #FF,#FF,#F8,#00,#00,#00,#FF,#FF,#F0,#00,#00,#01,#FF,#FF,#FF,#E0 ; #2490 
db #00,#00,#03,#FF,#FF,#FF,#C0,#00,#00,#07,#FF,#FF,#FF,#80,#00,#00 ; #24a0 
db #0F,#FF,#FF,#FF,#00,#00,#00,#1F,#FF,#FF,#FE,#00,#00,#00,#3F,#FF ; #24b0 ..............?.
db #F0,#00,#00,#0F,#FF,#FF,#FF,#00,#00,#00,#0F,#FF,#FF,#FF,#00,#00 ; #24c0 
db #00,#0F,#FF,#FF,#FF,#00,#00,#00,#0F,#FF,#FF,#FF,#00,#00,#00,#0F ; #24d0 
db #FF,#FF,#FF,#00,#00,#00,#0F,#FF,#F0,#00,#00,#7F,#FF,#FF,#FC,#00 ; #24e0 
db #00,#00,#3F,#FF,#FF,#FE,#00,#00,#00,#1F,#FF,#FF,#FF,#00,#00,#00 ; #24f0 ..?.............
db #0F,#FF,#FF,#FF,#80,#00,#00,#07,#FF,#FF,#FF,#C0,#00,#00,#03,#FF ; #2500 
db #F0,#00,#03,#FF,#FF,#FF,#E0,#00,#00,#00,#FF,#FF,#FF,#F8,#00,#00 ; #2510 
db #00,#3F,#FF,#FF,#FE,#00,#00,#00,#0F,#FF,#FF,#FF,#80,#00,#00,#03 ; #2520 .?..............
db #FF,#FF,#FF,#E0,#00,#00,#00,#FF,#F0,#00,#1F,#FF,#FF,#FF,#80,#00 ; #2530 
db #00,#03,#FF,#FF,#FF,#F0,#00,#00,#00,#7F,#FF,#FF,#FE,#00,#00,#00 ; #2540 
db #0F,#FF,#FF,#FF,#C0,#00,#00,#01,#FF,#FF,#FF,#F8,#00,#00,#00,#3F ; #2550 ...............?
db #F0,#00,#FF,#FF,#FF,#FC,#00,#00,#00,#0F,#FF,#FF,#FF,#C0,#00,#00 ; #2560 
db #00,#FF,#FF,#FF,#FC,#00,#00,#00,#0F,#FF,#FF,#FF,#C0,#00,#00,#00 ; #2570 
db #FF,#FF,#FF,#FC,#00,#00,#00,#0F,#F0,#07,#FF,#FF,#FF,#F0,#00,#00 ; #2580 
db #00,#3F,#FF,#FF,#FF,#80,#00,#00,#01,#FF,#FF,#FF,#FC,#00,#00,#00 ; #2590 .?..............
db #0F,#FF,#FF,#FF,#E0,#00,#00,#00,#7F,#FF,#FF,#FF,#00,#00,#00,#03 ; #25a0 
db #F0,#3F,#FF,#FF,#FF,#80,#00,#00,#00,#FF,#FF,#FF,#FE,#00,#00,#00 ; #25b0 .?..............
db #03,#FF,#FF,#FF,#F8,#00,#00,#00,#0F,#FF,#FF,#FF,#E0,#00,#00,#00 ; #25c0 
db #3F,#FF,#FF,#FF,#80,#00,#00,#00,#F1,#FF,#FF,#FF,#FE,#00,#00,#00 ; #25d0 ?...............
db #03,#FF,#FF,#FF,#FC,#00,#00,#00,#07,#FF,#FF,#FF,#F8,#00,#00,#00 ; #25e0 
db #0F,#FF,#FF,#FF,#F0,#00,#00,#00,#1F,#FF,#FF,#FF,#E0,#00,#00,#00 ; #25f0 
db #1C,#00,#03,#FF,#FC,#00,#03,#FF,#FC,#00,#03,#FF,#FC,#00,#03,#FF ; #2600 
db #FC,#00,#03,#FF,#FC,#00,#03,#FF,#FC,#00,#03,#FF,#FC,#00,#03,#FF ; #2610 
db #FC,#00,#03,#FF,#FC,#00,#03,#FF,#FC,#00,#7F,#FF,#80,#00,#3F,#FF ; #2620 ..............?.
db #C0,#00,#1F,#FF,#E0,#00,#0F,#FF,#F0,#00,#07,#FF,#F8,#00,#03,#FF ; #2630 
db #FC,#00,#01,#FF,#FE,#00,#00,#FF,#FF,#00,#00,#7F,#FF,#80,#00,#3F ; #2640 ...............?
db #FC,#1F,#FF,#F0,#00,#07,#FF,#FC,#00,#01,#FF,#FF,#00,#00,#7F,#FF ; #2650 
db #C0,#00,#1F,#FF,#F0,#00,#07,#FF,#FC,#00,#01,#FF,#FF,#00,#00,#7F ; #2660 
db #FF,#C0,#00,#1F,#FF,#F0,#00,#07,#FF,#FF,#FE,#00,#00,#7F,#FF,#C0 ; #2670 
db #00,#0F,#FF,#F8,#00,#01,#FF,#FF,#00,#00,#3F,#FF,#E0,#00,#07,#FF ; #2680 ..........?.....
db #FC,#00,#00,#FF,#FF,#80,#00,#1F,#FF,#F0,#00,#03,#FF,#FE,#00,#00 ; #2690 
db #7F,#FF,#C0,#00,#0F,#FF,#FC,#00,#00,#FF,#FF,#C0,#00,#0F,#FF,#FC ; #26a0 
db #00,#00,#FF,#FF,#C0,#00,#0F,#FF,#FC,#00,#00,#FF,#FF,#C0,#00,#0F ; #26b0 
db #FF,#FC,#00,#00,#FF,#FF,#C0,#00,#0C,#07,#FF,#FF,#00,#00,#3F,#FF ; #26c0 ..............?.
db #F8,#00,#01,#FF,#FF,#C0,#00,#0F,#FF,#FE,#00,#00,#7F,#FF,#F0,#00 ; #26d0 
db #03,#FF,#FF,#80,#00,#1F,#FF,#FC,#00,#00,#FF,#FF,#E0,#00,#07,#FF ; #26e0 
db #FC,#FF,#FF,#E0,#00,#03,#FF,#FF,#80,#00,#0F,#FF,#FE,#00,#00,#3F ; #26f0 ...............?

db #FF,#F8,#00,#00,#FF,#FF,#E0,#00,#03,#FF,#FF,#80,#00,#0F,#FF,#FE ; #2700 
db #00,#00,#3F,#FF,#F8,#00,#00,#FF,#FF,#FF,#FE,#00,#00,#3F,#FF,#FC ; #2710 ..?..........?..
db #00,#00,#7F,#FF,#F8,#00,#00,#FF,#FF,#F0,#00,#01,#FF,#FF,#E0,#00 ; #2720 
db #03,#FF,#FF,#C0,#00,#07,#FF,#FF,#80,#00,#0F,#FF,#FF,#00,#00,#1F ; #2730 
db #FF,#FF,#C0,#00,#03,#FF,#FF,#C0,#00,#03,#FF,#FF,#C0,#00,#03,#FF ; #2740 
db #FF,#C0,#00,#03,#FF,#FF,#C0,#00,#03,#FF,#FF,#C0,#00,#03,#FF,#FF ; #2750 
db #C0,#00,#03,#FF,#FF,#C0,#00,#03,#FF,#FC,#00,#00,#3F,#FF,#FE,#00 ; #2760 ............?...
db #00,#1F,#FF,#FF,#00,#00,#0F,#FF,#FF,#80,#00,#07,#FF,#FF,#C0,#00 ; #2770 
db #03,#FF,#FF,#E0,#00,#01,#FF,#FF,#F0,#00,#00,#FF,#FF,#F8,#00,#00 ; #2780 
db #7F,#80,#00,#03,#FF,#FF,#E0,#00,#00,#FF,#FF,#F8,#00,#00,#3F,#FF ; #2790 ..............?.
db #FE,#00,#00,#0F,#FF,#FF,#80,#00,#03,#FF,#FF,#E0,#00,#00,#FF,#FF ; #27a0 
db #F8,#00,#00,#3F,#FF,#FE,#00,#00,#0F,#FF,#FF,#C0,#00,#00,#FF,#FF ; #27b0 ...?............
db #F8,#00,#00,#1F,#FF,#FF,#00,#00,#03,#FF,#FF,#E0,#00,#00,#7F,#FF ; #27c0 
db #FC,#00,#00,#0F,#FF,#FF,#80,#00,#01,#FF,#FF,#F0,#00,#00,#3F,#FF ; #27d0 ..............?.
db #FF,#FF,#FC,#00,#00,#0F,#FF,#FF,#C0,#00,#00,#FF,#FF,#FC,#00,#00 ; #27e0 
db #0F,#FF,#FF,#C0,#00,#00,#FF,#FF,#FC,#00,#00,#0F,#FF,#FF,#C0,#00 ; #27f0 
db #00,#FF,#FF,#FC,#00,#00,#0F,#FF,#FF,#FF,#C0,#00,#00,#7F,#FF,#FE ; #2800 
db #00,#00,#03,#FF,#FF,#F0,#00,#00,#1F,#FF,#FF,#80,#00,#00,#FF,#FF ; #2810 
db #FC,#00,#00,#07,#FF,#FF,#E0,#00,#00,#3F,#FF,#FF,#00,#00,#01,#FF ; #2820 .........?......
db #FF,#FC,#00,#00,#07,#FF,#FF,#F0,#00,#00,#1F,#FF,#FF,#C0,#00,#00 ; #2830 
db #7F,#FF,#FF,#00,#00,#01,#FF,#FF,#FC,#00,#00,#07,#FF,#FF,#F0,#00 ; #2840 
db #00,#1F,#FF,#FF,#C0,#00,#00,#7F,#FF,#C0,#00,#00,#3F,#FF,#FF,#80 ; #2850 ............?...
db #00,#00,#7F,#FF,#FF,#00,#00,#00,#FF,#FF,#FE,#00,#00,#01,#FF,#FF ; #2860 
db #FC,#00,#00,#03,#FF,#FF,#F8,#00,#00,#07,#FF,#FF,#F0,#00,#00,#0F ; #2870 
db #FC,#00,#00,#03,#FF,#FF,#FC,#00,#00,#03,#FF,#FF,#FC,#00,#00,#03 ; #2880 
db #FF,#FF,#FC,#00,#00,#03,#FF,#FF,#FC,#00,#00,#03,#FF,#FF,#FC,#00 ; #2890 
db #00,#03,#FF,#FF,#FC,#00,#00,#03,#FC,#00,#00,#1F,#FF,#FF,#E0,#00 ; #28a0 
db #00,#0F,#FF,#FF,#F0,#00,#00,#07,#FF,#FF,#F8,#00,#00,#03,#FF,#FF ; #28b0 
db #FC,#00,#00,#01,#FF,#FF,#FE,#00,#00,#00,#FF,#FF,#FF,#00,#00,#00 ; #28c0 
db #7C,#00,#01,#FF,#FF,#FF,#00,#00,#00,#7F,#FF,#FF,#C0,#00,#00,#1F ; #28d0 |...............
db #FF,#FF,#F0,#00,#00,#07,#FF,#FF,#FC,#00,#00,#01,#FF,#FF,#FF,#00 ; #28e0 
db #00,#00,#7F,#FF,#FF,#C0,#00,#00,#1F,#FF,#F0,#00,#00,#07,#FF,#FF ; #28f0 
db #FE,#00,#00,#00,#FF,#FF,#FF,#C0,#00,#00,#1F,#FF,#FF,#F8,#00,#00 ; #2900 
db #03,#FF,#FF,#FF,#00,#00,#00,#7F,#FF,#FF,#E0,#00,#00,#0F,#FF,#FF ; #2910 
db #FF,#FF,#00,#00,#00,#3F,#FF,#FF,#F0,#00,#00,#03,#FF,#FF,#FF,#00 ; #2920 .....?..........
db #00,#00,#3F,#FF,#FF,#F0,#00,#00,#03,#FF,#FF,#FF,#00,#00,#00,#3F ; #2930 ..?............?
db #FF,#FF,#F0,#00,#00,#03,#FF,#FF,#FF,#F8,#00,#00,#01,#FF,#FF,#FF ; #2940 
db #C0,#00,#00,#0F,#FF,#FF,#FE,#00,#00,#00,#7F,#FF,#FF,#F0,#00,#00 ; #2950 
db #03,#FF,#FF,#FF,#80,#00,#00,#1F,#FF,#FF,#FC,#00,#00,#00,#FF,#FF ; #2960 
db #FF,#80,#00,#00,#0F,#FF,#FF,#FE,#00,#00,#00,#3F,#FF,#FF,#F8,#00 ; #2970 ...........?....
db #00,#00,#FF,#FF,#FF,#E0,#00,#00,#03,#FF,#FF,#FF,#80,#00,#00,#0F ; #2980 
db #FF,#FF,#FE,#00,#00,#00,#3F,#FF,#FC,#00,#00,#00,#7F,#FF,#FF,#F8 ; #2990 ......?.........
db #00,#00,#00,#FF,#FF,#FF,#F0,#00,#00,#01,#FF,#FF,#FF,#E0,#00,#00 ; #29a0 
db #03,#FF,#FF,#FF,#C0,#00,#00,#07,#FF,#FF,#FF,#80,#00,#00,#0F,#FF ; #29b0 
db #FC,#00,#00,#03,#FF,#FF,#FF,#C0,#00,#00,#03,#FF,#FF,#FF,#C0,#00 ; #29c0 
db #00,#03,#FF,#FF,#FF,#C0,#00,#00,#03,#FF,#FF,#FF,#C0,#00,#00,#03 ; #29d0 
db #FF,#FF,#FF,#C0,#00,#00,#03,#FF,#FC,#00,#00,#1F,#FF,#FF,#FF,#00 ; #29e0 
db #00,#00,#0F,#FF,#FF,#FF,#80,#00,#00,#07,#FF,#FF,#FF,#C0,#00,#00 ; #29f0 
db #03,#FF,#FF,#FF,#E0,#00,#00,#01,#FF,#FF,#FF,#F0,#00,#00,#00,#FF ; #2a00 
db #FC,#00,#00,#FF,#FF,#FF,#F8,#00,#00,#00,#3F,#FF,#FF,#FE,#00,#00 ; #2a10 ..........?.....
db #00,#0F,#FF,#FF,#FF,#80,#00,#00,#03,#FF,#FF,#FF,#E0,#00,#00,#00 ; #2a20 
db #FF,#FF,#FF,#F8,#00,#00,#00,#3F,#FC,#00,#07,#FF,#FF,#FF,#E0,#00 ; #2a30 .......?........
db #00,#00,#FF,#FF,#FF,#FC,#00,#00,#00,#1F,#FF,#FF,#FF,#80,#00,#00 ; #2a40 
db #03,#FF,#FF,#FF,#F0,#00,#00,#00,#7F,#FF,#FF,#FE,#00,#00,#00,#0F ; #2a50 
db #FC,#00,#3F,#FF,#FF,#FF,#00,#00,#00,#03,#FF,#FF,#FF,#F0,#00,#00 ; #2a60 ..?.............
db #00,#3F,#FF,#FF,#FF,#00,#00,#00,#03,#FF,#FF,#FF,#F0,#00,#00,#00 ; #2a70 .?..............
db #3F,#FF,#FF,#FF,#00,#00,#00,#03,#FC,#01,#FF,#FF,#FF,#FC,#00,#00 ; #2a80 ?...............
db #00,#0F,#FF,#FF,#FF,#E0,#00,#00,#00,#7F,#FF,#FF,#FF,#00,#00,#00 ; #2a90 
db #03,#FF,#FF,#FF,#F8,#00,#00,#00,#1F,#FF,#FF,#FF,#C0,#00,#00,#00 ; #2aa0 
db #FC,#0F,#FF,#FF,#FF,#E0,#00,#00,#00,#3F,#FF,#FF,#FF,#80,#00,#00 ; #2ab0 .........?......
db #00,#FF,#FF,#FF,#FE,#00,#00,#00,#03,#FF,#FF,#FF,#F8,#00,#00,#00 ; #2ac0 
db #0F,#FF,#FF,#FF,#E0,#00,#00,#00,#3C,#7F,#FF,#FF,#FF,#80,#00,#00 ; #2ad0 ........<.......
db #00,#FF,#FF,#FF,#FF,#00,#00,#00,#01,#FF,#FF,#FF,#FE,#00,#00,#00 ; #2ae0 
db #03,#FF,#FF,#FF,#FC,#00,#00,#00,#07,#FF,#FF,#FF,#F8,#00,#00,#00 ; #2af0 

; Sprites
db #00,#00,#02,#1C,#00,#02,#38,#00,#02,#54,#00,#02,#70,#00,#03,#9A ; #2b00 ......8..T..p...
db #00,#03,#C4,#00,#03,#EE,#00,#03,#18,#01,#02,#38,#01,#02,#58,#01 ; #2b10 ...........8..X.
db #03,#88,#01,#03,#B8,#01,#03,#E8,#01,#03,#18,#02,#03,#48,#02,#03 ; #2b20 .............H..
db #78,#02,#03,#B4,#02,#03,#F0,#02,#03,#2C,#03,#03,#68,#03,#03,#A4 ; #2b30 x........,..h...
db #03,#03,#E0,#03,#03,#1C,#04,#04,#6C,#04,#03,#BA,#04,#03,#08,#05 ; #2b40 ........l.......
db #03,#56,#05,#04,#BE,#05,#04,#26,#06,#04,#8E,#06,#04,#F6,#06,#04 ; #2b50 .V.....&........
db #5E,#07,#02,#6E,#07,#02,#7E,#07,#02,#8E,#07,#02,#9E,#07,#03,#B6 ; #2b60 ^..n..~.........
db #07,#03,#CE,#07,#03,#E6,#07,#03,#FE,#07,#02,#0E,#08,#02,#1E,#08 ; #2b70 
db #03,#36,#08,#03,#4E,#08,#03,#66,#08,#03,#7E,#08,#03,#96,#08,#03 ; #2b80 .6..N..f..~.....
db #AE,#08,#03,#CC,#08,#03,#EA,#08,#03,#08,#09,#03,#26,#09,#03,#44 ; #2b90 ............&..D
db #09,#03,#62,#09,#03,#80,#09,#04,#A8,#09,#03,#C6,#09,#03,#E4,#09 ; #2ba0 ..b.............
db #03,#02,#0A,#04,#2A,#0A,#04,#52,#0A,#04,#7A,#0A,#04,#A2,#0A,#04 ; #2bb0 ....*..R..z.....
db #CA,#0A,#02,#FA,#0A,#02,#2A,#0B,#02,#5A,#0B,#02,#8A,#0B,#03,#D2 ; #2bc0 ......*..Z......
db #0B,#03,#1A,#0C,#03,#62,#0C,#03,#AA,#0C,#02,#E2,#0C,#02,#1A,#0D ; #2bd0 .....b..........
db #03,#6E,#0D,#03,#C2,#0D,#03,#16,#0E,#03,#6A,#0E,#03,#BE,#0E,#03 ; #2be0 .n........j.....
db #12,#0F,#03,#6C,#0F,#03,#C6,#0F,#03,#20,#10,#03,#7A,#10,#03,#D4 ; #2bf0 ...l..... ..z...
db #10,#03,#2E,#11,#03,#88,#11,#04,#00,#12,#03,#6C,#12,#03,#D8,#12 ; #2c00 ...........l....
db #03,#44,#13,#04,#D4,#13,#04,#64,#14,#04,#F4,#14,#04,#84,#15,#04 ; #2c10 .D.....d........
db #14,#16,#02,#28,#16,#02,#3C,#16,#02,#50,#16,#02,#64,#16,#03,#82 ; #2c20 ...(..<..P..d...
db #16,#03,#A0,#16,#03,#BE,#16,#03,#DC,#16,#02,#F4,#16,#02,#0C,#17 ; #2c30 
db #03,#30,#17,#03,#54,#17,#03,#78,#17,#03,#9C,#17,#03,#C0,#17,#03 ; #2c40 .0..T..x........
db #E4,#17,#03,#14,#18,#03,#44,#18,#03,#74,#18,#03,#A4,#18,#03,#D4 ; #2c50 ......D..t......
db #18,#03,#04,#19,#03,#34,#19,#04,#74,#19,#03,#B6,#19,#03,#F8,#19 ; #2c60 .....4..t.......
db #03,#3A,#1A,#04,#92,#1A,#04,#EA,#1A,#04,#42,#1B,#04,#9A,#1B,#04 ; #2c70 .:........B.....
db #F2,#1B,#02,#22,#1C,#02,#52,#1C,#02,#82,#1C,#02,#B2,#1C,#02,#E2 ; #2c80 ..."..R.........
db #1C,#02,#12,#1D,#03,#5A,#1D,#03,#A2,#1D,#02,#D2,#1D,#02,#02,#1E ; #2c90 .....Z..........
db #02,#32,#1E,#02,#62,#1E,#02,#92,#1E,#02,#C2,#1E,#03,#0A,#1F,#03 ; #2ca0 .2..b...........
db #52,#1F,#02,#8E,#1F,#02,#CA,#1F,#02,#06,#20,#02,#42,#20,#02,#7E ; #2cb0 R......... .B .~
db #20,#03,#D8,#20,#03,#32,#21,#03,#8C,#21,#02,#C8,#21,#02,#04,#22 ; #2cc0  .. .2!..!..!.."
db #02,#40,#22,#02,#7C,#22,#02,#B8,#22,#03,#12,#23,#03,#6C,#23,#03 ; #2cd0 .@".|".."..#.l#.
db #C6,#23,#02,#0E,#24,#02,#56,#24,#03,#C2,#24,#03,#2E,#25,#03,#9A ; #2ce0 .#..$.V$..$..%..
db #25,#03,#06,#26,#03,#72,#26,#03,#DE,#26,#02,#26,#27,#02,#6E,#27 ; #2cf0 %..&.r&..&.&'.n'
db #03,#DA,#27,#03,#46,#28,#03,#B2,#28,#03,#1E,#29,#03,#8A,#29,#03 ; #2d00 ..'.F(..(..)..).
db #F6,#29,#03,#6E,#2A,#03,#E6,#2A,#03,#5E,#2B,#03,#D6,#2B,#03,#4E ; #2d10 .).n*..*.^+..+.N
db #2C,#03,#C6,#2C,#03,#3E,#2D,#03,#B6,#2D,#03,#2E,#2E,#03,#A6,#2E ; #2d20 ,..,.>-..-......
db #03,#1E,#2F,#03,#96,#2F,#03,#0E,#30,#03,#86,#30,#03,#FE,#30,#03 ; #2d30 ../../..0..0..0.
db #76,#31,#02,#A6,#31,#02,#D6,#31,#03,#1E,#32,#03,#66,#32,#03,#AE ; #2d40 v1..1..1..2.f2..
db #32,#03,#F6,#32,#03,#3E,#33,#03,#86,#33,#02,#B6,#33,#02,#E6,#33 ; #2d50 2..2.>3..3..3..3
db #03,#2E,#34,#03,#76,#34,#03,#BE,#34,#03,#06,#35,#03,#4E,#35,#03 ; #2d60 ..4.v4..4..5.N5.
db #96,#35,#03,#EA,#35,#03,#3E,#36,#03,#92,#36,#03,#E6,#36,#03,#3A ; #2d70 .5..5.>6..6..6.:
db #37,#03,#8E,#37,#03,#E2,#37,#04,#52,#38,#03,#A6,#38,#03,#FA,#38 ; #2d80 7..7..7.R8..8..8
db #03,#4E,#39,#03,#A2,#39,#03,#F6,#39,#03,#4A,#3A,#03,#9E,#3A,#04 ; #2d90 .N9..9..9.J:..:.
db #1F,#1F,#C0,#C0,#7F,#6A,#F0,#B0,#FF,#BF,#F8,#E8,#FF,#A2,#F8,#28 ; #2da0 .....j.........(
db #FF,#A2,#F8,#28,#7F,#62,#F0,#30,#1F,#1F,#C0,#C0,#0F,#0F,#E0,#E0 ; #2db0 ...(.b.0........
db #3F,#35,#F8,#58,#7F,#5F,#FC,#F4,#7F,#51,#FC,#14,#7F,#51,#FC,#14 ; #2dc0 ?5.X._...Q...Q..
db #3F,#31,#F8,#18,#0F,#0F,#E0,#E0,#07,#07,#F0,#F0,#1F,#1A,#FC,#AC ; #2dd0 ?1..............
db #3F,#2F,#FE,#FA,#3F,#28,#FE,#8A,#3F,#28,#FE,#8A,#1F,#18,#FC,#8C ; #2de0 ?/..?(..?(......
db #07,#07,#F0,#F0,#03,#03,#F8,#F8,#0F,#0D,#FE,#56,#1F,#17,#FF,#FD ; #2df0 ...........V....
db #1F,#14,#FF,#45,#1F,#14,#FF,#45,#0F,#0C,#FE,#46,#03,#03,#F8,#F8 ; #2e00 ...E...E...F....
db #01,#01,#FC,#FC,#00,#00,#07,#06,#FF,#AB,#00,#00,#0F,#0B,#FF,#FE ; #2e10 
db #80,#80,#0F,#0A,#FF,#22,#80,#80,#0F,#0A,#FF,#22,#80,#80,#07,#06 ; #2e20 ....."....."....
db #FF,#23,#00,#00,#01,#01,#FC,#FC,#00,#00,#00,#00,#FE,#FE,#00,#00 ; #2e30 .#..............
db #03,#03,#FF,#55,#80,#80,#07,#05,#FF,#FF,#C0,#40,#07,#05,#FF,#11 ; #2e40 ...U.......@....
db #C0,#40,#07,#05,#FF,#11,#C0,#40,#03,#03,#FF,#11,#80,#80,#00,#00 ; #2e50 .@.....@........
db #FE,#FE,#00,#00,#00,#00,#7F,#7F,#00,#00,#01,#01,#FF,#AA,#C0,#C0 ; #2e60 
db #03,#02,#FF,#FF,#E0,#A0,#03,#02,#FF,#88,#E0,#A0,#03,#02,#FF,#88 ; #2e70 
db #E0,#A0,#01,#01,#FF,#88,#C0,#C0,#00,#00,#7F,#7F,#00,#00,#00,#00 ; #2e80 
db #3F,#3F,#80,#80,#00,#00,#FF,#D5,#E0,#60,#01,#01,#FF,#7F,#F0,#D0 ; #2e90 ??.......`......
db #01,#01,#FF,#44,#F0,#50,#01,#01,#FF,#44,#F0,#50,#00,#00,#FF,#C4 ; #2ea0 ...D.P...D.P....
db #E0,#60,#00,#00,#3F,#3F,#80,#80,#1F,#1F,#F0,#F0,#7F,#6A,#FC,#AC ; #2eb0 .`..??.......j..
db #FF,#BF,#FE,#FA,#FF,#A1,#FE,#0A,#FF,#A1,#FE,#0A,#7F,#61,#FC,#0C ; #2ec0 .............a..
db #3F,#39,#F8,#38,#07,#07,#C0,#C0,#0F,#0F,#F8,#F8,#3F,#35,#FE,#56 ; #2ed0 ?9.8........?5.V
db #7F,#5F,#FF,#FD,#7F,#50,#FF,#85,#7F,#50,#FF,#85,#3F,#30,#FE,#86 ; #2ee0 ._...P...P..?0..
db #1F,#1C,#FC,#9C,#03,#03,#E0,#E0,#07,#07,#FC,#FC,#00,#00,#1F,#1A ; #2ef0 
db #FF,#AB,#00,#00,#3F,#2F,#FF,#FE,#80,#80,#3F,#28,#FF,#42,#80,#80 ; #2f00 ....?/....?(.B..
db #3F,#28,#FF,#42,#80,#80,#1F,#18,#FF,#43,#00,#00,#0F,#0E,#FE,#4E ; #2f10 ?(.B.....C.....N
db #00,#00,#01,#01,#F0,#F0,#00,#00,#03,#03,#FE,#FE,#00,#00,#0F,#0D ; #2f20 
db #FF,#55,#80,#80,#1F,#17,#FF,#FF,#C0,#40,#1F,#14,#FF,#21,#C0,#40 ; #2f30 .U.......@...!.@
db #1F,#14,#FF,#21,#C0,#40,#0F,#0C,#FF,#21,#80,#80,#07,#07,#FF,#27 ; #2f40 ...!.@...!.....'
db #00,#00,#00,#00,#F8,#F8,#00,#00,#01,#01,#FF,#FF,#00,#00,#07,#06 ; #2f50 
db #FF,#AA,#C0,#C0,#0F,#0B,#FF,#FF,#E0,#A0,#0F,#0A,#FF,#10,#E0,#A0 ; #2f60 
db #0F,#0A,#FF,#10,#E0,#A0,#07,#06,#FF,#10,#C0,#C0,#03,#03,#FF,#93 ; #2f70 
db #80,#80,#00,#00,#7C,#7C,#00,#00,#00,#00,#FF,#FF,#80,#80,#03,#03 ; #2f80 ....||..........
db #FF,#55,#E0,#60,#07,#05,#FF,#FF,#F0,#D0,#07,#05,#FF,#08,#F0,#50 ; #2f90 .U.`...........P
db #07,#05,#FF,#08,#F0,#50,#03,#03,#FF,#08,#E0,#60,#01,#01,#FF,#C9 ; #2fa0 .....P.....`....
db #C0,#C0,#00,#00,#3E,#3E,#00,#00,#00,#00,#7F,#7F,#C0,#C0,#01,#01 ; #2fb0 ....>>..........
db #FF,#AA,#F0,#B0,#03,#02,#FF,#FF,#F8,#E8,#03,#02,#FF,#84,#F8,#28 ; #2fc0 ...............(
db #03,#02,#FF,#84,#F8,#28,#01,#01,#FF,#84,#F0,#30,#00,#00,#FF,#E4 ; #2fd0 .....(.....0....
db #E0,#E0,#00,#00,#1F,#1F,#00,#00,#00,#00,#3F,#3F,#E0,#E0,#00,#00 ; #2fe0 ..........??....
db #FF,#D5,#F8,#58,#01,#01,#FF,#7F,#FC,#F4,#01,#01,#FF,#42,#FC,#14 ; #2ff0 ...X.........B..
db #01,#01,#FF,#42,#FC,#14,#00,#00,#FF,#C2,#F8,#18,#00,#00,#7F,#72 ; #3000 ...B...........r
db #F0,#70,#00,#00,#0F,#0F,#80,#80,#07,#07,#F8,#F8,#00,#00,#3F,#3D ; #3010 .p............?=
db #FF,#57,#00,#00,#7F,#6A,#FF,#AA,#80,#80,#FF,#BD,#FF,#57,#C0,#40 ; #3020 .W...j.......W.@
db #FF,#97,#FF,#FA,#C0,#40,#FF,#90,#FF,#42,#C0,#40,#7F,#50,#FF,#42 ; #3030 .....@...B.@.P.B
db #80,#80,#3F,#30,#FF,#43,#00,#00,#0F,#0E,#FC,#5C,#00,#00,#01,#01 ; #3040 ..?0.C.....\....
db #E0,#E0,#00,#00,#03,#03,#FC,#FC,#00,#00,#1F,#1E,#FF,#AB,#80,#80 ; #3050 
db #3F,#35,#FF,#55,#C0,#40,#7F,#5E,#FF,#AB,#E0,#A0,#7F,#4B,#FF,#FD ; #3060 ?5.U.@.^.....K..
db #E0,#20,#7F,#48,#FF,#21,#E0,#20,#3F,#28,#FF,#21,#C0,#40,#1F,#18 ; #3070 . .H.!. ?(.!.@..
db #FF,#21,#80,#80,#07,#07,#FE,#2E,#00,#00,#00,#00,#F0,#F0,#00,#00 ; #3080 .!..............
db #01,#01,#FE,#FE,#00,#00,#0F,#0F,#FF,#55,#C0,#C0,#1F,#1A,#FF,#AA ; #3090 .........U......
db #E0,#A0,#3F,#2F,#FF,#55,#F0,#D0,#3F,#25,#FF,#FE,#F0,#90,#3F,#24 ; #30a0 ..?/.U..?%....?$
db #FF,#10,#F0,#90,#1F,#14,#FF,#10,#E0,#A0,#0F,#0C,#FF,#10,#C0,#C0 ; #30b0 
db #03,#03,#FF,#97,#00,#00,#00,#00,#78,#78,#00,#00,#00,#00,#FF,#FF ; #30c0 ........xx......
db #00,#00,#07,#07,#FF,#AA,#E0,#E0,#0F,#0D,#FF,#55,#F0,#50,#1F,#17 ; #30d0 ...........U.P..
db #FF,#AA,#F8,#E8,#1F,#12,#FF,#FF,#F8,#48,#1F,#12,#FF,#08,#F8,#48 ; #30e0 .........H.....H
db #0F,#0A,#FF,#08,#F0,#50,#07,#06,#FF,#08,#E0,#60,#01,#01,#FF,#CB ; #30f0 .....P.....`....
db #80,#80,#00,#00,#3C,#3C,#00,#00,#00,#00,#7F,#7F,#80,#80,#03,#03 ; #3100 ....<<..........
db #FF,#D5,#F0,#70,#07,#06,#FF,#AA,#F8,#A8,#0F,#0B,#FF,#D5,#FC,#74 ; #3110 ...p...........t
db #0F,#09,#FF,#7F,#FC,#A4,#0F,#09,#FF,#04,#FC,#24,#07,#05,#FF,#04 ; #3120 ...........$....
db #F8,#28,#03,#03,#FF,#04,#F0,#30,#00,#00,#FF,#E5,#C0,#C0,#00,#00 ; #3130 .(.....0........
db #1E,#1E,#00,#00,#00,#00,#3F,#3F,#C0,#C0,#01,#01,#FF,#EA,#F8,#B8 ; #3140 ......??........
db #03,#03,#FF,#55,#FC,#54,#07,#05,#FF,#EA,#FE,#BA,#07,#04,#FF,#BF ; #3150 ...U.T..........
db #FE,#D2,#07,#04,#FF,#82,#FE,#12,#03,#02,#FF,#82,#FC,#14,#01,#01 ; #3160 
db #FF,#82,#F8,#18,#00,#00,#7F,#72,#E0,#E0,#00,#00,#0F,#0F,#00,#00 ; #3170 .......r........
db #00,#00,#1F,#1F,#E0,#E0,#00,#00,#FF,#F5,#FC,#5C,#01,#01,#FF,#AA ; #3180 ...........\....
db #FE,#AA,#03,#02,#FF,#F5,#FF,#5D,#03,#02,#FF,#5F,#FF,#E9,#03,#02 ; #3190 .......]..._....
db #FF,#41,#FF,#09,#01,#01,#FF,#41,#FE,#0A,#00,#00,#FF,#C1,#FC,#0C ; #31a0 .A.....A........
db #00,#00,#3F,#39,#F0,#70,#00,#00,#07,#07,#80,#80,#00,#00,#0F,#0F ; #31b0 ..?9.p..........
db #F0,#F0,#00,#00,#00,#00,#7F,#7A,#FE,#AE,#00,#00,#00,#00,#FF,#D5 ; #31c0 .......z........
db #FF,#55,#00,#00,#01,#01,#FF,#7A,#FF,#AE,#80,#80,#01,#01,#FF,#2F ; #31d0 .U.....z......./
db #FF,#F4,#80,#80,#01,#01,#FF,#20,#FF,#84,#80,#80,#00,#00,#FF,#A0 ; #31e0 ....... ........
db #FF,#85,#00,#00,#00,#00,#7F,#60,#FE,#86,#00,#00,#00,#00,#1F,#1C ; #31f0 .......`........
db #F8,#B8,#00,#00,#00,#00,#03,#03,#C0,#C0,#00,#00,#07,#07,#FF,#FF ; #3200 
db #80,#80,#3F,#3A,#FF,#AA,#F0,#F0,#7F,#55,#FF,#55,#F8,#58,#FF,#AA ; #3210 ..?:.....U.U.X..
db #FF,#AA,#FC,#AC,#FF,#D5,#FF,#55,#FC,#5C,#FF,#BA,#FF,#AA,#FC,#F4 ; #3220 .......U.\......
db #FF,#97,#FF,#FF,#FC,#A4,#FF,#90,#FF,#10,#FC,#24,#7F,#50,#FF,#10 ; #3230 ...........$.P..
db #F8,#28,#3F,#30,#FF,#10,#F0,#30,#1F,#18,#FF,#10,#E0,#60,#07,#07 ; #3240 .(?0...0.....`..
db #FF,#13,#80,#80,#00,#00,#FC,#FC,#00,#00,#03,#03,#FF,#FF,#C0,#C0 ; #3250 
db #1F,#1D,#FF,#55,#F8,#78,#3F,#2A,#FF,#AA,#FC,#AC,#7F,#55,#FF,#55 ; #3260 ...U.x?*.....U.U
db #FE,#56,#7F,#6A,#FF,#AA,#FE,#AE,#7F,#5D,#FF,#55,#FE,#7A,#7F,#4B ; #3270 .V.j.....].U.z.K
db #FF,#FF,#FE,#D2,#7F,#48,#FF,#08,#FE,#12,#3F,#28,#FF,#08,#FC,#14 ; #3280 .....H....?(....
db #1F,#18,#FF,#08,#F8,#18,#0F,#0C,#FF,#08,#F0,#30,#03,#03,#FF,#89 ; #3290 ...........0....
db #C0,#C0,#00,#00,#7E,#7E,#00,#00,#01,#01,#FF,#FF,#E0,#E0,#0F,#0E ; #32a0 ....~~..........
db #FF,#AA,#FC,#BC,#1F,#15,#FF,#55,#FE,#56,#3F,#2A,#FF,#AA,#FF,#AB ; #32b0 .......U.V?*....
db #3F,#35,#FF,#55,#FF,#57,#3F,#2E,#FF,#AA,#FF,#BD,#3F,#25,#FF,#FF ; #32c0 ?5.U.W?.....?%..
db #FF,#E9,#3F,#24,#FF,#04,#FF,#09,#1F,#14,#FF,#04,#FE,#0A,#0F,#0C ; #32d0 ..?$............
db #FF,#04,#FC,#0C,#07,#06,#FF,#04,#F8,#18,#01,#01,#FF,#C4,#E0,#E0 ; #32e0 
db #00,#00,#3F,#3F,#00,#00,#00,#00,#FF,#FF,#F0,#F0,#00,#00,#07,#07 ; #32f0 ..??............
db #FF,#55,#FE,#5E,#00,#00,#0F,#0A,#FF,#AA,#FF,#AB,#00,#00,#1F,#15 ; #3300 .U.^............
db #FF,#55,#FF,#55,#80,#80,#1F,#1A,#FF,#AA,#FF,#AB,#80,#80,#1F,#17 ; #3310 .U.U............
db #FF,#55,#FF,#5E,#80,#80,#1F,#12,#FF,#FF,#FF,#F4,#80,#80,#1F,#12 ; #3320 .U.^............
db #FF,#02,#FF,#04,#80,#80,#0F,#0A,#FF,#02,#FF,#05,#00,#00,#07,#06 ; #3330 
db #FF,#02,#FE,#06,#00,#00,#03,#03,#FF,#02,#FC,#0C,#00,#00,#00,#00 ; #3340 
db #FF,#E2,#F0,#70,#00,#00,#00,#00,#1F,#1F,#80,#80,#00,#00,#00,#00 ; #3350 ...p............
db #7F,#7F,#F8,#F8,#00,#00,#03,#03,#FF,#AA,#FF,#AF,#00,#00,#07,#05 ; #3360 
db #FF,#55,#FF,#55,#80,#80,#0F,#0A,#FF,#AA,#FF,#AA,#C0,#C0,#0F,#0D ; #3370 .U.U............
db #FF,#55,#FF,#55,#C0,#C0,#0F,#0B,#FF,#AA,#FF,#AF,#C0,#40,#0F,#09 ; #3380 .U.U.........@..
db #FF,#7F,#FF,#FA,#C0,#40,#0F,#09,#FF,#01,#FF,#02,#C0,#40,#07,#05 ; #3390 .....@.......@..
db #FF,#01,#FF,#02,#80,#80,#03,#03,#FF,#01,#FF,#03,#00,#00,#01,#01 ; #33a0 
db #FF,#81,#FE,#06,#00,#00,#00,#00,#7F,#71,#F8,#38,#00,#00,#00,#00 ; #33b0 .........q.8....
db #0F,#0F,#C0,#C0,#00,#00,#00,#00,#3F,#3F,#FC,#FC,#00,#00,#01,#01 ; #33c0 ........??......
db #FF,#D5,#FF,#57,#80,#80,#03,#02,#FF,#AA,#FF,#AA,#C0,#C0,#07,#05 ; #33d0 ...W............
db #FF,#55,#FF,#55,#E0,#60,#07,#06,#FF,#AA,#FF,#AA,#E0,#E0,#07,#05 ; #33e0 .U.U.`..........
db #FF,#D5,#FF,#57,#E0,#A0,#07,#04,#FF,#BF,#FF,#FD,#E0,#20,#07,#04 ; #33f0 ...W......... ..
db #FF,#80,#FF,#81,#E0,#20,#03,#02,#FF,#80,#FF,#81,#C0,#40,#01,#01 ; #3400 ..... .......@..
db #FF,#80,#FF,#81,#80,#80,#00,#00,#FF,#C0,#FF,#83,#00,#00,#00,#00 ; #3410 
db #3F,#38,#FC,#9C,#00,#00,#00,#00,#07,#07,#E0,#E0,#00,#00,#00,#00 ; #3420 ?8..............
db #1F,#1F,#FE,#FE,#00,#00,#00,#00,#FF,#EA,#FF,#AB,#C0,#C0,#01,#01 ; #3430 
db #FF,#55,#FF,#55,#E0,#60,#03,#02,#FF,#AA,#FF,#AA,#F0,#B0,#03,#03 ; #3440 .U.U.`..........
db #FF,#55,#FF,#55,#F0,#70,#03,#02,#FF,#EA,#FF,#AB,#F0,#D0,#03,#02 ; #3450 .U.U.p..........
db #FF,#5F,#FF,#FE,#F0,#90,#03,#02,#FF,#40,#FF,#40,#F0,#90,#01,#01 ; #3460 ._.......@.@....
db #FF,#40,#FF,#40,#E0,#A0,#00,#00,#FF,#C0,#FF,#40,#C0,#C0,#00,#00 ; #3470 .@.@.......@....
db #7F,#60,#FF,#41,#80,#80,#00,#00,#1F,#1C,#FE,#4E,#00,#00,#00,#00 ; #3480 .`.A.......N....
db #03,#03,#F0,#F0,#00,#00,#00,#00,#0F,#0F,#FF,#FF,#00,#00,#00,#00 ; #3490 
db #7F,#75,#FF,#55,#E0,#E0,#00,#00,#FF,#AA,#FF,#AA,#F0,#B0,#01,#01 ; #34a0 .u.U............
db #FF,#55,#FF,#55,#F8,#58,#01,#01,#FF,#AA,#FF,#AA,#F8,#B8,#01,#01 ; #34b0 .U.U.X..........
db #FF,#75,#FF,#55,#F8,#E8,#01,#01,#FF,#2F,#FF,#FF,#F8,#48,#01,#01 ; #34c0 .u.U...../...H..
db #FF,#20,#FF,#20,#F8,#48,#00,#00,#FF,#A0,#FF,#20,#F0,#50,#00,#00 ; #34d0 . . .H..... .P..
db #7F,#60,#FF,#20,#E0,#60,#00,#00,#3F,#30,#FF,#20,#C0,#C0,#00,#00 ; #34e0 .`. .`..?0. ....
db #0F,#0E,#FF,#27,#00,#00,#00,#00,#01,#01,#F8,#F8,#00,#00,#FF,#FF ; #34f0 ...'............
db #F8,#F8,#FF,#A2,#F8,#28,#7F,#62,#F0,#30,#1F,#1F,#C0,#C0,#7F,#7F ; #3500 .....(.b.0......
db #FC,#FC,#7F,#51,#FC,#14,#3F,#31,#F8,#18,#0F,#0F,#E0,#E0,#3F,#3F ; #3510 ...Q..?1......??
db #FE,#FE,#3F,#28,#FE,#8A,#1F,#18,#FC,#8C,#07,#07,#F0,#F0,#1F,#1F ; #3520 ..?(............
db #FF,#FF,#1F,#14,#FF,#45,#0F,#0C,#FE,#46,#03,#03,#F8,#F8,#0F,#0F ; #3530 .....E...F......
db #FF,#FF,#80,#80,#0F,#0A,#FF,#22,#80,#80,#07,#06,#FF,#23,#00,#00 ; #3540 .......".....#..
db #01,#01,#FC,#FC,#00,#00,#07,#07,#FF,#FF,#C0,#C0,#07,#05,#FF,#11 ; #3550 
db #C0,#40,#03,#03,#FF,#11,#80,#80,#00,#00,#FE,#FE,#00,#00,#03,#03 ; #3560 .@..............
db #FF,#FF,#E0,#E0,#03,#02,#FF,#88,#E0,#A0,#01,#01,#FF,#88,#C0,#C0 ; #3570 
db #00,#00,#7F,#7F,#00,#00,#01,#01,#FF,#FF,#F0,#F0,#01,#01,#FF,#44 ; #3580 ...............D
db #F0,#50,#00,#00,#FF,#C4,#E0,#60,#00,#00,#3F,#3F,#80,#80,#FF,#FF ; #3590 .P.....`..??....
db #FE,#FE,#FF,#A1,#FE,#0A,#7F,#61,#FC,#0C,#1F,#1F,#F0,#F0,#7F,#7F ; #35a0 .......a........
db #FF,#FF,#7F,#50,#FF,#85,#3F,#30,#FE,#86,#0F,#0F,#F8,#F8,#3F,#3F ; #35b0 ...P..?0......??
db #FF,#FF,#80,#80,#3F,#28,#FF,#42,#80,#80,#1F,#18,#FF,#43,#00,#00 ; #35c0 ....?(.B.....C..
db #07,#07,#FC,#FC,#00,#00,#1F,#1F,#FF,#FF,#C0,#C0,#1F,#14,#FF,#21 ; #35d0 ...............!
db #C0,#40,#0F,#0C,#FF,#21,#80,#80,#03,#03,#FE,#FE,#00,#00,#0F,#0F ; #35e0 .@...!..........
db #FF,#FF,#E0,#E0,#0F,#0A,#FF,#10,#E0,#A0,#07,#06,#FF,#10,#C0,#C0 ; #35f0 
db #01,#01,#FF,#FF,#00,#00,#07,#07,#FF,#FF,#F0,#F0,#07,#05,#FF,#08 ; #3600 
db #F0,#50,#03,#03,#FF,#08,#E0,#60,#00,#00,#FF,#FF,#80,#80,#03,#03 ; #3610 .P.....`........
db #FF,#FF,#F8,#F8,#03,#02,#FF,#84,#F8,#28,#01,#01,#FF,#84,#F0,#30 ; #3620 .........(.....0
db #00,#00,#7F,#7F,#C0,#C0,#01,#01,#FF,#FF,#FC,#FC,#01,#01,#FF,#42 ; #3630 ...............B
db #FC,#14,#00,#00,#FF,#C2,#F8,#18,#00,#00,#3F,#3F,#E0,#E0,#FF,#FF ; #3640 ..........??....
db #FF,#FF,#C0,#C0,#FF,#90,#FF,#42,#C0,#40,#7F,#50,#FF,#42,#80,#80 ; #3650 .......B.@.P.B..
db #3F,#38,#FF,#47,#00,#00,#07,#07,#F8,#F8,#00,#00,#7F,#7F,#FF,#FF ; #3660 ?8.G............
db #E0,#E0,#7F,#48,#FF,#21,#E0,#20,#3F,#28,#FF,#21,#C0,#40,#1F,#1C ; #3670 ...H.!. ?(.!.@..
db #FF,#23,#80,#80,#03,#03,#FC,#FC,#00,#00,#3F,#3F,#FF,#FF,#F0,#F0 ; #3680 .#........??....
db #3F,#24,#FF,#10,#F0,#90,#1F,#14,#FF,#10,#E0,#A0,#0F,#0E,#FF,#11 ; #3690 ?$..............
db #C0,#C0,#01,#01,#FE,#FE,#00,#00,#1F,#1F,#FF,#FF,#F8,#F8,#1F,#12 ; #36a0 
db #FF,#08,#F8,#48,#0F,#0A,#FF,#08,#F0,#50,#07,#07,#FF,#08,#E0,#E0 ; #36b0 ...H.....P......
db #00,#00,#FF,#FF,#00,#00,#0F,#0F,#FF,#FF,#FC,#FC,#0F,#09,#FF,#04 ; #36c0 
db #FC,#24,#07,#05,#FF,#04,#F8,#28,#03,#03,#FF,#84,#F0,#70,#00,#00 ; #36d0 .$.....(.....p..
db #7F,#7F,#80,#80,#07,#07,#FF,#FF,#FE,#FE,#07,#04,#FF,#82,#FE,#12 ; #36e0 
db #03,#02,#FF,#82,#FC,#14,#01,#01,#FF,#C2,#F8,#38,#00,#00,#3F,#3F ; #36f0 ...........8..??
db #C0,#C0,#03,#03,#FF,#FF,#FF,#FF,#03,#02,#FF,#41,#FF,#09,#01,#01 ; #3700 ...........A....
db #FF,#41,#FE,#0A,#00,#00,#FF,#E1,#FC,#1C,#00,#00,#1F,#1F,#E0,#E0 ; #3710 .A..............
db #01,#01,#FF,#FF,#FF,#FF,#80,#80,#01,#01,#FF,#20,#FF,#84,#80,#80 ; #3720 ........... ....
db #00,#00,#FF,#A0,#FF,#85,#00,#00,#00,#00,#7F,#70,#FE,#8E,#00,#00 ; #3730 ...........p....
db #00,#00,#0F,#0F,#F0,#F0,#00,#00,#FF,#FF,#FF,#FF,#FC,#FC,#FF,#90 ; #3740 
db #FF,#10,#FC,#24,#7F,#50,#FF,#10,#F8,#28,#3F,#38,#FF,#10,#F0,#70 ; #3750 ...$.P...(?8...p
db #07,#07,#FF,#FF,#80,#80,#7F,#7F,#FF,#FF,#FE,#FE,#7F,#48,#FF,#08 ; #3760 .............H..
db #FE,#12,#3F,#28,#FF,#08,#FC,#14,#1F,#1C,#FF,#08,#F8,#38,#03,#03 ; #3770 ..?(.........8..
db #FF,#FF,#C0,#C0,#3F,#3F,#FF,#FF,#FF,#FF,#3F,#24,#FF,#04,#FF,#09 ; #3780 ....??....?$....
db #1F,#14,#FF,#04,#FE,#0A,#0F,#0E,#FF,#04,#FC,#1C,#01,#01,#FF,#FF ; #3790 
db #E0,#E0,#1F,#1F,#FF,#FF,#FF,#FF,#80,#80,#1F,#12,#FF,#02,#FF,#04 ; #37a0 
db #80,#80,#0F,#0A,#FF,#02,#FF,#05,#00,#00,#07,#07,#FF,#02,#FE,#0E ; #37b0 
db #00,#00,#00,#00,#FF,#FF,#F0,#F0,#00,#00,#0F,#0F,#FF,#FF,#FF,#FF ; #37c0 
db #C0,#C0,#0F,#09,#FF,#01,#FF,#02,#C0,#40,#07,#05,#FF,#01,#FF,#02 ; #37d0 .........@......
db #80,#80,#03,#03,#FF,#81,#FF,#07,#00,#00,#00,#00,#7F,#7F,#F8,#F8 ; #37e0 
db #00,#00,#07,#07,#FF,#FF,#FF,#FF,#E0,#E0,#07,#04,#FF,#80,#FF,#81 ; #37f0 
db #E0,#20,#03,#02,#FF,#80,#FF,#81,#C0,#40,#01,#01,#FF,#C0,#FF,#83 ; #3800 . .......@......
db #80,#80,#00,#00,#3F,#3F,#FC,#FC,#00,#00,#03,#03,#FF,#FF,#FF,#FF ; #3810 ....??..........
db #F0,#F0,#03,#02,#FF,#40,#FF,#40,#F0,#90,#01,#01,#FF,#40,#FF,#40 ; #3820 .....@.@.....@.@
db #E0,#A0,#00,#00,#FF,#E0,#FF,#41,#C0,#C0,#00,#00,#1F,#1F,#FE,#FE ; #3830 .......A........
db #00,#00,#01,#01,#FF,#FF,#FF,#FF,#F8,#F8,#01,#01,#FF,#20,#FF,#20 ; #3840 ............. . 
db #F8,#48,#00,#00,#FF,#A0,#FF,#20,#F0,#50,#00,#00,#7F,#70,#FF,#20 ; #3850 .H..... .P...p. 
db #E0,#E0,#00,#00,#0F,#0F,#FF,#FF,#00,#00,#22,#22,#20,#20,#77,#55 ; #3860 ..........""  wU
db #70,#50,#3F,#2D,#E0,#A0,#1F,#15,#C0,#40,#7F,#72,#F0,#70,#FF,#88 ; #3870 pP?-.....@.r.p..
db #F8,#88,#7F,#72,#F0,#70,#1F,#15,#C0,#40,#3F,#2D,#E0,#A0,#3F,#37 ; #3880 ...r.p...@?-..?7
db #E0,#60,#0F,#0A,#80,#80,#07,#07,#00,#00,#11,#11,#10,#10,#3B,#2A ; #3890 .`............;*
db #B8,#A8,#1F,#16,#F0,#D0,#0F,#0A,#E0,#A0,#3F,#39,#F8,#38,#7F,#44 ; #38a0 ..........?9.8.D
db #FC,#44,#3F,#39,#F8,#38,#0F,#0A,#E0,#A0,#1F,#16,#F0,#D0,#1F,#1B ; #38b0 .D?9.8..........
db #F0,#B0,#07,#05,#C0,#40,#03,#03,#80,#80,#08,#08,#88,#88,#1D,#15 ; #38c0 .....@..........
db #DC,#54,#0F,#0B,#F8,#68,#07,#05,#F0,#50,#1F,#1C,#FC,#9C,#3F,#22 ; #38d0 .T...h...P....?"
db #FE,#22,#1F,#1C,#FC,#9C,#07,#05,#F0,#50,#0F,#0B,#F8,#68,#0F,#0D ; #38e0 .".......P...h..
db #F8,#D8,#03,#02,#E0,#A0,#01,#01,#C0,#C0,#04,#04,#44,#44,#0E,#0A ; #38f0 ............DD..
db #EE,#AA,#07,#05,#FC,#B4,#03,#02,#F8,#A8,#0F,#0E,#FE,#4E,#1F,#11 ; #3900 .............N..
db #FF,#11,#0F,#0E,#FE,#4E,#03,#02,#F8,#A8,#07,#05,#FC,#B4,#07,#06 ; #3910 .....N..........
db #FC,#EC,#01,#01,#F0,#50,#00,#00,#E0,#E0,#02,#02,#22,#22,#00,#00 ; #3920 .....P......""..
db #07,#05,#77,#55,#00,#00,#03,#02,#FE,#DA,#00,#00,#01,#01,#FC,#54 ; #3930 ..wU...........T
db #00,#00,#07,#07,#FF,#27,#00,#00,#0F,#08,#FF,#88,#80,#80,#07,#07 ; #3940 .....'..........
db #FF,#27,#00,#00,#01,#01,#FC,#54,#00,#00,#03,#02,#FE,#DA,#00,#00 ; #3950 .'.....T........
db #03,#03,#FE,#76,#00,#00,#00,#00,#F8,#A8,#00,#00,#00,#00,#70,#70 ; #3960 ...v..........pp
db #00,#00,#01,#01,#11,#11,#00,#00,#03,#02,#BB,#AA,#80,#80,#01,#01 ; #3970 
db #FF,#6D,#00,#00,#00,#00,#FE,#AA,#00,#00,#03,#03,#FF,#93,#80,#80 ; #3980 .m..............
db #07,#04,#FF,#44,#C0,#40,#03,#03,#FF,#93,#80,#80,#00,#00,#FE,#AA ; #3990 ...D.@..........
db #00,#00,#01,#01,#FF,#6D,#00,#00,#01,#01,#FF,#BB,#00,#00,#00,#00 ; #39a0 .....m..........
db #7C,#54,#00,#00,#00,#00,#38,#38,#00,#00,#00,#00,#88,#88,#80,#80 ; #39b0 |T....88........
db #01,#01,#DD,#55,#C0,#40,#00,#00,#FF,#B6,#80,#80,#00,#00,#7F,#55 ; #39c0 ...U.@.........U
db #00,#00,#01,#01,#FF,#C9,#C0,#C0,#03,#02,#FF,#22,#E0,#20,#01,#01 ; #39d0 ...........". ..
db #FF,#C9,#C0,#C0,#00,#00,#7F,#55,#00,#00,#00,#00,#FF,#B6,#80,#80 ; #39e0 .......U........
db #00,#00,#FF,#DD,#80,#80,#00,#00,#3E,#2A,#00,#00,#00,#00,#1C,#1C ; #39f0 ........>*......
db #00,#00,#00,#00,#44,#44,#40,#40,#00,#00,#EE,#AA,#E0,#A0,#00,#00 ; #3a00 ....DD@@........
db #7F,#5B,#C0,#40,#00,#00,#3F,#2A,#80,#80,#00,#00,#FF,#E4,#E0,#E0 ; #3a10 .[.@..?*........
db #01,#01,#FF,#11,#F0,#10,#00,#00,#FF,#E4,#E0,#E0,#00,#00,#3F,#2A ; #3a20 ..............?*
db #80,#80,#00,#00,#7F,#5B,#C0,#40,#00,#00,#7F,#6E,#C0,#C0,#00,#00 ; #3a30 .....[.@...n....
db #1F,#15,#00,#00,#00,#00,#0E,#0E,#00,#00,#21,#21,#08,#08,#73,#52 ; #3a40 ..........!!..sR
db #9C,#94,#3B,#2A,#B8,#A8,#1F,#16,#F0,#D0,#7F,#79,#FC,#3C,#FF,#84 ; #3a50 ..;*.......y.<..
db #FE,#42,#7F,#79,#FC,#3C,#1F,#16,#F0,#D0,#3B,#2A,#B8,#A8,#77,#56 ; #3a60 .B.y.<....;*..wV
db #DC,#D4,#2F,#2D,#E8,#68,#0F,#0A,#E0,#A0,#07,#06,#C0,#C0,#03,#03 ; #3a70 ../-.h..........
db #80,#80,#10,#10,#84,#84,#39,#29,#CE,#4A,#1D,#15,#DC,#54,#0F,#0B ; #3a80 ......9).J...T..
db #F8,#68,#3F,#3C,#FE,#9E,#7F,#42,#FF,#21,#3F,#3C,#FE,#9E,#0F,#0B ; #3a90 .h?<...B.!?<....
db #F8,#68,#1D,#15,#DC,#54,#3B,#2B,#EE,#6A,#17,#16,#F4,#B4,#07,#05 ; #3aa0 .h...T;+.j......
db #F0,#50,#03,#03,#E0,#60,#01,#01,#C0,#C0,#08,#08,#42,#42,#00,#00 ; #3ab0 .P...`......BB..
db #1C,#14,#E7,#A5,#00,#00,#0E,#0A,#EE,#AA,#00,#00,#07,#05,#FC,#B4 ; #3ac0 
db #00,#00,#1F,#1E,#FF,#4F,#00,#00,#3F,#21,#FF,#10,#80,#80,#1F,#1E ; #3ad0 .....O..?!......
db #FF,#4F,#00,#00,#07,#05,#FC,#B4,#00,#00,#0E,#0A,#EE,#AA,#00,#00 ; #3ae0 .O..............
db #1D,#15,#F7,#B5,#00,#00,#0B,#0B,#FA,#5A,#00,#00,#03,#02,#F8,#A8 ; #3af0 .........Z......
db #00,#00,#01,#01,#F0,#B0,#00,#00,#00,#00,#E0,#E0,#00,#00,#04,#04 ; #3b00 
db #21,#21,#00,#00,#0E,#0A,#73,#52,#80,#80,#07,#05,#77,#55,#00,#00 ; #3b10 !!....sR....wU..
db #03,#02,#FE,#DA,#00,#00,#0F,#0F,#FF,#27,#80,#80,#1F,#10,#FF,#88 ; #3b20 .........'......
db #C0,#40,#0F,#0F,#FF,#27,#80,#80,#03,#02,#FE,#DA,#00,#00,#07,#05 ; #3b30 .@...'..........
db #77,#55,#00,#00,#0E,#0A,#FB,#DA,#80,#80,#05,#05,#FD,#AD,#00,#00 ; #3b40 wU..............
db #01,#01,#FC,#54,#00,#00,#00,#00,#F8,#D8,#00,#00,#00,#00,#70,#70 ; #3b50 ...T..........pp
db #00,#00,#02,#02,#10,#10,#80,#80,#07,#05,#39,#29,#C0,#40,#03,#02 ; #3b60 ..........9).@..
db #BB,#AA,#80,#80,#01,#01,#FF,#6D,#00,#00,#07,#07,#FF,#93,#C0,#C0 ; #3b70 .......m........
db #0F,#08,#FF,#44,#E0,#20,#07,#07,#FF,#93,#C0,#C0,#01,#01,#FF,#6D ; #3b80 ...D. .........m
db #00,#00,#03,#02,#BB,#AA,#80,#80,#07,#05,#7D,#6D,#C0,#40,#02,#02 ; #3b90 ..........}m.@..
db #FE,#D6,#80,#80,#00,#00,#FE,#AA,#00,#00,#00,#00,#7C,#6C,#00,#00 ; #3ba0 ............|l..
db #00,#00,#38,#38,#00,#00,#01,#01,#08,#08,#40,#40,#03,#02,#9C,#94 ; #3bb0 ..88......@@....
db #E0,#A0,#01,#01,#DD,#55,#C0,#40,#00,#00,#FF,#B6,#80,#80,#03,#03 ; #3bc0 .....U.@........
db #FF,#C9,#E0,#E0,#07,#04,#FF,#22,#F0,#10,#03,#03,#FF,#C9,#E0,#E0 ; #3bd0 ......."........
db #00,#00,#FF,#B6,#80,#80,#01,#01,#DD,#55,#C0,#40,#03,#02,#BE,#B6 ; #3be0 .........U.@....
db #E0,#A0,#01,#01,#7F,#6B,#40,#40,#00,#00,#7F,#55,#00,#00,#00,#00 ; #3bf0 .....k@@...U....
db #3E,#36,#00,#00,#00,#00,#1C,#1C,#00,#00,#00,#00,#84,#84,#20,#20 ; #3c00 >6............  
db #01,#01,#CE,#4A,#70,#50,#00,#00,#EE,#AA,#E0,#A0,#00,#00,#7F,#5B ; #3c10 ...JpP.........[
db #C0,#40,#01,#01,#FF,#E4,#F0,#F0,#03,#02,#FF,#11,#F8,#08,#01,#01 ; #3c20 .@..............
db #FF,#E4,#F0,#F0,#00,#00,#7F,#5B,#C0,#40,#00,#00,#EE,#AA,#E0,#A0 ; #3c30 .......[.@......
db #01,#01,#DF,#5B,#70,#50,#00,#00,#BF,#B5,#A0,#A0,#00,#00,#3F,#2A ; #3c40 ...[pP........?*
db #80,#80,#00,#00,#1F,#1B,#00,#00,#00,#00,#0E,#0E,#00,#00,#00,#00 ; #3c50 
db #42,#42,#10,#10,#00,#00,#E7,#A5,#38,#28,#00,#00,#77,#55,#70,#50 ; #3c60 BB......8(..wUpP
db #00,#00,#3F,#2D,#E0,#A0,#00,#00,#FF,#F2,#F8,#78,#01,#01,#FF,#08 ; #3c70 ..?-.......x....
db #FC,#84,#00,#00,#FF,#F2,#F8,#78,#00,#00,#3F,#2D,#E0,#A0,#00,#00 ; #3c80 .......x..?-....
db #77,#55,#70,#50,#00,#00,#EF,#AD,#B8,#A8,#00,#00,#5F,#5A,#D0,#D0 ; #3c90 wUpP........_Z..
db #00,#00,#1F,#15,#C0,#40,#00,#00,#0F,#0D,#80,#80,#00,#00,#07,#07 ; #3ca0 .....@..........
db #00,#00,#10,#10,#41,#41,#00,#00,#38,#28,#E3,#A2,#80,#80,#1C,#14 ; #3cb0 ....AA..8(......
db #E7,#A5,#00,#00,#0E,#0A,#EE,#AA,#00,#00,#07,#05,#FC,#B4,#00,#00 ; #3cc0 
db #7F,#7E,#FF,#4F,#C0,#C0,#FF,#84,#FF,#04,#E0,#20,#7F,#7E,#FF,#4F ; #3cd0 .~.O....... .~.O
db #C0,#C0,#07,#05,#FC,#B4,#00,#00,#0E,#0A,#EE,#AA,#00,#00,#1D,#15 ; #3ce0 
db #F7,#B5,#00,#00,#1F,#1E,#FF,#EF,#00,#00,#07,#05,#FC,#14,#00,#00 ; #3cf0 
db #07,#07,#FC,#1C,#00,#00,#01,#01,#F0,#F0,#00,#00,#08,#08,#20,#20 ; #3d00 ..............  
db #80,#80,#1C,#14,#71,#51,#C0,#40,#0E,#0A,#73,#52,#80,#80,#07,#05 ; #3d10 ....qQ.@..sR....
db #77,#55,#00,#00,#03,#02,#FE,#DA,#00,#00,#3F,#3F,#FF,#27,#E0,#E0 ; #3d20 wU........??.'..
db #7F,#42,#FF,#02,#F0,#10,#3F,#3F,#FF,#27,#E0,#E0,#03,#02,#FE,#DA ; #3d30 .B....??.'......
db #00,#00,#07,#05,#77,#55,#00,#00,#0E,#0A,#FB,#DA,#80,#80,#0F,#0F ; #3d40 ....wU..........
db #FF,#77,#80,#80,#03,#02,#FE,#8A,#00,#00,#03,#03,#FE,#8E,#00,#00 ; #3d50 .w..............
db #00,#00,#F8,#F8,#00,#00,#04,#04,#10,#10,#40,#40,#0E,#0A,#38,#28 ; #3d60 ..........@@..8(
db #E0,#A0,#07,#05,#39,#29,#C0,#40,#03,#02,#BB,#AA,#80,#80,#01,#01 ; #3d70 ....9).@........
db #FF,#6D,#00,#00,#1F,#1F,#FF,#93,#F0,#F0,#3F,#21,#FF,#01,#F8,#08 ; #3d80 .m........?!....
db #1F,#1F,#FF,#93,#F0,#F0,#01,#01,#FF,#6D,#00,#00,#03,#02,#BB,#AA ; #3d90 .........m......
db #80,#80,#07,#05,#7D,#6D,#C0,#40,#07,#07,#FF,#BB,#C0,#C0,#01,#01 ; #3da0 ....}m.@........
db #FF,#45,#00,#00,#01,#01,#FF,#C7,#00,#00,#00,#00,#7C,#7C,#00,#00 ; #3db0 .E..........||..
db #02,#02,#08,#08,#20,#20,#07,#05,#1C,#14,#70,#50,#03,#02,#9C,#94 ; #3dc0 ....  ....pP....
db #E0,#A0,#01,#01,#DD,#55,#C0,#40,#00,#00,#FF,#B6,#80,#80,#0F,#0F ; #3dd0 .....U.@........
db #FF,#C9,#F8,#F8,#1F,#10,#FF,#80,#FC,#84,#0F,#0F,#FF,#C9,#F8,#F8 ; #3de0 
db #00,#00,#FF,#B6,#80,#80,#01,#01,#DD,#55,#C0,#40,#03,#02,#BE,#B6 ; #3df0 .........U.@....
db #E0,#A0,#03,#03,#FF,#DD,#E0,#E0,#00,#00,#FF,#A2,#80,#80,#00,#00 ; #3e00 
db #FF,#E3,#80,#80,#00,#00,#3E,#3E,#00,#00,#01,#01,#04,#04,#10,#10 ; #3e10 ......>>........
db #03,#02,#8E,#8A,#38,#28,#01,#01,#CE,#4A,#70,#50,#00,#00,#EE,#AA ; #3e20 ....8(...JpP....
db #E0,#A0,#00,#00,#7F,#5B,#C0,#40,#07,#07,#FF,#E4,#FC,#FC,#0F,#08 ; #3e30 .....[.@........
db #FF,#40,#FE,#42,#07,#07,#FF,#E4,#FC,#FC,#00,#00,#7F,#5B,#C0,#40 ; #3e40 .@.B.........[.@
db #00,#00,#EE,#AA,#E0,#A0,#01,#01,#DF,#5B,#70,#50,#01,#01,#FF,#EE ; #3e50 .........[pP....
db #F0,#F0,#00,#00,#7F,#51,#C0,#40,#00,#00,#7F,#71,#C0,#C0,#00,#00 ; #3e60 .....Q.@...q....
db #1F,#1F,#00,#00,#00,#00,#82,#82,#08,#08,#01,#01,#C7,#45,#1C,#14 ; #3e70 .............E..
db #00,#00,#E7,#A5,#38,#28,#00,#00,#77,#55,#70,#50,#00,#00,#3F,#2D ; #3e80 ....8(..wUpP..?-
db #E0,#A0,#03,#03,#FF,#F2,#FE,#7E,#07,#04,#FF,#20,#FF,#21,#03,#03 ; #3e90 .......~... .!..
db #FF,#F2,#FE,#7E,#00,#00,#3F,#2D,#E0,#A0,#00,#00,#77,#55,#70,#50 ; #3ea0 ...~..?-....wUpP
db #00,#00,#EF,#AD,#B8,#A8,#00,#00,#FF,#F7,#F8,#78,#00,#00,#3F,#28 ; #3eb0 ...........x..?(
db #E0,#A0,#00,#00,#3F,#38,#E0,#E0,#00,#00,#0F,#0F,#80,#80,#00,#00 ; #3ec0 ....?8..........
db #41,#41,#04,#04,#00,#00,#E3,#A2,#8E,#8A,#00,#00,#73,#52,#9C,#94 ; #3ed0 AA..........sR..
db #00,#00,#3B,#2A,#B8,#A8,#00,#00,#1F,#16,#F0,#D0,#01,#01,#FF,#F9 ; #3ee0 ..;*............
db #FF,#3F,#03,#02,#FF,#10,#FF,#10,#01,#01,#FF,#F9,#FF,#3F,#00,#00 ; #3ef0 .?...........?..
db #1F,#16,#F0,#D0,#00,#00,#3B,#2A,#B8,#A8,#00,#00,#77,#56,#DC,#D4 ; #3f00 ......;*....wV..
db #00,#00,#7F,#7B,#FC,#BC,#00,#00,#1F,#14,#F0,#50,#00,#00,#1F,#1C ; #3f10 ...{.......P....
db #F0,#70,#00,#00,#07,#07,#C0,#C0,#00,#00,#20,#20,#82,#82,#00,#00 ; #3f20 .p........  ....
db #00,#00,#71,#51,#C7,#45,#00,#00,#00,#00,#39,#29,#CE,#4A,#00,#00 ; #3f30 ..qQ.E....9).J..
db #00,#00,#1D,#15,#DC,#54,#00,#00,#00,#00,#0F,#0B,#F8,#68,#00,#00 ; #3f40 .....T.......h..
db #00,#00,#FF,#FC,#FF,#9F,#80,#80,#01,#01,#FF,#08,#FF,#08,#C0,#40 ; #3f50 ...............@
db #00,#00,#FF,#FC,#FF,#9F,#80,#80,#00,#00,#0F,#0B,#F8,#68,#00,#00 ; #3f60 .............h..
db #00,#00,#1D,#15,#DC,#54,#00,#00,#00,#00,#3B,#2B,#EE,#6A,#00,#00 ; #3f70 .....T....;+.j..
db #00,#00,#3F,#3D,#FE,#DE,#00,#00,#00,#00,#0F,#0A,#F8,#28,#00,#00 ; #3f80 ..?=.........(..
db #00,#00,#0F,#0E,#F8,#38,#00,#00,#00,#00,#03,#03,#E0,#E0,#00,#00 ; #3f90 .....8..........
db #18,#18,#20,#20,#C0,#C0,#1C,#14,#71,#51,#C0,#40,#0E,#0A,#73,#52 ; #3fa0 ..  ....qQ.@..sR
db #80,#80,#07,#05,#77,#55,#00,#00,#03,#02,#FE,#DA,#00,#00,#7F,#7D ; #3fb0 ....wU.........}
db #FF,#25,#F0,#F0,#FF,#82,#FF,#02,#F8,#08,#7F,#7D,#FF,#05,#F0,#F0 ; #3fc0 .%.........}....
db #03,#02,#FE,#AA,#00,#00,#07,#05,#77,#55,#00,#00,#0E,#0A,#73,#52 ; #3fd0 ........wU....sR
db #80,#80,#1C,#14,#F9,#D9,#C0,#40,#1B,#1B,#F6,#56,#C0,#C0,#07,#04 ; #3fe0 .......@...V....
db #FF,#F9,#00,#00,#07,#04,#FF,#89,#00,#00,#03,#02,#FE,#8A,#00,#00 ; #3ff0 
db #01,#01,#FC,#8C,#00,#00,#00,#00,#70,#70,#00,#00,#0C,#0C,#10,#10 ; #4000 ........pp......
db #60,#60,#0E,#0A,#38,#28,#E0,#A0,#07,#05,#39,#29,#C0,#40,#03,#02 ; #4010 ``..8(....9).@..
db #BB,#AA,#80,#80,#01,#01,#FF,#6D,#00,#00,#3F,#3E,#FF,#92,#F8,#F8 ; #4020 .......m..?>....
db #7F,#41,#FF,#01,#FC,#04,#3F,#3E,#FF,#82,#F8,#F8,#01,#01,#FF,#55 ; #4030 .A....?>.......U
db #00,#00,#03,#02,#BB,#AA,#80,#80,#07,#05,#39,#29,#C0,#40,#0E,#0A ; #4040 ..........9).@..
db #7C,#6C,#E0,#A0,#0D,#0D,#FB,#AB,#60,#60,#03,#02,#FF,#7C,#80,#80 ; #4050 |l......``...|..
db #03,#02,#FF,#44,#80,#80,#01,#01,#FF,#45,#00,#00,#00,#00,#FE,#C6 ; #4060 ...D.....E......
db #00,#00,#00,#00,#38,#38,#00,#00,#06,#06,#08,#08,#30,#30,#07,#05 ; #4070 ....88......00..
db #1C,#14,#70,#50,#03,#02,#9C,#94,#E0,#A0,#01,#01,#DD,#55,#C0,#40 ; #4080 ..pP.........U.@
db #00,#00,#FF,#B6,#80,#80,#1F,#1F,#FF,#49,#FC,#7C,#3F,#20,#FF,#80 ; #4090 .........I.|? ..
db #FE,#82,#1F,#1F,#FF,#41,#FC,#7C,#00,#00,#FF,#AA,#80,#80,#01,#01 ; #40a0 .....A.|........
db #DD,#55,#C0,#40,#03,#02,#9C,#94,#E0,#A0,#07,#05,#3E,#36,#70,#50 ; #40b0 .U.@........>6pP
db #06,#06,#FD,#D5,#B0,#B0,#01,#01,#FF,#3E,#C0,#40,#01,#01,#FF,#22 ; #40c0 .........>.@..."
db #C0,#40,#00,#00,#FF,#A2,#80,#80,#00,#00,#7F,#63,#00,#00,#00,#00 ; #40d0 .@.........c....
db #1C,#1C,#00,#00,#03,#03,#04,#04,#18,#18,#00,#00,#03,#02,#8E,#8A ; #40e0 
db #38,#28,#00,#00,#01,#01,#CE,#4A,#70,#50,#00,#00,#00,#00,#EE,#AA ; #40f0 8(.....JpP......
db #E0,#A0,#00,#00,#00,#00,#7F,#5B,#C0,#40,#00,#00,#0F,#0F,#FF,#A4 ; #4100 .......[.@......
db #FE,#BE,#00,#00,#1F,#10,#FF,#40,#FF,#41,#00,#00,#0F,#0F,#FF,#A0 ; #4110 .......@.A......
db #FE,#BE,#00,#00,#00,#00,#7F,#55,#C0,#40,#00,#00,#00,#00,#EE,#AA ; #4120 .......U.@......
db #E0,#A0,#00,#00,#01,#01,#CE,#4A,#70,#50,#00,#00,#03,#02,#9F,#9B ; #4130 .......JpP......
db #38,#28,#00,#00,#03,#03,#7E,#6A,#D8,#D8,#00,#00,#00,#00,#FF,#9F ; #4140 8(....~j........
db #E0,#20,#00,#00,#00,#00,#FF,#91,#E0,#20,#00,#00,#00,#00,#7F,#51 ; #4150 . ....... .....Q
db #C0,#40,#00,#00,#00,#00,#3F,#31,#80,#80,#00,#00,#00,#00,#0E,#0E ; #4160 .@....?1........
db #00,#00,#00,#00,#01,#01,#82,#82,#0C,#0C,#00,#00,#01,#01,#C7,#45 ; #4170 ...............E
db #1C,#14,#00,#00,#00,#00,#E7,#A5,#38,#28,#00,#00,#00,#00,#77,#55 ; #4180 ........8(....wU
db #70,#50,#00,#00,#00,#00,#3F,#2D,#E0,#A0,#00,#00,#07,#07,#FF,#D2 ; #4190 pP....?-........
db #FF,#5F,#00,#00,#0F,#08,#FF,#20,#FF,#20,#80,#80,#07,#07,#FF,#D0 ; #41a0 ._..... . ......
db #FF,#5F,#00,#00,#00,#00,#3F,#2A,#E0,#A0,#00,#00,#00,#00,#77,#55 ; #41b0 ._....?*......wU
db #70,#50,#00,#00,#00,#00,#E7,#A5,#38,#28,#00,#00,#01,#01,#CF,#4D ; #41c0 pP......8(.....M
db #9C,#94,#00,#00,#01,#01,#BF,#B5,#6C,#6C,#00,#00,#00,#00,#7F,#4F ; #41d0 ........ll.....O
db #F0,#90,#00,#00,#00,#00,#7F,#48,#F0,#90,#00,#00,#00,#00,#3F,#28 ; #41e0 .......H......?(
db #E0,#A0,#00,#00,#00,#00,#1F,#18,#C0,#C0,#00,#00,#00,#00,#07,#07 ; #41f0 
db #00,#00,#00,#00,#00,#00,#C1,#C1,#06,#06,#00,#00,#00,#00,#E3,#A2 ; #4200 
db #8E,#8A,#00,#00,#00,#00,#73,#52,#9C,#94,#00,#00,#00,#00,#3B,#2A ; #4210 ......sR......;*
db #B8,#A8,#00,#00,#00,#00,#1F,#16,#F0,#D0,#00,#00,#03,#03,#FF,#E9 ; #4220 
db #FF,#2F,#80,#80,#07,#04,#FF,#10,#FF,#10,#C0,#40,#03,#03,#FF,#E8 ; #4230 ./.........@....
db #FF,#2F,#80,#80,#00,#00,#1F,#15,#F0,#50,#00,#00,#00,#00,#3B,#2A ; #4240 ./.......P....;*
db #B8,#A8,#00,#00,#00,#00,#73,#52,#9C,#94,#00,#00,#00,#00,#E7,#A6 ; #4250 ......sR........
db #CE,#CA,#00,#00,#00,#00,#DF,#DA,#B6,#B6,#00,#00,#00,#00,#3F,#27 ; #4260 ..............?'
db #F8,#C8,#00,#00,#00,#00,#3F,#24,#F8,#48,#00,#00,#00,#00,#1F,#14 ; #4270 ......?$.H......
db #F0,#50,#00,#00,#00,#00,#0F,#0C,#E0,#60,#00,#00,#00,#00,#03,#03 ; #4280 .P.......`......
db #80,#80,#00,#00,#00,#00,#60,#60,#83,#83,#00,#00,#00,#00,#71,#51 ; #4290 ......``......qQ
db #C7,#45,#00,#00,#00,#00,#39,#29,#CE,#4A,#00,#00,#00,#00,#1D,#15 ; #42a0 .E....9).J......
db #DC,#54,#00,#00,#00,#00,#0F,#0B,#F8,#68,#00,#00,#01,#01,#FF,#F4 ; #42b0 .T.......h......
db #FF,#97,#C0,#C0,#03,#02,#FF,#08,#FF,#08,#E0,#20,#01,#01,#FF,#F4 ; #42c0 ........... ....
db #FF,#17,#C0,#C0,#00,#00,#0F,#0A,#F8,#A8,#00,#00,#00,#00,#1D,#15 ; #42d0 
db #DC,#54,#00,#00,#00,#00,#39,#29,#CE,#4A,#00,#00,#00,#00,#73,#53 ; #42e0 .T....9).J....sS
db #E7,#65,#00,#00,#00,#00,#6F,#6D,#DB,#5B,#00,#00,#00,#00,#1F,#13 ; #42f0 .e....om.[......
db #FC,#E4,#00,#00,#00,#00,#1F,#12,#FC,#24,#00,#00,#00,#00,#0F,#0A ; #4300 .........$......
db #F8,#28,#00,#00,#00,#00,#07,#06,#F0,#30,#00,#00,#00,#00,#01,#01 ; #4310 .(.......0......
db #C0,#C0,#00,#00,#00,#00,#30,#30,#41,#41,#80,#80,#00,#00,#38,#28 ; #4320 ......00AA....8(
db #E3,#A2,#80,#80,#00,#00,#1C,#14,#E7,#A5,#00,#00,#00,#00,#0E,#0A ; #4330 
db #EE,#AA,#00,#00,#00,#00,#07,#05,#FC,#B4,#00,#00,#00,#00,#FF,#FA ; #4340 
db #FF,#4B,#E0,#E0,#01,#01,#FF,#04,#FF,#04,#F0,#10,#00,#00,#FF,#FA ; #4350 .K..............
db #FF,#0B,#E0,#E0,#00,#00,#07,#05,#FC,#54,#00,#00,#00,#00,#0E,#0A ; #4360 .........T......
db #EE,#AA,#00,#00,#00,#00,#1C,#14,#E7,#A5,#00,#00,#00,#00,#39,#29 ; #4370 ..............9)
db #F3,#B2,#80,#80,#00,#00,#37,#36,#ED,#AD,#80,#80,#00,#00,#0F,#09 ; #4380 ......76........
db #FE,#F2,#00,#00,#00,#00,#0F,#09,#FE,#12,#00,#00,#00,#00,#07,#05 ; #4390 
db #FC,#14,#00,#00,#00,#00,#03,#03,#F8,#18,#00,#00,#00,#00,#00,#00 ; #43a0 
db #E0,#E0,#00,#00,#1F,#1F,#C0,#C0,#7F,#67,#F0,#30,#FF,#80,#F8,#08 ; #43b0 .........g.0....
db #7F,#60,#F0,#30,#1F,#1F,#C0,#C0,#0F,#0F,#E0,#E0,#3F,#33,#F8,#98 ; #43c0 .`.0........?3..
db #7F,#40,#FC,#04,#3F,#30,#F8,#18,#0F,#0F,#E0,#E0,#07,#07,#F0,#F0 ; #43d0 .@..?0..........
db #1F,#19,#FC,#CC,#3F,#20,#FE,#02,#1F,#18,#FC,#0C,#07,#07,#F0,#F0 ; #43e0 ....? ..........
db #03,#03,#F8,#F8,#0F,#0C,#FE,#E6,#1F,#10,#FF,#01,#0F,#0C,#FE,#06 ; #43f0 
db #03,#03,#F8,#F8,#01,#01,#FC,#FC,#00,#00,#07,#06,#FF,#73,#00,#00 ; #4400 .............s..
db #0F,#08,#FF,#00,#80,#80,#07,#06,#FF,#03,#00,#00,#01,#01,#FC,#FC ; #4410 
db #00,#00,#00,#00,#FE,#FE,#00,#00,#03,#03,#FF,#39,#80,#80,#07,#04 ; #4420 ...........9....
db #FF,#00,#C0,#40,#03,#03,#FF,#01,#80,#80,#00,#00,#FE,#FE,#00,#00 ; #4430 ...@............
db #00,#00,#7F,#7F,#00,#00,#01,#01,#FF,#9C,#C0,#C0,#03,#02,#FF,#00 ; #4440 
db #E0,#20,#01,#01,#FF,#80,#C0,#C0,#00,#00,#7F,#7F,#00,#00,#00,#00 ; #4450 . ..............
db #3F,#3F,#80,#80,#00,#00,#FF,#CE,#E0,#60,#01,#01,#FF,#00,#F0,#10 ; #4460 ??.......`......
db #00,#00,#FF,#C0,#E0,#60,#00,#00,#3F,#3F,#80,#80,#0F,#0F,#E0,#E0 ; #4470 .....`..??......
db #3F,#33,#F8,#98,#FF,#C7,#FC,#C4,#7F,#40,#FE,#06,#3F,#30,#F8,#18 ; #4480 ?3.......@..?0..
db #0F,#0F,#E0,#E0,#07,#07,#F0,#F0,#1F,#19,#FC,#CC,#7F,#63,#FE,#E2 ; #4490 .............c..
db #3F,#20,#FF,#03,#1F,#18,#FC,#0C,#07,#07,#F0,#F0,#03,#03,#F8,#F8 ; #44a0 ? ..............
db #00,#00,#0F,#0C,#FE,#E6,#00,#00,#3F,#31,#FF,#F1,#00,#00,#1F,#10 ; #44b0 ........?1......
db #FF,#01,#80,#80,#0F,#0C,#FE,#06,#00,#00,#03,#03,#F8,#F8,#00,#00 ; #44c0 
db #01,#01,#FC,#FC,#00,#00,#07,#06,#FF,#73,#00,#00,#1F,#18,#FF,#F8 ; #44d0 .........s......
db #80,#80,#0F,#08,#FF,#00,#C0,#C0,#07,#06,#FF,#03,#00,#00,#01,#01 ; #44e0 
db #FC,#FC,#00,#00,#00,#00,#FE,#FE,#00,#00,#03,#03,#FF,#39,#80,#80 ; #44f0 .............9..
db #0F,#0C,#FF,#7C,#C0,#40,#07,#04,#FF,#00,#E0,#60,#03,#03,#FF,#01 ; #4500 ...|.@.....`....
db #80,#80,#00,#00,#FE,#FE,#00,#00,#00,#00,#7F,#7F,#00,#00,#01,#01 ; #4510 
db #FF,#9C,#C0,#C0,#07,#06,#FF,#3E,#E0,#20,#03,#02,#FF,#00,#F0,#30 ; #4520 .......>. .....0
db #01,#01,#FF,#80,#C0,#C0,#00,#00,#7F,#7F,#00,#00,#00,#00,#3F,#3F ; #4530 ..............??
db #80,#80,#00,#00,#FF,#CE,#E0,#60,#03,#03,#FF,#1F,#F0,#10,#01,#01 ; #4540 .......`........
db #FF,#00,#F8,#18,#00,#00,#FF,#C0,#E0,#60,#00,#00,#3F,#3F,#80,#80 ; #4550 .........`..??..
db #00,#00,#1F,#1F,#C0,#C0,#00,#00,#7F,#67,#F0,#30,#01,#01,#FF,#8F ; #4560 .........g.0....
db #F8,#88,#00,#00,#FF,#80,#FC,#0C,#00,#00,#7F,#60,#F0,#30,#00,#00 ; #4570 ...........`.0..
db #1F,#1F,#C0,#C0,#03,#03,#F0,#F0,#00,#00,#1F,#1C,#FE,#0E,#00,#00 ; #4580 
db #7F,#63,#FF,#F1,#80,#80,#FF,#C7,#FF,#F8,#C0,#C0,#FF,#A1,#FF,#E1 ; #4590 .c..............
db #C0,#40,#7F,#60,#FF,#01,#80,#80,#1F,#1C,#FE,#0E,#00,#00,#03,#03 ; #45a0 .@.`............
db #F0,#F0,#00,#00,#01,#01,#F8,#F8,#00,#00,#0F,#0E,#FF,#07,#00,#00 ; #45b0 
db #3F,#31,#FF,#F8,#C0,#C0,#7F,#63,#FF,#FC,#E0,#60,#7F,#50,#FF,#F0 ; #45c0 ?1.....c...`.P..
db #E0,#A0,#3F,#30,#FF,#00,#C0,#C0,#0F,#0E,#FF,#07,#00,#00,#01,#01 ; #45d0 ..?0............
db #F8,#F8,#00,#00,#00,#00,#FC,#FC,#00,#00,#07,#07,#FF,#03,#80,#80 ; #45e0 
db #1F,#18,#FF,#FC,#E0,#60,#3F,#31,#FF,#FE,#F0,#30,#3F,#28,#FF,#78 ; #45f0 .....`?1...0?(.x
db #F0,#50,#1F,#18,#FF,#00,#E0,#60,#07,#07,#FF,#03,#80,#80,#00,#00 ; #4600 .P.....`........
db #FC,#FC,#00,#00,#00,#00,#7E,#7E,#00,#00,#03,#03,#FF,#81,#C0,#C0 ; #4610 ......~~........
db #0F,#0C,#FF,#7E,#F0,#30,#1F,#18,#FF,#FF,#F8,#18,#1F,#14,#FF,#3C ; #4620 ...~.0.........<
db #F8,#28,#0F,#0C,#FF,#00,#F0,#30,#03,#03,#FF,#81,#C0,#C0,#00,#00 ; #4630 .(.....0........
db #7E,#7E,#00,#00,#00,#00,#3F,#3F,#00,#00,#01,#01,#FF,#C0,#E0,#E0 ; #4640 ~~....??........
db #07,#06,#FF,#3F,#F8,#18,#0F,#0C,#FF,#7F,#FC,#8C,#0F,#0A,#FF,#1E ; #4650 ...?............
db #FC,#14,#07,#06,#FF,#00,#F8,#18,#01,#01,#FF,#C0,#E0,#E0,#00,#00 ; #4660 
db #3F,#3F,#00,#00,#00,#00,#1F,#1F,#80,#80,#00,#00,#FF,#E0,#F0,#70 ; #4670 ??.............p
db #03,#03,#FF,#1F,#FC,#8C,#07,#06,#FF,#3F,#FE,#C6,#07,#05,#FF,#0F ; #4680 .........?......
db #FE,#0A,#03,#03,#FF,#00,#FC,#0C,#00,#00,#FF,#E0,#F0,#70,#00,#00 ; #4690 .............p..
db #1F,#1F,#80,#80,#00,#00,#0F,#0F,#C0,#C0,#00,#00,#7F,#70,#F8,#38 ; #46a0 .............p.8
db #01,#01,#FF,#8F,#FE,#C6,#03,#03,#FF,#1F,#FF,#E3,#03,#02,#FF,#87 ; #46b0 
db #FF,#85,#01,#01,#FF,#80,#FE,#06,#00,#00,#7F,#70,#F8,#38,#00,#00 ; #46c0 ...........p.8..
db #0F,#0F,#C0,#C0,#00,#00,#07,#07,#E0,#E0,#00,#00,#00,#00,#3F,#38 ; #46d0 ..............?8
db #FC,#1C,#00,#00,#00,#00,#FF,#C7,#FF,#E3,#00,#00,#01,#01,#FF,#8F ; #46e0 
db #FF,#F1,#80,#80,#01,#01,#FF,#43,#FF,#C2,#80,#80,#00,#00,#FF,#C0 ; #46f0 .......C........
db #FF,#03,#00,#00,#00,#00,#3F,#38,#FC,#1C,#00,#00,#00,#00,#07,#07 ; #4700 ......?8........
db #E0,#E0,#00,#00,#01,#01,#FE,#FE,#00,#00,#0F,#0E,#FF,#01,#C0,#C0 ; #4710 
db #3F,#30,#FF,#00,#F0,#30,#7F,#40,#FF,#FC,#F8,#08,#FF,#A3,#FF,#FF ; #4720 ?0...0.@........
db #FC,#14,#FF,#C1,#FF,#FE,#FC,#0C,#FF,#A0,#FF,#78,#FC,#14,#7F,#40 ; #4730 ...........x...@
db #FF,#00,#F8,#08,#3F,#30,#FF,#00,#F0,#30,#0F,#0E,#FF,#01,#C0,#C0 ; #4740 ....?0...0......
db #01,#01,#FE,#FE,#00,#00,#00,#00,#FF,#FF,#00,#00,#07,#07,#FF,#00 ; #4750 
db #E0,#E0,#1F,#18,#FF,#00,#F8,#18,#3F,#20,#FF,#7E,#FC,#04,#7F,#51 ; #4760 ........? .~...Q
db #FF,#FF,#FE,#8A,#7F,#60,#FF,#FF,#FE,#06,#7F,#50,#FF,#3C,#FE,#0A ; #4770 .....`.....P.<..
db #3F,#20,#FF,#00,#FC,#04,#1F,#18,#FF,#00,#F8,#18,#07,#07,#FF,#00 ; #4780 ? ..............
db #E0,#E0,#00,#00,#FF,#FF,#00,#00,#00,#00,#7F,#7F,#80,#80,#03,#03 ; #4790 
db #FF,#80,#F0,#70,#0F,#0C,#FF,#00,#FC,#0C,#1F,#10,#FF,#3F,#FE,#02 ; #47a0 ...p.........?..
db #3F,#28,#FF,#FF,#FF,#C5,#3F,#30,#FF,#7F,#FF,#83,#3F,#28,#FF,#1E ; #47b0 ?(....?0....?(..
db #FF,#05,#1F,#10,#FF,#00,#FE,#02,#0F,#0C,#FF,#00,#FC,#0C,#03,#03 ; #47c0 
db #FF,#80,#F0,#70,#00,#00,#7F,#7F,#80,#80,#00,#00,#3F,#3F,#C0,#C0 ; #47d0 ...p........??..
db #00,#00,#01,#01,#FF,#C0,#F8,#38,#00,#00,#07,#06,#FF,#00,#FE,#06 ; #47e0 .......8........
db #00,#00,#0F,#08,#FF,#1F,#FF,#81,#00,#00,#1F,#14,#FF,#7F,#FF,#E2 ; #47f0 
db #80,#80,#1F,#18,#FF,#3F,#FF,#C1,#80,#80,#1F,#14,#FF,#0F,#FF,#02 ; #4800 .....?..........
db #80,#80,#0F,#08,#FF,#00,#FF,#01,#00,#00,#07,#06,#FF,#00,#FE,#06 ; #4810 
db #00,#00,#01,#01,#FF,#C0,#F8,#38,#00,#00,#00,#00,#3F,#3F,#C0,#C0 ; #4820 .......8....??..
db #00,#00,#00,#00,#1F,#1F,#E0,#E0,#00,#00,#00,#00,#FF,#E0,#FC,#1C ; #4830 
db #00,#00,#03,#03,#FF,#00,#FF,#03,#00,#00,#07,#04,#FF,#0F,#FF,#C0 ; #4840 
db #80,#80,#0F,#0A,#FF,#3F,#FF,#F1,#C0,#40,#0F,#0C,#FF,#1F,#FF,#E0 ; #4850 .....?...@......
db #C0,#C0,#0F,#0A,#FF,#07,#FF,#81,#C0,#40,#07,#04,#FF,#00,#FF,#00 ; #4860 .........@......
db #80,#80,#03,#03,#FF,#00,#FF,#03,#00,#00,#00,#00,#FF,#E0,#FC,#1C ; #4870 
db #00,#00,#00,#00,#1F,#1F,#E0,#E0,#00,#00,#00,#00,#0F,#0F,#F0,#F0 ; #4880 
db #00,#00,#00,#00,#7F,#70,#FE,#0E,#00,#00,#01,#01,#FF,#80,#FF,#01 ; #4890 .....p..........
db #80,#80,#03,#02,#FF,#07,#FF,#E0,#C0,#40,#07,#05,#FF,#1F,#FF,#F8 ; #48a0 .........@......
db #E0,#A0,#07,#06,#FF,#0F,#FF,#F0,#E0,#60,#07,#05,#FF,#03,#FF,#C0 ; #48b0 .........`......
db #E0,#A0,#03,#02,#FF,#00,#FF,#00,#C0,#40,#01,#01,#FF,#80,#FF,#01 ; #48c0 .........@......
db #80,#80,#00,#00,#7F,#70,#FE,#0E,#00,#00,#00,#00,#0F,#0F,#F0,#F0 ; #48d0 .....p..........
db #00,#00,#00,#00,#07,#07,#F8,#F8,#00,#00,#00,#00,#3F,#38,#FF,#07 ; #48e0 ............?8..
db #00,#00,#00,#00,#FF,#C0,#FF,#00,#C0,#C0,#01,#01,#FF,#03,#FF,#F0 ; #48f0 
db #E0,#20,#03,#02,#FF,#8F,#FF,#FC,#F0,#50,#03,#03,#FF,#07,#FF,#F8 ; #4900 . .......P......
db #F0,#30,#03,#02,#FF,#81,#FF,#E0,#F0,#50,#01,#01,#FF,#00,#FF,#00 ; #4910 .0.......P......
db #E0,#20,#00,#00,#FF,#C0,#FF,#00,#C0,#C0,#00,#00,#3F,#38,#FF,#07 ; #4920 . ..........?8..
db #00,#00,#00,#00,#07,#07,#F8,#F8,#00,#00,#00,#00,#03,#03,#FC,#FC ; #4930 
db #00,#00,#00,#00,#1F,#1C,#FF,#03,#80,#80,#00,#00,#7F,#60,#FF,#00 ; #4940 .............`..
db #E0,#60,#00,#00,#FF,#81,#FF,#F8,#F0,#10,#01,#01,#FF,#47,#FF,#FE ; #4950 .`...........G..
db #F8,#28,#01,#01,#FF,#83,#FF,#FC,#F8,#18,#01,#01,#FF,#40,#FF,#F0 ; #4960 .(...........@..
db #F8,#28,#00,#00,#FF,#80,#FF,#00,#F0,#10,#00,#00,#7F,#60,#FF,#00 ; #4970 .(...........`..
db #E0,#60,#00,#00,#1F,#1C,#FF,#03,#80,#80,#00,#00,#03,#03,#FC,#FC ; #4980 .`..............
db #00,#00,#00,#00,#60,#60,#01,#01,#E0,#A0,#07,#06,#C0,#40,#1F,#19 ; #4990 ....``.......@..
db #C0,#40,#7F,#60,#80,#80,#7F,#42,#80,#80,#3F,#21,#00,#00,#3F,#22 ; #49a0 .@.`...B..?!..?"
db #80,#80,#1F,#11,#C0,#40,#3F,#22,#80,#80,#7F,#45,#00,#00,#FE,#FE ; #49b0 .....@?"...E....
db #00,#00,#00,#00,#30,#30,#00,#00,#F0,#D0,#03,#03,#E0,#20,#0F,#0C ; #49c0 ....00....... ..
db #E0,#A0,#3F,#30,#C0,#40,#3F,#21,#C0,#40,#1F,#10,#80,#80,#1F,#11 ; #49d0 ..?0.@?!.@......
db #C0,#40,#0F,#08,#E0,#A0,#1F,#11,#C0,#40,#3F,#22,#80,#80,#7F,#7F ; #49e0 .@.......@?"....
db #00,#00,#00,#00,#18,#18,#00,#00,#78,#68,#01,#01,#F0,#90,#07,#06 ; #49f0 ........xh......
db #F0,#50,#1F,#18,#E0,#20,#1F,#10,#E0,#A0,#0F,#08,#C0,#40,#0F,#08 ; #4a00 .P... .......@..
db #E0,#A0,#07,#04,#F0,#50,#0F,#08,#E0,#A0,#1F,#11,#C0,#40,#3F,#3F ; #4a10 .....P.......@??
db #80,#80,#00,#00,#0C,#0C,#00,#00,#3C,#34,#00,#00,#F8,#C8,#03,#03 ; #4a20 ........<4......
db #F8,#28,#0F,#0C,#F0,#10,#0F,#08,#F0,#50,#07,#04,#E0,#20,#07,#04 ; #4a30 .(.......P... ..
db #F0,#50,#03,#02,#F8,#28,#07,#04,#F0,#50,#0F,#08,#E0,#A0,#1F,#1F ; #4a40 .P...(...P......
db #C0,#C0,#00,#00,#06,#06,#00,#00,#1E,#1A,#00,#00,#7C,#64,#01,#01 ; #4a50 ............|d..
db #FC,#94,#07,#06,#F8,#08,#07,#04,#F8,#28,#03,#02,#F0,#10,#03,#02 ; #4a60 .........(......
db #F8,#28,#01,#01,#FC,#14,#03,#02,#F8,#28,#07,#04,#F0,#50,#0F,#0F ; #4a70 .(.......(...P..
db #E0,#E0,#00,#00,#03,#03,#00,#00,#0F,#0D,#00,#00,#3E,#32,#00,#00 ; #4a80 ............>2..
db #FE,#CA,#03,#03,#FC,#04,#03,#02,#FC,#14,#01,#01,#F8,#08,#01,#01 ; #4a90 
db #FC,#14,#00,#00,#FE,#8A,#01,#01,#FC,#14,#03,#02,#F8,#28,#07,#07 ; #4aa0 .............(..
db #F0,#F0,#00,#00,#01,#01,#80,#80,#00,#00,#07,#06,#80,#80,#00,#00 ; #4ab0 
db #1F,#19,#00,#00,#00,#00,#7F,#65,#00,#00,#01,#01,#FE,#82,#00,#00 ; #4ac0 .......e........
db #01,#01,#FE,#0A,#00,#00,#00,#00,#FC,#84,#00,#00,#00,#00,#FE,#8A ; #4ad0 
db #00,#00,#00,#00,#7F,#45,#00,#00,#00,#00,#FE,#8A,#00,#00,#01,#01 ; #4ae0 .....E..........
db #FC,#14,#00,#00,#03,#03,#F8,#F8,#00,#00,#00,#00,#00,#00,#C0,#C0 ; #4af0 
db #00,#00,#03,#03,#C0,#40,#00,#00,#0F,#0C,#80,#80,#00,#00,#3F,#32 ; #4b00 .....@........?2
db #80,#80,#00,#00,#FF,#C1,#00,#00,#00,#00,#FF,#85,#00,#00,#00,#00 ; #4b10 
db #7E,#42,#00,#00,#00,#00,#7F,#45,#00,#00,#00,#00,#3F,#22,#80,#80 ; #4b20 ~B.....E....?"..
db #00,#00,#7F,#45,#00,#00,#00,#00,#FE,#8A,#00,#00,#01,#01,#FC,#FC ; #4b30 ...E............
db #00,#00,#C0,#C0,#00,#00,#F0,#B0,#00,#00,#7C,#4C,#00,#00,#7F,#53 ; #4b40 ..........|L...S
db #00,#00,#3F,#20,#C0,#C0,#3F,#28,#C0,#40,#1F,#10,#80,#80,#3F,#28 ; #4b50 ..? ..?(.@....?(
db #80,#80,#7F,#51,#00,#00,#3F,#28,#80,#80,#1F,#14,#C0,#40,#0F,#0F ; #4b60 ...Q..?(.....@..
db #E0,#E0,#60,#60,#00,#00,#78,#58,#00,#00,#3E,#26,#00,#00,#3F,#29 ; #4b70 ..``..xX..>&..?)
db #80,#80,#1F,#10,#E0,#60,#1F,#14,#E0,#20,#0F,#08,#C0,#40,#1F,#14 ; #4b80 .....`... ...@..
db #C0,#40,#3F,#28,#80,#80,#1F,#14,#C0,#40,#0F,#0A,#E0,#20,#07,#07 ; #4b90 .@?(.....@... ..
db #F0,#F0,#30,#30,#00,#00,#3C,#2C,#00,#00,#1F,#13,#00,#00,#1F,#14 ; #4ba0 ..00..<,........
db #C0,#C0,#0F,#08,#F0,#30,#0F,#0A,#F0,#10,#07,#04,#E0,#20,#0F,#0A ; #4bb0 .....0....... ..
db #E0,#20,#1F,#14,#C0,#40,#0F,#0A,#E0,#20,#07,#05,#F0,#10,#03,#03 ; #4bc0 . ...@... ......
db #F8,#F8,#18,#18,#00,#00,#1E,#16,#00,#00,#0F,#09,#80,#80,#0F,#0A ; #4bd0 
db #E0,#60,#07,#04,#F8,#18,#07,#05,#F8,#08,#03,#02,#F0,#10,#07,#05 ; #4be0 .`..............
db #F0,#10,#0F,#0A,#E0,#20,#07,#05,#F0,#10,#03,#02,#F8,#88,#01,#01 ; #4bf0 ..... ..........
db #FC,#FC,#0C,#0C,#00,#00,#0F,#0B,#00,#00,#07,#04,#C0,#C0,#07,#05 ; #4c00 
db #F0,#30,#03,#02,#FC,#0C,#03,#02,#FC,#84,#01,#01,#F8,#08,#03,#02 ; #4c10 .0..............
db #F8,#88,#07,#05,#F0,#10,#03,#02,#F8,#88,#01,#01,#FC,#44,#00,#00 ; #4c20 .............D..
db #FE,#FE,#06,#06,#00,#00,#07,#05,#80,#80,#03,#02,#E0,#60,#03,#02 ; #4c30 .............`..
db #F8,#98,#01,#01,#FE,#06,#01,#01,#FE,#42,#00,#00,#FC,#84,#01,#01 ; #4c40 .........B......
db #FC,#44,#03,#02,#F8,#88,#01,#01,#FC,#44,#00,#00,#FE,#A2,#00,#00 ; #4c50 .D.......D......
db #7F,#7F,#03,#03,#00,#00,#00,#00,#03,#02,#C0,#C0,#00,#00,#01,#01 ; #4c60 
db #F0,#30,#00,#00,#01,#01,#FC,#4C,#00,#00,#00,#00,#FF,#83,#00,#00 ; #4c70 .0.....L........
db #00,#00,#FF,#A1,#00,#00,#00,#00,#7E,#42,#00,#00,#00,#00,#FE,#A2 ; #4c80 ........~B......
db #00,#00,#01,#01,#FC,#44,#00,#00,#00,#00,#FE,#A2,#00,#00,#00,#00 ; #4c90 .....D..........
db #7F,#51,#00,#00,#00,#00,#3F,#3F,#80,#80,#01,#01,#80,#80,#00,#00 ; #4ca0 .Q....??........
db #01,#01,#E0,#60,#00,#00,#00,#00,#F8,#98,#00,#00,#00,#00,#FE,#A6 ; #4cb0 ...`............
db #00,#00,#00,#00,#7F,#41,#80,#80,#00,#00,#7F,#50,#80,#80,#00,#00 ; #4cc0 .....A.....P....
db #3F,#21,#00,#00,#00,#00,#7F,#51,#00,#00,#00,#00,#FE,#A2,#00,#00 ; #4cd0 ?!.....Q........
db #00,#00,#7F,#51,#00,#00,#00,#00,#3F,#28,#80,#80,#00,#00,#1F,#1F ; #4ce0 ...Q....?(......
db #C0,#C0,#00,#00,#E0,#E0,#03,#03,#C0,#40,#0F,#0D,#C0,#40,#3F,#32 ; #4cf0 .........@...@?2
db #80,#80,#FF,#C1,#00,#00,#FF,#85,#00,#00,#7F,#42,#80,#80,#3F,#21 ; #4d00 ...........B..?!
db #C0,#40,#1F,#10,#E0,#A0,#0F,#08,#F0,#50,#0F,#08,#F0,#50,#1F,#10 ; #4d10 .@.......P...P..
db #E0,#A0,#3F,#21,#C0,#40,#7F,#42,#80,#80,#FF,#FF,#00,#00,#00,#00 ; #4d20 ..?!.@.B........
db #70,#70,#01,#01,#E0,#A0,#07,#06,#E0,#A0,#1F,#19,#C0,#40,#7F,#60 ; #4d30 pp...........@.`
db #80,#80,#7F,#42,#80,#80,#3F,#21,#C0,#40,#1F,#10,#E0,#A0,#0F,#08 ; #4d40 ...B..?!.@......
db #F0,#50,#07,#04,#F8,#28,#07,#04,#F8,#28,#0F,#08,#F0,#50,#1F,#10 ; #4d50 .P...(...(...P..
db #E0,#A0,#3F,#21,#C0,#40,#7F,#7F,#80,#80,#00,#00,#38,#38,#00,#00 ; #4d60 ..?!.@......88..
db #F0,#D0,#03,#03,#F0,#50,#0F,#0C,#E0,#A0,#3F,#30,#C0,#40,#3F,#21 ; #4d70 .....P....?0.@?!
db #C0,#40,#1F,#10,#E0,#A0,#0F,#08,#F0,#50,#07,#04,#F8,#28,#03,#02 ; #4d80 .@.......P...(..
db #FC,#14,#03,#02,#FC,#14,#07,#04,#F8,#28,#0F,#08,#F0,#50,#1F,#10 ; #4d90 .........(...P..
db #E0,#A0,#3F,#3F,#C0,#C0,#00,#00,#1C,#1C,#00,#00,#78,#68,#01,#01 ; #4da0 ..??........xh..
db #F8,#A8,#07,#06,#F0,#50,#1F,#18,#E0,#20,#1F,#10,#E0,#A0,#0F,#08 ; #4db0 .....P... ......
db #F0,#50,#07,#04,#F8,#28,#03,#02,#FC,#14,#01,#01,#FE,#0A,#01,#01 ; #4dc0 .P...(..........
db #FE,#0A,#03,#02,#FC,#14,#07,#04,#F8,#28,#0F,#08,#F0,#50,#1F,#1F ; #4dd0 .........(...P..
db #E0,#E0,#00,#00,#0E,#0E,#00,#00,#3C,#34,#00,#00,#FC,#D4,#03,#03 ; #4de0 ........<4......
db #F8,#28,#0F,#0C,#F0,#10,#0F,#08,#F0,#50,#07,#04,#F8,#28,#03,#02 ; #4df0 .(.......P...(..
db #FC,#14,#01,#01,#FE,#0A,#00,#00,#FF,#85,#00,#00,#FF,#85,#01,#01 ; #4e00 
db #FE,#0A,#03,#02,#FC,#14,#07,#04,#F8,#28,#0F,#0F,#F0,#F0,#00,#00 ; #4e10 .........(......
db #07,#07,#00,#00,#00,#00,#1E,#1A,#00,#00,#00,#00,#7E,#6A,#00,#00 ; #4e20 ............~j..
db #01,#01,#FC,#94,#00,#00,#07,#06,#F8,#08,#00,#00,#07,#04,#F8,#28 ; #4e30 ...............(
db #00,#00,#03,#02,#FC,#14,#00,#00,#01,#01,#FE,#0A,#00,#00,#00,#00 ; #4e40 
db #FF,#85,#00,#00,#00,#00,#7F,#42,#80,#80,#00,#00,#7F,#42,#80,#80 ; #4e50 .......B.....B..
db #00,#00,#FF,#85,#00,#00,#01,#01,#FE,#0A,#00,#00,#03,#02,#FC,#14 ; #4e60 
db #00,#00,#07,#07,#F8,#F8,#00,#00,#00,#00,#03,#03,#80,#80,#00,#00 ; #4e70 
db #0F,#0D,#00,#00,#00,#00,#3F,#35,#00,#00,#00,#00,#FE,#CA,#00,#00 ; #4e80 ......?5........
db #03,#03,#FC,#04,#00,#00,#03,#02,#FC,#14,#00,#00,#01,#01,#FE,#0A ; #4e90 
db #00,#00,#00,#00,#FF,#85,#00,#00,#00,#00,#7F,#42,#80,#80,#00,#00 ; #4ea0 ...........B....
db #3F,#21,#C0,#40,#00,#00,#3F,#21,#C0,#40,#00,#00,#7F,#42,#80,#80 ; #4eb0 ?!.@..?!.@...B..
db #00,#00,#FF,#85,#00,#00,#01,#01,#FE,#0A,#00,#00,#03,#03,#FC,#FC ; #4ec0 
db #00,#00,#00,#00,#01,#01,#C0,#C0,#00,#00,#07,#06,#80,#80,#00,#00 ; #4ed0 
db #1F,#1A,#80,#80,#00,#00,#7F,#65,#00,#00,#01,#01,#FE,#82,#00,#00 ; #4ee0 .......e........
db #01,#01,#FE,#0A,#00,#00,#00,#00,#FF,#85,#00,#00,#00,#00,#7F,#42 ; #4ef0 ...............B
db #80,#80,#00,#00,#3F,#21,#C0,#40,#00,#00,#1F,#10,#E0,#A0,#00,#00 ; #4f00 ....?!.@........
db #1F,#10,#E0,#A0,#00,#00,#3F,#21,#C0,#40,#00,#00,#7F,#42,#80,#80 ; #4f10 ......?!.@...B..
db #00,#00,#FF,#85,#00,#00,#01,#01,#FE,#FE,#00,#00,#70,#70,#00,#00 ; #4f20 ............pp..
db #3C,#2C,#00,#00,#3F,#23,#00,#00,#1F,#14,#C0,#C0,#0F,#0B,#F0,#30 ; #4f30 <,..?#.........0
db #0F,#08,#F0,#50,#1F,#10,#E0,#A0,#3F,#21,#C0,#40,#7F,#42,#80,#80 ; #4f40 ...P....?!.@.B..
db #FF,#85,#00,#00,#FF,#85,#00,#00,#7F,#42,#80,#80,#3F,#21,#C0,#40 ; #4f50 .........B..?!.@
db #1F,#10,#E0,#A0,#0F,#0F,#F0,#F0,#38,#38,#00,#00,#1E,#16,#00,#00 ; #4f60 ........88......
db #1F,#11,#80,#80,#0F,#0A,#E0,#60,#07,#05,#F8,#98,#07,#04,#F8,#28 ; #4f70 .......`.......(
db #0F,#08,#F0,#50,#1F,#10,#E0,#A0,#3F,#21,#C0,#40,#7F,#42,#80,#80 ; #4f80 ...P....?!.@.B..
db #7F,#42,#80,#80,#3F,#21,#C0,#40,#1F,#10,#E0,#A0,#0F,#08,#F0,#50 ; #4f90 .B..?!.@.......P
db #07,#07,#F8,#F8,#1C,#1C,#00,#00,#0F,#0B,#00,#00,#0F,#08,#C0,#C0 ; #4fa0 
db #07,#05,#F0,#30,#03,#02,#FC,#CC,#03,#02,#FC,#14,#07,#04,#F8,#28 ; #4fb0 ...0...........(
db #0F,#08,#F0,#50,#1F,#10,#E0,#A0,#3F,#21,#C0,#40,#3F,#21,#C0,#40 ; #4fc0 ...P....?!.@?!.@
db #1F,#10,#E0,#A0,#0F,#08,#F0,#50,#07,#04,#F8,#28,#03,#03,#FC,#FC ; #4fd0 .......P...(....
db #0E,#0E,#00,#00,#07,#05,#80,#80,#07,#04,#E0,#60,#03,#02,#F8,#98 ; #4fe0 ...........`....
db #01,#01,#FE,#66,#01,#01,#FE,#0A,#03,#02,#FC,#14,#07,#04,#F8,#28 ; #4ff0 ...f...........(
db #0F,#08,#F0,#50,#1F,#10,#E0,#A0,#1F,#10,#E0,#A0,#0F,#08,#F0,#50 ; #5000 ...P...........P
db #07,#04,#F8,#28,#03,#02,#FC,#14,#01,#01,#FE,#FE,#07,#07,#00,#00 ; #5010 ...(............
db #03,#02,#C0,#C0,#03,#02,#F0,#30,#01,#01,#FC,#4C,#00,#00,#FF,#B3 ; #5020 .......0...L....
db #00,#00,#FF,#85,#01,#01,#FE,#0A,#03,#02,#FC,#14,#07,#04,#F8,#28 ; #5030 ...............(
db #0F,#08,#F0,#50,#0F,#08,#F0,#50,#07,#04,#F8,#28,#03,#02,#FC,#14 ; #5040 ...P...P...(....
db #01,#01,#FE,#0A,#00,#00,#FF,#FF,#03,#03,#80,#80,#00,#00,#01,#01 ; #5050 
db #E0,#60,#00,#00,#01,#01,#F8,#18,#00,#00,#00,#00,#FE,#A6,#00,#00 ; #5060 .`..............
db #00,#00,#7F,#59,#80,#80,#00,#00,#7F,#42,#80,#80,#00,#00,#FF,#85 ; #5070 ...Y.....B......
db #00,#00,#01,#01,#FE,#0A,#00,#00,#03,#02,#FC,#14,#00,#00,#07,#04 ; #5080 
db #F8,#28,#00,#00,#07,#04,#F8,#28,#00,#00,#03,#02,#FC,#14,#00,#00 ; #5090 .(.....(........
db #01,#01,#FE,#0A,#00,#00,#00,#00,#FF,#85,#00,#00,#00,#00,#7F,#7F ; #50a0 
db #80,#80,#01,#01,#C0,#C0,#00,#00,#00,#00,#F0,#B0,#00,#00,#00,#00 ; #50b0 
db #FC,#8C,#00,#00,#00,#00,#7F,#53,#00,#00,#00,#00,#3F,#2C,#C0,#C0 ; #50c0 .......S....?,..
db #00,#00,#3F,#21,#C0,#40,#00,#00,#7F,#42,#80,#80,#00,#00,#FF,#85 ; #50d0 ..?!.@...B......
db #00,#00,#01,#01,#FE,#0A,#00,#00,#03,#02,#FC,#14,#00,#00,#03,#02 ; #50e0 
db #FC,#14,#00,#00,#01,#01,#FE,#0A,#00,#00,#00,#00,#FF,#85,#00,#00 ; #50f0 
db #00,#00,#7F,#42,#80,#80,#00,#00,#3F,#3F,#C0,#C0,#00,#00,#E0,#E0 ; #5100 ...B....??......
db #00,#00,#00,#00,#78,#58,#00,#00,#00,#00,#7E,#46,#00,#00,#00,#00 ; #5110 ....xX....~F....
db #3F,#29,#80,#80,#00,#00,#1F,#16,#E0,#60,#00,#00,#1F,#10,#E0,#A0 ; #5120 ?).......`......
db #00,#00,#3F,#21,#C0,#40,#00,#00,#7F,#42,#80,#80,#00,#00,#FF,#85 ; #5130 ..?!.@...B......
db #00,#00,#01,#01,#FE,#0A,#00,#00,#01,#01,#FE,#0A,#00,#00,#00,#00 ; #5140 
db #FF,#85,#00,#00,#00,#00,#7F,#42,#80,#80,#00,#00,#3F,#21,#C0,#40 ; #5150 .......B....?!.@
db #00,#00,#1F,#1F,#E0,#E0,#00,#00,#38,#38,#00,#00,#F8,#C8,#03,#03 ; #5160 ........88......
db #F0,#10,#0F,#0C,#F0,#50,#3F,#30,#E0,#A0,#FF,#C1,#C0,#40,#FF,#81 ; #5170 .....P?0.....@..
db #C0,#40,#7F,#40,#E0,#A0,#3F,#20,#F0,#50,#1F,#10,#F8,#28,#0F,#08 ; #5180 .@.@..? .P...(..
db #FC,#14,#07,#04,#FE,#0A,#07,#04,#FE,#0A,#0F,#08,#FC,#14,#1F,#10 ; #5190 
db #F8,#28,#3F,#20,#F0,#50,#7F,#40,#E0,#A0,#7F,#7F,#C0,#C0,#00,#00 ; #51a0 .(? .P.@........
db #1C,#1C,#00,#00,#7C,#64,#01,#01,#F8,#88,#07,#06,#F8,#28,#1F,#18 ; #51b0 ....|d.......(..
db #F0,#50,#7F,#60,#E0,#A0,#7F,#40,#E0,#A0,#3F,#20,#F0,#50,#1F,#10 ; #51c0 .P.`...@..? .P..
db #F8,#28,#0F,#08,#FC,#14,#07,#04,#FE,#0A,#03,#02,#FF,#05,#03,#02 ; #51d0 .(..............
db #FF,#05,#07,#04,#FE,#0A,#0F,#08,#FC,#14,#1F,#10,#F8,#28,#3F,#20 ; #51e0 .............(? 
db #F0,#50,#3F,#3F,#E0,#E0,#00,#00,#0E,#0E,#00,#00,#00,#00,#3E,#32 ; #51f0 .P??..........>2
db #00,#00,#00,#00,#FC,#C4,#00,#00,#03,#03,#FC,#14,#00,#00,#0F,#0C ; #5200 
db #F8,#28,#00,#00,#3F,#30,#F0,#50,#00,#00,#3F,#20,#F0,#50,#00,#00 ; #5210 .(..?0.P..? .P..
db #1F,#10,#F8,#28,#00,#00,#0F,#08,#FC,#14,#00,#00,#07,#04,#FE,#0A ; #5220 ...(............
db #00,#00,#03,#02,#FF,#05,#00,#00,#01,#01,#FF,#02,#80,#80,#01,#01 ; #5230 
db #FF,#02,#80,#80,#03,#02,#FF,#05,#00,#00,#07,#04,#FE,#0A,#00,#00 ; #5240 
db #0F,#08,#FC,#14,#00,#00,#1F,#10,#F8,#28,#00,#00,#1F,#1F,#F0,#F0 ; #5250 .........(......
db #00,#00,#00,#00,#07,#07,#00,#00,#00,#00,#1F,#19,#00,#00,#00,#00 ; #5260 
db #7E,#62,#00,#00,#01,#01,#FE,#8A,#00,#00,#07,#06,#FC,#14,#00,#00 ; #5270 ~b..............
db #1F,#18,#F8,#28,#00,#00,#1F,#10,#F8,#28,#00,#00,#0F,#08,#FC,#14 ; #5280 ...(.....(......
db #00,#00,#07,#04,#FE,#0A,#00,#00,#03,#02,#FF,#05,#00,#00,#01,#01 ; #5290 
db #FF,#02,#80,#80,#00,#00,#FF,#81,#C0,#40,#00,#00,#FF,#81,#C0,#40 ; #52a0 .........@.....@
db #01,#01,#FF,#02,#80,#80,#03,#02,#FF,#05,#00,#00,#07,#04,#FE,#0A ; #52b0 
db #00,#00,#0F,#08,#FC,#14,#00,#00,#0F,#0F,#F8,#F8,#00,#00,#00,#00 ; #52c0 
db #03,#03,#80,#80,#00,#00,#0F,#0C,#80,#80,#00,#00,#3F,#31,#00,#00 ; #52d0 ............?1..
db #00,#00,#FF,#C5,#00,#00,#03,#03,#FE,#0A,#00,#00,#0F,#0C,#FC,#14 ; #52e0 
db #00,#00,#0F,#08,#FC,#14,#00,#00,#07,#04,#FE,#0A,#00,#00,#03,#02 ; #52f0 
db #FF,#05,#00,#00,#01,#01,#FF,#02,#80,#80,#00,#00,#FF,#81,#C0,#40 ; #5300 ...............@
db #00,#00,#7F,#40,#E0,#A0,#00,#00,#7F,#40,#E0,#A0,#00,#00,#FF,#81 ; #5310 ...@.....@......
db #C0,#40,#01,#01,#FF,#02,#80,#80,#03,#02,#FF,#05,#00,#00,#07,#04 ; #5320 .@..............
db #FE,#0A,#00,#00,#07,#07,#FC,#FC,#00,#00,#00,#00,#01,#01,#C0,#C0 ; #5330 
db #00,#00,#07,#06,#C0,#40,#00,#00,#1F,#18,#80,#80,#00,#00,#7F,#62 ; #5340 .....@.........b
db #80,#80,#01,#01,#FF,#85,#00,#00,#07,#06,#FE,#0A,#00,#00,#07,#04 ; #5350 
db #FE,#0A,#00,#00,#03,#02,#FF,#05,#00,#00,#01,#01,#FF,#02,#80,#80 ; #5360 
db #00,#00,#FF,#81,#C0,#40,#00,#00,#7F,#40,#E0,#A0,#00,#00,#3F,#20 ; #5370 .....@...@....? 
db #F0,#50,#00,#00,#3F,#20,#F0,#50,#00,#00,#7F,#40,#E0,#A0,#00,#00 ; #5380 .P..? .P...@....
db #FF,#81,#C0,#40,#01,#01,#FF,#02,#80,#80,#03,#02,#FF,#05,#00,#00 ; #5390 ...@............
db #03,#03,#FE,#FE,#00,#00,#00,#00,#00,#00,#E0,#E0,#00,#00,#03,#03 ; #53a0 
db #E0,#20,#00,#00,#0F,#0C,#C0,#40,#00,#00,#3F,#31,#C0,#40,#00,#00 ; #53b0 . .....@..?1.@..
db #FF,#C2,#80,#80,#03,#03,#FF,#05,#00,#00,#03,#02,#FF,#05,#00,#00 ; #53c0 
db #01,#01,#FF,#02,#80,#80,#00,#00,#FF,#81,#C0,#40,#00,#00,#7F,#40 ; #53d0 ...........@...@
db #E0,#A0,#00,#00,#3F,#20,#F0,#50,#00,#00,#1F,#10,#F8,#28,#00,#00 ; #53e0 ....? .P.....(..
db #1F,#10,#F8,#28,#00,#00,#3F,#20,#F0,#50,#00,#00,#7F,#40,#E0,#A0 ; #53f0 ...(..? .P...@..
db #00,#00,#FF,#81,#C0,#40,#01,#01,#FF,#02,#80,#80,#01,#01,#FF,#FF ; #5400 .....@..........
db #00,#00,#00,#00,#00,#00,#70,#70,#00,#00,#01,#01,#F0,#90,#00,#00 ; #5410 ......pp........
db #07,#06,#E0,#20,#00,#00,#1F,#18,#E0,#A0,#00,#00,#7F,#61,#C0,#40 ; #5420 ... .........a.@
db #01,#01,#FF,#82,#80,#80,#01,#01,#FF,#02,#80,#80,#00,#00,#FF,#81 ; #5430 
db #C0,#40,#00,#00,#7F,#40,#E0,#A0,#00,#00,#3F,#20,#F0,#50,#00,#00 ; #5440 .@...@....? .P..
db #1F,#10,#F8,#28,#00,#00,#0F,#08,#FC,#14,#00,#00,#0F,#08,#FC,#14 ; #5450 ...(............
db #00,#00,#1F,#10,#F8,#28,#00,#00,#3F,#20,#F0,#50,#00,#00,#7F,#40 ; #5460 .....(..? .P...@
db #E0,#A0,#00,#00,#FF,#81,#C0,#40,#00,#00,#FF,#FF,#80,#80,#38,#38 ; #5470 .......@......88
db #00,#00,#3E,#26,#00,#00,#1F,#15,#80,#80,#1F,#13,#E0,#60,#0F,#08 ; #5480 ..>&.........`..
db #F8,#D8,#07,#04,#FE,#36,#07,#04,#FE,#0A,#0F,#08,#FC,#14,#1F,#10 ; #5490 .....6..........
db #F8,#28,#3F,#20,#F0,#50,#7F,#40,#E0,#A0,#FF,#81,#C0,#40,#FF,#81 ; #54a0 .(? .P.@.....@..
db #C0,#40,#7F,#40,#E0,#A0,#3F,#20,#F0,#50,#1F,#10,#F8,#28,#0F,#08 ; #54b0 .@.@..? .P...(..
db #FC,#14,#07,#07,#FC,#FC,#1C,#1C,#00,#00,#1F,#13,#00,#00,#0F,#0A ; #54c0 
db #C0,#C0,#0F,#09,#F0,#B0,#07,#04,#FC,#6C,#03,#02,#FF,#1B,#03,#02 ; #54d0 .........l......
db #FF,#05,#07,#04,#FE,#0A,#0F,#08,#FC,#14,#1F,#10,#F8,#28,#3F,#20 ; #54e0 .............(? 
db #F0,#50,#7F,#40,#E0,#A0,#7F,#40,#E0,#A0,#3F,#20,#F0,#50,#1F,#10 ; #54f0 .P.@...@..? .P..
db #F8,#28,#0F,#08,#FC,#14,#07,#04,#FE,#0A,#03,#03,#FE,#FE,#0E,#0E ; #5500 .(..............
db #00,#00,#00,#00,#0F,#09,#80,#80,#00,#00,#07,#05,#E0,#60,#00,#00 ; #5510 .............`..
db #07,#04,#F8,#D8,#00,#00,#03,#02,#FE,#36,#00,#00,#01,#01,#FF,#0D ; #5520 .........6......
db #80,#80,#01,#01,#FF,#02,#80,#80,#03,#02,#FF,#05,#00,#00,#07,#04 ; #5530 
db #FE,#0A,#00,#00,#0F,#08,#FC,#14,#00,#00,#1F,#10,#F8,#28,#00,#00 ; #5540 .............(..
db #3F,#20,#F0,#50,#00,#00,#3F,#20,#F0,#50,#00,#00,#1F,#10,#F8,#28 ; #5550 ? .P..? .P.....(
db #00,#00,#0F,#08,#FC,#14,#00,#00,#07,#04,#FE,#0A,#00,#00,#03,#02 ; #5560 
db #FF,#05,#00,#00,#01,#01,#FF,#FF,#00,#00,#07,#07,#00,#00,#00,#00 ; #5570 
db #07,#04,#C0,#C0,#00,#00,#03,#02,#F0,#B0,#00,#00,#03,#02,#FC,#6C ; #5580 ...............l
db #00,#00,#01,#01,#FF,#1B,#00,#00,#00,#00,#FF,#86,#C0,#C0,#00,#00 ; #5590 
db #FF,#81,#C0,#40,#01,#01,#FF,#02,#80,#80,#03,#02,#FF,#05,#00,#00 ; #55a0 ...@............
db #07,#04,#FE,#0A,#00,#00,#0F,#08,#FC,#14,#00,#00,#1F,#10,#F8,#28 ; #55b0 ...............(
db #00,#00,#1F,#10,#F8,#28,#00,#00,#0F,#08,#FC,#14,#00,#00,#07,#04 ; #55c0 .....(..........
db #FE,#0A,#00,#00,#03,#02,#FF,#05,#00,#00,#01,#01,#FF,#02,#80,#80 ; #55d0 
db #00,#00,#FF,#FF,#80,#80,#03,#03,#80,#80,#00,#00,#03,#02,#E0,#60 ; #55e0 ...............`
db #00,#00,#01,#01,#F8,#58,#00,#00,#01,#01,#FE,#36,#00,#00,#00,#00 ; #55f0 .....X.....6....
db #FF,#8D,#80,#80,#00,#00,#7F,#43,#E0,#60,#00,#00,#7F,#40,#E0,#A0 ; #5600 .......C.`...@..
db #00,#00,#FF,#81,#C0,#40,#01,#01,#FF,#02,#80,#80,#03,#02,#FF,#05 ; #5610 .....@..........
db #00,#00,#07,#04,#FE,#0A,#00,#00,#0F,#08,#FC,#14,#00,#00,#0F,#08 ; #5620 
db #FC,#14,#00,#00,#07,#04,#FE,#0A,#00,#00,#03,#02,#FF,#05,#00,#00 ; #5630 
db #01,#01,#FF,#02,#80,#80,#00,#00,#FF,#81,#C0,#40,#00,#00,#7F,#7F ; #5640 ...........@....
db #C0,#C0,#01,#01,#C0,#C0,#00,#00,#01,#01,#F0,#30,#00,#00,#00,#00 ; #5650 ...........0....
db #FC,#AC,#00,#00,#00,#00,#FF,#9B,#00,#00,#00,#00,#7F,#46,#C0,#C0 ; #5660 .............F..
db #00,#00,#3F,#21,#F0,#B0,#00,#00,#3F,#20,#F0,#50,#00,#00,#7F,#40 ; #5670 ..?!....? .P...@
db #E0,#A0,#00,#00,#FF,#81,#C0,#40,#01,#01,#FF,#02,#80,#80,#03,#02 ; #5680 .......@........
db #FF,#05,#00,#00,#07,#04,#FE,#0A,#00,#00,#07,#04,#FE,#0A,#00,#00 ; #5690 
db #03,#02,#FF,#05,#00,#00,#01,#01,#FF,#02,#80,#80,#00,#00,#FF,#81 ; #56a0 
db #C0,#40,#00,#00,#7F,#40,#E0,#A0,#00,#00,#3F,#3F,#E0,#E0,#00,#00 ; #56b0 .@...@....??....
db #E0,#E0,#00,#00,#00,#00,#F8,#98,#00,#00,#00,#00,#7E,#56,#00,#00 ; #56c0 ............~V..
db #00,#00,#7F,#4D,#80,#80,#00,#00,#3F,#23,#E0,#60,#00,#00,#1F,#10 ; #56d0 ...M....?#.`....
db #F8,#D8,#00,#00,#1F,#10,#F8,#28,#00,#00,#3F,#20,#F0,#50,#00,#00 ; #56e0 .......(..? .P..
db #7F,#40,#E0,#A0,#00,#00,#FF,#81,#C0,#40,#01,#01,#FF,#02,#80,#80 ; #56f0 .@.......@......
db #03,#02,#FF,#05,#00,#00,#03,#02,#FF,#05,#00,#00,#01,#01,#FF,#02 ; #5700 
db #80,#80,#00,#00,#FF,#81,#C0,#40,#00,#00,#7F,#40,#E0,#A0,#00,#00 ; #5710 .......@...@....
db #3F,#20,#F0,#50,#00,#00,#1F,#1F,#F0,#F0,#00,#00,#70,#70,#00,#00 ; #5720 ? .P........pp..
db #00,#00,#7C,#4C,#00,#00,#00,#00,#3F,#2B,#00,#00,#00,#00,#3F,#26 ; #5730 ..|L....?+....?&
db #C0,#C0,#00,#00,#1F,#11,#F0,#B0,#00,#00,#0F,#08,#FC,#6C,#00,#00 ; #5740 .............l..
db #0F,#08,#FC,#14,#00,#00,#1F,#10,#F8,#28,#00,#00,#3F,#20,#F0,#50 ; #5750 .........(..? .P
db #00,#00,#7F,#40,#E0,#A0,#00,#00,#FF,#81,#C0,#40,#01,#01,#FF,#02 ; #5760 ...@.......@....
db #80,#80,#01,#01,#FF,#02,#80,#80,#00,#00,#FF,#81,#C0,#40,#00,#00 ; #5770 .............@..
db #7F,#40,#E0,#A0,#00,#00,#3F,#20,#F0,#50,#00,#00,#1F,#10,#F8,#28 ; #5780 .@....? .P.....(
db #00,#00,#0F,#0F,#F8,#F8,#00,#00,#18,#18,#00,#00,#00,#00,#78,#68 ; #5790 ..............xh
db #00,#00,#01,#01,#F0,#90,#00,#00,#07,#06,#F0,#50,#00,#00,#1F,#18 ; #57a0 ...........P....
db #E0,#A0,#00,#00,#7F,#60,#E0,#A0,#00,#00,#7F,#40,#C0,#40,#00,#00 ; #57b0 .....`.....@.@..
db #3F,#20,#E0,#A0,#00,#00,#3F,#20,#F0,#50,#00,#00,#1F,#10,#F8,#28 ; #57c0 ? ....? .P.....(
db #00,#00,#1F,#10,#FC,#14,#00,#00,#0F,#08,#FE,#0A,#00,#00,#0F,#08 ; #57d0 
db #FF,#05,#00,#00,#07,#04,#FF,#02,#80,#80,#07,#04,#FF,#05,#00,#00 ; #57e0 
db #0F,#08,#FE,#0A,#00,#00,#1F,#10,#FC,#14,#00,#00,#3F,#20,#F8,#28 ; #57f0 ............? .(
db #00,#00,#7F,#40,#F0,#50,#00,#00,#FF,#FF,#E0,#E0,#00,#00,#00,#00 ; #5800 ...@.P..........
db #0C,#0C,#00,#00,#00,#00,#3C,#34,#00,#00,#00,#00,#F8,#C8,#00,#00 ; #5810 ......<4........
db #03,#03,#F8,#28,#00,#00,#0F,#0C,#F0,#50,#00,#00,#3F,#30,#F0,#50 ; #5820 ...(.....P..?0.P
db #00,#00,#3F,#20,#E0,#20,#00,#00,#1F,#10,#F0,#50,#00,#00,#1F,#10 ; #5830 ..? . .....P....
db #F8,#28,#00,#00,#0F,#08,#FC,#14,#00,#00,#0F,#08,#FE,#0A,#00,#00 ; #5840 .(..............
db #07,#04,#FF,#05,#00,#00,#07,#04,#FF,#02,#80,#80,#03,#02,#FF,#01 ; #5850 
db #C0,#40,#03,#02,#FF,#02,#80,#80,#07,#04,#FF,#05,#00,#00,#0F,#08 ; #5860 .@..............
db #FE,#0A,#00,#00,#1F,#10,#FC,#14,#00,#00,#3F,#20,#F8,#28,#00,#00 ; #5870 ..........? .(..
db #7F,#7F,#F0,#F0,#00,#00,#00,#00,#06,#06,#00,#00,#00,#00,#1E,#1A ; #5880 
db #00,#00,#00,#00,#7C,#64,#00,#00,#01,#01,#FC,#94,#00,#00,#07,#06 ; #5890 ....|d..........
db #F8,#28,#00,#00,#1F,#18,#F8,#28,#00,#00,#1F,#10,#F0,#10,#00,#00 ; #58a0 .(.....(........
db #0F,#08,#F8,#28,#00,#00,#0F,#08,#FC,#14,#00,#00,#07,#04,#FE,#0A ; #58b0 ...(............
db #00,#00,#07,#04,#FF,#05,#00,#00,#03,#02,#FF,#02,#80,#80,#03,#02 ; #58c0 
db #FF,#01,#C0,#40,#01,#01,#FF,#00,#E0,#A0,#01,#01,#FF,#01,#C0,#40 ; #58d0 ...@...........@
db #03,#02,#FF,#02,#80,#80,#07,#04,#FF,#05,#00,#00,#0F,#08,#FE,#0A ; #58e0 
db #00,#00,#1F,#10,#FC,#14,#00,#00,#3F,#3F,#F8,#F8,#00,#00,#00,#00 ; #58f0 ........??......
db #03,#03,#00,#00,#00,#00,#0F,#0D,#00,#00,#00,#00,#3E,#32,#00,#00 ; #5900 ............>2..
db #00,#00,#FE,#CA,#00,#00,#03,#03,#FC,#14,#00,#00,#0F,#0C,#FC,#14 ; #5910 
db #00,#00,#0F,#08,#F8,#08,#00,#00,#07,#04,#FC,#14,#00,#00,#07,#04 ; #5920 
db #FE,#0A,#00,#00,#03,#02,#FF,#05,#00,#00,#03,#02,#FF,#02,#80,#80 ; #5930 
db #01,#01,#FF,#01,#C0,#40,#01,#01,#FF,#00,#E0,#A0,#00,#00,#FF,#80 ; #5940 .....@..........
db #F0,#50,#00,#00,#FF,#80,#E0,#A0,#01,#01,#FF,#01,#C0,#40,#03,#02 ; #5950 .P...........@..
db #FF,#02,#80,#80,#07,#04,#FF,#05,#00,#00,#0F,#08,#FE,#0A,#00,#00 ; #5960 
db #1F,#1F,#FC,#FC,#00,#00,#00,#00,#01,#01,#80,#80,#00,#00,#07,#06 ; #5970 
db #80,#80,#00,#00,#1F,#19,#00,#00,#00,#00,#7F,#65,#00,#00,#01,#01 ; #5980 ...........e....
db #FE,#8A,#00,#00,#07,#06,#FE,#0A,#00,#00,#07,#04,#FC,#04,#00,#00 ; #5990 
db #03,#02,#FE,#0A,#00,#00,#03,#02,#FF,#05,#00,#00,#01,#01,#FF,#02 ; #59a0 
db #80,#80,#01,#01,#FF,#01,#C0,#40,#00,#00,#FF,#80,#E0,#A0,#00,#00 ; #59b0 .......@........
db #FF,#80,#F0,#50,#00,#00,#7F,#40,#F8,#28,#00,#00,#7F,#40,#F0,#50 ; #59c0 ...P...@.(...@.P
db #00,#00,#FF,#80,#E0,#A0,#01,#01,#FF,#01,#C0,#40,#03,#02,#FF,#02 ; #59d0 ...........@....
db #80,#80,#07,#04,#FF,#05,#00,#00,#0F,#0F,#FE,#FE,#00,#00,#00,#00 ; #59e0 
db #00,#00,#C0,#C0,#00,#00,#03,#03,#C0,#40,#00,#00,#0F,#0C,#80,#80 ; #59f0 .........@......
db #00,#00,#3F,#32,#80,#80,#00,#00,#FF,#C5,#00,#00,#03,#03,#FF,#05 ; #5a00 ..?2............
db #00,#00,#03,#02,#FE,#02,#00,#00,#01,#01,#FF,#05,#00,#00,#01,#01 ; #5a10 
db #FF,#02,#80,#80,#00,#00,#FF,#81,#C0,#40,#00,#00,#FF,#80,#E0,#A0 ; #5a20 .........@......
db #00,#00,#7F,#40,#F0,#50,#00,#00,#7F,#40,#F8,#28,#00,#00,#3F,#20 ; #5a30 ...@.P...@.(..? 
db #FC,#14,#00,#00,#3F,#20,#F8,#28,#00,#00,#7F,#40,#F0,#50,#00,#00 ; #5a40 ....? .(...@.P..
db #FF,#80,#E0,#A0,#01,#01,#FF,#01,#C0,#40,#03,#02,#FF,#02,#80,#80 ; #5a50 .........@......
db #07,#07,#FF,#FF,#00,#00,#00,#00,#00,#00,#60,#60,#00,#00,#01,#01 ; #5a60 ..........``....
db #E0,#A0,#00,#00,#07,#06,#C0,#40,#00,#00,#1F,#19,#C0,#40,#00,#00 ; #5a70 .......@.....@..
db #7F,#62,#80,#80,#01,#01,#FF,#82,#80,#80,#01,#01,#FF,#01,#00,#00 ; #5a80 .b..............
db #00,#00,#FF,#82,#80,#80,#00,#00,#FF,#81,#C0,#40,#00,#00,#7F,#40 ; #5a90 ...........@...@
db #E0,#A0,#00,#00,#7F,#40,#F0,#50,#00,#00,#3F,#20,#F8,#28,#00,#00 ; #5aa0 .....@.P..? .(..
db #3F,#20,#FC,#14,#00,#00,#1F,#10,#FE,#0A,#00,#00,#1F,#10,#FC,#14 ; #5ab0 ? ..............
db #00,#00,#3F,#20,#F8,#28,#00,#00,#7F,#40,#F0,#50,#00,#00,#FF,#80 ; #5ac0 ..? .(...@.P....
db #E0,#A0,#01,#01,#FF,#01,#C0,#40,#03,#03,#FF,#FF,#80,#80,#00,#00 ; #5ad0 .......@........
db #00,#00,#30,#30,#00,#00,#00,#00,#F0,#D0,#00,#00,#03,#03,#E0,#20 ; #5ae0 ..00........... 
db #00,#00,#0F,#0C,#E0,#A0,#00,#00,#3F,#31,#C0,#40,#00,#00,#FF,#C1 ; #5af0 ........?1.@....
db #C0,#40,#00,#00,#FF,#80,#80,#80,#00,#00,#7F,#41,#C0,#40,#00,#00 ; #5b00 .@.........A.@..
db #7F,#40,#E0,#A0,#00,#00,#3F,#20,#F0,#50,#00,#00,#3F,#20,#F8,#28 ; #5b10 .@....? .P..? .(
db #00,#00,#1F,#10,#FC,#14,#00,#00,#1F,#10,#FE,#0A,#00,#00,#0F,#08 ; #5b20 
db #FF,#05,#00,#00,#0F,#08,#FE,#0A,#00,#00,#1F,#10,#FC,#14,#00,#00 ; #5b30 
db #3F,#20,#F8,#28,#00,#00,#7F,#40,#F0,#50,#00,#00,#FF,#80,#E0,#A0 ; #5b40 ? .(...@.P......
db #01,#01,#FF,#FF,#C0,#C0,#0C,#0C,#00,#00,#00,#00,#0F,#0B,#00,#00 ; #5b50 
db #00,#00,#07,#04,#C0,#C0,#00,#00,#07,#05,#F0,#30,#00,#00,#03,#02 ; #5b60 ...........0....
db #FC,#CC,#00,#00,#03,#02,#FF,#33,#00,#00,#01,#01,#FF,#05,#00,#00 ; #5b70 .......3........
db #03,#02,#FE,#0A,#00,#00,#07,#04,#FE,#0A,#00,#00,#0F,#08,#FC,#14 ; #5b80 
db #00,#00,#1F,#10,#FC,#14,#00,#00,#3F,#20,#F8,#28,#00,#00,#7F,#40 ; #5b90 ........? .(...@
db #F8,#28,#00,#00,#FF,#80,#F0,#50,#00,#00,#7F,#40,#F0,#50,#00,#00 ; #5ba0 .(.....P...@.P..
db #3F,#20,#F8,#28,#00,#00,#1F,#10,#FC,#14,#00,#00,#0F,#08,#FE,#0A ; #5bb0 ? .(............
db #00,#00,#07,#04,#FF,#05,#00,#00,#03,#03,#FF,#FF,#80,#80,#06,#06 ; #5bc0 
db #00,#00,#00,#00,#07,#05,#80,#80,#00,#00,#03,#02,#E0,#60,#00,#00 ; #5bd0 .............`..
db #03,#02,#F8,#98,#00,#00,#01,#01,#FE,#66,#00,#00,#01,#01,#FF,#19 ; #5be0 .........f......
db #80,#80,#00,#00,#FF,#82,#80,#80,#01,#01,#FF,#05,#00,#00,#03,#02 ; #5bf0 
db #FF,#05,#00,#00,#07,#04,#FE,#0A,#00,#00,#0F,#08,#FE,#0A,#00,#00 ; #5c00 
db #1F,#10,#FC,#14,#00,#00,#3F,#20,#FC,#14,#00,#00,#7F,#40,#F8,#28 ; #5c10 ......? .....@.(
db #00,#00,#3F,#20,#F8,#28,#00,#00,#1F,#10,#FC,#14,#00,#00,#0F,#08 ; #5c20 ..? .(..........
db #FE,#0A,#00,#00,#07,#04,#FF,#05,#00,#00,#03,#02,#FF,#02,#80,#80 ; #5c30 
db #01,#01,#FF,#FF,#C0,#C0,#03,#03,#00,#00,#00,#00,#03,#02,#C0,#C0 ; #5c40 
db #00,#00,#01,#01,#F0,#30,#00,#00,#01,#01,#FC,#4C,#00,#00,#00,#00 ; #5c50 .....0.....L....
db #FF,#B3,#00,#00,#00,#00,#FF,#8C,#C0,#C0,#00,#00,#7F,#41,#C0,#40 ; #5c60 .............A.@
db #00,#00,#FF,#82,#80,#80,#01,#01,#FF,#02,#80,#80,#03,#02,#FF,#05 ; #5c70 
db #00,#00,#07,#04,#FF,#05,#00,#00,#0F,#08,#FE,#0A,#00,#00,#1F,#10 ; #5c80 
db #FE,#0A,#00,#00,#3F,#20,#FC,#14,#00,#00,#1F,#10,#FC,#14,#00,#00 ; #5c90 ....? ..........
db #0F,#08,#FE,#0A,#00,#00,#07,#04,#FF,#05,#00,#00,#03,#02,#FF,#02 ; #5ca0 
db #80,#80,#01,#01,#FF,#01,#C0,#40,#00,#00,#FF,#FF,#E0,#E0,#01,#01 ; #5cb0 .......@........
db #80,#80,#00,#00,#01,#01,#E0,#60,#00,#00,#00,#00,#F8,#98,#00,#00 ; #5cc0 .......`........
db #00,#00,#FE,#A6,#00,#00,#00,#00,#7F,#59,#80,#80,#00,#00,#7F,#46 ; #5cd0 .........Y.....F
db #E0,#60,#00,#00,#3F,#20,#E0,#A0,#00,#00,#7F,#41,#C0,#40,#00,#00 ; #5ce0 .`..? .....A.@..
db #FF,#81,#C0,#40,#01,#01,#FF,#02,#80,#80,#03,#02,#FF,#02,#80,#80 ; #5cf0 ...@............
db #07,#04,#FF,#05,#00,#00,#0F,#08,#FF,#05,#00,#00,#1F,#10,#FE,#0A ; #5d00 
db #00,#00,#0F,#08,#FE,#0A,#00,#00,#07,#04,#FF,#05,#00,#00,#03,#02 ; #5d10 
db #FF,#02,#80,#80,#01,#01,#FF,#01,#C0,#40,#00,#00,#FF,#80,#E0,#A0 ; #5d20 .........@......
db #00,#00,#7F,#7F,#F0,#F0,#00,#00,#C0,#C0,#00,#00,#00,#00,#F0,#B0 ; #5d30 
db #00,#00,#00,#00,#7C,#4C,#00,#00,#00,#00,#7F,#53,#00,#00,#00,#00 ; #5d40 ....|L.....S....
db #3F,#2C,#C0,#C0,#00,#00,#3F,#23,#F0,#30,#00,#00,#1F,#10,#F0,#50 ; #5d50 ?,....?#.0.....P
db #00,#00,#3F,#20,#E0,#A0,#00,#00,#7F,#40,#E0,#A0,#00,#00,#FF,#81 ; #5d60 ..? .....@......
db #C0,#40,#01,#01,#FF,#01,#C0,#40,#03,#02,#FF,#02,#80,#80,#07,#04 ; #5d70 .@.....@........
db #FF,#02,#80,#80,#0F,#08,#FF,#05,#00,#00,#07,#04,#FF,#05,#00,#00 ; #5d80 
db #03,#02,#FF,#02,#80,#80,#01,#01,#FF,#01,#C0,#40,#00,#00,#FF,#80 ; #5d90 ...........@....
db #E0,#A0,#00,#00,#7F,#40,#F0,#50,#00,#00,#3F,#3F,#F8,#F8,#00,#00 ; #5da0 .....@.P..??....
db #60,#60,#00,#00,#00,#00,#78,#58,#00,#00,#00,#00,#3E,#26,#00,#00 ; #5db0 ``....xX....>&..
db #00,#00,#3F,#29,#80,#80,#00,#00,#1F,#16,#E0,#60,#00,#00,#1F,#11 ; #5dc0 ..?).......`....
db #F8,#98,#00,#00,#0F,#08,#F8,#28,#00,#00,#1F,#10,#F0,#50,#00,#00 ; #5dd0 .......(.....P..
db #3F,#20,#F0,#50,#00,#00,#7F,#40,#E0,#A0,#00,#00,#FF,#80,#E0,#A0 ; #5de0 ? .P...@........
db #01,#01,#FF,#01,#C0,#40,#03,#02,#FF,#01,#C0,#40,#07,#04,#FF,#02 ; #5df0 .....@.....@....
db #80,#80,#03,#02,#FF,#02,#80,#80,#01,#01,#FF,#01,#C0,#40,#00,#00 ; #5e00 .............@..
db #FF,#80,#E0,#A0,#00,#00,#7F,#40,#F0,#50,#00,#00,#3F,#20,#F8,#28 ; #5e10 .......@.P..? .(
db #00,#00,#1F,#1F,#FC,#FC,#00,#00,#30,#30,#00,#00,#00,#00,#3C,#2C ; #5e20 ........00....<,
db #00,#00,#00,#00,#1F,#13,#00,#00,#00,#00,#1F,#14,#C0,#C0,#00,#00 ; #5e30 
db #0F,#0B,#F0,#30,#00,#00,#0F,#08,#FC,#CC,#00,#00,#07,#04,#FC,#14 ; #5e40 ...0............
db #00,#00,#0F,#08,#F8,#28,#00,#00,#1F,#10,#F8,#28,#00,#00,#3F,#20 ; #5e50 .....(.....(..? 
db #F0,#50,#00,#00,#7F,#40,#F0,#50,#00,#00,#FF,#80,#E0,#A0,#01,#01 ; #5e60 .P...@.P........
db #FF,#00,#E0,#A0,#03,#02,#FF,#01,#C0,#40,#01,#01,#FF,#01,#C0,#40 ; #5e70 .........@.....@
db #00,#00,#FF,#80,#E0,#A0,#00,#00,#7F,#40,#F0,#50,#00,#00,#3F,#20 ; #5e80 .........@.P..? 
db #F8,#28,#00,#00,#1F,#10,#FC,#14,#00,#00,#0F,#0F,#FE,#FE,#00,#00 ; #5e90 .(..............
db #18,#18,#00,#00,#00,#00,#1E,#16,#00,#00,#00,#00,#0F,#09,#80,#80 ; #5ea0 
db #00,#00,#0F,#0A,#E0,#60,#00,#00,#07,#05,#F8,#98,#00,#00,#07,#04 ; #5eb0 .....`..........
db #FE,#66,#00,#00,#03,#02,#FE,#0A,#00,#00,#07,#04,#FC,#14,#00,#00 ; #5ec0 .f..............
db #0F,#08,#FC,#14,#00,#00,#1F,#10,#F8,#28,#00,#00,#3F,#20,#F8,#28 ; #5ed0 .........(..? .(
db #00,#00,#7F,#40,#F0,#50,#00,#00,#FF,#80,#F0,#50,#01,#01,#FF,#00 ; #5ee0 ...@.P.....P....
db #E0,#A0,#00,#00,#FF,#80,#E0,#A0,#00,#00,#7F,#40,#F0,#50,#00,#00 ; #5ef0 ...........@.P..
db #3F,#20,#F8,#28,#00,#00,#1F,#10,#FC,#14,#00,#00,#0F,#08,#FE,#0A ; #5f00 ? .(............
db #00,#00,#07,#07,#FF,#FF,#1C,#1C,#00,#00,#3E,#22,#00,#00,#7F,#49 ; #5f10 ..........>"...I
db #00,#00,#F7,#96,#C0,#C0,#E1,#A1,#F0,#30,#F8,#98,#F8,#C8,#7E,#46 ; #5f20 .........0....~F
db #3C,#24,#3F,#31,#1E,#12,#0F,#0C,#9E,#92,#03,#02,#FE,#62,#01,#01 ; #5f30 <$?1.........b..
db #FC,#84,#00,#00,#78,#78,#0E,#0E,#00,#00,#1F,#11,#00,#00,#3F,#24 ; #5f40 ....xx........?$
db #80,#80,#7B,#4B,#E0,#60,#70,#50,#F8,#98,#7C,#4C,#7C,#64,#3F,#23 ; #5f50 ..{K.`pP..|L|d?#
db #1E,#12,#1F,#18,#8F,#89,#07,#06,#CF,#49,#01,#01,#FF,#31,#00,#00 ; #5f60 .........I...1..
db #FE,#C2,#00,#00,#3C,#3C,#07,#07,#00,#00,#00,#00,#0F,#08,#80,#80 ; #5f70 ....<<..........
db #00,#00,#1F,#12,#C0,#40,#00,#00,#3D,#25,#F0,#B0,#00,#00,#38,#28 ; #5f80 .....@..=%....8(
db #7C,#4C,#00,#00,#3E,#26,#3E,#32,#00,#00,#1F,#11,#8F,#89,#00,#00 ; #5f90 |L..>&>2........
db #0F,#0C,#C7,#44,#80,#80,#03,#03,#E7,#24,#80,#80,#00,#00,#FF,#98 ; #5fa0 ...D.....$......
db #80,#80,#00,#00,#7F,#61,#00,#00,#00,#00,#1E,#1E,#00,#00,#03,#03 ; #5fb0 .....a..........
db #80,#80,#00,#00,#07,#04,#C0,#40,#00,#00,#0F,#09,#E0,#20,#00,#00 ; #5fc0 .......@..... ..
db #1E,#12,#F8,#D8,#00,#00,#1C,#14,#3E,#26,#00,#00,#1F,#13,#1F,#19 ; #5fd0 ........>&......
db #00,#00,#0F,#08,#C7,#C4,#80,#80,#07,#06,#E3,#22,#C0,#40,#01,#01 ; #5fe0 ...........".@..
db #F3,#92,#C0,#40,#00,#00,#7F,#4C,#C0,#40,#00,#00,#3F,#30,#80,#80 ; #5ff0 ...@...L.@..?0..
db #00,#00,#0F,#0F,#00,#00,#01,#01,#C0,#C0,#00,#00,#03,#02,#E0,#20 ; #6000 ............... 
db #00,#00,#07,#04,#F0,#90,#00,#00,#0F,#09,#7C,#6C,#00,#00,#0E,#0A ; #6010 ..........|l....
db #1F,#13,#00,#00,#0F,#09,#8F,#8C,#80,#80,#07,#04,#E3,#62,#C0,#40 ; #6020 .............b.@
db #03,#03,#F1,#11,#E0,#20,#00,#00,#F9,#C9,#E0,#20,#00,#00,#3F,#26 ; #6030 ..... ..... ..?&
db #E0,#20,#00,#00,#1F,#18,#C0,#40,#00,#00,#07,#07,#80,#80,#00,#00 ; #6040 . .....@........
db #E0,#E0,#00,#00,#01,#01,#F0,#10,#00,#00,#03,#02,#F8,#48,#00,#00 ; #6050 .............H..
db #07,#04,#BE,#B6,#00,#00,#07,#05,#0F,#09,#80,#80,#07,#04,#C7,#C6 ; #6060 
db #C0,#40,#03,#02,#F1,#31,#E0,#20,#01,#01,#F8,#88,#F0,#90,#00,#00 ; #6070 .@...1. ........
db #7C,#64,#F0,#90,#00,#00,#1F,#13,#F0,#10,#00,#00,#0F,#0C,#E0,#20 ; #6080 |d............. 
db #00,#00,#03,#03,#C0,#C0,#00,#00,#70,#70,#00,#00,#00,#00,#F8,#88 ; #6090 ........pp......
db #00,#00,#01,#01,#FC,#24,#00,#00,#03,#02,#DF,#5B,#00,#00,#03,#02 ; #60a0 .....$.....[....
db #87,#84,#C0,#C0,#03,#02,#E3,#63,#E0,#20,#01,#01,#F8,#18,#F0,#90 ; #60b0 .......c. ......
db #00,#00,#FC,#C4,#78,#48,#00,#00,#3E,#32,#78,#48,#00,#00,#0F,#09 ; #60c0 ....xH..>2xH....
db #F8,#88,#00,#00,#07,#06,#F0,#10,#00,#00,#01,#01,#E0,#E0,#00,#00 ; #60d0 
db #38,#38,#00,#00,#00,#00,#7C,#44,#00,#00,#00,#00,#FE,#92,#00,#00 ; #60e0 88....|D........
db #01,#01,#EF,#2D,#80,#80,#01,#01,#C3,#42,#E0,#60,#01,#01,#F1,#31 ; #60f0 ...-.....B.`...1
db #F0,#90,#00,#00,#FC,#8C,#78,#48,#00,#00,#7E,#62,#3C,#24,#00,#00 ; #6100 ......xH..~b<$..
db #1F,#19,#3C,#24,#00,#00,#07,#04,#FC,#C4,#00,#00,#03,#03,#F8,#08 ; #6110 ..<$............
db #00,#00,#00,#00,#F0,#F0,#00,#00,#70,#70,#00,#00,#F8,#88,#01,#01 ; #6120 ........pp......
db #FC,#24,#07,#06,#DE,#D2,#1F,#19,#0E,#0A,#3E,#26,#3E,#32,#78,#48 ; #6130 .$........>&>2xH
db #FC,#C4,#F1,#91,#F8,#18,#F3,#92,#E0,#60,#FF,#8C,#80,#80,#7F,#43 ; #6140 .........`.....C
db #00,#00,#3C,#3C,#00,#00,#00,#00,#38,#38,#00,#00,#7C,#44,#00,#00 ; #6150 ..<<....88..|D..
db #FE,#92,#03,#03,#EF,#69,#0F,#0C,#87,#85,#1F,#13,#1F,#19,#3C,#24 ; #6160 .....i........<$
db #7E,#62,#78,#48,#FC,#8C,#79,#49,#F0,#30,#7F,#46,#C0,#40,#3F,#21 ; #6170 ~bxH..yI.0.F.@?!
db #80,#80,#1E,#1E,#00,#00,#00,#00,#1C,#1C,#00,#00,#00,#00,#3E,#22 ; #6180 ..............>"
db #00,#00,#00,#00,#7F,#49,#00,#00,#01,#01,#F7,#B4,#80,#80,#07,#06 ; #6190 .....I..........
db #C3,#42,#80,#80,#0F,#09,#8F,#8C,#80,#80,#1E,#12,#3F,#31,#00,#00 ; #61a0 .B..........?1..
db #3C,#24,#7E,#46,#00,#00,#3C,#24,#F8,#98,#00,#00,#3F,#23,#E0,#20 ; #61b0 <$~F..<$....?#. 
db #00,#00,#1F,#10,#C0,#C0,#00,#00,#0F,#0F,#00,#00,#00,#00,#00,#00 ; #61c0 
db #0E,#0E,#00,#00,#00,#00,#1F,#11,#00,#00,#00,#00,#3F,#24,#80,#80 ; #61d0 ............?$..
db #00,#00,#FB,#DA,#C0,#40,#03,#03,#E1,#21,#C0,#40,#07,#04,#C7,#C6 ; #61e0 .....@...!.@....
db #C0,#40,#0F,#09,#1F,#18,#80,#80,#1E,#12,#3F,#23,#00,#00,#1E,#12 ; #61f0 .@........?#....
db #7C,#4C,#00,#00,#1F,#11,#F0,#90,#00,#00,#0F,#08,#E0,#60,#00,#00 ; #6200 |L...........`..
db #07,#07,#80,#80,#00,#00,#00,#00,#07,#07,#00,#00,#00,#00,#0F,#08 ; #6210 
db #80,#80,#00,#00,#1F,#12,#C0,#40,#00,#00,#7D,#6D,#E0,#20,#01,#01 ; #6220 .......@..}m. ..
db #F0,#90,#E0,#A0,#03,#02,#E3,#63,#E0,#20,#07,#04,#8F,#8C,#C0,#40 ; #6230 .......c. .....@
db #0F,#09,#1F,#11,#80,#80,#0F,#09,#3E,#26,#00,#00,#0F,#08,#F8,#C8 ; #6240 ........>&......
db #00,#00,#07,#04,#F0,#30,#00,#00,#03,#03,#C0,#C0,#00,#00,#00,#00 ; #6250 .....0..........
db #03,#03,#80,#80,#00,#00,#07,#04,#C0,#40,#00,#00,#0F,#09,#E0,#20 ; #6260 .........@..... 
db #00,#00,#3E,#36,#F0,#90,#00,#00,#F8,#C8,#70,#50,#01,#01,#F1,#31 ; #6270 ..>6......pP...1
db #F0,#90,#03,#02,#C7,#46,#E0,#20,#07,#04,#8F,#88,#C0,#C0,#07,#04 ; #6280 .....F. ........
db #9F,#93,#00,#00,#07,#04,#FC,#64,#00,#00,#03,#02,#F8,#18,#00,#00 ; #6290 .......d........
db #01,#01,#E0,#E0,#00,#00,#00,#00,#01,#01,#C0,#C0,#00,#00,#03,#02 ; #62a0 
db #E0,#20,#00,#00,#07,#04,#F0,#90,#00,#00,#1F,#1B,#78,#48,#00,#00 ; #62b0 . ..........xH..
db #7C,#64,#38,#28,#00,#00,#F8,#98,#F8,#C8,#01,#01,#E3,#23,#F0,#10 ; #62c0 |d8(.........#..
db #03,#02,#C7,#44,#E0,#60,#03,#02,#CF,#49,#80,#80,#03,#02,#FE,#32 ; #62d0 ...D.`...I.....2
db #00,#00,#01,#01,#FC,#0C,#00,#00,#00,#00,#F0,#F0,#00,#00,#00,#00 ; #62e0 
db #00,#00,#E0,#E0,#00,#00,#01,#01,#F0,#10,#00,#00,#03,#02,#F8,#48 ; #62f0 ...............H
db #00,#00,#0F,#0D,#BC,#A4,#00,#00,#3E,#32,#1C,#14,#00,#00,#7C,#4C ; #6300 ........>2....|L
db #7C,#64,#00,#00,#F1,#91,#F8,#88,#01,#01,#E3,#22,#F0,#30,#01,#01 ; #6310 |d.........".0..
db #E7,#24,#C0,#C0,#01,#01,#FF,#19,#00,#00,#00,#00,#FE,#86,#00,#00 ; #6320 .$..............
db #00,#00,#78,#78,#00,#00,#07,#07,#00,#00,#00,#00,#1F,#18,#C0,#C0 ; #6330 ..xx............
db #00,#00,#3F,#22,#E0,#20,#00,#00,#7D,#45,#F8,#98,#00,#00,#78,#48 ; #6340 ..?". ..}E....xH
db #7E,#46,#00,#00,#F0,#90,#3F,#31,#00,#00,#F0,#90,#0F,#0C,#80,#80 ; #6350 ~F....?1........
db #7E,#4E,#03,#02,#80,#80,#3F,#21,#03,#02,#C0,#40,#1F,#1C,#83,#82 ; #6360 ~N....?!...@....
db #C0,#40,#03,#02,#E7,#64,#80,#80,#01,#01,#FF,#18,#80,#80,#00,#00 ; #6370 .@...d..........
db #FF,#C3,#00,#00,#00,#00,#3C,#3C,#00,#00,#03,#03,#80,#80,#00,#00 ; #6380 ......<<........
db #0F,#0C,#E0,#60,#00,#00,#1F,#11,#F0,#10,#00,#00,#3E,#22,#FC,#CC ; #6390 ...`........>"..
db #00,#00,#3C,#24,#3F,#23,#00,#00,#78,#48,#1F,#18,#80,#80,#78,#48 ; #63a0 ..<$?#..xH....xH
db #07,#06,#C0,#40,#3F,#27,#01,#01,#C0,#40,#1F,#10,#81,#81,#E0,#20 ; #63b0 ...@?'...@..... 
db #0F,#0E,#C1,#41,#E0,#20,#01,#01,#F3,#32,#C0,#40,#00,#00,#FF,#8C ; #63c0 ...A. ...2.@....
db #C0,#40,#00,#00,#7F,#61,#80,#80,#00,#00,#1E,#1E,#00,#00,#01,#01 ; #63d0 .@...a..........
db #C0,#C0,#00,#00,#07,#06,#F0,#30,#00,#00,#0F,#08,#F8,#88,#00,#00 ; #63e0 .......0........
db #1F,#11,#7E,#66,#00,#00,#1E,#12,#1F,#11,#80,#80,#3C,#24,#0F,#0C ; #63f0 ..~f........<$..
db #C0,#40,#3C,#24,#03,#03,#E0,#20,#1F,#13,#80,#80,#E0,#A0,#0F,#08 ; #6400 .@<$... ........
db #C0,#40,#F0,#90,#07,#07,#E0,#20,#F0,#90,#00,#00,#F9,#99,#E0,#20 ; #6410 .@..... ....... 
db #00,#00,#7F,#46,#E0,#20,#00,#00,#3F,#30,#C0,#C0,#00,#00,#0F,#0F ; #6420 ...F. ..?0......
db #00,#00,#00,#00,#E0,#E0,#00,#00,#03,#03,#F8,#18,#00,#00,#07,#04 ; #6430 
db #FC,#44,#00,#00,#0F,#08,#BF,#B3,#00,#00,#0F,#09,#0F,#08,#C0,#C0 ; #6440 .D..............
db #1E,#12,#07,#06,#E0,#20,#1E,#12,#01,#01,#F0,#90,#0F,#09,#C0,#C0 ; #6450 ..... ..........
db #70,#50,#07,#04,#E0,#20,#78,#48,#03,#03,#F0,#90,#78,#48,#00,#00 ; #6460 pP... xH....xH..
db #7C,#4C,#F0,#90,#00,#00,#3F,#23,#F0,#10,#00,#00,#1F,#18,#E0,#60 ; #6470 |L....?#.......`
db #00,#00,#07,#07,#80,#80,#00,#00,#70,#70,#00,#00,#01,#01,#FC,#8C ; #6480 ........pp......
db #00,#00,#03,#02,#FE,#22,#00,#00,#07,#04,#DF,#59,#80,#80,#07,#04 ; #6490 .....".....Y....
db #87,#84,#E0,#60,#0F,#09,#03,#03,#F0,#10,#0F,#09,#00,#00,#F8,#C8 ; #64a0 ...`............
db #07,#04,#E0,#E0,#38,#28,#03,#02,#F0,#10,#3C,#24,#01,#01,#F8,#C8 ; #64b0 ....8(....<$....
db #3C,#24,#00,#00,#3E,#26,#78,#48,#00,#00,#1F,#11,#F8,#88,#00,#00 ; #64c0 <$..>&xH........
db #0F,#0C,#F0,#30,#00,#00,#03,#03,#C0,#C0,#00,#00,#38,#38,#00,#00 ; #64d0 ...0........88..
db #00,#00,#FE,#C6,#00,#00,#01,#01,#FF,#11,#00,#00,#03,#02,#EF,#2C ; #64e0 ...............,
db #C0,#C0,#03,#02,#C3,#42,#F0,#30,#07,#04,#81,#81,#F8,#88,#07,#04 ; #64f0 .....B.0........
db #80,#80,#7C,#64,#03,#02,#F0,#70,#1C,#14,#01,#01,#F8,#08,#1E,#12 ; #6500 ..|d...p........
db #00,#00,#FC,#E4,#1E,#12,#00,#00,#1F,#13,#3C,#24,#00,#00,#0F,#08 ; #6510 ..........<$....
db #FC,#C4,#00,#00,#07,#06,#F8,#18,#00,#00,#01,#01,#E0,#E0,#00,#00 ; #6520 
db #1C,#1C,#00,#00,#00,#00,#7F,#63,#00,#00,#00,#00,#FF,#88,#80,#80 ; #6530 .......c........
db #01,#01,#F7,#16,#E0,#60,#01,#01,#E1,#21,#F8,#18,#03,#02,#C0,#40 ; #6540 .....`...!.....@
db #FC,#C4,#03,#02,#C0,#40,#3E,#32,#01,#01,#F8,#38,#0E,#0A,#00,#00 ; #6550 .....@>2...8....
db #FC,#84,#0F,#09,#00,#00,#7E,#72,#0F,#09,#00,#00,#0F,#09,#9E,#92 ; #6560 ......~r........
db #00,#00,#07,#04,#FE,#62,#00,#00,#03,#03,#FC,#0C,#00,#00,#00,#00 ; #6570 .....b..........
db #F0,#F0,#00,#00,#0E,#0E,#00,#00,#00,#00,#00,#00,#3F,#31,#80,#80 ; #6580 ............?1..
db #00,#00,#00,#00,#7F,#44,#C0,#40,#00,#00,#00,#00,#FB,#8B,#F0,#30 ; #6590 .....D.@.......0
db #00,#00,#00,#00,#F0,#90,#FC,#8C,#00,#00,#01,#01,#E0,#20,#7E,#62 ; #65a0 ............. ~b
db #00,#00,#01,#01,#E0,#20,#1F,#19,#00,#00,#00,#00,#FC,#9C,#07,#05 ; #65b0 ..... ..........
db #00,#00,#00,#00,#7E,#42,#07,#04,#80,#80,#00,#00,#3F,#39,#07,#04 ; #65c0 ....~B......?9..
db #80,#80,#00,#00,#07,#04,#CF,#C9,#00,#00,#00,#00,#03,#02,#FF,#31 ; #65d0 ...............1
db #00,#00,#00,#00,#01,#01,#FE,#86,#00,#00,#00,#00,#00,#00,#78,#78 ; #65e0 ..............xx
db #00,#00,#00,#00,#38,#38,#00,#00,#00,#00,#FE,#C6,#00,#00,#01,#01 ; #65f0 ....88..........
db #FF,#11,#00,#00,#07,#06,#EF,#68,#80,#80,#1F,#18,#87,#84,#80,#80 ; #6600 .......h........
db #3F,#23,#03,#02,#C0,#40,#7C,#4C,#03,#02,#C0,#40,#70,#50,#1F,#1C ; #6610 ?#...@|L...@pP..
db #80,#80,#F0,#90,#3F,#21,#00,#00,#F0,#90,#7E,#4E,#00,#00,#79,#49 ; #6620 ....?!....~N..yI
db #F0,#90,#00,#00,#7F,#46,#E0,#20,#00,#00,#3F,#30,#C0,#C0,#00,#00 ; #6630 .....F. ..?0....
db #0F,#0F,#00,#00,#00,#00,#00,#00,#1C,#1C,#00,#00,#00,#00,#7F,#63 ; #6640 ...............c
db #00,#00,#00,#00,#FF,#88,#80,#80,#03,#03,#F7,#34,#C0,#40,#0F,#0C ; #6650 ...........4.@..
db #C3,#42,#C0,#40,#1F,#11,#81,#81,#E0,#20,#3E,#26,#01,#01,#E0,#20 ; #6660 .B.@..... >&... 
db #38,#28,#0F,#0E,#C0,#40,#78,#48,#1F,#10,#80,#80,#78,#48,#3F,#27 ; #6670 8(...@xH....xH?'
db #00,#00,#3C,#24,#F8,#C8,#00,#00,#3F,#23,#F0,#10,#00,#00,#1F,#18 ; #6680 ..<$....?#......
db #E0,#60,#00,#00,#07,#07,#80,#80,#00,#00,#00,#00,#0E,#0E,#00,#00 ; #6690 .`..............
db #00,#00,#3F,#31,#80,#80,#00,#00,#7F,#44,#C0,#40,#01,#01,#FB,#9A ; #66a0 ..?1.....D.@....
db #E0,#20,#07,#06,#E1,#21,#E0,#20,#0F,#08,#C0,#C0,#F0,#90,#1F,#13 ; #66b0 . ...!. ........
db #00,#00,#F0,#90,#1C,#14,#07,#07,#E0,#20,#3C,#24,#0F,#08,#C0,#40 ; #66c0 ......... <$...@
db #3C,#24,#1F,#13,#80,#80,#1E,#12,#7C,#64,#00,#00,#1F,#11,#F8,#88 ; #66d0 <$......|d......
db #00,#00,#0F,#0C,#F0,#30,#00,#00,#03,#03,#C0,#C0,#00,#00,#00,#00 ; #66e0 .....0..........
db #07,#07,#00,#00,#00,#00,#1F,#18,#C0,#C0,#00,#00,#3F,#22,#E0,#20 ; #66f0 ............?". 
db #00,#00,#FD,#CD,#F0,#10,#03,#03,#F0,#10,#F0,#90,#07,#04,#E0,#60 ; #6700 ...............`
db #78,#48,#0F,#09,#80,#80,#78,#48,#0E,#0A,#03,#03,#F0,#90,#1E,#12 ; #6710 xH....xH........
db #07,#04,#E0,#20,#1E,#12,#0F,#09,#C0,#C0,#0F,#09,#3E,#32,#00,#00 ; #6720 ... ........>2..
db #0F,#08,#FC,#C4,#00,#00,#07,#06,#F8,#18,#00,#00,#01,#01,#E0,#E0 ; #6730 
db #00,#00,#00,#00,#03,#03,#80,#80,#00,#00,#0F,#0C,#E0,#60,#00,#00 ; #6740 .............`..
db #1F,#11,#F0,#10,#00,#00,#7E,#66,#F8,#88,#01,#01,#F8,#88,#78,#48 ; #6750 ......~f......xH
db #03,#02,#F0,#30,#3C,#24,#07,#04,#C0,#C0,#3C,#24,#07,#05,#01,#01 ; #6760 ...0<$....<$....
db #F8,#C8,#0F,#09,#03,#02,#F0,#10,#0F,#09,#07,#04,#E0,#E0,#07,#04 ; #6770 
db #9F,#99,#00,#00,#07,#04,#FE,#62,#00,#00,#03,#03,#FC,#0C,#00,#00 ; #6780 .......b........
db #00,#00,#F0,#F0,#00,#00,#00,#00,#01,#01,#C0,#C0,#00,#00,#07,#06 ; #6790 
db #F0,#30,#00,#00,#0F,#08,#F8,#88,#00,#00,#3F,#33,#7C,#44,#00,#00 ; #67a0 .0........?3|D..
db #FC,#C4,#3C,#24,#01,#01,#F8,#18,#1E,#12,#03,#02,#E0,#60,#1E,#12 ; #67b0 ..<$.........`..
db #03,#02,#80,#80,#FC,#E4,#07,#04,#81,#81,#F8,#08,#07,#04,#83,#82 ; #67c0 
db #F0,#70,#03,#02,#CF,#4C,#80,#80,#03,#02,#FF,#31,#00,#00,#01,#01 ; #67d0 .p...L.....1....
db #FE,#86,#00,#00,#00,#00,#78,#78,#00,#00,#00,#00,#00,#00,#E0,#E0 ; #67e0 ......xx........
db #00,#00,#03,#03,#F8,#18,#00,#00,#07,#04,#FC,#44,#00,#00,#1F,#19 ; #67f0 ...........D....
db #BE,#A2,#00,#00,#7E,#62,#1E,#12,#00,#00,#FC,#8C,#0F,#09,#01,#01 ; #6800 ....~b..........
db #F0,#30,#0F,#09,#01,#01,#C0,#40,#7E,#72,#03,#02,#C0,#40,#FC,#84 ; #6810 .0.....@~r...@..
db #03,#02,#C1,#41,#F8,#38,#01,#01,#E7,#26,#C0,#40,#01,#01,#FF,#18 ; #6820 ...A.8...&.@....
db #80,#80,#00,#00,#FF,#C3,#00,#00,#00,#00,#3C,#3C,#00,#00,#00,#00 ; #6830 ..........<<....
db #00,#00,#70,#70,#00,#00,#00,#00,#01,#01,#FC,#8C,#00,#00,#00,#00 ; #6840 ..pp............
db #03,#02,#FE,#22,#00,#00,#00,#00,#0F,#0C,#DF,#D1,#00,#00,#00,#00 ; #6850 ..."............
db #3F,#31,#0F,#09,#00,#00,#00,#00,#7E,#46,#07,#04,#80,#80,#00,#00 ; #6860 ?1......~F......
db #F8,#98,#07,#04,#80,#80,#00,#00,#E0,#A0,#3F,#39,#00,#00,#01,#01 ; #6870 ..........?9....
db #E0,#20,#7E,#42,#00,#00,#01,#01,#E0,#20,#FC,#9C,#00,#00,#00,#00 ; #6880 . ~B..... ......
db #F3,#93,#E0,#20,#00,#00,#00,#00,#FF,#8C,#C0,#40,#00,#00,#00,#00 ; #6890 ... .......@....
db #7F,#61,#80,#80,#00,#00,#00,#00,#1E,#1E,#00,#00,#00,#00,#00,#00 ; #68a0 .a..............
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #68b0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #68c0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #68d0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #68e0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #68f0 
db #48,#00,#54,#00,#68,#00,#7C,#00,#90,#00,#AC,#00,#C8,#00,#E4,#00 ; #6900 H.T.h.|.........
db #00,#01,#24,#01,#48,#01,#6C,#01,#90,#01,#E8,#01,#40,#02,#98,#02 ; #6910 ..$.H.l.....@...
db #00,#03,#68,#03,#48,#04,#4C,#04,#50,#04,#54,#04,#5A,#04,#60,#04 ; #6920 ..h.H.L.P.T.Z.`.
db #66,#04,#6E,#04,#76,#04,#7E,#04,#88,#04,#92,#04,#9C,#04,#B4,#04 ; #6930 f.n.v.~.........
db #CC,#04,#E4,#04,#00,#05,#1C,#05,#03,#03,#80,#80,#07,#04,#C0,#40 ; #6940 ...............@
db #03,#03,#80,#80,#01,#01,#80,#80,#03,#02,#C0,#40,#07,#04,#E0,#20 ; #6950 ...........@... 
db #03,#02,#C0,#40,#01,#01,#80,#80,#03,#03,#80,#80,#07,#04,#C0,#40 ; #6960 ...@...........@
db #0F,#08,#E0,#20,#07,#04,#C0,#40,#03,#03,#80,#80,#03,#03,#C0,#C0 ; #6970 ... ...@........
db #07,#04,#E0,#20,#0F,#08,#F0,#10,#07,#04,#E0,#20,#03,#03,#C0,#C0 ; #6980 ... ....... ....
db #03,#03,#80,#80,#07,#04,#C0,#40,#0F,#08,#E0,#20,#1F,#10,#F0,#10 ; #6990 .......@... ....
db #0F,#08,#E0,#20,#07,#04,#C0,#40,#03,#03,#80,#80,#01,#01,#80,#80 ; #69a0 ... ...@........
db #07,#06,#E0,#60,#0F,#08,#F0,#10,#1F,#10,#F8,#08,#0F,#08,#F0,#10 ; #69b0 ...`............
db #07,#06,#E0,#60,#01,#01,#80,#80,#03,#03,#80,#80,#0F,#0C,#E0,#60 ; #69c0 ...`...........`
db #1F,#10,#F0,#10,#3F,#20,#F8,#08,#1F,#10,#F0,#10,#0F,#0C,#E0,#60 ; #69d0 ....? .........`
db #03,#03,#80,#80,#03,#03,#C0,#C0,#0F,#0C,#F0,#30,#1F,#10,#F8,#08 ; #69e0 ...........0....
db #3F,#20,#FC,#04,#1F,#10,#F8,#08,#0F,#0C,#F0,#30,#03,#03,#C0,#C0 ; #69f0 ? .........0....
db #03,#03,#80,#80,#0F,#0C,#E0,#60,#1F,#10,#F0,#10,#3F,#20,#F8,#08 ; #6a00 .......`....? ..
db #7F,#40,#FC,#04,#3F,#20,#F8,#08,#1F,#10,#F0,#10,#0F,#0C,#E0,#60 ; #6a10 .@..? .........`
db #03,#03,#80,#80,#03,#03,#C0,#C0,#0F,#0C,#F0,#30,#1F,#10,#F8,#08 ; #6a20 ...........0....
db #3F,#20,#FC,#04,#7F,#40,#FE,#02,#3F,#20,#FC,#04,#1F,#10,#F8,#08 ; #6a30 ? ...@..? ......
db #0F,#0C,#F0,#30,#03,#03,#C0,#C0,#07,#07,#C0,#C0,#1F,#18,#F0,#30 ; #6a40 ...0...........0
db #3F,#20,#F8,#08,#7F,#40,#FC,#04,#FF,#80,#FE,#02,#7F,#40,#FC,#04 ; #6a50 ? ...@.......@..
db #3F,#20,#F8,#08,#1F,#18,#F0,#30,#07,#07,#C0,#C0,#07,#07,#E0,#E0 ; #6a60 ? .....0........
db #1F,#18,#F8,#18,#3F,#20,#FC,#04,#7F,#40,#FE,#02,#FF,#80,#FF,#01 ; #6a70 ....? ...@......
db #7F,#40,#FE,#02,#3F,#20,#FC,#04,#1F,#18,#F8,#18,#07,#07,#E0,#E0 ; #6a80 .@..? ..........
db #00,#00,#07,#07,#C0,#C0,#00,#00,#00,#00,#1F,#18,#F0,#30,#00,#00 ; #6a90 .............0..
db #00,#00,#7F,#60,#FC,#0C,#00,#00,#00,#00,#7F,#40,#FC,#04,#00,#00 ; #6aa0 ...`.......@....
db #00,#00,#FF,#80,#FE,#02,#00,#00,#01,#01,#FF,#00,#FF,#01,#00,#00 ; #6ab0 
db #00,#00,#FF,#80,#FE,#02,#00,#00,#00,#00,#7F,#40,#FC,#04,#00,#00 ; #6ac0 ...........@....
db #00,#00,#7F,#60,#FC,#0C,#00,#00,#00,#00,#1F,#18,#F0,#30,#00,#00 ; #6ad0 ...`.........0..
db #00,#00,#07,#07,#C0,#C0,#00,#00,#00,#00,#07,#07,#E0,#E0,#00,#00 ; #6ae0 
db #00,#00,#1F,#18,#F8,#18,#00,#00,#00,#00,#7F,#60,#FE,#06,#00,#00 ; #6af0 ...........`....
db #00,#00,#7F,#40,#FE,#02,#00,#00,#00,#00,#FF,#80,#FF,#01,#00,#00 ; #6b00 ...@............
db #01,#01,#FF,#00,#FF,#00,#80,#80,#00,#00,#FF,#80,#FF,#01,#00,#00 ; #6b10 
db #00,#00,#7F,#40,#FE,#02,#00,#00,#00,#00,#7F,#60,#FE,#06,#00,#00 ; #6b20 ...@.......`....
db #00,#00,#1F,#18,#F8,#18,#00,#00,#00,#00,#07,#07,#E0,#E0,#00,#00 ; #6b30 
db #00,#00,#0F,#0F,#E0,#E0,#00,#00,#00,#00,#3F,#30,#F8,#18,#00,#00 ; #6b40 ..........?0....
db #00,#00,#7F,#40,#FC,#04,#00,#00,#00,#00,#FF,#80,#FE,#02,#00,#00 ; #6b50 ...@............
db #01,#01,#FF,#00,#FF,#01,#00,#00,#03,#02,#FF,#00,#FF,#00,#80,#80 ; #6b60 
db #01,#01,#FF,#00,#FF,#01,#00,#00,#00,#00,#FF,#80,#FE,#02,#00,#00 ; #6b70 
db #00,#00,#7F,#40,#FC,#04,#00,#00,#00,#00,#3F,#30,#F8,#18,#00,#00 ; #6b80 ...@......?0....
db #00,#00,#0F,#0F,#E0,#E0,#00,#00,#00,#00,#07,#07,#E0,#E0,#00,#00 ; #6b90 
db #00,#00,#1F,#18,#F8,#18,#00,#00,#00,#00,#3F,#20,#FC,#04,#00,#00 ; #6ba0 ..........? ....
db #00,#00,#7F,#40,#FE,#02,#00,#00,#00,#00,#FF,#80,#FF,#01,#00,#00 ; #6bb0 ...@............
db #01,#01,#FF,#00,#FF,#00,#80,#80,#03,#02,#FF,#00,#FF,#00,#C0,#40 ; #6bc0 ...............@
db #01,#01,#FF,#00,#FF,#00,#80,#80,#00,#00,#FF,#80,#FF,#01,#00,#00 ; #6bd0 
db #00,#00,#7F,#40,#FE,#02,#00,#00,#00,#00,#3F,#20,#FC,#04,#00,#00 ; #6be0 ...@......? ....
db #00,#00,#1F,#18,#F8,#18,#00,#00,#00,#00,#07,#07,#E0,#E0,#00,#00 ; #6bf0 
db #00,#00,#0F,#0F,#E0,#E0,#00,#00,#00,#00,#3F,#30,#F8,#18,#00,#00 ; #6c00 ..........?0....
db #00,#00,#7F,#40,#FC,#04,#00,#00,#00,#00,#FF,#80,#FE,#02,#00,#00 ; #6c10 ...@............
db #01,#01,#FF,#00,#FF,#01,#00,#00,#03,#02,#FF,#00,#FF,#00,#80,#80 ; #6c20 
db #07,#04,#FF,#00,#FF,#00,#C0,#40,#03,#02,#FF,#00,#FF,#00,#80,#80 ; #6c30 .......@........
db #01,#01,#FF,#00,#FF,#01,#00,#00,#00,#00,#FF,#80,#FE,#02,#00,#00 ; #6c40 
db #00,#00,#7F,#40,#FC,#04,#00,#00,#00,#00,#3F,#30,#F8,#18,#00,#00 ; #6c50 ...@......?0....
db #00,#00,#0F,#0F,#E0,#E0,#00,#00,#00,#00,#07,#07,#E0,#E0,#00,#00 ; #6c60 
db #00,#00,#3F,#38,#FC,#1C,#00,#00,#00,#00,#7F,#40,#FE,#02,#00,#00 ; #6c70 ..?8.......@....
db #00,#00,#FF,#80,#FF,#01,#00,#00,#01,#01,#FF,#00,#FF,#00,#80,#80 ; #6c80 
db #03,#02,#FF,#00,#FF,#00,#C0,#40,#07,#04,#FF,#00,#FF,#00,#E0,#20 ; #6c90 .......@....... 
db #03,#02,#FF,#00,#FF,#00,#C0,#40,#01,#01,#FF,#00,#FF,#00,#80,#80 ; #6ca0 .......@........
db #00,#00,#FF,#80,#FF,#01,#00,#00,#00,#00,#7F,#40,#FE,#02,#00,#00 ; #6cb0 ...........@....
db #00,#00,#3F,#38,#FC,#1C,#00,#00,#00,#00,#07,#07,#E0,#E0,#00,#00 ; #6cc0 ..?8............
db #0F,#00,#F0,#00,#07,#07,#E0,#E0,#0F,#00,#23,#DD,#7E,#05,#FE,#FF ; #6cd0 ..........#.~...
db #3E,#FC,#28,#02,#3E,#04,#86,#77,#DD,#BE,#03,#28,#02,#30,#0A,#DD ; #6ce0 >.(.>..w...(.0..
db #7E,#03,#77,#DD,#36,#05,#01,#18,#0D,#DD,#BE,#04,#38,#08,#DD,#7E ; #6cf0 ~.w.6.......8..~
db #04,#77,#DD,#36,#05,#FF,#C1,#C9,#3A,#1A,#BF,#6F,#3A,#1B,#BF,#CD ; #6d00 .w.6....:..o:...
db #8B,#77,#21,#45,#BE,#22,#75,#BF,#AF,#32,#44,#BE,#CD,#C0,#85,#32 ; #6d10 .w!E."u..2D....2
db #73,#BF,#CD,#E6,#83,#AF,#32,#74,#BF,#3E,#08,#32,#71,#BF,#3E,#00 ; #6d20 s.....2t.>.2q.>.
db #32,#72,#BF,#CD,#FC,#85,#D5,#3A,#44,#BF,#57,#1E,#00,#CD,#17,#86 ; #6d30 2r.....:D.W.....
db #E3,#3A,#43,#BF,#57,#1E,#00,#CD,#FC,#7F,#F8,#3F,#FE,#7F,#F8,#1F ; #6d40 .:C.W......?....
db #FC,#7F,#F0,#1F,#FC,#3F,#F0,#0F,#FC,#3F,#F8,#3F,#E0,#0F,#F8,#3F ; #6d50 .....?...?.?...?
db #F8,#1F,#E0,#07,#F8,#1F,#F0,#1F,#C0,#07,#C0,#07,#F0,#1F,#F0,#0F ; #6d60 
db #C0,#03,#C0,#03,#F0,#0F,#E0,#0F,#80,#03,#80,#03,#E0,#0F,#F0,#0F ; #6d70 
db #C0,#03,#80,#01,#C0,#03,#F0,#0F,#E0,#0F,#80,#03,#00,#01,#80,#03 ; #6d80 
db #E0,#0F,#E0,#07,#80,#01,#00,#00,#80,#01,#E0,#07,#FF,#E0,#0F,#FF ; #6d90 
db #FF,#00,#01,#FF,#FE,#00,#00,#FF,#FE,#00,#00,#FF,#FF,#00,#01,#FF ; #6da0 
db #FF,#E0,#0F,#FF,#FF,#E0,#07,#FF,#FF,#00,#00,#FF,#FE,#00,#00,#7F ; #6db0 
db #FE,#00,#00,#7F,#FF,#00,#00,#FF,#FF,#E0,#07,#FF,#FF,#C0,#07,#FF ; #6dc0 
db #FE,#00,#00,#FF,#FC,#00,#00,#7F,#FC,#00,#00,#7F,#FE,#00,#00,#FF ; #6dd0 
db #FF,#C0,#07,#FF,#FF,#C0,#03,#FF,#FE,#00,#00,#7F,#FC,#00,#00,#3F ; #6de0 ...............?
db #FC,#00,#00,#3F,#FC,#00,#00,#3F,#FE,#00,#00,#7F,#FF,#C0,#03,#FF ; #6df0 ...?...?........
db #FF,#80,#03,#FF,#FC,#00,#00,#7F,#F8,#00,#00,#3F,#F8,#00,#00,#3F ; #6e00 ...........?...?
db #F8,#00,#00,#3F,#FC,#00,#00,#7F,#FF,#80,#03,#FF,#FF,#80,#01,#FF ; #6e10 ...?............
db #FC,#00,#00,#3F,#F8,#00,#00,#1F,#F8,#00,#00,#1F,#F8,#00,#00,#1F ; #6e20 ...?............
db #FC,#00,#00,#3F,#FF,#80,#01,#FF,#00,#00,#00,#00,#00,#00,#00,#00 ; #6e30 ...?............

db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #6e40 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #6e50 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #6e60 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #6e70 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #6e80 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #6e90 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #6ea0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #6eb0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #6ec0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #6ed0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #6ee0 
db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ; #6ef0 
