; initially retreived here https://github.com/cpcsdk/vasm_z80_oldstyle_eval/blob/master/good/all.asm




    ; Docume$20ted instructio$20s
;   ADC
    adc a,(hl)
    adc a,(ix + $05)
    adc a,(iy + $05)
    adc a,a
    adc a,b
    adc a,c
    adc a,d
    adc a,e
    adc a,h
    adc a,l
    adc a,$20

    adc hl,bc
    adc hl,de
    adc hl,hl
    adc hl,sp

;   ADD
    add a,(hl)
    add a,(ix + $05)
    add a,(iy + $05)
    add a,a
    add a,b
    add a,c
    add a,d
    add a,e
    add a,h
    add a,l
    add a,$20

    add hl,bc
    add hl,de
    add hl,hl
    add hl,sp

    add ix,bc
    add ix,de
    add ix,ix
    add ix,sp

    add iy,bc
    add iy,de
    add iy,iy
    add iy,sp

;   and
    and (hl)
    and (ix + $05)
    and (iy + $05)
    and a
    and b
    and c
    and d
    and e
    and h
    and l
    and $20

;   BIT
    bit 0,(hl)
    bit 0,(ix + $05)
    bit 0,(iy + $05)
    bit 0,a
    bit 0,b
    bit 0,c
    bit 0,d
    bit 0,e
    bit 0,h
    bit 0,l

    bit 1,(hl)
    bit 1,(ix + $05)
    bit 1,(iy + $05)
    bit 1,a
    bit 1,b
    bit 1,c
    bit 1,d
    bit 1,e
    bit 1,h
    bit 1,l

    bit 2,(hl)
    bit 2,(ix + $05)
    bit 2,(iy + $05)
    bit 2,a
    bit 2,b
    bit 2,c
    bit 2,d
    bit 2,e
    bit 2,h
    bit 2,l

    bit 3,(hl)
    bit 3,(ix + $05)
    bit 3,(iy + $05)
    bit 3,a
    bit 3,b
    bit 3,c
    bit 3,d
    bit 3,e
    bit 3,h
    bit 3,l

    bit 4,(hl)
    bit 4,(ix + $05)
    bit 4,(iy + $05)
    bit 4,a
    bit 4,b
    bit 4,c
    bit 4,d
    bit 4,e
    bit 4,h
    bit 4,l

    bit 5,(hl)
    bit 5,(ix + $05)
    bit 5,(iy + $05)
    bit 5,a
    bit 5,b
    bit 5,c
    bit 5,d
    bit 5,e
    bit 5,h
    bit 5,l

    bit 6,(hl)
    bit 6,(ix + $05)
    bit 6,(iy + $05)
    bit 6,a
    bit 6,b
    bit 6,c
    bit 6,d
    bit 6,e
    bit 6,h
    bit 6,l

    bit 7,(hl)
    bit 7,(ix + $05)
    bit 7,(iy + $05)
    bit 7,a
    bit 7,b
    bit 7,c
    bit 7,d
    bit 7,e
    bit 7,h
    bit 7,l

;   CALL
    call $0584

    call nz,$0584
    call z,$0584
    call nc,$0584
    call c,$0584
    call po,$0584
    call pe,$0584
    call p,$0584
    call m,$0584

;   CCF
    ccf

;   CP
    cp (hl)
    cp (ix + $05)
    cp (iy + $05)
    cp a
    cp b
    cp c
    cp d
    cp e
    cp h
    cp l
    cp $20

    cpd
    cpdr
    cpir
    cpi

;   CPL
    cpl

;   DAA
    daa

;   DEC
    dec (hl)
    dec (ix + $05)
    dec (iy + $05)
    dec a
    dec b
    dec c
    dec d
    dec e
    dec h
    dec l

    dec bc
    dec de
    dec hl
    dec ix
    dec iy
    dec sp

;   DI
    di

;   DJnz
  djnz $

;   EI
    ei

;   EX
    ex af,af'

    ex de,hl

    ex (sp),hl
    ex (sp),ix
    ex (sp),iy

    exx

;   HALT
    halt

;   IM
    im 0
    im 1
    im 2

;   in
    in a,(c)
    in b,(c)
    in c,(c)
    in d,(c)
    in e,(c)
    in h,(c)
    in l,(c)

    in a,($20)

    ind
    indr
    ini
    inir

;   inC
    inc (hl)
    inc (ix + $05)
    inc (iy + $05)
    inc a
    inc b
    inc c
    inc d
    inc e
    inc h
    inc l

    inc bc
    inc de
    inc hl
    inc ix
    inc iy
    inc sp

;   JP
    jp $0584

    jp (hl)
    jp (ix)
    jp (iy)

    jp nz,$0584
    jp z,$0584
    jp nc,$0584
    jp c,$0584
    jp po,$0584
    jp pe,$0584
    jp p,$0584
    jp m,$0584

;   JR
    jr $ + $22

    jr nz,$ + $22
    jr z,$ + $22
    jr nc,$ + $22
    jr c,$ + $22

;   LD
    ld (bc),a
    ld (de),a

    ld (hl),a
    ld (hl),b
    ld (hl),c
    ld (hl),d
    ld (hl),e
    ld (hl),h
    ld (hl),l
    ld (hl),$20

    ld (ix + $05),a
    ld (ix + $05),b
    ld (ix + $05),c
    ld (ix + $05),d
    ld (ix + $05),e
    ld (ix + $05),h
    ld (ix + $05),l
    ld (ix + $05),$20

    ld (iy + $05),a
    ld (iy + $05),b
    ld (iy + $05),c
    ld (iy + $05),d
    ld (iy + $05),e
    ld (iy + $05),h
    ld (iy + $05),l
    ld (iy + $05),$20

    ld ($0584),a

    ld ($0584),bc
    ld ($0584),de
    ld ($0584),hl
    ld ($0584),ix
    ld ($0584),iy

    ld ($0584),sp

    ld a,(bc)
    ld a,(de)
    ld a,(hl)
    ld a,(ix + $05)
    ld a,(iy + $05)
    ld a,($0584)
    ld a,a
    ld a,b
    ld a,c
    ld a,d
    ld a,e
    ld a,h
    ld a,l
    ld a,$20

    ld b,(hl)
    ld b,(ix + $05)
    ld b,(iy + $05)
    ld b,a
    ld b,b
    ld b,c
    ld b,d
    ld b,e
    ld b,h
    ld b,l
    ld b,$20

    ld c,(hl)
    ld c,(ix + $05)
    ld c,(iy + $05)
    ld c,a
    ld c,b
    ld c,c
    ld c,d
    ld c,e
    ld c,h
    ld c,l
    ld c,$20

    ld d,(hl)
    ld d,(ix + $05)
    ld d,(iy + $05)
    ld d,a
    ld d,b
    ld d,c
    ld d,d
    ld d,e
    ld d,h
    ld d,l
    ld d,$20

    ld e,(hl)
    ld e,(ix + $05)
    ld e,(iy + $05)
    ld e,a
    ld e,b
    ld e,c
    ld e,d
    ld e,e
    ld e,h
    ld e,l
    ld e,$20

    ld h,(hl)
    ld h,(ix + $05)
    ld h,(iy + $05)
    ld h,a
    ld h,b
    ld h,c
    ld h,d
    ld h,e
    ld h,h
    ld h,l
    ld h,$20

    ld l,(hl)
    ld l,(ix + $05)
    ld l,(iy + $05)
    ld l,a
    ld l,b
    ld l,c
    ld l,d
    ld l,e
    ld l,h
    ld l,l
    ld l,$20

    ld a,i
    ld i,a

    ld a,r
    ld r,a
    
    ld bc,($0584)
    ld de,($0584)
    ld hl,($0584)
    ld ix,($0584)
    ld iy,($0584)
    ld sp,($0584)

    ld bc,$0584
    ld de,$0584
    ld hl,$0584
    ld ix,$0584
    ld iy,$0584


    ld sp,hl
    ld sp,ix
    ld sp,iy
    ld sp,$0584

    ldd
    lddr
    ldi
    ldir

;   $20EG
    neg

;   $20OP
    nop

;   OR
    or (hl)
    or (ix + $05)
    or (iy + $05)
    or a
    or b
    or c
    or d
    or e
    or h
    or l
    or $20


;   OUT
    out (c),a
    out (c),b
    out (c),c
    out (c),d
    out (c),e
    out (c),h
    out (c),l
    out ($20),a

    outd
    otdr
    outi
    otir

;   POP
    pop af
    pop bc
    pop de
    pop hl
    pop ix
    pop iy

;   PUSH
    push af
    push bc
    push de
    push hl
    push ix
    push iy

;   RES
    res 0,(hl)
    res 0,(ix + $05)
    res 0,(iy + $05)
    res 0,a
    res 0,b
    res 0,c
    res 0,d
    res 0,e
    res 0,h
    res 0,l

    res 1,(hl)
    res 1,(ix + $05)
    res 1,(iy + $05)
    res 1,a
    res 1,b
    res 1,c
    res 1,d
    res 1,e
    res 1,h
    res 1,l

    res 2,(hl)
    res 2,(ix + $05)
    res 2,(iy + $05)
    res 2,a
    res 2,b
    res 2,c
    res 2,d
    res 2,e
    res 2,h
    res 2,l

    res 3,(hl)
    res 3,(ix + $05)
    res 3,(iy + $05)
    res 3,a
    res 3,b
    res 3,c
    res 3,d
    res 3,e
    res 3,h
    res 3,l

    res 4,(hl)
    res 4,(ix + $05)
    res 4,(iy + $05)
    res 4,a
    res 4,b
    res 4,c
    res 4,d
    res 4,e
    res 4,h
    res 4,l

    res 5,(hl)
    res 5,(ix + $05)
    res 5,(iy + $05)
    res 5,a
    res 5,b
    res 5,c
    res 5,d
    res 5,e
    res 5,h
    res 5,l

    res 6,(hl)
    res 6,(ix + $05)
    res 6,(iy + $05)
    res 6,a
    res 6,b
    res 6,c
    res 6,d
    res 6,e
    res 6,h
    res 6,l

    res 7,(hl)
    res 7,(ix + $05)
    res 7,(iy + $05)
    res 7,a
    res 7,b
    res 7,c
    res 7,d
    res 7,e
    res 7,h
    res 7,l

;   RET
    ret

    ret z
    ret nz
    ret c
    ret nc
    ret po
    ret pe
    ret p
    ret m

    reti
    retn

;   RL
    rl (hl)
    rl (ix + $05)
    rl (iy + $05)
    rl a
    rl b
    rl c
    rl d
    rl e
    rl h
    rl l

;   RLA
    rla

;   RLC
    rlc (hl)
    rlc (ix + $05)
    rlc (iy + $05)
    rlc a
    rlc b
    rlc c
    rlc d
    rlc e
    rlc h
    rlc l

;   RLCA
    rlca

;   RLD
    rld

;   RR
    rr (hl)
    rr (ix + $05)
    rr (iy + $05)
    rr a
    rr b
    rr c
    rr d
    rr e
    rr h
    rr l

;   RRA
    rra

;   RRC
    rrc (hl)
    rrc (ix + $05)
    rrc (iy + $05)
    rrc a
    rrc b
    rrc c
    rrc d
    rrc e
    rrc h
    rrc l

;   RRCA
    rrca

;   RRD
    rrd

;   RST
    rst $00
    rst $08
    rst $10
    rst $18
    rst $20
    rst $28
    rst $30
    rst $38

;   SBC
    sbc a,(hl)
    sbc a,(ix + $05)
    sbc a,(iy + $05)
    sbc a,a
    sbc a,b
    sbc a,c
    sbc a,d
    sbc a,e
    sbc a,h
    sbc a,l
    sbc a,$20

    sbc hl,bc
    sbc hl,de
    sbc hl,hl
    sbc hl,sp

;   SCF
    scf

;   SET
    set 0,(hl)
    set 0,(ix + $05)
    set 0,(iy + $05)
    set 0,a
    set 0,b
    set 0,c
    set 0,d
    set 0,e
    set 0,h
    set 0,l

    set 1,(hl)
    set 1,(ix + $05)
    set 1,(iy + $05)
    set 1,a
    set 1,b
    set 1,c
    set 1,d
    set 1,e
    set 1,h
    set 1,l

    set 2,(hl)
    set 2,(ix + $05)
    set 2,(iy + $05)
    set 2,a
    set 2,b
    set 2,c
    set 2,d
    set 2,e
    set 2,h
    set 2,l

    set 3,(hl)
    set 3,(ix + $05)
    set 3,(iy + $05)
    set 3,a
    set 3,b
    set 3,c
    set 3,d
    set 3,e
    set 3,h
    set 3,l

    set 4,(hl)
    set 4,(ix + $05)
    set 4,(iy + $05)
    set 4,a
    set 4,b
    set 4,c
    set 4,d
    set 4,e
    set 4,h
    set 4,l

    set 5,(hl)
    set 5,(ix + $05)
    set 5,(iy + $05)
    set 5,a
    set 5,b
    set 5,c
    set 5,d
    set 5,e
    set 5,h
    set 5,l

    set 6,(hl)
    set 6,(ix + $05)
    set 6,(iy + $05)
    set 6,a
    set 6,b
    set 6,c
    set 6,d
    set 6,e
    set 6,h
    set 6,l

    set 7,(hl)
    set 7,(ix + $05)
    set 7,(iy + $05)
    set 7,a
    set 7,b
    set 7,c
    set 7,d
    set 7,e
    set 7,h
    set 7,l

;   SLA
    sla (hl)
    sla (ix + $05)
    sla (iy + $05)
    sla a
    sla b
    sla c
    sla d
    sla e
    sla h
    sla l

;   SRA
    sra (hl)
    sra (ix + $05)
    sra (iy + $05)
    sra a
    sra b
    sra c
    sra d
    sra e
    sra h
    sra l

;   SRL
    srl (hl)
    srl (ix + $05)
    srl (iy + $05)
    srl a
    srl b
    srl c
    srl d
    srl e
    srl h
    srl l

;   SUB
    sub (hl)
    sub (ix + $05)
    sub (iy + $05)
    sub a
    sub b
    sub c
    sub d
    sub e
    sub h
    sub l
    sub $20

;   XOR
    xor (hl)
    xor (ix + $05)
    xor (iy + $05)
    xor a
    xor b
    xor c
    xor d
    xor e
    xor h
    xor l
    xor $20

    ; U$20docume$20ted instructio$20s
; in
 ;   in (c)      ; DEFB $ED,$70
    in f,(c)    ; DEFB $ED,$70

; OUT
;    out (c)     ; DEFB $ED,$71
;    out (c),f   ; DEFB $ED,$71

; SLL
    sll (hl)
    sll (ix+$05)
    sll (iy+$05)
    sll a
    sll b
    sll c
    sll d
    sll e
    sll h
    sll l

; IX and IY 8 bits halfs
    add a,ixh
    add a,ixl
    add a,iyh
    add a,iyl

    adc a,ixh
    adc a,ixl
    adc a,iyh
    adc a,iyl

    and ixh
    and ixl
    and iyh
    and iyl

    cp ixh
    cp ixl
    cp iyh
    cp iyl

    dec ixh
    dec ixl
    dec iyh
    dec iyl

    inc ixh
    inc ixl
    inc iyh
    inc iyl

    ld a,ixh
    ld b,ixh
    ld c,ixh
    ld d,ixh
    ld e,ixh
;    ld h,ixh
;    ld l,ixh

    ld a,ixl
    ld b,ixl
    ld c,ixl
    ld d,ixl
    ld e,ixl
;    ld h,ixl
;    ld l,ixl

    ld a,iyh
    ld b,iyh
    ld c,iyh
    ld d,iyh
    ld e,iyh
;    ld h,iyh
;    ld l,iyh

    ld a,iyl
    ld b,iyl
    ld c,iyl
    ld d,iyl
    ld e,iyl
;    ld h,iyl
;    ld l,iyl

    ld ixh,a
    ld ixh,b
    ld ixh,c
    ld ixh,d
    ld ixh,e
    ld ixh,ixh
    ld ixh,ixl
    ld ixh,$20

    ld ixl,a
    ld ixl,b
    ld ixl,c
    ld ixl,d
    ld ixl,e
    ld ixl,ixh
    ld ixl,ixl
    ld ixl,$20

    ld iyh,a
    ld iyh,b
    ld iyh,c
    ld iyh,d
    ld iyh,e
    ld iyh,iyh
    ld iyh,iyl
    ld iyh,$20

    ld iyl,a
    ld iyl,b
    ld iyl,c
    ld iyl,d
    ld iyl,e
    ld iyl,iyh
    ld iyl,iyl
    ld iyl,$20

    or ixh
    or ixl
    or iyh
    or iyl

    sbc a,ixh
    sbc a,ixl
    sbc a,iyh
    sbc a,iyl

    sub ixh
    sub ixl
    sub iyh
    sub iyl

    xor ixh
    xor ixl
    xor iyh
    xor iyl
 