;-------------------------------------
; SPACE SHOOTER
; MOS 6502 ASSEMBLER FOR C-64
; BY FREDRIK ROOS 2020
;-------------------------------------

        *=$0801 
        !BYTE $0C, $08, $0A, $00
        !BYTE $9E, $20 ,$34, $30
        !BYTE $39, $36, $00, $00, $00
        
        *=$1000

        jsr $e544
        
        ldx #0
        stx 53280
        ldx #0
        stx 53281
        ldx #1
        stx 646
        
        v = 53248
      
        ldx #13
        stx 2040
        ldx #14
        stx 2041
        ldx #1
        stx v+39
        stx v+40
        ldx #0
        stx v+23
        stx v+29
      
        ldx #%00000011
        stx v+16
        ldx #60
        stx v  
        ldx #50
        stx v+1
        stx v+3
        ldx #36
        stx v+2
        
        ldx #200  
        stx 2042
        ldx #%01111100
        stx v+28
        ldx #10
        stx v+37
        ldx #7
        stx v+38
        ldx #1
        stx v+41
        ldx #200
        stx v+4
        ldx #230
        stx v+5
        jsr ani_ship
        
        ldx #201
        stx 2043
        ldx #1
        stx v+42
        ldx #0
srdg    lda shot,x 
        sta 12864,x
        inx
        cpx #63
        bne srdg
        
        ldx #200
        stx v+6
        stx v+7
        
        ;-----------------------
        ; foe1
        ldx #202
        stx 2044
        ldx #1
        stx v+43
        ldx #50
        stx v+8
        stx v+9
        ;-----------------------
        ; foe2
        ldx #203
        stx 2045
        ldx #1
        stx v+44
        ldx #108
        stx v+10
        ldx #90
        stx v+11
        ;-----------------------
        ; foe3
        ldx #204
        stx 2046
        ldx #1
        stx v+45
        ldx #250
        stx v+12
        ldx #90
        stx v+13
        jsr ani_foes
        
        ldx #%01100000
        stx v+21
        
        jsr draw_stars

        jsr clean
        lda #2
        ldx #0
        jsr sprnum

        lda #2
        ldx #0
        jsr sprnum        

        ldx #23
        stx 53272
  
       ;jmp clear ; skip startscr
startscr
        inc strtscrf
        ldx #6
        stx 214
        ldx #14
        stx 211
        jsr 58732
        ldx #<title
        lda #>title
        jsr string
        ldx #9
        stx 214
        ldx #9
        stx 211
        jsr 58732
        ldx #<cpr
        lda #>cpr
        jsr string
        ldx #13
        stx 214
        ldx #16
        stx 211
        jsr 58732
        ldx #<kl1
        lda #>kl1
        jsr string
        ldx #15
        stx 214
        ldx #16
        stx 211
        jsr 58732
        ldx #<kl2
        lda #>kl2
        jsr string
        ldx #17
        stx 214
        ldx #12
        stx 211
        jsr 58732
        ldx #<kl3
        lda #>kl3
        jsr string

        lda v+21
        eor #%00000100
        sta v+21
        
        ldx #1
        stx dir
uin     
        jsr $ff94
        jsr $ffe4
        cmp #32
        beq clear
        
        jsr sleep
        jsr ani_ship
        jsr ani_foes
        jsr move_foes
        jsr move_ship
        ldx dir
        cpx #0
        bne go1
        ldx v+4
        cpx #60
        bne go0
        ldx #2
        stx dir
go0
        cpx #30
        bne go1
        ldx #1
        stx dir
go1        
        jmp uin
;------------------------------
string
        stx $fa
        sta $fb
        
        ldy #0
strl    lda ($fa),y
        cmp #0
        beq stret
        jsr $e716
        iny
        tya
        pha
        jsr shot_s
        ldx #0
        ldy #0
stt     inx
        bne stt
        iny
        cpy #32
        bne stt
        pla
        tay
        jmp strl
stret        
        rts 
;----------------------------------------------------
uin_r
        jmp uin
;-----------------------------------

clear
        dec strtscrf
        jsr $e544
        jsr draw_stars
        ldx #%01110111
        stx v+21
        ldx #0
        stx dir
        
        ldx #0
        stx v+9
        ldx #40
        stx v+11
        ldx #20
        stx v+13
        jsr showscore
        
;-----------------------------------       
; main game loop
;-----------------------------------
main
        ldx strtscrf
        cpx #1
        beq uin_r

        jsr sleep

        jsr $ff94
        jsr $ffe4
        sta key
        
        cmp #87
        bne ss
        lda foe_t
        eor #%10000000
        sta foe_t
ss        
        lda key
        cmp #68     ;right
        bne s1
        ldy dir
        cpy #2
        bne _s1
        ldx #0
        stx dir
        jmp s1
_s1     ldx #1
        stx dir
s1        
        lda key
        cmp #65     ;left
        bne s2
        ldy dir
        cpy #1
        bne _s2
        ldx #0
        stx dir
        jmp s2
_s2     ldx #2
        stx dir
        
s2      lda key
        cmp #83
        beq fire_r
        
head    
        jsr ani_ship
        jsr ani_foes
        jsr move_foes
        jsr check_col
        jsr move_shot
        jsr move_ship
    
        lda key
        cmp #13
        beq end
   
        jmp main
end
        rts
;------------------------------
fire_r
        jmp fire
;------------------------------
check_col

;       sta 12928,y
;       sta 12992,y
;       sta 13056,y

        lda 53278
        sta colis
        and #%00010000
        cmp #%00010000
        bne c1
        lda v+21
        eor #%10001000
        sta v+21
        ldx #<12928
        stx $fc
        ldx #>12928
        stx $fd
        jsr ani_crash
        ldx #0
        stx v+9
        ldx #220
        stx v+7
        jsr showscore
        jmp cret
c1
        lda colis
        and #%00100000
        cmp #%00100000
        bne c2
        lda v+21
        eor #%10001000
        sta v+21
        ldx #<12992
        stx $fc
        ldx #>12992
        stx $fd
        jsr ani_crash
        ldx #0
        stx v+11
        ldx #220
        stx v+7
        jsr showscore
        jmp cret
c2
        lda colis
        and #%01000000
        cmp #%01000000
        bne cret
        lda v+21
        eor #%00001000
        sta v+21
        ldx #<13056
        stx $fc
        ldx #>13056
        stx $fd
        jsr ani_crash
        ldx #0
        stx v+13
        ldx #220
        stx v+7
        jsr showscore

cret
        rts
;------------------------------
fire
        
        lda v+21
        and #%00001000
        cmp #%00001000
        beq fret
        ldx #0
        stx temp
        ldx v+4
        stx v+6
        ldx #220
        stx v+7
        lda v+21
        eor #%00001000
        sta v+21
        jsr shot_s
fret
        jmp head
;------------------------------
crash_s 
        rts
;------------------------------
shot_s
      
        ldx #255
shls    dex
        stx sv
        ldy #129
        sty sw
        ldy #15
        sty sa
        ldy #40
        sty sh
        ldy #200
        sty sl
        cpx #0
        bne shls
        ldy #0
        sty sw
        sty sa
        rts
;---------------------------------
move_shot
        lda v+21
        and #%00001000
        cmp #%00001000
        bne ffret
        dec v+7
        dec v+7
        dec v+7
        dec v+7
        ldx v+7
        cpx #20
        bcs ffret
        lda v+21
        eor #%00001000
        sta v+21
        ldx #220
        stx v+7
ffret
        rts
;------------------------------
move_ship
        ldx dir
        cpx #0
        beq msret
           
        cpx #1
        beq ship_right
        
        cpx #2
        beq ship_left
msret
        rts
;------------------------------
ship_right
        lda v+16
        and #%00000100
        cmp #%00000100
        bne _srr
        ldx v+4
        cpx #60
        beq reset_dir
_srr
        inc v+4
        ldx v+4
        cpx #255
        bcc srr
        lda v+16
        eor #%00000100
        eor #%00001000
        sta v+16
srr
        jmp main
;------------------------------
reset_dir
        ldx #0
        stx dir
        jmp main
;------------------------------
ship_left
        lda v+16
        and #%00000100
        cmp #%00000100
        beq _sll
        ldx v+4
        cpx #30
        beq reset_dir
        
_sll
        dec v+4
        ldx v+4
        cpx #20
        cpx #0
        bne sll
        lda v+16
        eor #%00000100
        eor #%00001000
        sta v+16
sll
        jmp main
;------------------------------
ani_ship
       
        ldy ship_i
        ldx ship_a,y
        ldy #0
ranis   lda ship,x
        sta 12800,y
        inx
        iny
        cpy #64
        bne ranis
        
        inc ship_i
        ldy ship_i
        cpy #32
        bne saret
        ldy #0
        sty ship_i
saret        
        rts
;------------------------------
ani_foes

        lda #<foe
        clc
        adc foe_t
        sta $fa
        lda #>foe
        sta $fb

        ldx foe_i
        ldy foe_a,x
        ldx #0
fanis   lda ($fa),y ; lda foe+128,x
        sta 12928,x
        sta 12992,x
        sta 13056,x
        inx
        iny
        cpx #64
        bne fanis
        
        inc foe_i
        ldy foe_i
        cpy #32
        bne faret
        ldy #0
        sty foe_i
faret        
        rts
;------------------------------
move_foes

        ldx foei
        cpx fy_spd
        beq mfo
        inc foei
        rts
mfo     
        ldx #0
        stx foei
        inc v+9
        inc v+11
        inc v+13
        
        ldx v+9
        cpx #250
        bne mf1
        ldx #0
        ldx v+9
mf1        
        ldx v+11
        cpx #250
        bne mf2
        ldx #0
        ldx v+11
mf2
        ldx v+13
        cpx #250
        bne mf3
        ldx #0
        ldx v+13
mf3        
        rts
;------------------------------
ani_crash

        ldx #0
        ldy #0
ard     lda crash,x
        sta ($fc),y
        inx
        iny

        txa
        pha
        ldx #0
sard    inx
        cpx #64
        bne sard
        pla
        tax
        
        cpy #64
        bne ssa
        ldy #0
            
ssa     cpx #192
        bne ard

        jsr crash_s
        rts
;------------------------------
showscore

        ldx #0
        stx sptr
  
        sed
        ldx #2
        jsr plow
        tax
        lda #2
        cld
        jsr sprnum
        sed
        ldx #2
        jsr plhi
        tax
        lda #1
        cld
        jsr sprnum
        sed
        ldx #1
        jsr plow
        tax
        lda #0
        cld
        jsr sprnum
      
        ldx #64
        stx sptr
      
        sed
        ldx #1
        jsr plhi
        tax
        lda #2
        cld
        jsr sprnum

        sed
        ldx #0
        jsr plow
        tax
        lda #1
        cld
        jsr sprnum

        sed
        ldx #0
        jsr plhi
        tax
        lda #0
        cld
        jsr sprnum

        jsr addscore
        jmp head
;------------------------------------
addscore
        sed
        lda score+2
        clc
        adc #$50
        sta score+2
        bcc ahead
        lda score+1
        clc
        adc #$01
        sta score+1
        bcc ahead
        lda score
        clc
        adc #$01
        sta score
ahead   cld
        rts
;------------------------------------
plow
        lda score,x
        asl a
        asl a
        asl a
        asl a
        jsr div
        tya
        rts
;------------------------------------
plhi
        lda score,x
        lsr a
        lsr a
        lsr a
        lsr a
        rts
;------------------------------------
clean
        ldy #0
        lda #0
cl      sta 832,y
        iny
        cpy #128
        bne cl
        rts
;------------------------------------
sprnum      
        pha
        lda #0
        lda #<numbers
        clc
        adc nindex,x
        sta $fa
        lda #>numbers
        sta $fb
      
        ldy #0
        pla
        clc
        adc sptr
        tax
srd     lda ($fa),y
        sta 832,x
        inx
        inx
        inx
        iny
        cpy #7
        bne srd
        rts
;---------------------------------------
div     ldy #0
        cmp #0
        beq dr
divl    sec
        sbc #10
        iny
        cmp #10
        bcs divl
dr      rts
;---------------------------------------
draw_stars
        ldy #0
        lda #46
dloop   ldx stars+1,y
        cpx #0
        beq send
        stx $fa
        ldx stars,y
        stx $fb
        sta ($fa),y
        pha
        lda $fb
        clc
        adc #$d4
        sta $fb
        lda #1
        sta ($fa),y
        pla
        sta $fb
        iny
        jmp dloop
send
        rts     
;---------------------------------------
sleep
        ldx #0
        ldy #0
sls     iny
        bne sls
        inx
        cpx #2
        bne sls
        rts
;------------------------------------         
title   !text "space shooter"
        !byte 0
cpr     !text "crazedoutsoft (c) 2020"
        !byte 0
kl1     !text "a - left"
        !byte 0
kl2     !text "d - right"
        !byte 0
kl3     !text "s or space - fire"
        !byte 0
foei    !byte 0
fy_spd  !byte 3
sv      =54296
sw      =54276
sa      =54277
sh      =54273
sl      =54272
strtscrf !byte 0                
nindex   !byte 0,7,14,21,28,35,42,49,56,63
score    !byte 0,0,0
sptr     !byte 0
key      !byte 0
ship_a   !byte 0,0,0,0,0,0,0,0,0,0,0,0
         !byte 0,0,0,0,64,64,64,64,64
         !byte 64,64,64,64,64,64,64,64
         !byte 64,64,64
ship_i   !byte 0
foe_a    !byte 0,0,0,0,0,0,0,0,0,0,0,0
         !byte 0,0,0,0,64,64,64,64,64
         !byte 64,64,64,64,64,64,64,64
         !byte 64,64,64
foe_i    !byte 0
foe_t    !byte 0
dir      !byte 0
temp     !byte 0
colis    !byte 0

numbers  
!byte $7e,$7e,$66,$66,$66,$7e,$7e
!byte $38,$38,$18,$18,$18,$3c,$3c
!byte $7e,$7e,$02,$7e,$40,$7e,$7e
!byte $7e,$7e,$06,$1e,$06,$7e,$7e
!byte $66,$66,$66,$7e,$7e,$06,$06
!byte $7e,$7e,$40,$7e,$02,$7e,$7e
!byte $7e,$7e,$60,$7e,$66,$7e,$7e
!byte $7e,$7e,$0e,$0e,$0e,$0e,$0e
!byte $7e,$7e,$66,$7e,$66,$7e,$7e
!byte $7e,$7e,$66,$7e,$7e,$06,$06
!byte $00,$00

ship
!byte  $00,$00,$00,$00,$00,$00,$00,$14
!byte  $00,$08,$14,$20,$08,$55,$20,$09
!byte  $55,$60,$09,$41,$60,$08,$41,$20
!byte  $00,$41,$00,$00,$55,$00,$00,$55
!byte  $00,$00,$55,$00,$00,$41,$00,$08
!byte  $41,$20,$08,$41,$20,$09,$55,$60
!byte  $09,$55,$60,$08,$55,$20,$08,$00
!byte  $20,$08,$00,$20,$00,$00,$00,$81

!byte  $00,$00,$00,$00,$00,$00,$00,$14
!byte  $00,$08,$14,$20,$08,$55,$20,$09
!byte  $55,$60,$09,$7d,$60,$08,$7d,$20
!byte  $00,$7d,$00,$00,$55,$00,$00,$55
!byte  $00,$00,$55,$00,$00,$7d,$00,$08
!byte  $7d,$20,$08,$7d,$20,$09,$55,$60
!byte  $09,$55,$60,$08,$55,$20,$08,$3c
!byte  $20,$08,$00,$20,$00,$00,$00,$81

shot
!byte  $00,$00,$00,$00,$00,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$00
!byte  $08,$00,$20,$08,$00,$20,$08,$00
!byte  $20,$08,$00,$20,$00,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$08,$00,$20
!byte  $08,$00,$20,$08,$00,$20,$08,$00
!byte  $20,$00,$00,$00,$00,$00,$00,$81

foe
!byte  $00,$00,$00,$00,$00,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$08
!byte  $00,$20,$08,$14,$20,$08,$14,$20
!byte  $08,$7d,$20,$09,$ff,$60,$09,$c3
!byte  $60,$09,$ff,$60,$08,$7d,$20,$08
!byte  $14,$20,$08,$14,$20,$08,$00,$20
!byte  $00,$00,$00,$00,$00,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$81
!byte  $00,$00,$00,$00,$00,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$08
!byte  $00,$20,$08,$14,$20,$0c,$14,$30
!byte  $08,$7d,$20,$0d,$ff,$70,$09,$d7
!byte  $60,$0d,$ff,$70,$08,$7d,$20,$0c
!byte  $14,$30,$08,$14,$20,$08,$00,$20
!byte  $00,$00,$00,$00,$00,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$81

!byte  $00,$00,$00,$00,$00,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$00
!byte  $a8,$00,$03,$33,$00,$02,$aa,$00
!byte  $09,$55,$80,$24,$a8,$60,$50,$00
!byte  $14,$00,$00,$00,$00,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$81
!byte  $00,$00,$00,$00,$00,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$00
!byte  $a8,$00,$03,$77,$00,$82,$aa,$08
!byte  $a9,$55,$a8,$54,$a8,$54,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$81

stars
!byte $04,$5c,$04,$67,$04,$7c,$04,$9b
!byte $04,$ec,$05,$01,$05,$0c,$05,$45
!byte $05,$9e,$06,$03,$06,$0c,$06,$22
!byte $06,$28,$06,$6c,$06,$89,$06,$e0
!byte $06,$e6,$06,$fb,$07,$44,$07,$64,$00,$00

crash
!byte  $00,$00,$00,$00,$00,$00,$03,$23
!byte  $00,$00,$00,$00,$00,$c4,$84,$02
!byte  $00,$00,$00,$0c,$00,$08,$b0,$80
!byte  $00,$0b,$08,$03,$02,$40,$08,$80
!byte  $40,$01,$04,$20,$00,$cb,$80,$00
!byte  $92,$00,$02,$20,$00,$00,$01,$20
!byte  $00,$20,$00,$00,$00,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$81
!byte  $00,$20,$00,$20,$00,$20,$03,$23
!byte  $00,$00,$00,$00,$80,$c4,$84,$02
!byte  $00,$02,$00,$00,$00,$08,$00,$80
!byte  $00,$00,$08,$03,$00,$40,$80,$00
!byte  $40,$01,$00,$22,$20,$00,$80,$00
!byte  $82,$00,$02,$00,$00,$00,$01,$20
!byte  $20,$20,$02,$00,$00,$00,$00,$00
!byte  $80,$00,$80,$00,$00,$08,$00,$81
!byte  $00,$20,$00,$20,$00,$20,$03,$23
!byte  $00,$00,$00,$00,$80,$00,$04,$00
!byte  $00,$02,$00,$00,$00,$00,$00,$00
!byte  $00,$00,$08,$00,$00,$00,$80,$00
!byte  $00,$00,$00,$02,$20,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$20
!byte  $20,$00,$02,$00,$00,$00,$00,$00
!byte  $80,$00,$80,$00,$00,$08,$00,$81

