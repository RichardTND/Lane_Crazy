;========================================
;Lane Crazy 
;by Richard (Starhawk) Bayliss
;(C) 2021 Blazon Games Division
;========================================
;Lane Crazy - Picture + music linker
;========================================
        
vidram=$3f40
colram=$4328        
        !to "lane_crazy_disk.prg",cbm
  
        *=$0801
        !basic 2022,2064
        *=$0810
         sei
        lda $02a6
        sta system
        lda #251
        sta 808
       
        lda #$0b ;Clear screen 
        sta $d011
        lda #0
        sta $d020
        lda $4710
        sta $d021
        ldx #$00
paintpic 
        lda vidram,x
        sta $0400,x
        lda vidram+$100,x
        sta $0500,x
        lda vidram+$200,x
        sta $0600,x
        lda vidram+$2e8,x
        sta $06e8,x
        lda colram,x
        sta $d800,x
        lda colram+$100,x
        sta $d900,x
        lda colram+$200,x
        sta $da00,x
        lda colram+$2e8,x
        sta $dae8,x
        inx
        bne paintpic
        lda #$18
        sta $d018
        sta $d016
        lda #$03
        sta $dd00
        ldx #<irq
        ldy #>irq
        stx $0314
        sty $0315
        lda #$7f
        sta $dc0d
        sta $dd0d
        lda #$2e
        sta $d012
        lda #$3b
        sta $d011
        lda #$01
        sta $d019
        sta $d01a
        lda #0
        jsr $1000
        cli
        
        
mainloop
        lda $dc00
        lsr
        lsr
        lsr
        lsr
        lsr
        bcs ml2
        jmp exitintro
ml2     lda $dc01
        lsr
        lsr
        lsr
        lsr
        lsr
        bcs mainloop
exitintro
        sei
        lda #$00
        sta $d011
        ldx #$31
        ldy #$ea
        lda #$81
        stx $0314
        sty $0315
        sta $dc0d
        sta $dd0d
        lda #$00
        sta $d019
        sta $d01a
        
        lda #$1b
        sta $d011
        lda #$14
        sta $d018
        lda #$08
        sta $d016
        ldx #$00
trcopy  lda transfer,x
        sta $0400,x
        lda #$20
        sta $0500,x
        sta $0600,x
        sta $06e8,x
        lda #0
        sta $d800,x
        sta $d900,x
        sta $da00,x
        sta $dae8,x
        inx
        bne trcopy
       
        ldx #$00
silentchip
        lda #$00
        sta $d400,x
        inx
        cpx #$18
        bne silentchip
        lda #$00
        sta $d020
        sta $d021 
        sta $0800
        cli
        jmp $0400
transfer 
        sei
       
        lda #$34
        sta $01
tloop1  ldx #0
tloop2  lda $4800,x
        sta $0801,x
        inx
        bne tloop2
        inc $0409
        inc $040c
        lda $0409
        bne tloop1
        lda #$37
        sta $01
        jsr $a659
        jmp $a7ae
        
irq     asl $d019
        lda $dc0d
        sta $dd0d
        lda #$fa
        sta $d012
        jsr pnplayer
        jmp $ea7e
        
pnplayer 
        lda system
        cmp #1
        beq pal
        inc ntsctimer
        lda ntsctimer
        cmp #6
        beq resetntsc
pal     jsr $1003
        rts
resetntsc 
        lda #$00
nono    sta ntsctimer
        rts
system !byte 0
ntsctimer !byte 0        

*=$1000
!bin "bin\intromusic.prg",,2
*=$2000
!bin "bin\loaderpic.prg",,2        
*=$4800
!bin "lanecrazy.prg",,2
       
        
        
        
        