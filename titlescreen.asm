;========================================
;Lane Crazy 
;by Richard (Starhawk) Bayliss
;(C) 2021 Blazon Games Division
;========================================
;Title screen code
;========================================

title_screen
        sei 
        jsr clearint
        
        lda #<scrolltext
        sta messread+1
        lda #>scrolltext
        sta messread+2
        lda #0
        sta case
        sta char
        sta textpointer
        sta firebutton
        sta joydown_p1
        sta joyup_p1
        sta joydown_p2
        sta joyup_p2
        ldx #$00
cleartext
        lda #$20
        sta textscreen,x
        inx
        cpx #80
        bne cleartext
        ldx #$00
clearscreenfull
        lda #$20
        sta screen,x
        sta screen+$100,x
        sta screen+$200,x
        sta screen+$2e8,x
        lda #0
        sta colour,x
        sta colour+$100,x
        sta colour+$200,x
        sta colour+$2e8,x
        inx
        bne clearscreenfull
        
        lda #0
        sta $d020
        sta $d021
        
        ;Setup title screen logo colour 
        ldx #$00
copycolrs
        lda logocolour,x
        sta colour,x
        lda logocolour+(1*40),x 
        sta colour+(1*40),x
        lda logocolour+(2*40),x
        sta colour+(2*40),x
        lda logocolour+(3*40),x
        sta colour+(3*40),x
        lda logocolour+(4*40),x
        sta colour+(4*40),x
        lda logocolour+(5*40),x
        sta colour+(5*40),x
        lda logocolour+(6*40),x
        sta colour+(6*40),x
        lda logocolour+(7*40),x
        sta colour+(7*40),x
        lda logocolour+(8*40),x
        sta colour+(8*40),x
        lda logocolour+(9*40),x
        sta colour+(9*40),x
        lda logocolour+(10*40),x
        sta colour+(10*40),x
        lda #$00
        sta colour+(11*40),x
        sta $8400+(11*40),x
        inx
        cpx #$28
        bne copycolrs
        lda #0
        sta $d015
      
        ;Setup title screen interrupts
        
        ldx #<tirq1
        ldy #>tirq1
        lda #$7f
        stx $0314
        sty $0315
        sta $dc0d
        sta $dd0d
        lda #$2e
        sta $d012
        lda #$1b
        sta $d011
        lda #$01
        sta $d01a
        sta $d019
        lda #$00
        jsr music2init
        cli
        jmp titleloop
;---------------------------------------
;Main IRQ Raster interrupt for front end
;---------------------------------------
        ;Smooth scroll
tirq1    asl $d019
        lda $dc0d
        sta $dd0d
        lda #$2e
        sta $d012
        nop
        nop
        
        lda #0
        sta $d020
        lda #$1b 
        sta $d011
        lda xpos
        sta $d016 
        lda #$1c 
        sta $d018 
        lda #$03
        sta $dd00
       
      
        ;Raster bar for scroll text
        ldy $d012
        ldx #scrollcolourend-scrollcolour
rloop1        
        lda scrollcolour,x 
        cpy $d012
        beq *-3 
        sta $d021
        iny
        dex
        bpl rloop1
        nop 
       
        ldx #<tirq2
        ldy #>tirq2 
        stx $0314
        sty $0315
        jmp $ea7e
        
tirq2    ;Logo displayer 
        asl $d019 
        lda #$8e
        sta $d012
        
        lda #$3b 
        sta $d011
        lda #$18
        sta $d016 
        lda #$18 
        sta $d018 
        lda #$01
        sta $dd00
        lda #1
        sta $d021
    ;    lda #2
    ;    sta $d020 
        ldx #<tirq3
        ldy #>tirq3 
        stx $0314
        sty $0315
        jmp $ea7e 
        
tirq3    ;Text fader 
        asl $d019 
        lda #$e2 
        sta $d012 
        
        
        lda #$1b 
        sta $d011
        lda #$08
        sta $d016 
        lda #$1c 
        sta $d018 
        lda #$03
        sta $dd00
        lda fadestore
        sta $d021
        
       ; lda #3
       ; sta $d020
        jsr pnplayer
        ldx #<tirq1
        ldy #>tirq1
        stx $0314
        sty $0315
        jmp $ea7e 
        
titleloop
        jsr sync_timer
        jsr scrolltext_routine
        jsr fadetext_routine
        jsr scroll_colour
        lda $dc01
        lsr
        lsr
        lsr
        lsr
        lsr
        bit firebutton
        ror firebutton
        bmi jp2
        bvc jp2 
        jmp options_screen
jp2     lda $dc00
        lsr
        lsr
        lsr
        lsr
        lsr
        bit firebutton
        ror firebutton 
        bmi titleloop
        bvc titleloop
        jmp options_screen

;---------------------------------
;Scroll text routine
;---------------------------------        
scrolltext_routine
        lda xpos
        sec
        sbc #2
        and #7
        sta xpos
        bcs exitscroll
        ldx #$00
shiftcolumns
        lda $0799,x
        sta $0798,x
        inx
        cpx #$50
        bne shiftcolumns
messread
        lda scrolltext
        cmp #0
        bne store 
        lda #<scrolltext
        sta messread+1
        lda #>scrolltext
        sta messread+2
        jmp messread
store   sta char
        lda case 
        cmp #1
        beq upper
lower   lda char
        sta $07bf
        eor #$80
        sta $07e7 
        lda #1
        sta case
        rts 
upper   lda char
        eor #$40 
        sta $07bf
        eor #$80
        sta $07e7 
        lda #0
        sta case
        inc messread+1
        bne exitscroll
        inc messread+2
exitscroll rts        
        
        
;---------------------------------
;Fadetext routine
;---------------------------------        
fadetext_routine
         lda fadedelay
         cmp #3
         beq fadeok
         inc fadedelay 
         rts
fadeok   lda #0
         sta fadedelay
         ldx fadepointer
         lda fadecolourtable,x
         sta fadestore
         inx
         cpx #fadecolourtableend-fadecolourtable 
         beq switchnext
         inc fadepointer
         rts 
switchnext 
         ldx #0
         stx fadepointer 
         ldx textpointer 
         lda line_table_lo,x
         sta textstore+1
         lda line_table_hi,x
         sta textstore+2
         inx 
         cpx #texttableend-texttable
         beq resettext 
         inc textpointer 
         jmp storenext
resettext
        ldx #0
        stx textpointer 
storenext
        ldx #$00
textstore
        lda line1,x
        sta textscreen,x
        inx
        cpx #80
        bne textstore 
        
;Copy text to main screen        
        ldx #$00
        ldy #$00
loop2x2 txa 
        lda textscreen,x
        sta screen+(13*40),y
        eor #$40
        sta screen+(13*40)+1,y
        lda textscreen,x 
        eor #$80
        sta screen+(14*40),y 
        eor #$40 
        sta screen+(14*40)+1,y 
        
        lda textscreen+(1*20),x
        sta screen+(15*40),y
        eor #$40
        sta screen+(15*40)+1,y
        lda textscreen+(1*20),x 
        eor #$80
        sta screen+(16*40),y 
        eor #$40 
        sta screen+(16*40)+1,y 
        
        lda textscreen+(2*20),x
        sta screen+(17*40),y
        eor #$40
        sta screen+(17*40)+1,y
        lda textscreen+(2*20),x 
        eor #$80
        sta screen+(18*40),y 
        eor #$40 
        sta screen+(18*40)+1,y 
        
        lda textscreen+(3*20),x
        sta screen+(19*40),y
        eor #$40
        sta screen+(19*40)+1,y
        lda textscreen+(3*20),x 
        eor #$80
        sta screen+(20*40),y 
        eor #$40 
        sta screen+(20*40)+1,y 
        inx
        iny
        iny
        cpy #$28
        bne loop2x2
        rts
        
;--------------------------------
;Scroll colour text        
;--------------------------------        
scroll_colour
        lda scrolldelay 
        cmp #3
        beq scrollok 
        inc scrolldelay
        rts 
scrollok 
        lda #0
        sta scrolldelay 
        lda scrollcolourend-1
        pha
        ldx #scrollcolourend-scrollcolour
shiftscroll 
        lda scrollcolour-1,x
        sta scrollcolour,x
        dex
        bpl shiftscroll
        pla
        sta scrollcolour
        rts
        
        
         
;---------------------------------
; PAL/NTSC detection music player        
; routine
;---------------------------------

pnplayer  lda #1
          sta rt
          lda system
          cmp #1
          beq paltitle 
          inc ntsctimer
          lda ntsctimer
          cmp #6
          beq resetntsctitle
paltitle  jsr music2play
          rts
resetntsctitle 
          lda #0
          sta ntsctimer
          rts
 
;---------------------------------------------
;Game Options screen code (IRQ is still on)
;---------------------------------------------
options_screen
          ldx #$00
removealltext
          lda #$20
plot      sta screen+(13*40),x
          sta screen+(14*40),x
          sta screen+(15*40),x
          sta screen+(16*40),x
          sta screen+(17*40),x
          sta screen+(18*40),x 
          sta screen+(19*40),x 
          sta screen+(20*40),x
          sta screen+(21*40),x
          sta screen+(22*40),x
          inx
          cpx #40
          bne plot
          
          ;Display the options screen text 
          lda #1
          sta fadestore
          
          ldx #$00
          ldy #$00
fetch_option_text          
          txa
          lda gameoptions,x
          sta screen+(13*40),y
          eor #$40
          sta screen+(13*40)+1,y 
          lda gameoptions,x 
          eor #$80 
          sta screen+(14*40),y 
          eor #$40
          sta screen+(14*40)+1,y 
          
          lda gameoptions+(1*20),x
          sta screen+(17*40),y 
          eor #$40
          sta screen+(17*40)+1,y
          lda gameoptions+(1*20),x
          eor #$80
          sta screen+(18*40),y
          eor #$40
          sta screen+(18*40)+1,y 
          
          lda gameoptions+(2*20),x
          sta screen+(19*40),y
          eor #$40
          sta screen+(19*40)+1,y 
          lda gameoptions+(2*20),x
          eor #$80
          sta screen+(20*40),y
          eor #$40
          sta screen+(20*40)+1,y
          inx
          iny
          iny
          cpy #$28
          bne fetch_option_text
          lda #0
          sta firebutton
;----------------------------------------
;Like with the title screen. Keep the
;scrolling message active and colour
;rasters cycling. Also use joystick 
;control to select option.
;----------------------------------------
          
game_options_loop
          jsr sync_timer
          jsr scrolltext_routine
          jsr scroll_colour
          jsr option_selector 
          
          
          lda #1
          bit $dc00
          bne jp2optup
          lda #0
          sta gameoption 
          jmp game_options_loop 
          
jp2optup  bit $dc01 
          bne jp1optdown 
          lda #0
          sta gameoption 
          jmp game_options_loop 
          
jp1optdown 
          lda #2
          bit $dc00 
          bne jp2optdown 
          lda #1
          sta gameoption 
          jmp game_options_loop 
          
jp2optdown
         
          bit $dc01 
          bne j2optfire 
          lda #1
          sta gameoption 
          jmp game_options_loop
          
j2optfire lda $dc00
          lsr
          lsr
          lsr
          lsr
          lsr
          bit firebutton
          ror firebutton 
          bmi j1optfire
          bvc j1optfire
          lda #0
          sta firebutton
          jmp check_option_before_play 
          
j1optfire lda $dc01
          lsr
          lsr
          lsr
          lsr
          lsr
          bit firebutton
          ror firebutton
          bmi skip_options
          bvc skip_options 
          jmp check_option_before_play 
          
skip_options
          jmp game_options_loop
          
check_option_before_play
          lda gameoption 
          beq play_game 
          jmp display_hiscores
          jmp game_options_loop 
          
play_game jmp game_code          
          
          
;-----------------------------------------------
;Game options selector up/down should display 
;the arrow chars next to the game option.
;-----------------------------------------------

option_selector
            lda gameoption
            beq highlight_start_game
            jmp highlight_view_hi_scores
            
;------------------------------------------------
highlight_start_game
            lda #$24
            sta screen+(17*40)
            lda #$24+$40
            sta screen+(17*40)+1
            lda #$24+$80
            sta screen+(18*40)
            lda #$24+$c0 
            sta screen+(18*40)+1
            lda #$25 
            sta screen+(17*40)+38 
            lda #$25+$40 
            sta screen+(17*40)+39 
            lda #$25+$80 
            sta screen+(18*40)+38 
            lda #$25+$c0 
            sta screen+(18*40)+39
            lda #$20
            sta screen+(19*40) 
            sta screen+(19*40)+1
            sta screen+(20*40)
            sta screen+(20*40)+1 
            sta screen+(19*40)+38
            sta screen+(19*40)+39
            sta screen+(20*40)+38
            sta screen+(20*40)+39
            rts
            
highlight_view_hi_scores
            
            lda #$24
            sta screen+(19*40)
            lda #$24+$40
            sta screen+(19*40)+1
            lda #$24+$80
            sta screen+(20*40)
            lda #$24+$c0 
            sta screen+(20*40)+1
            lda #$25 
            sta screen+(19*40)+38 
            lda #$25+$40 
            sta screen+(19*40)+39 
            lda #$25+$80 
            sta screen+(20*40)+38 
            lda #$25+$c0 
            sta screen+(20*40)+39
            lda #$20
            sta screen+(17*40) 
            sta screen+(17*40)+1
            sta screen+(18*40)
            sta screen+(18*40)+1 
            sta screen+(17*40)+38
            sta screen+(17*40)+39
            sta screen+(18*40)+38
            sta screen+(18*40)+39
            rts
          
xpos !byte 0        
fadedelay !byte 0
fadepointer  !byte 0
fadestore !byte 0
textpointer !byte 0
scrolldelay !byte 0
gameoption !byte 0
case !byte 0
char !byte 0
texttable        
line_table_lo !byte <line1,<line2,<line3,<line4,<line5,<line6,<line7,<line8
texttableend
line_table_hi !byte >line1,>line2,>line3,>line4,>line5,>line6,>line7,>line8

fadecolourtable   
             !byte $06,$06,$09,$02,$0b,$04,$08,$0c,$0e,$05,$0a,$03,$0f,$07,$0d,$01 
              !byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
              !byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01 
              !byte $01,$0d,$07,$0f,$03,$0a,$05,$0e,$0c,$08,$04,$0b,$02,$09,$06,$06,$00,$00
fadecolourtableend              

scrollcolour !byte $06,$09,$02,$0b,$04,$08,$0c,$0e,$05,$0a,$03,$0f,$07,$0d,$01 
              !byte $01,$0d,$07,$0f,$03,$0a,$05,$0e,$0c,$08,$04,$0b,$02,$09,$06,$09,$02,$0b,$04,$08,$0c,$0e,$05,$0a,$03,$0f,$07,$0d,$01 
              !byte $01,$0d,$07,$0f,$03,$0a,$05,$0e,$0c,$08,$04,$0b,$02,$09
scrollcolourend              

        
!align $ff,0
;2x2 charset text line presentation 

!ct scr

line1   !text "                    "
        !text " copyright (c) 2022 "
        !text "       blazon       "
        !text "                    "
     
line2   !text " brought to you on  "
        !text "      zzap 64       "
        !text "   micro action's   "
        !text "     covermount     "
     
line3   !text " code, game gfx and "
        !text "      music by      "
        !text "   richard bayliss  "
        !text "  (starhawk/blazon) "
        
line4   !text " loading bitmap and "
        !text " front end graphics "
        !text "         by         "
        !text "   firelord/excess  "
        
line5   
        !text " tape loader system "
        !text "         by         "
        !text "  martin piper and  "
        !text "  richard  bayliss  "
line6   
        !text "     testing by     "
        !text " baracuda,  sleeper,"
        !text "firelord and logiker"
        !text "                    "
line7   

        !text "  press spacebar or "
        !text " fire button on any "
        !text "    joystick for    "
        !text " main options screen"
        
line8
        !text "                    "
        !text " have fun losing at "
        !text "     this game!     "
        !text "        :-)         "
        
        
        

             
        
gameoptions
        !text "      options:      "
        !text "      play game     "
        !text "   hi score table   "
                
textscreen
        !fill 80,$20
        