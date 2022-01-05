;========================================
;Lane Crazy 
;by Richard (Starhawk) Bayliss
;(C) 2021 Blazon Games Division
;========================================
;Hi score name entry and also hi score
;display routine.
;========================================

;The fully aligned, low and high byte tables for moving player names
nmlo !byte <name1,<name2,<name3,<name4,<name5,<name6,<name7,<name8,<name9,<name10 
nmhi !byte >name1,>name2,>name3,>name4,>name5,>name6,>name7,>name8,>name9,>name10

;Low and high byte tables for moving the hi scores.
hslo !byte <hiscore1,<hiscore2,<hiscore3,<hiscore4,<hiscore5
     !byte <hiscore6,<hiscore7,<hiscore8,<hiscore9,<hiscore10 
hshi !byte >hiscore1,>hiscore2,>hiscore3,>hiscore4,>hiscore5
     !byte >hiscore6,>hiscore7,>hiscore8,>hiscore9,>hiscore10
     
!ct scr 
name !text "richard b"

!align $ff,0
hiscore_checker 

              sei
              jsr clearint 
            
              lda #$03
              sta $dd00
              lda #$1b
              sta $d011
              lda #$1e
              sta $d018
              lda #$08
              sta $d016 
              lda #0
              sta $d020
              sta $d021
              sta $d015
              sta firebutton
              sta name_finished
              sta hi_colour_pointer
              sta hi_colourstore
              lda #1
              sta letter_char
              sta joy_delay 
              ldx #$00
clearscreenhi lda #$20
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
              bne clearscreenhi 
         
              ;Do check 
              ldx #$00
next_one      lda hslo,x
              sta $c1
              lda hshi,x
              sta $c2 
              
              ldy #$00
score_get     lda score,y
score_cmp     cmp ($c1),y
              bcc pos_down 
              beq next_digit 
              bcs pos_found 
next_digit    iny
              cpy #scorelength
              bne score_get 
              beq pos_found 
pos_down      inx
              cpx #listlength
              bne next_one 
              beq no_hiscore
pos_found     stx $02
              cpx #listlength-1
              beq last_score
              
              ldx #listlength-1
copy_next     lda hslo,x
              sta $c1
              lda hshi,x
              sta $c2 
              lda nmlo,x
              sta $d1
              lda nmhi,x
              sta $d2 
              dex
              lda hslo,x
              sta $c3
              lda hshi,x
              sta $c4 
              lda nmlo,x
              sta $d3
              lda nmhi,x
              sta $d4 
              
              ldy #scorelength-1
copy_score    lda ($c3),y
              sta ($c1),y
              dey 
              bpl copy_score 
              
              ldy #namelength+1
copy_name     lda ($d3),y
              sta ($d1),y
              dey 
              bpl copy_name 
              cpx $02 
              bne copy_next 
              
last_score    ldx $02
              lda hslo,x
              sta $c1
              lda hshi,x
              sta $d1
              lda nmlo,x
              sta $d1
              lda nmhi,x
              sta $d2
              jmp name_entry 
              
place_new_score 
              ldy #scorelength-1
put_score     lda score,y
              sta ($c1),y
              dey
              bpl put_score 
              
              ldy #namelength-1
putname       lda name,y
              sta ($d1),y 
              dey 
              bpl putname 
              jsr save_hi_scores
no_hiscore    jmp display_hiscores
              
;----------------------------------------
;Hi score name entry routine. We have 
;already cleared the screen from the 
;start, so now all that is required to 
;do is display the well done text
;----------------------------------------             
name_entry      ldx #$00
puthimessage    lda well_done_message,x 
                sta screen+(8*40),x
                lda well_done_message+40,x
                sta screen+(10*40),x 
                lda well_done_message+80,x 
                sta screen+(11*40),x
                lda well_done_message+120,x
                sta screen+(13*40),x
                inx
                cpx #40
                bne puthimessage 

                ldx #$00
clear_name      lda #$20
                sta name,x 
                inx
                cpx #9
                bne clear_name 
              
                lda #<name 
                sta namesm+1
                lda #>name
                sta namesm+2 
                
                ldx #<hi_irq 
                ldy #>hi_irq
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
                sta $d019
                sta $d01a
                lda #0
                jsr music3init
                cli 
;------------------------------------------
;Main name entry loop. This ends when the 
;name entry has been triggered.
;------------------------------------------
name_entry_loop 
                jsr sync_timer
                jsr hi_colour_wash
                ldx #$00
show_name       lda name,x 
                sta $06e0-120,x
                inx 
                cpx #9
                bne show_name
                
                lda name_finished
                bne stop_name_entry
                jsr joycheck 
                jmp name_entry_loop
                
stop_name_entry 
                jmp place_new_score 
                
;----------------------------------------
;Joystick control name entry check 
;routine
;----------------------------------------

joycheck        lda letter_char 
namesm          sta name 
                lda joy_delay 
                cmp #4 
                beq joy_hi_ok
                inc joy_delay
                rts
                
joy_hi_ok       lda #0
                sta joy_delay 
                
                ;Check joystick direction up 
hi_up1          lda #1
                bit $dc00 
                bne hi_up2
                jmp letter_up 
hi_up2          lda #1
                bit $dc01
                bne hi_down1
                jmp letter_up
hi_down1        lda #2
                bit $dc00
                bne hi_down2
                jmp letter_down 
hi_down2        lda #2
                bit $dc01 
                bne hi_fire1 
                jmp letter_down 
hi_fire1        lda $dc00
                lsr
                lsr
                lsr
                lsr
                lsr
                bit firebutton
                ror firebutton
                bmi hi_fire2 
                bvc hi_fire2 
                jmp char_selected

hi_fire2        lda $dc01 
                lsr
                lsr
                lsr
                lsr
                lsr
                bit firebutton 
                ror firebutton 
                bmi no_control_hi
                bvc no_control_hi
                jmp char_selected
no_control_hi                
                rts 
         
;---------------------------------------
;Increase letter up one character 
;If reached the END after character (29)
;jump to the Space character. If past the
;space character. Reset to character A 
;once more.
;----------------------------------------                
letter_up       inc letter_char 
                lda letter_char
                cmp #29 ;Char 29 overrides end key
                beq auto_space
                cmp #33 ;Char 33 orverride spacebar
                beq a_char 
                rts 
auto_space      lda #32         ;Enforce space char
                sta letter_char 
                rts 
a_char          lda #$01        ;Enforce letter A
                sta letter_char
                rts
                
;Decrease letter by one character. If reached
;char 00 or 31 reset char to new char. 

letter_down     dec letter_char
                lda letter_char
                beq spacebar_char
                lda letter_char 
                cmp #31
                beq end_char 
                rts
spacebar_char   lda #32
                sta letter_char
                rts 
end_char        lda #28
                sta letter_char
                rts
                
;---------------------------------------------
;The character for the player's name has been
;selected. Check for delete or end charaacter 
;or switch to new char until last position
;has expired.
;---------------------------------------------
char_selected   lda #0
                sta firebutton 
                
                lda letter_char 
                
                ;Check delete character
check_delete_char                
                cmp #27
                bne check_end_char 
                lda namesm+1
                cmp #<name 
                beq do_not_go_back 
                dec namesm+1
                jsr name_housekeep
do_not_go_back  rts 
                ;Check end character
check_end_char 
                cmp #28 
                bne char_is_ok 
                lda #$20 
                sta letter_char
                jmp finished_now 
                
char_is_ok      inc namesm+1 
                lda namesm+1 
                cmp #<name+9 
                beq finished_now
hi_no_fire      rts                 
                
finished_now    jsr name_housekeep
                lda #1
                sta name_finished
                rts 
                
;-------------------------------------
;Name housekeeping - This will clear 
;delete or end characters on screen. 
;-----------------------------------
name_housekeep
                ldx #$00
clearchars      lda name,x
                cmp #27 
                beq cleanup
                cmp #28 
                beq cleanup 
                jmp skip_cleanup 
cleanup         lda #$20
                sta name,x
skip_cleanup    inx
                cpx #namelength
                bne clearchars
                rts

;Initialize pointers (char position and
;IRQ Raster interrupts
;----------------------------------------

                
;Clear IRQ interrupts and setup VIC2 and
;screen settings.
;----------------------------------------

display_hiscores
                sei
                jsr clearint
                lda #$03
                sta $dd00
                lda #$1b
                sta $d011
                lda #$1e
                sta $d018
                lda #$08
                sta $d016 
                lda #0
                sta $d020
                sta $d021
                sta hi_colour_pointer
                sta hi_colour_delay
                sta hi_colourstore
                ldx #$00
clearhidisplay  lda #$20
                sta screen,x
                sta screen+$100,x
                sta screen+$200,x
                sta screen+$2e8,x
                lda #$00
                sta $d800,x
                sta $d900,x
                sta $da00,x
                sta $dae8,x
                inx
                bne clearhidisplay

;----------------------------------------
;Display the hi score list
;----------------------------------------                

                ldx #$00
gethitable      lda hiscore_table,x
                sta screen+(5*40),x
                lda hiscore_table+(1*40),x
                sta screen+(6*40),x
                lda hiscore_table+(2*40),x
                sta screen+(7*40),x
                lda hiscore_table+(3*40),x
                sta screen+(8*40),x
                lda hiscore_table+(4*40),x
                sta screen+(9*40),x
                lda hiscore_table+(5*40),x
                sta screen+(10*40),x
                lda hiscore_table+(6*40),x
                sta screen+(11*40),x
                lda hiscore_table+(7*40),x
                sta screen+(12*40),x
                lda hiscore_table+(8*40),x
                sta screen+(13*40),x
                inx
                cpx #40
                bne gethitable
                ldx #$00
gethitable2     lda hiscore_table+(9*40),x
                sta screen+(14*40),x 
                lda hiscore_table+(10*40),x
                sta screen+(15*40),x
                lda hiscore_table+(11*40),x
                sta screen+(16*40),x
                lda hiscore_table+(12*40),x
                sta screen+(17*40),x
                lda hiscore_table+(13*40),x
                sta screen+(18*40),x
                lda hiscore_table+(14*40),x
                sta screen+(19*40),x
                lda hiscore_table+(15*40),x
                sta screen+(20*40),x
                lda hiscore_table+(16*40),x
                sta screen+(21*40),x
                inx
                cpx #40
                bne gethitable2

;----------------------------------------
;Initialise interrupts for HI SCORE 
;TABLE. 
;----------------------------------------              
                ldx #<hi_irq
                ldy #>hi_irq
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
                sta $d019
                sta $d01a
                lda #0
                jsr music3init
                cli
                jmp hi_display_loop

;----------------------------------------
;The main IRQ raster interrupt. Only one
;this time round :)
;----------------------------------------

hi_irq          asl $d019
                lda $dc0d
                sta $dd0d
                lda #$fa
                sta $d012
                jsr hi_music_player
                jmp $ea31

;----------------------------------------
; PAL/NTSC music player
;----------------------------------------                
hi_music_player lda #1
                sta rt
                lda system
                cmp #1
                beq hi_pal
                inc ntsctimer
                lda ntsctimer
                cmp #6
                beq reset_himus
hi_pal          jsr music3play
                rts
reset_himus     lda #0
                sta ntsctimer
                rts
                
;----------------------------------------                
;Main hi score display loop, which also
;consists of code for flash fading all
;the text displayed
;----------------------------------------                 
hi_display_loop
                jsr sync_timer
                jsr hi_colour_wash
                lda $dc00
                lsr
                lsr
                lsr
                lsr
                lsr
                bit firebutton
                ror firebutton
                bmi hi_display_jp1 
                bvc hi_display_jp1
                lda #0
                sta firebutton
                jmp title_screen
hi_display_jp1  lda $dc01
                lsr
                lsr
                lsr
                lsr
                lsr
                bit firebutton 
                ror firebutton 
                bmi hi_display_loop
                bvc hi_display_loop
                lda #0
                sta firebutton
                jmp title_screen
                

;----------------------------------------
;Colour wash subroutine for hiscore 
;display.
;----------------------------------------                

hi_colour_wash
                lda hi_colour_delay 
                cmp #1
                beq hi_colour_ok 
                inc hi_colour_delay
                rts
hi_colour_ok    lda #0
                sta hi_colour_delay 
                ldx hi_colour_pointer
                lda hi_colourtable,x
                sta hi_colourstore
                inx
                cpx #hi_colourtable_end-hi_colourtable
                beq hi_colour_reset
                inc hi_colour_pointer
                jmp store_hi_colour
hi_colour_reset 
                ldx #$00
                stx hi_colour_pointer
store_hi_colour
                lda hi_colourstore
               
                sta colour+(5*40)
                sta colour+(6*40)+39
                sta colour+(7*40)
                sta colour+(8*40)+39
                sta colour+(9*40)
                sta colour+(10*40)+39
                sta colour+(11*40)
                sta colour+(12*40)+39
                sta colour+(13*40)
                sta colour+(14*40)+39
                sta colour+(15*40)
                sta colour+(16*40)+39
                sta colour+(17*40)
                sta colour+(18*40)+39
                sta colour+(19*40)
                sta colour+(20*40)
                
                ldx #$27
wash_forward    lda colour+(5*40)-1,x
                sta colour+(5*40),x
                lda colour+(7*40)-1,x
                sta colour+(7*40),x
                lda colour+(9*40)-1,x
                sta colour+(9*40),x
                lda colour+(11*40)-1,x
                sta colour+(11*40),x
                lda colour+(13*40)-1,x
                sta colour+(13*40),x
                lda colour+(15*40)-1,x
                sta colour+(15*40),x
                lda colour+(17*40)-1,x
                sta colour+(17*40),x
                lda colour+(19*40)-1,x
                sta colour+(19*40),x
                dex
                bpl wash_forward
                ldx #$00
wash_backwards  lda colour+(6*40)+1,x
                sta colour+(6*40),x
                lda colour+(8*40)+1,x
                sta colour+(8*40),x
                lda colour+(10*40)+1,x
                sta colour+(10*40),x
                lda colour+(12*40)+1,x
                sta colour+(12*40),x
                lda colour+(14*40)+1,x
                sta colour+(14*40),x
                lda colour+(16*40)+1,x
                sta colour+(16*40),x
                lda colour+(18*40)+1,x
                sta colour+(18*40),x
                lda colour+(20*40)+1,x
                sta colour+(20*40),x
                inx
                cpx #$28
                bne wash_backwards
                rts
;Hi score names 
hi_colour_delay !byte 0
hi_colour_pointer !byte 0           
hi_colourstore !byte 0     
name_finished !byte 0
letter_char !byte 1
joy_delay !byte 0
                !ct scr  
hiscore_table   
hi_line1  !text "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
hi_line2  !text "          today's high rollers          "
hi_line3  !text "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
hi_score_table_start
hi_line4  !text "          01. "
name1     !text "starhawk  "
hiscore1  !text "036000          "
hi_line5  !text "          02. "
name2     !text "logiker   "
hiscore2  !text "028530          "
hi_line6  !text "          03. "
name3     !text "firelord  "
hiscore3  !text "016280          "
hi_line7  !text "          04. "
name4     !text "baracuda  "
hiscore4  !text "012660          "
hi_line8  !text "          05. "
name5     !text "blazon    "
hiscore5  !text "011000          "
hi_line9  !text "          06. "
name6     !text "for       "
hiscore6  !text "009320          "
hi_line10 !text "          07. "
name7     !text "zzap      "
hiscore7  !text "008400          "
hi_line11 !text "          08. "
name8     !text "sixty     "
hiscore8  !text "006600          "        
hi_line12 !text "          09. "
name9     !text "four      "
hiscore9  !text "004200          "
hi_line13 !text "          10. "
name10    !text "magazine  "
hiscore10 !text "002700          "
hi_score_table_end
hi_line14 !text "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
hi_line15 !text "   press spacebar or fire to continue   "
hi_line16 !text "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
          !text "                                        "
well_done_message
          !text "             congratulations            "
          !text "  your score has earned its position on "
          !text "           the high score table         "
          !text "          please enter your name        "
hi_colourtable 
           !byte $06,$06,$09,$02,$0b,$04,$08,$0c,$0e,$05,$0a,$03,$0f,$07,$0d
           !byte $01,$0d,$07,$0f,$03,$0a,$05,$0e,$0c,$08,$04,$0b,$02,$09,$06,$06,$00
hi_colourtable_end          
                
                    