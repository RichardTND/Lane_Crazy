;------------------------------------------
;Lane Crazy 
;Programmed by Richard (Starhawk)
;(C) 2021 Blazon Games Division
;------------------------------------------

 !to "lanecrazy.prg",cbm

;Variables:
screentemp = $c000
screenbackup = $c100
;Hardware screen + Colour RAM
screen = $0400
colour = $d800

;Music pointers
musicinit = $1000
musicplay = $1003

;Hazard char values 
holechar1 = $a0
holechar2 = $a0
holechar3 = $a0
holechar4 = $a0

;Stop zone for balls

xleftstop1 = $16
xleftstop2 = $3e 
xleftstop3 = $66
xleftstop4 = $8e 
xrightstop1 = $1e
xrightstop2 = $46 
xrightstop3 = $6e
xrightstop4 = $96

;Zero page address for sprite to background collision
zp = $02

;Import game charset data
 *=$0800
 !bin "bin\gamecharset.bin"
  
;Import game music data
 *=$1000
 !bin "bin\music.prg",,2
    
;Import game sprites data 
 *=$2000
 !bin "bin\gamesprites.spr"
 
 ;Import game screen + colour data
 *=$2500
gamescreen
 !bin "bin\gamescreen.bin"
 
;----------------------------------------
;Main game code
 
 *=$3000
          sei
          
          lda #0
          sta firebutton
          
          jsr clearint
          
          ;Initialise pointers 
          
          ldx #$00
initptr   lda #$00
          sta initpointersstart,x
          inx
          cpx #initpointersend-initpointersstart
          bne initptr
          
          ;Prepare game graphics 
          
          lda #$12  
          sta $d018
          lda #$08
          sta $d016 
          
          ldx #$00
drawgamescreen
          lda gamescreen,x
          sta colour,x
          lda gamescreen+$100,x
          sta colour+$100,x
          lda gamescreen+$200,x
          sta colour+$200,x
          lda gamescreen+$300,x
          sta colour+$300,x
          lda gamescreen+$3e8,x 
          sta screen,x
          lda gamescreen+$4e8,x
          sta screen+$100,x
          lda gamescreen+$5e8,x
          sta screen+$200,x
          lda gamescreen+$6e8,x
          sta screen+$300,x
          inx
          bne drawgamescreen
          
          lda #$31
          sta leveltext+1 
          lda #$30
          sta leveltext
          ldx #$00
zeroscore lda #$30
          sta score,x
          inx
          cpx #$06
          bne zeroscore
          
          lda #$0f
          sta $d020
          sta $d021
          
          ;Revert to level 1
          
          lda #0
          sta levelpointer
          
          ;Copydraw 
          
          ldx #$00
getscreen lda gamescreen+$3e8,x          
          sta screenbackup,x
          inx
          cpx #$50
          bne getscreen
          
          jsr maskpanel
          
          ;Setup the game sprites 
          
          lda #$81
          sta $07f8
          sta $07fa
          sta $07fc
          sta $07fe
          lda #$80
          sta $07f9
          sta $07fb
          sta $07fd
          sta $07ff
          
          ;Setup the starting position for all the game sprites 
          
          ldx #$00
set_start lda ball_start_position,x
          sta objpos,x
          inx
          cpx #$10
          bne set_start
          
          ;Setup the game sprite colour (read from colour table)
          
          ldx #$00
paintspr  lda sprite_colour_table,x
          sta $d027,x
          inx
          cpx #$08
          bne paintspr
          
          lda #$ff
          sta $d015
          lda #$00
          sta $d01b
          sta $d01d
          sta $d017
          sta $d01c
          
          ;Multiple JSR routines increase 4x 
          
          lda #$fb
          txs
          
          ;Initialise interrupts 
          
          ldx #<game_irq1
          ldy #>game_irq1
          stx $0314
          sty $0315
          lda #$7f
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
          jsr musicinit
          jmp game_main_loop
          
;Main IRQ interrupts. This has to be divided, where one part is 
;used for smooth scrolling and the other is being used for 
;the score panel.

game_irq1 asl $d019
          lda $dc0d
          sta $dd0d
          lda #$fa
          sta $d012
          lda #1
          sta rt
          jsr musicplay
          jmp $ea7e
          
;Main routines for game

game_main_loop
          jsr sync_timer
          jsr expand_sprite_area
          jsr scroll_screen
          jsr animate_sprites
          jsr ball_shifter
          jsr player_control
          jsr collision
          
          jmp game_main_loop
          
;Synchronise game timer           

sync_timer  
          lda #0
          sta rt
          cmp rt
          beq *-3
          rts

;Expand the sprite area so that all sprites can use more than 256 pixels on screen.
;The game screen has been designed that way.

expand_sprite_area
          ldx #$00
expandmsb lda objpos+1,x
          sta $d001,x
          lda objpos,x
          asl
          ror $d010
          sta $d000,x
          inx
          inx
          cpx #$10
          bne expandmsb
          rts
          
;Animate the game sprites 

animate_sprites
          lda animdelay
          cmp #3
          beq animdelayok
          inc animdelay
          rts
animdelayok
          lda #$00
          sta animdelay
          ldx animpointer
          lda ball_overlay_anim1,x
          sta $07f8
          lda ball_overlay_anim2,x
          sta $07fa
          lda ball_overlay_anim3,x
          sta $07fc
          lda ball_overlay_anim4,x 
          sta $07fe
          inx
          cpx #4
          beq resetanim
          inc animpointer
          rts
resetanim ldx #$00
          stx animpointer
          rts
          
;Player game control 

player_control
         
upj2      lda #1 ;UP - Joystick port 2
          bit $dc00
          bne upj1
          jsr ball1_trigger ;Movement control for first ball
          
upj1      lda #1
          bit $dc01 ;Now test joystick port 1
          bne downj2
          jsr ball1_trigger

downj2    lda #2 ;DOWN - Joystick port 2
          bit $dc00
          bne downj1
          jsr ball2_trigger ;Movement control for second ball
downj1    lda #2
          bit $dc01 ;Now try joystick port 1
          bne leftj2
          jsr ball2_trigger

leftj2    lda #4 ;LEFT - Joystick port 2
          bit $dc00
          bne leftj1 
          jsr ball3_trigger ;Movement control for third ball
          
leftj1    lda #4
          bit $dc01
          bne rightj2
          jsr ball3_trigger
          
rightj2   lda #8 ;RIGHT - Joystick port 2
          bit $dc00
          bne rightj1 
          jsr ball4_trigger ;Movement control for final ball
rightj1   lda #8
          bit $dc01 
          bne nojoy
          jmp ball4_trigger
nojoy     rts          
;Ball trigger routines. Basically, we test to see whether or
;not the ball is already moving, and if it isn't test the 
;last direction of the ball. 
             
              ;Setup macro that creates the ball trigger
              
!macro triggerball ball_moving, ball_dir {
              
              lda ball_moving
              beq .checkdirection
              rts
.checkdirection
              lda ball_dir
              beq .swapdirection
              lda #0
              sta ball_dir
              jmp .reactivateball
.swapdirection
              lda #1
              sta ball_dir
.reactivateball
              lda #1
              sta ball_moving
              rts 
                           
}          
            
ball1_trigger +triggerball ball_1_is_moving, ball_1_dir
ball2_trigger +triggerball ball_2_is_moving, ball_2_dir
ball3_trigger +triggerball ball_3_is_moving, ball_3_dir
ball4_trigger +triggerball ball_4_is_moving, ball_4_dir
            
;Ball shifter - Every time the ball is not moving, and the
;joystick direction has been triggered. We should move the 
;ball quickly according to direction. After it has reached
;the stop position, the ball movement should stop.

!macro moveball ball_moving, ball_dir, ball_underlay_x, ball_overlay_x, stop_zone_left, stop_zone_right {

              lda ball_moving
              bne .moveok
              rts
.moveok       lda ball_dir 
              beq .moveleft
              
              ;Ball moves to the right 
              ldx ball_underlay_x
              inx
              inx
              cpx #stop_zone_right 
              bcc .storright
              lda #0
              sta ball_moving
              ldx #stop_zone_right
.storright    stx ball_underlay_x
              stx ball_overlay_x
              rts
 
              ;Ball moves to the left  
.moveleft     ldx ball_underlay_x
              dex
              dex
              cpx #stop_zone_left 
              bcs .storleft 
              lda #0
              sta ball_moving
              ldx #stop_zone_left 
.storleft             
              stx ball_underlay_x 
              stx ball_overlay_x
              rts
           
}              
              

;Main ball shifter macro links 

ball_shifter 
               jsr shiftb1
               jsr shiftb2
               jsr shiftb3
               jsr shiftb4
               rts
               
shiftb1        +moveball ball_1_is_moving, ball_1_dir, objpos, objpos+2, xleftstop1, xrightstop1
shiftb2        +moveball ball_2_is_moving, ball_2_dir, objpos+4, objpos+6, xleftstop2, xrightstop2
shiftb3        +moveball ball_3_is_moving, ball_3_dir, objpos+8, objpos+10, xleftstop3, xrightstop3
shiftb4        +moveball ball_4_is_moving, ball_4_dir, objpos+12, objpos+14, xleftstop4, xrightstop4
                    
          
;Clear out interrupts
clearint
          ldx #$31
          ldy #$ea
          stx $0314
          sty $0315
          lda #$81
          sta $dc0d
          sta $dd0d
          lda #$00
          sta $d019
          sta $d01a
          cli 
skipscroll          
          rts
          
;Mask score panel 

maskpanel
          ldx #$27
getpanel  lda panel,x
          sta $0770,x 
          dex 
          bpl getpanel 
          rts          
                    
;Scroll the main game screen ... (YPOS)

scroll_screen
          lda ypos
          sec
speed     sbc #1
          and #7
          sta ypos 
          bcs skipscroll
     
          jsr shiftrows1
          jsr shiftrows2
          jsr pick_holes
          ldx #$00
checkpass
          lda screen+(19*40),x
          cmp #holechar1
          beq scoreit
          cmp #holechar2
          beq scoreit
          cmp #holechar3
          beq scoreit
          cmp #holechar4
          beq scoreit
          inx
          cpx #40
          bne checkpass
          rts
          
scoreit   inc score+4
          ldx #$04
calc      lda score,x
          cmp #$3a
          bne nextd
          lda #$30
          sta score,x
          inc score-1,x
nextd     dex
          bne calc
          jsr maskpanel
          rts
            
          ;Hard scroll segment 1
shiftrows1
          ldx #$27
sr01          
          lda screen+(19*40),x
          sta screen+(20*40),x
          lda screen+(18*40),x
          sta screen+(19*40),x
          lda screen+(17*40),x
          sta screen+(18*40),x
          lda screen+(16*40),x
          sta screen+(17*40),x
          lda screen+(15*40),x
          sta screen+(16*40),x
          lda screen+(14*40),x
          sta screen+(15*40),x
          lda screen+(13*40),x
          sta screen+(14*40),x
          lda screen+(12*40),x
          sta screen+(13*40),x
          lda screen+(11*40),x
          sta screentemp,x
          dex
          bpl sr01
          
          rts
          
shiftrows2          
          ldx #$27
sr02      lda screentemp,x
          sta screen+(12*40),x
          lda screen+(10*40),x
          sta screen+(11*40),x
          lda screen+(9*40),x
          sta screen+(10*40),x
          lda screen+(8*40),x
          sta screen+(9*40),x
          lda screen+(7*40),x
          sta screen+(8*40),x
          lda screen+(6*40),x
          sta screen+(7*40),x
          lda screen+(5*40),x
          sta screen+(6*40),x
          lda screen+(4*40),x
          sta screen+(5*40),x
          lda screen+(3*40),x
          sta screen+(4*40),x
          lda screen+(2*40),x
          sta screen+(3*40),x
          lda screen+(1*40),x
          sta screen+(2*40),x
          lda screen,x
          sta screen+(1*40),x
          lda screenbackup+(1*40),x
          sta screen,x
          lda screenbackup,x
          sta screenbackup+(1*40),x
          dex
          bpl sr02
skipspawn          
          rts
          
;Randomizer - Pick holes for game_irq1
          
pick_holes          
          inc spawntime
          lda spawntime
          cmp #1
          beq skipspawn
          cmp #$10
          beq spawnhole
          lda #$20 ;Empty lane/space
          sta screenbackup+$03
          sta screenbackup+$04
          sta screenbackup+$05
          sta screenbackup+$06
          sta screenbackup+$03+40
          sta screenbackup+$04+40
          sta screenbackup+$05+40
          sta screenbackup+$06+40
          sta screenbackup+$0d
          sta screenbackup+$0e
          sta screenbackup+$0f
          sta screenbackup+$10
          sta screenbackup+$0d+40
          sta screenbackup+$0e+40
          sta screenbackup+$0f+40
          sta screenbackup+$10+40
          sta screenbackup+$17
          sta screenbackup+$18
          sta screenbackup+$19
          sta screenbackup+$1a
          sta screenbackup+$17+40
          sta screenbackup+$18+40
          sta screenbackup+$19+40
          sta screenbackup+$1a+40
          sta screenbackup+$21
          sta screenbackup+$22
          sta screenbackup+$23
          sta screenbackup+$24
          sta screenbackup+$21+40
          sta screenbackup+$22+40
          sta screenbackup+$23+40
          sta screenbackup+$24+40
          
          rts 
spawnhole          
          lda #0
          sta spawntime
          lda random+1
          sta rantemp1
          lda random
          asl
          rol rantemp1
          asl
          rol rantemp1
          clc
          adc random
          pha
          lda rantemp1
          adc random+1
          sta random+1
          pla
          adc #31
          sta random
          lda random+1
          adc #$36
          sta random+1
          and #7
          sta holeposition
          ldy holeposition
          lda hole1,y
          sta csm1+1
          lda hole2,y
          sta csm2+1
          lda hole3,y
          sta csm3+1
          lda hole4,y
          sta csm4+1
          
          ;Store holes 
          
          lda #holechar1
csm1      sta screenbackup
          lda #holechar2
csm2      sta screenbackup
          lda #holechar3
csm3      sta screenbackup
          lda #holechar4
csm4      sta screenbackup
          rts
         
         
          ;Sprite to character set collision - If the player
          ;falls down a hole, the game is up. <- Note, this needs 
          ;to correspond to all sprites.
          
collision 
          lda #$00
          sta spritex+1
          lda #$01
          sta spritey+1
          lda #0
          sta msb+1
          lda #<ball1death
          sta death+1
          lda #>ball1death
          sta death+2
          
          jsr readcollision

          lda #$04
          sta spritex+1
          lda #$05
          sta spritey+1
          lda #0
          sta msb+1
          lda #<ball2death
          sta death+1
          lda #>ball2death
          sta death+2
          jsr readcollision
          
          lda #$08
          sta spritex+1
          lda #$09
          sta spritey+1
          lda #0
          sta msb+1
          lda #<ball3death
          sta death+1
          lda #>ball3death
          sta death+2
          jsr readcollision
          
          lda #$0c
          sta spritex+1
          lda #$0d
          sta spritey+1
          lda #1
          sta msb+1
          lda #<ball4death
          sta death+1
          lda #>ball4death
          sta death+2
          
          
readcollision

spritex   lda $d000
          sec
          sbc #$10 ;Central 
          sta zp 
          lda $d010
msb       sbc #$00
          lsr
          lda zp
          ror
          lsr
          lsr
          sta zp+3
          
spritey   lda $d001
          sec
          sbc #$22
          lsr
          lsr
          lsr
          sta zp+4
          lda #$00
          sta zp+1
          lda #$04
          sta zp+2
          
          ldx zp+4
          beq checkchars
          
bgloop    lda zp+1
          clc
          adc #$28
          sta zp+1
          lda zp+2
          adc #$00
          sta zp+2 
          dex 
          bne bgloop
          
checkchars
          ldy zp+3
          lda (zp+1),y 
          cmp #holechar1
          beq death
          cmp #holechar2
          beq death
          cmp #holechar3
          beq ball1death
          cmp #holechar4 
          beq death 
          rts 
          
death     jmp ball1death

ball1death
         ; lda #2
         ; sta $d020
          lda #$f8
          sta deathframe1+1
          lda #$f9
          sta deathframe2+1
          jmp death_sequence
          
ball2death
         ; lda #6
         ; sta $d020
          lda #$fa
          sta deathframe1+1
          lda #$fb
          sta deathframe2+1
          jmp death_sequence
          rts
          
ball3death
         ; lda #5
         ; sta $d020
          lda #$fc
          sta deathframe1+1
          lda #$fd
          sta deathframe2+1
          jmp death_sequence
          rts
          
ball4death 
         ; lda #7
         ; sta $d020
          lda #$fe
          sta deathframe1+1
          lda #$ff
          sta deathframe2+1
          jmp death_sequence
          rts 
          
;The ball has reached the hole and falls down it 

death_sequence  
          
          
          lda #0
          sta death_anim_delay
          sta death_anim_pointer
          
          lda #1
          jsr musicinit
      
death_loop 
          jsr sync_timer
          jsr expand_sprite_area
          jsr animate_death 
          jmp death_loop
          
animate_death 
  
          lda death_anim_delay 
          cmp #6
          beq death_anim_ok
          inc death_anim_delay
          rts 
          
death_anim_ok 
          lda #0
          sta death_anim_delay 
          ldx death_anim_pointer 
          lda ball_fall_anim1,x
deathframe1
          sta $07f8 
          lda ball_fall_anim2,x
deathframe2          
          sta $07f9 
          inx
          cpx #5
          beq do_game_over
          inc death_anim_pointer
          rts 
do_game_over
          ldx #$00
removesprites
          lda #$00
          sta $d000,x
          sta objpos,x
          inx
          cpx #$10
          bne removesprites
          
          ;Wait for the player to press fire to play 
          
game_over_loop          
          lda $dc00
          lsr
          lsr
          lsr
          lsr
          lsr
          bit firebutton
          ror firebutton
          bmi game_over_loop
          bvc game_over_loop
          jmp $3000
          
          
          
random !byte %10011101,%01011011
rantemp1 !byte 0
          
          
          
          
          
;Game pointers and tables
rt        !byte 0 
firebutton !byte 0
levelpointer !byte 0

initpointersstart
death_anim_delay !byte 0
death_anim_pointer !byte 0
;Move direction for ball         
ball_1_dir !byte 0
ball_2_dir !byte 0
ball_3_dir !byte 0
ball_4_dir !byte 0
;Move indicators
ball_1_is_moving !byte 0
ball_2_is_moving !byte 0
ball_3_is_moving !byte 0
ball_4_is_moving !byte 0
animdelay !byte 0
animpointer !byte 0

holeposition !byte 0
spawntime !byte 0
initpointersend
ypos !byte 0
;Game player object position
objpos !fill $10,$00
            
;Game sprites starting position

ball_start_position
          !byte $16,$b8 ;$16 = Left, $20 = Right
          !byte $16,$b8 ;-----------------------
          !byte $3e,$b8 ;$3e = Left, $48 = Right
          !byte $3e,$b8 ;-----------------------
          !byte $66,$b8 ;$66 = Left, $70 = Right
          !byte $66,$b8 ;-----------------------
          !byte $8e,$b8 ;$8e = Left, $98 = Right
          !byte $8e,$b8 ;-----------------------
          
;Sprite colour table 
sprite_colour_table
            !byte $02,$0a
            !byte $06,$0e
            !byte $05,$0d
            !byte $08,$07
          
;Sprite animation table (Rolling ball)
ball_overlay_anim1 !byte $81,$82,$83,$84
ball_overlay_anim2 !byte $83,$84,$81,$82
ball_overlay_anim3 !byte $82,$83,$84,$81
ball_overlay_anim4 !byte $84,$81,$82,$83


;Animation table for ball death - Underlay 
ball_fall_anim1  !byte $85,$86,$87,$88,$89
;Animation table for ball death - Overlay
ball_fall_anim2  !byte $8a,$8b,$8c,$8d,$8e

;Possible hole spawn locations

            ;Lane 1 ;Lane 2  ;Lane 3  ;Lane 4
hole1 !byte $03,$05 ,$0d,$0f ,$17,$19 ,$21,$23
hole2 !byte $04,$06 ,$0e,$10 ,$18,$1a ,$22,$24
hole3 !byte $2b,$2d ,$35,$37 ,$3f,$41 ,$49,$4b
hole4 !byte $2c,$2e ,$36,$38 ,$40,$42 ,$4a,$4c

  !ct scr
panel            ;!text "----------------------------------------"
                  
                  !text "score: "
score             !text "000000    "
                  !text "level: "
leveltext         !text "01    "
                  !text "hi: "
hiscore           !text "000000"
                  
                  

;---------------------------------------- 