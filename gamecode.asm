;========================================
;Lane Crazy 
;by Richard (Starhawk) Bayliss
;(C) 2021 Blazon Games Division
;========================================
;Main game code
;========================================
onetime
          lda $02a6 ;<--- Remove this when final build
          sta system ;is ready and place at the beginning of code
          lda #251
          sta 808
          lda #8
          jsr $ffd2
game_code          
         
;----------------------------------------
;Initialise game settings and clear all
;pointers
;----------------------------------------          
                    
          sei
          lda #0
          sta firebutton
            
          jsr clearint
          lda #$7b 
          sta $d011
          ldx #$00
initptr   lda #$00
          sta initpointersstart,x
          inx
          cpx #initpointersend-initpointersstart
          bne initptr
          lda #$10
          sta ypos

;----------------------------------------          
;Prepare VIC2 and hardware settings 
;also draw the game screen data to the 
;screen RAM.
;----------------------------------------          

          lda #$12  
          sta $d018
          lda #$18
          sta $d016 
          lda #$0e
          sta $d022
          lda #$01
          sta $d023
          
          lda #0
          sta levelpointer
          
          ldx #$00
drawgamescreen
          lda gamescreen,x
          sta screen,x
          lda gamescreen+$100,x
          sta screen+$100,x
          lda gamescreen+$200,x
          sta screen+$200,x
          lda gamescreen+$2e8,x
          sta screen+$2e8,x
          lda gamescreen+$3e8,x 
          sta colour,x
          lda gamescreen+$4e8,x
          sta colour+$100,x
          lda gamescreen+$5e8,x
          sta colour+$200,x
          lda gamescreen+$6e8,x
          sta colour+$300,x
          inx
          bne drawgamescreen
          
          lda #$30
          sta leveltext+1 
          lda #$30
          sta leveltext
          ldx #$00
zeroscore lda #$30
          sta score,x
          inx
          cpx #$06
          bne zeroscore
          
          lda #$00
          sta $d020
          sta $d021
          
          ;Revert to level 1
          
          lda #0
          sta levelpointer
          
          ;Copydraw 
          
          ldx #$00
getscreen lda gamescreen,x          
          sta screenbackup,x
          inx
          cpx #$50
          bne getscreen

          ;Just this once, draw the score panel. 
          jsr maskpanel
          
 
          ;Enable the sprites 
          
          lda #$ff
          sta $d015
          lda #$00
          sta $d01b
          sta $d01d
          sta $d017
          sta $d01c
          
          ;Multiple JSR routines increase 4x 
          
          ldx #$fb
          txs
          
;----------------------------------------
;Initialise IRQ raster interrupts for the
;game.
;----------------------------------------
         
          lda #$7f
          sta $dc0d
          sta $dd0d
          lda #$32
          sta $d012
          lda #$1b
          sta $d011
          lda #$01
          sta $d019
          sta $d01a
          ldx #<game_irq1
          ldy #>game_irq1
          stx $0314
          sty $0315
          ;Init GET READY jingle for game start
          lda #get_ready_jingle
          jsr musicinit
          cli
          jmp setup_get_ready 

;---------------------------------------          
;Main IRQ interrupts. This has to be 
;divided, where one part is used for 
;smooth scrolling and the other is being 
;used for the score panel.
;----------------------------------------
;Main IRQ interrupts. This has to be 
;divided, where one part is used for 
;smooth scrolling and the other is being 
;used for the score panel.
;----------------------------------------
                    ;Game scrolling background IRQ

game_irq1
          ;Raster split 1 
          inc $d019
          lda #split1
          sta $d012
levelcolour1 
          lda #$0b
          sta $d022
levelcolour2          
          lda #$01
          sta $d023
          
          jsr musicplayer
         
          ldx #<game_irq2
          ldy #>game_irq2 
          stx $0314
          sty $0315
          jmp $ea31
          
          ;Raster split2 
game_irq2 inc $d019
          lda #split2
          sta $d012 
          lda ypos
          sta $d011
          ldx #<game_irq3
          ldy #>game_irq3 
          stx $0314
          sty $0315
        
          jmp $ea7e
          
          ;Raster 3
game_irq3 
          inc $d019
          lda #split3
          sta $d012
          lda #$7f 
          sta $d011
          lda #$0e
          sta $d022
          lda #$01
          sta $d023
         
          ldx #<game_irq4
          ldy #>game_irq4
          stx $0314
          sty $0315 
          jmp $ea7e 
          
game_irq4
          inc $d019 
          lda #split4
          sta $d012 
          lda #$1f
          sta $d011
             lda #1
          sta rt   
          ldx #<game_irq1
          ldy #>game_irq1
          stx $0314
          sty $0315
          jmp $ea7e
          
          
setup_get_ready          
          ;Setup the game level (once)
          jsr level_setup
          
          lda #0
          sta firebutton
          lda #$10
          sta ypos
          
;----------------------------------------
;Setup and display the GET READY sprites 
;Main routines for game until fire or
;spacebar has been pressed, then clear 
;sprites, and then run the main game.
;----------------------------------------

          ldx #$00
setupgrsprites
          lda get_ready_sprite_table,x
          sta $07f8,x
          lda #3
          sta $d027,x
          inx
          cpx #8
          bne setupgrsprites
          
          lda #$46 ;Starting X position 
          sta objpos
          clc
          adc #$10
          sta objpos+2
          adc #$10
          sta objpos+4
          lda #$36
          sta objpos+6
          clc
          adc #$10 
          sta objpos+8
          adc #$10
          sta objpos+10
          adc #$10
          sta objpos+12
          adc #$10
          sta objpos+14
          lda #$68
          sta objpos+1
          sta objpos+3
          sta objpos+5
          clc 
          adc #$20
          sta objpos+7
          sta objpos+9
          sta objpos+11
          sta objpos+13
          sta objpos+15
         
          ;Get ready loop 
              
get_ready_loop
          jsr sync_timer
          jsr expand_sprite_area
          jsr sprite_flasher
          ldy #$00
putcolr1  lda spriteflashcolour1
          sta $d027,y
          iny
          cpy #8
          bne putcolr1
          lda $dc00
          lsr
          lsr
          lsr
          lsr
          lsr
          bit firebutton
          ror firebutton
          bmi grp1
          bvc grp1
          jmp setup_game
grp1      lda $dc01 
          lsr
          lsr
          lsr
          lsr
          lsr
          bit firebutton
          ror firebutton
          bmi get_ready_loop
          bvc get_ready_loop
          
         ;Temporarily setup the game sprites 
setup_game        
          lda #0
          sta firebutton
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

          ;Now init in game music 
          lda #in_game_music
          jsr musicinit
;----------------------------------------
;The main has started. This is the main
;game loop, with multiple pointers. 
;----------------------------------------
game_main_loop
          ;Synchronise game timer via 
          ;subroutine.
          
          jsr sync_timer
          jsr scroll_properties
       
          jsr sprite_properties
          jsr player_properties
            
          ;Loop routine until game over.
          jmp game_main_loop
          
;----------------------------------------
;Synchronise main game timer with IRQ
;interrupt pointer (rt - which stands for
;raster time).
;----------------------------------------
sync_timer  
          lda #0
          sta rt
          cmp rt
          beq *-3
          rts
;----------------------------------------
;Game scroll properties          
;----------------------------------------
scroll_properties
          
          jsr scroll_screen
          jsr playing_time
          jsr parallax
          rts
;Sprite properties and animation
;----------------------------------------
sprite_properties
          jsr expand_sprite_area
          jsr animate_sprites
          rts
;----------------------------------------
;Expand the sprite area so that all 
;sprites can use more than 256 pixels
;on screen. 
;----------------------------------------
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

;----------------------------------------          
;Animate the game sprites 
;----------------------------------------
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
;----------------------------------------
;Player game properties 
;----------------------------------------          
player_properties 

          jsr player_control
          jsr ball_shifter
          jsr collision
          rts
;Player game control. This can be by 
;using a joystick in port 1, 2
;----------------------------------------

player_control
          lda $dc01 ;Read joystick port 1 up
          lsr 
          bit joyup_p1
          ror joyup_p1 
          bmi not_joy_up_port1
          bvc not_joy_up_port1
          lda #0
          sta joyup_p1
          jmp triggerball2
not_joy_up_port1      
        
          lsr ;Read joystick port 1 down
          bit joydown_p1
          ror joydown_p1 
          bmi not_joy_down_port1
          bvc not_joy_down_port1
          lda #0
          sta joyup_p1
          jmp triggerball3 
         
not_joy_down_port1          

          lsr ;Read joystick port 1 left
          bit joyleft_p1 
          ror joyleft_p1 
          bmi not_joy_left_port1
          bvc not_joy_left_port2 
          lda #0
          sta joyleft_p1 
          jmp triggerball1 
          
not_joy_left_port1

          lsr ;Read joystick port 1 right
          bit joyright_p1 
          ror joyright_p1
          bmi not_joy_right_port1
          bvc not_joy_right_port1 
          lda #0
          sta joyright_p1 
          jmp triggerball4
          
not_joy_right_port1 ;Now read joystick port 2 
          lda $dc00
          lsr       ;Read up 
          bit joyup_p2 
          ror joyup_p2 
          bmi not_joy_up_port2 
          bvc not_joy_up_port2
          lda #0
          sta joyup_p2
          jmp triggerball2 
          
not_joy_up_port2 
          lsr       ;Read down 
          bit joydown_p2
          ror joydown_p2
          bmi not_joy_down_port2
          bvc not_joy_down_port2 
          lda #0
          sta joydown_p2 
          jmp triggerball3
          
not_joy_down_port2 
          lsr       ;Read left 
          bit joyleft_p2
          ror joyleft_p2 
          bmi not_joy_left_port2
          bvc not_joy_left_port2
          lda #0
          sta joyleft_p2 
          jmp triggerball1
          
not_joy_left_port2
          lsr       ;Read right 
          bit joyright_p2 
          ror joyright_p2 
          bmi not_joy_right_port2
          bvc not_joy_right_port2 
          lda #0
          sta joyright_p2 
          jmp triggerball4
          
not_joy_right_port2          
          ;Test keyboard input
          jsr $ffe4
          cmp #'A'
          beq triggerball1
          cmp #'S'
          beq triggerball2
          cmp #'D'
          beq triggerball3
          cmp #'F'
          beq triggerball4
          cmp #'H' 
          beq triggerball1
          cmp #'J' 
          beq triggerball2
          cmp #'K' 
          beq triggerball3
          cmp #'L'
          beq triggerball4
          ;Now check if RUN/STOP has been pressed 
          ;to pause game.
          
          jmp check_key_runstop
          
triggerball1
          jmp ball1_trigger
triggerball2          
          jmp ball2_trigger
triggerball3
          jmp ball3_trigger
triggerball4
          jmp ball4_trigger
          
          
          ;Check key Run/Stop for pause 
          ;mode 
check_key_runstop
          lda #$7f
          sta $dc00
          lda $dc01
          cmp #$7f 
          bne not_pause_game
          jmp pause_mode
not_pause_game          
          rts
          
          ;The game is paused, Fire resumes, or Q to abort game 
          
pause_mode          
          lda #16
          bit $dc00
          bne check_pause_fire2
          rts
check_pause_fire2
          lda #16 
          bit $dc00
          bne check_quit_key
          rts
check_quit_key 
          lda #$7f 
          sta $dc00
          lda $dc01
          cmp #$bf
          bne pause_mode
quitgame
          jmp titlescreen
          
          rts          

;----------------------------------------          
;Ball trigger routines. Basically, 
;we test to see whether or
;not the ball is already moving, 
;and if it isn't test the 
;last direction of the ball. 
;----------------------------------------             

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


;----------------------------------------            
;Ball shifter - Every time the ball is 
;not moving, and the joystick direction 
;has been triggered. We should move the 
;ball quickly according to direction. 
;After it has reachedthe stop position, 
;the ball movement should stop.
;----------------------------------------

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
                    

;----------------------------------------          
;Clear out interrupts
;----------------------------------------

clearint  
          ldx #$31
          ldy #$ea
          stx $0314
          sty $0315
          lda #$00
          sta $d019
          sta $d01a
      
          lda #$81
          sta $dc0d
          sta $dd0d
          ldx #$00
zerosidaway
          lda #$00
          sta $d400,x
          inx
          cpx #$18
          bne zerosidaway
        
skipscroll          
          rts

;----------------------------------------          
;Mask score panel 
;----------------------------------------
maskpanel
          ldx #$27
getpanel  lda panel,x
          sta $0770,x 
          dex 
          bpl getpanel 
          rts          

;----------------------------------------                    
;Scroll the main game screen ... (YPOS)
;----------------------------------------
scroll_screen
          lda ypos 
          clc
speed     adc #1
          sta ypos 
          lda ypos 
          cmp #$18 
          bcs scrollactive
          rts
         
scrollactive          
          lda #$10
          sta ypos
          
     
          jsr shiftrows1
          ;jsr shiftrows2
          jsr pick_holes
          jsr scoreit2 
passcheck          
          ldx #$27
checkpass
          lda screen+(19*40),x
          cmp #holechar1
          beq scoreit_
          dex
          bpl checkpass
          rts
scoreit_  jmp scoreit
          ;Hard scroll segment 1
      
shiftrows1
          ldx #$27
sr01          
          lda screen+(10*40),x 
          sta screentemp,x 
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
          
          lda screenbackup+40,x
          sta screen,x
          lda screenbackup,x
          sta screenbackup+40,x
          dex
          bpl sr01
          
          ldx #$27
sr02      lda screen+(18*40),x
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
          sta screen+(12*40),x
          lda screentemp,x
          sta screen+(11*40),x
         
          dex
          bpl sr02
          jsr fetch_lane
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
          lda #lanechar ;Empty lane/space
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

;Backup and fetch last lane position 

fetch_lane
          ldx #$27
placenext lda screen+(18*40),x
          sta screenbackup,x 
          dex
          bpl placenext
          rts
;----------------------------------------          
;Sprite to character set collision - 
;If the player falls down a hole, the 
;game is up. <- Note, this needs to 
;correspond to all sprites.
;----------------------------------------          
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
          
          ;Main sprite/character collision 
          ;routine.
          
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
          beq death
          cmp #holechar4 
          beq death 
          rts 
          
          ;Setup ball type death. (falling through hole)
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

;----------------------------------------          
;Death animation
;The ball has reached the hole 
;and falls down it 
;----------------------------------------
death_sequence  
 
          lda #0
          sta death_anim_delay
          sta death_anim_pointer
          
;----------------------------------------
;Main death sequence loop
;----------------------------------------      
death_loop 
          jsr sync_timer
          jsr expand_sprite_area
          jsr animate_death 
          jmp death_loop
          

;----------------------------------------
;Ball death animation routine
;----------------------------------------          
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
          

;----------------------------------------
;The game is over. Play GAME OVER jingle
;and display flashing GAME OVER sprites
;----------------------------------------          
do_game_over
          
          ldx #$00
removesprites
          lda #$00
          sta $d000,x
          sta objpos,x
          inx
          cpx #$10
          bne removesprites
     
          ;Setup GAME OVER sprites 
          
          ldx #$00
setgospr  lda game_over_sprite_table,x
          sta $07f8,x
          lda #$03
          sta $d027,x
          inx
          cpx #$08
          bne setgospr
          
          ;Setup start position for GAME OVER text
          
          lda #$3e ;Starting X position 
          sta objpos
          clc
          adc #$10
          sta objpos+2
          adc #$10
          sta objpos+4
          adc #$10
          sta objpos+6
          lda #$3d 
          sta objpos+8
          clc
          adc #$10
          sta objpos+10
          adc #$10
          sta objpos+12
          adc #$10
          sta objpos+14
          lda #$68
          sta objpos+1
          sta objpos+3
          sta objpos+5
          sta objpos+7
          clc 
          adc #$20
          sta objpos+9
          sta objpos+11
          sta objpos+13
          sta objpos+15

          ;Init game over jingle 
          lda #game_over_jingle 
          jsr musicinit
;----------------------------------------          
          ;Check player score with hi score
          
          lda score
          sec 
          lda hiscore+5
          sbc score+5
          lda hiscore+4
          sbc score+4
          lda hiscore+3
          sbc score+3
          lda hiscore+2
          sbc score+2
          lda hiscore+1
          sbc score+1
          lda hiscore
          sbc score
          bpl game_over_loop 

          ;Player score becomes new hi score 
          
          ldx #$00
makenewhi lda score,x
          sta hiscore,x 
          inx
          cpx #$06
          bne makenewhi
          jsr maskpanel
          
          ;Wait for the player to press fire to play 
            
game_over_loop        
          jsr sync_timer
          jsr expand_sprite_area
          jsr sprite_flasher
          ldy #$00
putcolr2  lda spriteflashcolour2
          sta $d027,y
          iny
          cpy #8
          bne putcolr2
          lda $dc00
          lsr
          lsr
          lsr
          lsr
          lsr
          bit firebutton
          ror firebutton
          bmi game_over_loop2
          bvc game_over_loop2
          jmp check_hall_of_fame
          
game_over_loop2          
          lda $dc01 
          lsr
          lsr
          lsr
          lsr
          lsr
          bit firebutton
          ror firebutton
          bmi game_over_loop
          bvc game_over_loop
          jmp check_hall_of_fame
;----------------------------------------
          
;Game playing time. This will of course control 
;the game playing time during play. 1 minute until 
;level up.
          
;----------------------------------------
playing_time 
          lda leveltime
          cmp #$32
          beq onesecond
          inc leveltime
          rts
onesecond 
          lda #0
          sta leveltime 
          
         
          lda leveltime+1
          cmp #30
          beq oneminute
          inc leveltime+1
          rts
oneminute          
          
;----------------------------------------          
;Setup levels 
;----------------------------------------
level_setup
          lda #0
          sta spawntime
          lda #0
          sta leveltime
          sta leveltime+1
         
          ldx levelpointer
    
          lda level_speed_table,x
          sta speed+1
          lda level_time_table,x
          sta spawnlimit
          lda level_colour_table,x
          sta levelcolour1+1
          lda level_charset_table_lo,x 
          sta charsm+1
          lda level_charset_table_hi,x
          sta charsm+2
          
        
          lda levelpointer
          cmp #9
          beq stop
          inc levelpointer
          inc leveltext+1
          ldx #$00
charsm    lda scrollchar1,x
          sta paralaxchar1,x
          inx
          cpx #32
          bne charsm
          jsr maskpanel
          rts
stop      ldx #8
          stx levelpointer
          jsr maskpanel
          rts
          
;----------------------------------------
;Scoring          
;----------------------------------------          
scoreit2  inc score+4
          ldx #4
calc2     lda score,x
          cmp #$3a
          bne scok2
          lda #$30
          sta score,x 
          inc score-1,X
scok2     dex
          bne calc2
          jsr maskpanel
          rts
          
scoreit   inc score+3
          ldx #$03
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
                      
            
;----------------------------------------                   
;Sprite flash routine                  
;---------------------------------------- 
sprite_flasher    lda spriteflashdelay
                  cmp #2
                  beq spriteflashok
                  inc spriteflashdelay
                  rts
spriteflashok     lda #0
                  sta spriteflashdelay
                  ldx spriteflashpointer
                  lda get_ready_flash_table,x
                  sta spriteflashcolour1
                  lda game_over_flash_table,x
                  sta spriteflashcolour2
                  inx
                  cpx #flash_length-game_over_flash_table
                  beq resetspriteflash
                  inc spriteflashpointer
                  rts
resetspriteflash  ldx #$00
                  stx spriteflashpointer
                  rts
      
;----------------------------------------
;Paralax scrolling                  
;----------------------------------------          

parallax          lda paradelay 
                  cmp #3
                  beq scrollpara 
                  inc paradelay 
                  rts
scrollpara        lda #0
                  sta paradelay
                  lda paralaxchar1
                  pha 
                  lda paralaxchar3
                  pha 
                  ldx #$00
uploop1           lda paralaxchar1+1,x
                  sta paralaxchar1,x
                  inx 
                  cpx #$07
                  bne uploop1 
                  pla 
                  sta paralaxchar1+7
                  ldx #$00
uploop2           lda paralaxchar3+1,x
                  sta paralaxchar3,x
                  inx
                  cpx #$07
                  bne uploop2
                  pla
                  sta paralaxchar3+7
                  lda paralaxchar2
                  pha
                  lda paralaxchar4
                  pha
                  ldx #$00
uploop3           lda paralaxchar2+1,x
                  sta paralaxchar2,x
                  inx
                  cpx #$07
                  bne uploop3
                  pla
                  sta paralaxchar2+7
                  ldx #$00
uploop4           lda paralaxchar4+1,x
                  sta paralaxchar4,x
                  inx
                  cpx #7
                  bne uploop4 
                  pla
                  sta paralaxchar4+7
                  rts
                  
;----------------------------------------
;PAL/NTSC music IRQ player          
;----------------------------------------          
musicplayer
      
        
          lda system
          cmp #1
          beq pal 
          inc ntsctimer
          lda ntsctimer
          cmp #6
          beq resetntsc
pal       jsr musicplay
          
          rts 
resetntsc lda #0
          sta ntsctimer
          rts
          
          
;----------------------------------------
;Game pointers          
;----------------------------------------          
!align $ff,0
system !byte 0
ntsctimer !byte 0
random !byte %10011101,%01011011
rantemp1 !byte 0
          
leveltime
          !byte 0,0
                    
;Game pointers and tables
rt        !byte 0 
firebutton !byte 0
levelpointer !byte 0
ypos !byte 0

initpointersstart
;Controlled joystick pointers, (simple taps rather than holding)
joyup_p1 !byte 0
joydown_p1 !byte 0
joyleft_p1 !byte 0
joyright_p1 !byte 0
joyup_p2 !byte 0
joydown_p2 !byte 0
joyleft_p2 !byte 0
joyright_p2 !byte 0

death_anim_delay !byte 0
death_anim_pointer !byte 0
paradelay !byte 0
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
spawnlimit !byte 0
spriteflashdelay !byte 0
spriteflashpointer !byte 0
spriteflashcolour1 !byte 0
spriteflashcolour2 !byte 0
initpointersend

;Game player object position
objpos !fill $10,$00
            
;Game sprites starting position

ball_start_position
          !byte $16,$a8 ;$16 = Left, $20 = Right
          !byte $16,$a8 ;-----------------------
          !byte $3e,$a8 ;$3e = Left, $48 = Right
          !byte $3e,$a8 ;-----------------------
          !byte $66,$a8 ;$66 = Left, $70 = Right
          !byte $66,$a8 ;-----------------------
          !byte $8e,$a8 ;$8e = Left, $98 = Right
          !byte $8e,$a8 ;-----------------------
          
;Sprite colour table 
sprite_colour_table
            !byte $0a,$02
            !byte $0e,$06
            !byte $0d,$05
            !byte $07,$0f
          
;Sprite animation table (Rolling ball)
ball_overlay_anim1 !byte $81,$82,$83,$84
ball_overlay_anim2 !byte $83,$84,$81,$82
ball_overlay_anim3 !byte $82,$83,$84,$81
ball_overlay_anim4 !byte $84,$81,$82,$83


;Animation table for ball death - Underlay 
ball_fall_anim2  !byte $85,$86,$87,$88,$89
;Animation table for ball death - Overlay
ball_fall_anim1  !byte $8a,$8b,$8c,$8d,$8e

;Possible hole spawn locations

            ;Lane 1 ;Lane 2  ;Lane 3  ;Lane 4
hole1 !byte $03,$05 ,$0d,$0f ,$17,$19 ,$21,$23
hole2 !byte $04,$06 ,$0e,$10 ,$18,$1a ,$22,$24
hole3 !byte $2b,$2d ,$35,$37 ,$3f,$41 ,$49,$4b
hole4 !byte $2c,$2e ,$36,$38 ,$40,$42 ,$4a,$4c

!align $ff,$00

;Level table. (Based on speed and amount)

level_speed_table 
            !byte 1,1,1,1,2,2,2,2
level_time_table
            !byte $10,$08,$06,$04,$0e,$0c,$0a,$08
level_colour_table
            !byte $06,$0b,$09,$02,$0e,$0c,$08,$04
level_charset_table_lo 
            !byte <scrollchar1, <scrollchar2, <scrollchar3, <scrollchar4 
            !byte <scrollchar2, <scrollchar1, <scrollchar4, <scrollchar3
            
level_charset_table_hi 
            !byte >scrollchar1, >scrollchar2, >scrollchar3, >scrollchar4 
            !byte >scrollchar2, >scrollchar1, >scrollchar4, >scrollchar3
            
                  !ct scr
panel             !text "score: "
score             !text "000000    "
                  !text "level: "
leveltext         !text "01    "
                  !text "hi: "
hiscore           !text "000000"
                  
                  
;Sprite - GET READY / GAME OVER sprite table 

get_ready_sprite_table 
                  !byte sprite_G, sprite_E, sprite_T
                  !byte sprite_R, sprite_E, sprite_A, sprite_D, sprite_Y
game_over_sprite_table 
                  !byte sprite_G, sprite_A, sprite_M, sprite_E
                  !byte sprite_O, sprite_V, sprite_E, sprite_R
                  
;Sprite flashing colour table 
get_ready_flash_table 
                  !byte $06,$04,$0e,$03,$0d,$01,$0d,$03,$0e,$04,$06
                  !byte $09,$02,$08,$0a,$07,$01,$07,$0a,$08,$02,$09
game_over_flash_table 
                  
                  !byte $06,$04,$0e,$03,$0d,$01,$0d,$03,$0e,$04,$06
                  !byte $09,$02,$08,$0a,$07,$01,$07,$0a,$08,$02,$09
flash_length      
                  
                  