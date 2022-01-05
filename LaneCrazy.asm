;========================================
;Lane Crazy 
;by Richard (Starhawk) Bayliss
;(C) 2021 Blazon Games Division
;========================================
;Initialisation code and variables
;========================================

 ;Generate target program file 
 ;(No BASIC run, unless crunched)
 
 !to "lanecrazy.prg",cbm 
;----------------------------------------
;VARIABLES 
 
;Run address for title screen code
titlescreen = title_screen
;Run address
check_hall_of_fame = hiscore_checker

;Where the wrapping screen backup is 
;positioned in order to build a lane 
screentemp = $ce00
screenbackup = $cf00

;Hardware screen + Colour RAM 
screen = $0400
colour = $d800

;Music pointers
in_game_music = $00
get_ready_jingle = $01
game_over_jingle = $02

;Music properties
musicinit = $1000
musicplay = $1003

;Title music
music2init = $6000
music2play = $6003

;Hi score name entry music 
music3init = $7100
music3play = $7103

;Char values (These are
;the characters that form the
;game background objects

holechar1 = 34
holechar2 = 35
holechar3 = 36
holechar4 = 37
lanechar = 38

paralaxchar1 = gamecharset+(41*8)
paralaxchar2 = gamecharset+(42*8)
paralaxchar3 = gamecharset+(43*8)
paralaxchar4 = gamecharset+(44*8)

scrollchar1 = gamecharset+(80*8)
scrollchar2 = gamecharset+(84*8)
scrollchar3 = gamecharset+(88*8) 
scrollchar4 = gamecharset+(90*8)
;Stop zone position for balls 
;every time they move.

xleftstop1 = $16
xleftstop2 = $3e 
xleftstop3 = $66
xleftstop4 = $8e 
xrightstop1 = $1e
xrightstop2 = $46 
xrightstop3 = $6e
xrightstop4 = $96

;Zero page address for sprite to background collision
;this can be changed where necessary.

zp = $70

;Sprite values 

sprite_G = $8f 
sprite_E = $90
sprite_T = $91
sprite_R = $92 
sprite_A = $93 
sprite_D = $94
sprite_Y = $95
sprite_M = $96
sprite_O = $97
sprite_V = $98

;In game raster splits 
split1 = $22 
split2 = $d0
split3 = $da
split4 = $fa

;Colour RAM position for logo colours 

logocolour = $8000

;Hi Score table variables 

scorelength = 6
listlength = 10
namelength = 9

;----------------------------------------
;Import in game charset data from raw
;binary 
 *=$0800
gamecharset
 !bin "bin\gamecharset.chr"
;----------------------------------------
;Import in game music and jingles data
;(DMC V4.0)
 *=$1000
 !bin "bin\music.prg",,2
;---------------------------------------- 
;Import game sprites data from raw 
;binary.
 *=$2000
 !bin "bin\gamesprites.spr"
;---------------------------------------- 
;Import game screen + colour data
;from raw binary
 *=$2800
gamescreen
 !bin "bin\gamescreen.bin" 
;----------------------------------------
;2x2 Character set for front end 
 *=$3000
 !bin "bin\titlecharset.chr"
;----------------------------------------
;1x1 Character set for hi score table 
 !bin "bin\hiscorecharset.chr"
;---------------------------------------- 
;Insert Main game code
 *=$4000
 sei

 jsr load_hi_scores
 !source "gamecode.asm"
;----------------------------------------
;Insert title screen code 
 *=$5000
 !source "titlescreen.asm" 
;---------------------------------------- 
;Import title music data (DMC V4.0)
*=$6000 
!bin "bin\music2.prg",,2
;----------------------------------------
;Import hall of fame music data (DMC V4.0) 
*=$7100
!bin "bin\music3.prg",,2
;-----------------------------------------
;Import title logo colour RAM, video RAM 
;and bitmap data
*=$8000
  !bin "bin\logo-colram.prg",,2
*=$8400
  !bin "bin\logo-vidram.prg",,2
*=$8800
  !source "scrolltext.asm"  
*=$a000
  !bin "bin\logo-bitmap.prg",,2
;------------------------------------------
;Hi score display and routines.
*=$c000
  !source "hiscore.asm"
;------------------------------------------
;Disk access routines  
  !align $ff,$00
  !source "diskaccess.asm"
  