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
titlescreen = $4000 
;Run address
check_hall_of_fame = $4000 

;In game raster splits 
split1 = $30
split2 = $d0
split3 = $da
split4 = $fa

;Where the wrapping screen backup is 
;positioned in order to build a lane 
screentemp = $c000
screenbackup = $c100

;Hardware screen + Colour RAM 
screen = $0400
colour = $d800

;Music pointers
titlemusicinit = $1000
titlemusicplay = $1003
musicinit = $1000
musicplay = $1003

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

zp = $02

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
;Insert Main game code
 *=$4000
 !source "gamecode.asm"
;---------------------------------------- 