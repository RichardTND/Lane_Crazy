;========================================
;Lane Crazy 
;by Richard (Starhawk) Bayliss
;(C) 2021 Blazon Games Division
;========================================
;Hi score saver/loader DOS
;========================================

!align $ff,0

dname !text "S:"
fname !text "HI ROLLERS  /BLZ"
fnamelen = *-fname 
dnamelen = *-dname 

;------------------------------------
;Save hi scores from disk, if device
;is 8 or above. Otherwise tape device 
;is detected. Then should be skipped. 
;-------------------------------------
save_hi_scores
              sei
              jsr clearint
              lda #0
              sta $d011
              
              ldx $ba
              cpx #$08
              bcc skip_save
              lda #$0f
              tay 
              jsr $ffba
              jsr reset_drive 
              lda #dnamelen
              ldx #<dname 
              ldy #>dname 
              jsr $ffbd 
              jsr $ffc0 
              lda #$0f
              jsr $ffc3
              jsr $ffcc
              
              lda #$0f
              ldx $ba
              tay 
              jsr $ffba 
              jsr reset_drive
              lda #fnamelen
              ldx #<fname
              ldy #>fname 
              jsr $ffbd
              lda #$fb
              ldx #<hi_score_table_start
              ldy #>hi_score_table_start
              stx $fb
              sty $fc 
              ldx #<hi_score_table_end
              ldy #>hi_score_table_end 
              jsr $ffd8 
skip_save     rts              
              

;------------------------------------
;Load hi scores from disk, if device
;is 8 or above. Otherwise tape device 
;is detected. Then should be skipped. 
;-------------------------------------
load_hi_scores
      sei
      jsr clearint
      lda #0
      sta $d011
      ldx $ba
      cpx #$08
      bcc skip_load
      lda #$0f 
      tay 
      jsr $ffba 
      jsr reset_drive 
      lda #fnamelen 
      ldx #<fname 
      ldy #>fname 
      jsr $ffbd 
      lda #$00
      jsr $ffd5 
      bcc loaded
      ;Hi score file does not exist on file, so 
      ;save it 
      jsr save_hi_scores
loaded
skip_load
      rts
      
;========================================
;Reset the disk drive track/sector
;========================================      

reset_drive 
      lda #$01
      ldx #<initdrive
      ldy #>initdrive
      jsr $ffbd 
      jsr $ffc0 
      lda #$0f
      jsr $ffc3
      jsr $ffcc
      rts
      
initdrive
     !text "I:"
     rts