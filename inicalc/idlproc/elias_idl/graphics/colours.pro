;***********************************************************************

PRO COLOURS,cmax

;+
; NAME:
;	COLOURS
;
; PURPOSE:
;	Sets up a few colours into low numbers on the LUT for use in
;       graphs.
;       Resulting colors
;       0:    Black
;       1:    White
;       2:    Red
;       3:    Green
;       4:    Blue
;       5:    Yellow
;       6:    Cyan
;       7:    Magenta
;       8:    Orange
;       9:    Pink
;      10:    Brown
;
; CATEGORY:
;	graphics
;
; CALLING SEQUENCE:
;	colours
;
; INPUTS:
;	
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	
;
; OUTPUTS:
;	
;
; OPTIONAL OUTPUTS:
;	
;
; COMMON BLOCKS:
;	
;
; SIDE EFFECTS:
;	Alters LUT
;
; RESTRICTIONS:
;	
;
; PROCEDURE:
;	
;
; EXAMPLE:
;	colours
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver June 1996
;-
; color set up


 common colors,r_orig,g_orig,b_orig,r_cur,g_cur,b_cur

 on_error,2

; if a lookup table is not set load the black and white one
 if n_elements(r_orig) eq 0 then loadct,0


 if n_params() eq 0 then cmax=11

 red=  [  0B,255B,255B,  0B,  0B,255B,  0B,255B,255B,255B,150B]
 green=[  0B,255B,  0B,255B,  0B,255B,255B,  0B,204B,204B,  0B]
 blue= [  0B,255B,  0B,  0B,255B,  0B,255B,255B, 51B,224B, 80B]

; setting current to have colours as first cmax elements
 r_cur=[red(0:cmax-1),r_orig(cmax:*)]
 g_cur=[green(0:cmax-1),g_orig(cmax:*)]
 b_cur=[blue(0:cmax-1),b_orig(cmax:*)]


 tvlct,r_cur,g_cur,b_cur

; setting original to current No I don't understand this
; either but it is the IDL standard way of doing things

 r_orig=r_cur 
 g_orig=g_cur 
 b_orig=b_cur 

END

