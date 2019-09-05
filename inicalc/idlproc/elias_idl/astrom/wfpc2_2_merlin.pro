;***********************************************************************

PRO wfpc2_2_merlin,ra,dec,reverse=reverse

;+
; NAME:
;	wfpc2_2_merlin
;
; PURPOSE:
;	changes raw HDF WPFC2 positions to the Merlin reference Frame
;
; CATEGORY:
;	analysis
;
; CALLING SEQUENCE:
;	
;
; INPUTS:
;	ra: either hours or degrees as long as within the local of HDF
;      dec: in degrees
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	reverse: if set then the reverse transformation is performed
;
; OUTPUTS:
;	
;	ra: either hours or degrees as long as within the local of HDF
;      dec: in degrees
;
; OPTIONAL OUTPUTS:
;	
;
; COMMON BLOCKS:
;	
;
; SIDE EFFECTS:
;	
;
; RESTRICTIONS:
;	
;
; PROCEDURE:
;	
;
; EXAMPLE:
;	
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver 13th Dec 1996
;	July, 1996	
;-

if ra(0) gt 24. then deg=1 else deg=0

dra=0.089/60./60.
ddec=-1.03/60./60.

; if coordinates are in degrees then scale up correction

if deg eq 1 then dra=dra*15.

if keyword_set(reverse) then begin
  dra=-dra
  ddec=-ddec
endif


ra=ra+dra
dec=dec+ddec
   


END

