;***********************************************************************

PRO icbackground,  ra , dec, year, month, day, lam, tot, zod, cir,sol_elong

;+
; NAME:
;	icbackground
;
; PURPOSE:
;	gives the background from the IC models at and given positions
;       and dates.
;
; CATEGORY:
;	analysis
;
; CALLING SEQUENCE:
;	
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
;       Bug that meant the cirrus background was screwy if a vector entered
;       removed  Seb Oliver 23 June 1997
;	July, 1996	
;-

if n_params()  lt  7 then message,$
'Calling Sequence: icbackground,  ra , dec , year, month, day, lam, tot, zod, cir,sol_elong


sc_coords, year, month, day, ra/15., dec, roll, sol_elong=sol_elong,/silent
euler,ra,dec,elon,elat,3

zod=ic_zodiacal_backgd( elat, sol_elong, lam)

zhmpnt,ra,dec,i100,/j2000

;stop
c100=newcirrus_sed(100.)
c100=c100(0)/100.

scale=newcirrus_sed(lam)/lam/c100
if n_elements(scale) eq 1 then scale=scale(0)

cir=i100*scale

tot=zod+cir

END

