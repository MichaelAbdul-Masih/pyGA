;***********************************************************************

PRO icbackground,  ra , dec, year, month, day, lam, tot, zod, cir

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
;	July, 1996	
;-

sc_coords, year, month, day, ra/15., dec, roll, elong=sol_elong
euler,ra,dec,elon,elat,3

zod=ic_zodiacal_backgd, elat, sol_elong, lambda

zhmpnt,ra,dec,i100,/j2000

scale=newcirrus_sed(lam)/newcirrus_sed(100.)*lam/100.
cir=i100*scale

END

