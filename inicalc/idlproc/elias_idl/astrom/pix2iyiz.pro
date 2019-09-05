;***********************************************************************

PRO pix2iyiz, ipix, iy, iz,npix=npix

;+
; NAME:
;	pix2iyiz
;
; PURPOSE:
;	Converts PHOT Pixel indicies into two element indicies
;       incrementing allomg space-craft coordinates
;
; CATEGORY:
;	
;
; CALLING SEQUENCE:
;	pix2iyiz, ipix, iy, iz
;
; INPUTS:
;	ipix:  Scalar or array of pixel indicies (0:8)
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	npix: number of pixels on x and y axis (i.e. 2 or 3 for c100/c200)
;
; OUTPUTS:
;	iy: Scalar of array of same dimensions as ipix
;           containing index along space craft Y-axis (0:2)
;       iz: As above but along Space-craft Z-axis
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
; 	Written by:	Seb Oliver 1st August 1996
;       21st April 1998 npix keyword added
;	July, 1994	Any additional mods get described here.
;-

on_error,2


 IF N_PARAMS() LT 3 THEN $
   Message, 'CALLING SEQUENCE: pix2iyiz, ipix, iy, iz'

 if not keyword_set(npix) then npix=3

 bad=where(ipix ge npix^2 or ipix lt 0, badcount)
 if badcount gt 0 then message,'Some IPIX values out of range 0<8'


iz=ipix mod npix
iy=ipix/npix

iz=ipix mod npix 
iy=ipix/npix

END









