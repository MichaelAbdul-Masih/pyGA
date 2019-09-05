;***********************************************************************

PRO ICAD2XY, A , D, HD, X, Y,disto=disto

;+
; NAME: 
;       ICAD2XY
;	
;
; PURPOSE:
;	Compute and X and Y position given a FITS header and A and D
;       able to handle GSS astrometry
;
; CATEGORY:
;	
;
; CALLING SEQUENCE:
;	ICAD2XY, a, d, HD, x, y
;
; INPUTS:
;	A:  Longitude in degrees (scalar or vector)
;       D:  Latitude in degrees  (scalar or vector)
;      HD:  Fits header
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	
;
; OUTPUTS:
;       X:  Row position (IDL convention)
;       Y:  Col position (IDL convention)
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
; 	Written by:	Seb Oliver July 1 1996
;	July, 1994	Any additional mods get described here.
;-

; on_error,2

 if N_params() lT 5 then begin
        print,'Syntax -- ICAD2XY, a, d, hd, x, y
        return
 endif

 if not keyword_set(disto) then disto=0

 ; get astronometric header info
 extast,hd,astr

 if(astr.ctype(0) eq 'RA---GSS')then begin

    GSSSextast,hd,astr
    gsssadxy,astr,a,d,x,y

 endif else begin

    ad2xy,a,d,astr,x,y,disto=disto

 endelse


END

