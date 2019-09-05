;***********************************************************************

PRO rectangle,he,ra,dec,roll,x,y,_extra=_extra,h_dum

;+
; NAME:
;	rectangle
;
; PURPOSE:
;	Overplots rectangle
;       on an image with astrometry defined by header HD1
;
; CATEGORY:
;	
;
; CALLING SEQUENCE:
;	rectangle,he,ra,dec,roll,x,y
;
;
; INPUTS:
;	HD:  FITS header of displayed image
;       ra: RA in degrees (J2000)
;      dec: DEC in degrees
;     roll: ROLL angle (FITS convention)
;        x: size in x
;        y: size in y
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	
;
; OUTPUTS:
;	h_dum, dummy header used for overplotting
;
; OPTIONAL OUTPUTS:
;	
;
; COMMON BLOCKS:
;	
;
; SIDE EFFECTS:
;	Plots to current window
;
; RESTRICTIONS:
;	
;
; PROCEDURE:
;	
;
; EXAMPLE:
;	IDL> implot, im1, hd1
;       IDL> retangle, hd1, hd2
;       
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver 24-May-1996
;	June, 1996	Rename ICHEADER_OPLOT
;       31 August 2000; fixed bug if x and y are sent down as integers
;                       Seb Oliver
;       
;-

on_error,2

if n_params() lt 1 then message,'rectangle,he,ra,dec,roll,x,y,_extra=_extra,h_dum'

;set up dummy fits header has to be a 2-D array thus 1 pixel by 1 pixel
; doesn't work

if x lt y then begin
  naxis1=100
  naxis2=nint(100.*float(y)/float(x))
endif else begin
  naxis2=100
  naxis1=nint(100.*float(x)/float(y))
endelse

for i=0,n_elements(ra)-1 do begin
  icmkhdr,naxis1,naxis2,float(x)/naxis1,float(y)/naxis2,ra[i],dec[i],roll,h_dum
  icheader_oplot,he,h_dum,_extra=_extra
endfor

;stop
END

