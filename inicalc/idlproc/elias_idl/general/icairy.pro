;***********************************************************************

function icairy,theta,lambda,diam=diam

;+
; NAME:
;	icairy
;
; PURPOSE:
;	Produces the intensity of an ISO airy disk at a specified
;       radius
;
; CATEGORY:
;	analysis
;
; CALLING SEQUENCE:
;	psf=icairy(theta,lambda)
;
; INPUTS:
;	
;       theta: angle from peak of source position in arc seconds
;      lambda: wavelength in microns
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	diam:  diameter of telescope in microns
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
; 	Written by:	Seb Oliver 19th September 1996
;       19th January 1999 - diamter keyword added to allow other telescopes
;	September, 1996	
;-

; default diameter is for ISO.


if not keyword_set(diam) then diam=0.6
if n_elements(diam) ne 1 then message,'Diam must be scalar'
diam=diam(0)


; aperture in units of microns
aperture= diam*1.e6

ps=theta

m= (!dpi^2/ 648000.* aperture/ lambda)* theta 
small=where(m le 1.e-8,small_count)
big=where(m gt 1.e-8,big_count)

if big_count gt 0 then ps(big)=(2.* beselj(m(big), 1)/m(big))^2
if small_count gt 0 then ps(small)=1.


return, ps


END

