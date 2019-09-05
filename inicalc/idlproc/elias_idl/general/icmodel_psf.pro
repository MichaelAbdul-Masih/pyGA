;***********************************************************************

PRO icmodel_psf,psf,cen0,pfov,lambda,diam=diam

;+
; NAME:
;	icmodel_psf
;
; PURPOSE:
;	models a psf of a given image size 
;       image, pixel field of view, wavelength and
;       source cen0
;
; CATEGORY:
;	analysis
;
; CALLING SEQUENCE:
;	icmodel_psf,psf,cen0,pfov,lambda
;
; INPUTS:
;	psf: 2-D array for psf (Defualt 32,32)
;       cen0: pixel coordinates of cen0 of source
;       pfov: pixel field of view in arc sec
;       lambda: wavelength in micron
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	diam: diameter of telescope
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
; 	Written by:	Seb Oliver 26th September 1996
;       19th January 1999 - diamter keyword added to allow other telescopes
;	September, 1996	
;-


on_error,2
if not keyword_set(diam) then diam=0.6

if n_params() lt 4 then $
  message,'icmodel_psf,psf,cen0,pfov,lambda'

psf_size=size(psf)
if psf_size(0) ne 2 then psf=fltarr(32,32)
psf_size=size(psf)

; coordinates in large pixel array
; of pixels in 10 times oversampled array

i=(0.5+findgen(psf_size(1)*10)#replicate(1,psf_size(2)*10))/10. -0.5
j=(0.5+replicate(1,psf_size(1)*10)#findgen(psf_size(2)*10))/10. -0.5

; r is radius from cen0 of psf in arc seconds
r=sqrt((i-cen0(0))^2+(j-cen0(1))^2)*pfov
psf_oversampled=icairy(r,lambda,diam=diam)
psf=rebin(psf_oversampled,psf_size(1),psf_size(2))
psf=psf/total(psf)

END

