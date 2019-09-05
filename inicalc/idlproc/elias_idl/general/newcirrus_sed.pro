;***********************************************************************
       function newcirrus_sed, lam

;+
; NAME:
;	newcirrus_sed
;
; PURPOSE:
;	Returns an SED that fits the spectrum of cirrus towards
;       the center of the galaxy.
;
; CATEGORY:
;	analysis
;
; CALLING SEQUENCE:
;	sed=newcirrus_sed(lam)
;
; INPUTS:
;
; OPTIONAL INPUTS:
;	lam:  wavelength in Micron
;	
;	
; KEYWORD PARAMETERS:
;	
;
; OUTPUTS:
;	newcirrus_sed:  nuFnu of cirrus SED. Normalised so that the
;                       bolometric luminosity of the model integrates to 1
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
; 	Written by:	Seb Oliver 20 th August 1996
;	November, 1996	Andreas Efstathiou
;                       Updated to  read cirrus spectrum from a file
;-                      Cirrus model now includes PAHs

get_lun, unit
openr, unit, !elais_dir+'data/cirrus.spec'
readf, unit, nv, npoint
spectrum=fltarr(nv+1,npoint)
readf, unit, spectrum
free_lun, unit

wel=fltarr(npoint)
nuFnu=wel
wel=spectrum(0,*)
nu=1./wel
nuFnu=spectrum(1,*)

newcirrus_sed=10.0^interpol(alog10(nuFnu),alog10(wel),alog10(lam))
;
; normalize to the bolometric luminosity
;
newcirrus_sed=newcirrus_sed/int_tabulated(nu,nuFnu/nu)

return, newcirrus_sed

 end



