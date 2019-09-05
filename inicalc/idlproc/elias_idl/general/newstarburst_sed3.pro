;***********************************************************************
       function newstarburst_sed3, lam, lam0=lam0,m82=m82

;+
; NAME:
;	newstarburst_sed
;
; PURPOSE:
;	Returns a starburst SED 
;
; CATEGORY:
;	analysis
;
; CALLING SEQUENCE:
;	sed=newstarburst_sed(lam)
;
; INPUTS:
;
; OPTIONAL INPUTS:
;	lam:  wavelength in Micron
;	
;	
; KEYWORD PARAMETERS:
;	lam0: if set and lam is not then walues from data file are returned
;        m82: if set then m82 spectrum is used
;
; OUTPUTS:
;	newstarburst_sed:  nuFnu of starburst SED. Normalised so that the
;                       bolometric luminosity of the model is equal to 1
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
;                       Updated to  read starburst spectrum from a file
;                       Starburst model now includes PAHs
;       Decmeber 2, 1997 Seb Oliver New M82 model added and 
;                        does not require input lambda to be specfied
;-

if keyword_set(m82) then begin
  file='/home/ane/Other_sources/xx.spec'
endif else begin
  file='/home/ane/Other_sources/xx.spec'
endelse

get_lun,unit
openr, unit, file
readf, unit, nv, npoint
spectrum=fltarr(nv+1,npoint)
readf, unit, spectrum
free_lun,unit


lam0=fltarr(npoint)
nuFnu=lam0
lam0=reform(spectrum(0,*),npoint)
nu=1./lam0
nuFnu=reform(spectrum(1,*),npoint)

if n_params() eq 1 then begin
  newstarburst_sed=10.0^interpol(alog10(nuFnu),alog10(lam0),alog10(lam))
endif else begin
  if keyword_set(lam0) then begin
    newstarburst_sed=nuFnu
  endif else begin
    message,'CALLING SEQUENCE: newstarburst_sed, lam, lam0=lam0,/m82'
  endelse
endelse

;
; normalize to the bolometric luminosity
;
newstarburst_sed3=newstarburst_sed/int_tabulated2(nu,nuFnu/nu)

return, newstarburst_sed3


end

