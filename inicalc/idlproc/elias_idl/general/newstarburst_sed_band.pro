;***********************************************************************
       function newstarburst_sed_band, lam, band,lam0=lam0,  radio=radio, m82=m82
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
;	sed=newstarburst_sed_band(lam,band,/radio)
;
; INPUTS:
;
; OPTIONAL INPUTS:
;	lam:  wavelength in Micron
;	band: bandwidth in Micron if zero then nuFnu is returned
;	
; KEYWORD PARAMETERS:
;	
;
; OUTPUTS:
;	newstarburst_sed_band:  
;                       Integral of Fnu dnu over band
;                       of starburst SED. Normalised so that the
;                       bolometric luminosity of the model is equal to 1
;                       If a bandwidth of zero is set then nuFnu is returned
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
; 	Written by:	Seb Oliver 20 th August 1996
;	November, 1996	Andreas Efstathiou
;                       Updated to  read starburst spectrum from a file
;                      Starburst model now includes PAHs
;       November, 1996  Bandpass added and radio option added
;-
on_error,2
   COMMON newsb, npoint, spectrum, m82flag


if n_params() lt 0 then $
   message,'calling sequence: sed=newstarburst_sed(lam,band)'
if n_params() lt 2 then begin
   print, 'Band not set or zero returning nuFnu'
   donufnu=1
   band=0.
endif else begin
  if  n_elements(band) eq 1 and band(0) le 0. then begin
     donufnu=1
  endif else begin
      if n_elements(lam) ne n_elements(band) then begin
         message,'lambda and bandwidth must have same number of elements'
      endif else begin
         donufnu=0
      endelse
  endelse
endelse
;

if keyword_set(m82) then begin
  file=!elais_dir+'data/zz3.spec'
endif else begin
  file=!elais_dir+'data/starburst.spec'
  m82 = 0
endelse

   IF NOT keyword_set(spectrum) THEN BEGIN
      read_file = 1   
   ENDIF ELSE BEGIN 
      IF m82flag NE m82 THEN BEGIN
         read_file = 1
      ENDIF ELSE read_file = 0
   ENDELSE

   IF read_file EQ 1 THEN BEGIN 
      
      get_lun,unit
      openr, unit, file
      readf, unit, nv, npoint
      spectrum=fltarr(nv+1,npoint)
      readf, unit, spectrum
      free_lun,unit
      m82flag = m82

   ENDIF


   lam0=fltarr(npoint)
   nuFnu=lam0
   lam0=reform(spectrum(0,*),npoint)
   nu=1./lam0
   nuFnu=reform(spectrum(1,*),npoint)

   IF n_params() EQ 0 THEN lam = lam0


if donufnu then begin

   if n_params() eq 1 then begin
      newstarburst_sed=10.0^interpol(alog10(nuFnu),alog10(lam0),alog10(lam))
   endif else begin
;      if keyword_set(lam0) then begin
         newstarburst_sed=nuFnu
;         lam = lam0
;      endif else begin
;         message,'CALLING SEQUENCE: newstarburst_sed, lam, lam0=lam0,/m82'
;      endelse
   endelse
   newstarburst_sed_band0 = newstarburst_sed

endif else begin

lamstart=lam-band/2.>min(lam0)<max(lam0)
lamend=lam+band/2.>min(lam0)<max(lam0)
nu_start=1./lamend
nu_end=1./lamstart


nufnustart=10.0^interpol(alog10(nuFnu),alog10(lam0),alog10(lamend))
nufnuend=10.0^interpol(alog10(nuFnu),alog10(lam0),alog10(lamstart))

   newstarburst_sed_band0=fltarr(n_elements(lam))

   for i=0, n_elements(lam)-1 do begin

      middle=where(nu gt nu_start(i) and nu lt nu_end(i),count)
      if count gt 0 then begin
        nu_use=[nu_start(i),nu(middle),nu_end(i)]
        nufnu_use=[nufnustart(i),nufnu(middle),nufnuend(i)]
      endif else begin
        nu_use=[nu_start(i),nu_end(i)]
        nufnu_use=[nufnustart(i),nufnuend(i)]
      endelse
  
      newstarburst_sed_band0(i)=int_tabulated2(alog(nu_use),nufnu_use)
  
   endfor
endelse

;
; normalize to the FIR bolometric luminosity
;
   lbol=int_tabulated2(nu,nuFnu/nu)
   newstarburst_sed_band=newstarburst_sed_band0/lbol

if keyword_set(radio) then begin
;
; Adding in Radio component Srad=S60/90.*(nu/nu1.4GHz)^-0.8
; Nu(1.4Ghz)=1./2.14e5 (frequencey units are 1/micron)
  nfnu60=10.0^interpol(alog10(nuFnu),alog10(lam0),alog10(60.))

; Fnu(1.4Ghz)

rad_1_4Gy=interpol(alog10(nuFnu),alog10(lam0),alog10(60.))+$
                   alog10(60.)-alog10(2.14e5)-alog10(90.)-alog10(lbol)
radio_sed=10.0^rad_1_4Gy(0)*(lam/2.141e5)^(0.8)

if donufnu then begin
  radio_sed=radio_sed*2.145e5/lam
endif else begin

endelse
radio_sed(where(lam lt 100.))=0.

newstarburst_sed_band=newstarburst_sed_band+radio_sed

endif

return, newstarburst_sed_band

end





