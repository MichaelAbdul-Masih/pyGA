;***********************************************************************       

function newstarburst_sed, lam, lam0=lam0,m82=m82, band, $
                           radio=radio, arp220=arp220

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

   

   COMMON newsb, npoint, spectrum, m82flag

   if keyword_set(m82) then begin

      file = !elais_dir+'data/zz3.spec'
   endif else begin
      IF keyword_set(arp220) THEN BEGIN 
         file = !elais_dir+'data/starburst.spec'
         m82 = 0
      ENDIF ELSE begin
         file = !elais_dir+'data/starburst.spec'
         m82 = 0
      ENDELSE

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

;
; normalize to the bolometric luminosity
;

;
; normalize to the FIR bolometric luminosity
;
   lbol=int_tabulated2(nu,nuFnu/nu)

   newstarburst_sed=newstarburst_sed/lbol

   if keyword_set(radio) then begin
;
; Adding in Radio component Srad=S60/90.*(nu/nu1.4GHz)^-0.8
; Nu(1.4Ghz)=1./2.14e5 (frequencey units are 1/micron)
      nfnu60=10.0^interpol(alog10(nuFnu),alog10(lam0),alog10(60.))

; Fnu(1.4Ghz)

      rad_1_4Gy=interpol(alog10(nuFnu),alog10(lam0),alog10(60.))+$
       alog10(60.)-alog10(2.14e5)-alog10(90.)-alog10(lbol)
      radio_sed=10.0^rad_1_4Gy(0)*(lam/2.141e5)^(0.8)

      donufnu = 1
      if donufnu then begin
         radio_sed=radio_sed*2.145e5/lam
      endif else begin

      endelse
      radio_sed(where(lam lt 100.))=0.


      newstarburst_sed=newstarburst_sed+radio_sed
   ENDIF


   return, newstarburst_sed


end


