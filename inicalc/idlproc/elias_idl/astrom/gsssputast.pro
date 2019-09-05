 pro gsssputast, hdr, astr,  EQUINOX=equinox

;+
; NAME:
;       GSSSPUTAST
; PURPOSE:
;       Put astrometry parameters into a given FITS header.
;
; CALLING SEQUENCE:
;       gsssputast, hdr, astr, [EQUINOX]
;
; INPUTS:
;       HDR -  FITS header, string array.   HDR will be updated to contain
;               the supplied astrometry.
;       ASTR - IDL structure containing values of the GSSS astrometry parameters
;              See GSSEXTAST for more info about the structure definition
;                            or
;
; OUTPUTS:
;       HDR - FITS header now contains the updated astrometry parameters
;               A brief HISTORY record is also added.
;
; OPTIONAL KEYWORD INPUTS:
;       EQUINOX - numeric scalar giving equinox (e.g. 2000) of the reference 
;                coordinates 
;
;
; NOTES:
;
; PROMPTS:
;
; PROCEDURES USED:
;    SXADDPAR, SXPAR, zparcheck, sixty
; REVISION HISTORY:
;     Original Version, Seb Oliver (ICSTM) 4th August 1998.
;
; EXAMPLE:
;
; IDL> im=readfits('AFGL_4463S.fits.gz',hd)
; IDL> hd2=[hd[0:5],hd[106]]
; IDL> gsssextast,hd,a
; IDL> gsssputast,hd2,a
; IDL> gsssextast,hd2,a2
;-
 npar = N_params()

 if ( npar EQ 0 ) then begin	;Was header supplied?
	print,'Syntax: GSSSPUTAST, astr, [ EQUINOX = ]
	return
 end

 zparcheck, 'GSSSPUTAST', hdr, 1, 7, 1, 'FITS image header'


 sxaddpar,hdr,'CRPIX1',astr.xll
 sxaddpar,hdr,'CRPIX2',astr.yll

 sxaddpar,hdr,'XPIXELSZ',  astr.xsz 
 sxaddpar,hdr,'YPIXELSZ',  astr.ysz 
 sxaddpar,hdr,'PPO3'    ,  astr.ppo3 
 sxaddpar,hdr,'PPO6'    ,  astr.ppo6 
 sxaddpar,hdr,'PLTSCALE',  astr.pltscl

 ra=astr.crval[0]/15.0d0
 dec=astr.crval[1]
 
 rasix=sixty(ra)
 decsix=sixty(abs(dec))

 sxaddpar, hdr, 'PLTRAH'  ,rasix(0)
 sxaddpar, hdr, 'PLTRAM'  ,rasix(1)
 sxaddpar, hdr, 'PLTRAS'  ,rasix(2)
 if dec lt 0 then sxaddpar, hdr,'PLTDECSN','-' else  sxaddpar, hdr,'PLTDECSN','+'
 sxaddpar, hdr, 'PLTDECD' ,decsix(0)
 sxaddpar, hdr, 'PLTDECM' ,decsix(1)
 sxaddpar, hdr, 'PLTDECS' ,decsix(2)



 ii = strtrim(indgen(11)+1,2)
  for i = 0,10 do begin

    sxaddpar,hdr, 'AMDX' + ii[i], astr.amdx[i] 
    sxaddpar,hdr, 'AMDY' + ii[i], astr.amdy[i]
 
 endfor



;   Add EQUINOX keyword and value to FITS header

 if N_elements( equinox ) EQ 0 then begin
    equinox = sxpar( hdr, 'EQUINOX')            ;Is EQUINOX already in header?
    if !ERR eq -1 then begin
      read,'Enter equinox for reference pixel coordinates (e.g. 2000): ',equinox
    if (equinox LT 1850.) or (equinox GT 2100.) then message, $
     'Equinox value of '+ strtrim(equinox,2) + ' not added to header', /INFORM $
   else sxaddpar, hdr, 'EQUINOX', equinox, ' Equinox of Ref. Coord.', 'HISTORY'
   endif 

 endif else $
     sxaddpar,hdr, 'EQUINOX', equinox, 'Equinox of Ref. Coord.', 'HISTORY'

 
 sxaddhist,'GSSSPUTAST: ' + strmid(systime(),4,20) ,hdr

 return
 end
