 pro gss_putast, hdr, astr,  EQUINOX=equinox

;+
; NAME:
;       GSS_PUTAST
; PURPOSE:
;       Put astrometry parameters into a given FITS header.
;
; CALLING SEQUENCE:
;       gss_putast, hdr, astr, [EQUINOX]
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
;               As of September 1997, there is still no general agreement on
;               the notation to use for the CD matrix; however form (0) will 
;               almost certainly not be used.   In addition, since form
;               (2) is used for HST images, it almost certainly will be 
;               supported in the future.     The document "Representations of 
;               Celestial Coordinates in FITS" by Griesen and Calabretta at   
;               http://www.cv.nrao.edu/fits/documents/wcs/wcs.html discusses
;               these issues but note this is only a draft.
;               If CD_TYPE is not supplied, GSS_PUTAST will try to determine the 
;               type of astrometry already in the header.   If there is no 
;               astrometry in the header then the default is CD_TYPE = 2
; NOTES:
;       The recommended use of this procedure is to supply an astrometry
;       structure.    
;
; PROMPTS:
;       If only a header is supplied, the user will be prompted for a plate 
;       scale, the X and Y coordinates of a reference pixel, the RA and
;       DEC of the reference pixel, the equinox of the RA and Dec and a 
;       rotation angle.
;
; PROCEDURES USED:
;       DATATYPE(), GET_COORDS, SXADDPAR, SXPAR(), ZPARCHECK
; REVISION HISTORY:
;
; EXAMPLE:
;
; IDL> im=readfits('AFGL_4463S.fits.gz',hd)
; IDL> hd2=hd
; IDL> gsssextast,hd,a
; IDL> gss_putast,hd2,a
; IDL> gsssextast,hd2,a2
;-
 npar = N_params()

 if ( npar EQ 0 ) then begin	;Was header supplied?
	print,'Syntax: GSS_PUTAST, astr, [ EQUINOX = ]
	return
 end

 zparcheck, 'GSS_PUTAST', hdr, 1, 7, 1, 'FITS image header'


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

 
 sxaddhist,'GSS_PUTAST: ' + strmid(systime(),4,20) ,hdr

 return
 end
