;***********************************************************************

PRO ICMKHDR, NAXIS1, NAXIS2, CDELT1, CDELT2, RA, DEC, ROT , hd, $
             equinox=equinox, crpix=crpix, detector=detector, type=type


;+
; NAME:
;	ICMKHDR
;
; PURPOSE:
;	Makes a  FITS header with simple astrometry
;
; CATEGORY:
;	
;
; CALLING SEQUENCE:
;	ICMKHDR, NAXIS1, NAXIS2,  CDELT1, CDELT2, RA, DEC, ROLL, HD, $
;                equinox=equinox, crpix=crpix
;
; INPUTS:
;       NAXIS1: Number of pixels on axis 1
;       NAXIS2: Number of pixels on axis 2
;       CDELT1: FITS parameter
;       CDELT1: FITS parameter
;       RA:    RA in degrees
;       DEC:   DEC in degrees
;	ROT:  ROT paramter
;
; KEYWORDS:
;       EQUINOX: Equinox of Astrometry (default 2000)
;       CRPIX: 2 element vector giving FITS reference pixel
;       DETECTOR: If set then image is a view onto the detector
;                 rather than onto the sky e.g. for CAM
;
; OUTPUTS:
;	HD: FITS header with appropriate astrometric keywords
;
; OPTIONAL OUTPUTS:
;	Describe optional outputs here.
;
; COMMON BLOCKS:
;	BLOCK1:	Describe any common blocks here.
;
; SIDE EFFECTS:
;	Describe "side effects" here.
;
; RESTRICTIONS:
;	Describe any "restrictions" here.
;
; PROCEDURE:

;
; EXAMPLE:
;	Please provide a simple example here.
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver 23-July-1996 (adapted from ICCAM_MKHDR)
;	June, 1996	
;       June, 1996      
;-

;  if(n_elements(hd) gt 1) then print,'Overwriting header'


; Return to caller for error conditions
;on_error, 2

; Check that there are sufficient input parameters 
if ( n_params() lt 8 ) then begin
   message, $
  'Usage: ICMKHDR, NAXIS1, NAXIS2, CDELT1, CDELT2, RA, DEC, ROT , hd, equinox=equinox, /detector, crpix=crpix'

   return
endif

if not keyword_set(type) then type=4
; Setting defaults for optional keywords

  if (not keyword_set(equinox))then equinox=2000.
  if (keyword_set(detector))then flip=-1 else flip=1

; Reference Pixel (centre of image)
  if keyword_set(crpix) then begin
    crpix1=crpix(0)
    crpix2=crpix(1)
  endif else begin
    crpix1=(naxis1+1.)/2.
    crpix2=(naxis2+1.)/2.
  endelse

; ROTA FITS keyword
  crota2=rot*flip
  srot=sin(crota2/180.*!pi)
  crot=cos(crota2/180.*!pi)




; Setting  pixe size
;  cdelt1= 0.001666666707
;  cdelt2= 0.001666666707

; Setting Projection to Equatorial TAN.
  ctype1='RA---TAN'
  ctype2='DEC--TAN'

; Basic Header
; no longer passes an array down as this can be very slow for large images.
; Seb Oliver 5th May 1998 
  mkhdr,hd,type,[naxis1,naxis2]

; FITS Astrometry Header stuff
  sxaddpar,hd,'CRPIX1',crpix1,'Reference Pixel'
  sxaddpar,hd,'CRPIX2',crpix2,'Reference Pixel'

  sxaddpar,hd,'CRVAL1',ra,'Reference Pixel Value /Degrees'
  sxaddpar,hd,'CRVAL2',dec,'Reference Pixel Value /Degrees'

  sxaddpar,hd,'CTYPE1',ctype1,'X-axis type'
  sxaddpar,hd,'CTYPE2',ctype2,'Y-axis type'
  sxaddpar,hd,'CDELT1',cdelt1,'Degrees/pixel'
; flipping detector axis if necessary
  sxaddpar,hd,'CDELT2',cdelt2*flip,'Degrees/pixel'

  sxaddpar,hd,'CROTA2',crota2,'Rotation/Degrees'

; Deleting these Obsolete FITS astrometry keywords
  sxdelpar,hd,'CD001001'
  sxdelpar,hd,'CD002002'
  sxdelpar,hd,'CD001002'
  sxdelpar,hd,'CD002001'

; Putting in the Recommend FITS astrometry keywords
  sxaddpar,hd,'PC001001',crot,'Coordinate Increment'
  sxaddpar,hd,'PC002002',crot,'Coordinate Increment'
  sxaddpar,hd,'PC001002',-srot,'Coordinate Increment'
  sxaddpar,hd,'PC002001',srot,'Coordinate Increment'

; EQUINOX
  sxaddpar,hd,'EQUINOX',equinox,'Equinox of coordinates'



end


