;***********************************************************************

PRO ICCAM_MKHDR, RA, DEC, ROLL, HD, NAXIS1=NAXIS1, NAXIS2=NAXIS2, $
                 FLIPJ=FLIPJ,  EQUINOX=EQUINOX, pfov=pfov

;+
; NAME:
;	ICCAM_MKHDR
;
; PURPOSE:
;	Makes a proper FITS header for a single CAM frame
;
; CATEGORY:
;	
;
; CALLING SEQUENCE:
;	ICCAM_MKHDR, RA, DEC, ROLL, HD
;
; INPUTS:
;       RA:    ISO RA in degrees
;       DEC:   ISO DEC in degrees
;	ROLL:  ISO ROLL paramter
;
; OPTIONAL INPUTS:
;
;	
; KEYWORD PARAMETERS:
;	NAXIS1=NAXIS1: Number of pixels in x-axis (Default=32)
;	NAXIS2=NAXIS2: Number of pixels in y-axis (Default=32)
;	FLIPJ:  If set to 1 Astrometry is calculated assuming J-axis is
;               flipped w.r.t the natural CAM orienation
;               If unset of -1 then natural CAM orientation used 
;       EQUINOX: Equinox of Astrometry (default 2000)
;       pfov:  Pixel Field of view in arc seconds (Default 6")
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
;	Sets up a header which rotates the i-axis to the RA axis
;       and then flips the DEC axis if necessary.  N.B. the
;       rotation is in the opposite sense from ROLL if in
;       natural CAM coordinates.
;       Could be improved by checking which coordinate is
;       varying most and ajusting CTYPE and CRVAL accordingly.
;
; EXAMPLE:
;	Please provide a simple example here.
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver 23-May-1996
;	June, 1996	Error handling and EQUINOX added
;       June, 1996      renamed ICCAM_MKHDR
;       26 Nov 1998:    pfov keyword added.
;       13th May 1999:  Bug fixed so that repeated calls with
;                       flipj=flipj when flipj starts off at 0 
;                       did not change to the opposite sense
;                       since flipj ends up "set" to -1 
;                      
;-

;  if(n_elements(hd) gt 1) then print,'Overwriting header'

; Return to caller for error conditions
   on_error, 2

   if not keyword_set(pfov) then pfov=6.

; Check that there are sufficient input parameters 
   if ( n_params() lt 4 ) then begin
      print, 'Usage: ICCAM_MKHDR, ra, dec, roll, head, [NAXIS1=NAXIS1, NAXIS2=NAXIS2, FLIPJ=FLIPJ,  EQUINOX=EQUINOX]
      return
   endif

; Setting defaults for optional keywords
   if (not keyword_set(naxis1))then naxis1=32
   if (not keyword_set(naxis2))then naxis2=32
   if (not keyword_set(equinox))then equinox=2000.

   if (keyword_set(flipj))then BEGIN
      CASE flipj OF
         -1:  BEGIN
            message, 'Previous uses of this code may have caused ERRORS', /inf
            flipj= -1
         END
         1: flipj = 1
         ELSE: message, 'FLIPJ not recognised'
      ENDCASE

   ENDIF ELSE BEGIN
      flipj=-1
   ENDELSE


; Reference Pixel (centre of image)
   crpix1=(naxis1+1.)/2.
   crpix2=(naxis2+1.)/2.

; ROTA FITS keyword
   crota2=roll*flipj
   srot=sin(crota2/180.*!pi)
   crot=cos(crota2/180.*!pi)

; Setting CAM pixe size
   cdelt1= pfov/3600.0d0

   cdelt2= cdelt1 * flipj

; Setting Projection to Equatorial TAN.
   ctype1='RA---TAN'
   ctype2='DEC--TAN'

; Basic Header
   mkhdr,hd,fltarr(naxis1,naxis2)

; FITS Astrometry Header stuff
   sxaddpar,hd,'CRPIX1',crpix1,'Reference Pixel'
   sxaddpar,hd,'CRPIX2',crpix2,'Reference Pixel'

   sxaddpar,hd,'CRVAL1',ra,'Reference Pixel Value /Degrees'
   sxaddpar,hd,'CRVAL2',dec,'Reference Pixel Value /Degrees'

   sxaddpar,hd,'CTYPE1',ctype1,'X-axis type'
   sxaddpar,hd,'CTYPE2',ctype2,'Y-axis type'
   sxaddpar,hd,'CDELT1',cdelt1,'Degrees/pixel'
   sxaddpar,hd,'CDELT2',cdelt2,'Degrees/pixel'

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


