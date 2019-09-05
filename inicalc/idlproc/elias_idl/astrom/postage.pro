;***********************************************************************

PRO POSTAGE, IM1, HD1, RA, DEC, IM2, HD2, PSIZE=PSIZE, EQUINOX=EQUINOX

;+
; NAME:
;	POSTAGE
;
; PURPOSE:
;	Extracts a postage stamp image approximately centred on a cellestial
;       position
;
; CATEGORY:
;	Database tools
;
; CALLING SEQUENCE:
;	POSTAGE, IM1, HD1, RA, DEC, IM2, HD2, PSIZE=PSIZE
;
;
; INPUTS:
;	IM1:  Image from which to extract Postage stamp
;       HD1:  Header of Image 1
;       RA:   RA of target (Decimal Degrees) scalar
;       DEC:  DEC of target (Decimal Degrees) scalar
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	PSIZE: Approximate  SIZE of postage stamp (Degrees), Default=0.05
;       EQUINOX:  Equinox of RA and DEC (Default J2000)
; OUTPUTS:
;	IM2: Postage stamp Image centred around target position
;       HD2: FITS Header for postage stamp
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
;	Rotation of postage stamp is same as original image
;       Image centre and size are to nearest pixel only
;       Checked for GSSS astrometry only
;       Ideally should work with vector input
;
; PROCEDURE:
;	
;
; EXAMPLE:
;	
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver June 1996
;	July, 1994	Any additional mods get described here.
;-

; on_error,2

 if N_params() lT 6 then begin
        print,'Syntax -- Postage, im1, hd1, ra, dec, im2, hd2,',$
              '[equinox=equinox], [psize=psize]'
        return
 endif

if not keyword_set(equinox) then equinox = 2000
if not keyword_set(PSIZE) then PSIZE = 0.05

; image size
imsize=size(im1)
ixmax=imsize(1)-1
iymax=imsize(2)-1


; precessing header info to RA/DEC equinox

 htemp=hd1
 if(get_equinox(htemp) ne equinox) then hprecess,htemp,equinox


; target position on image
 icad2xy,ra,dec,htemp,x,y

; calculating approx  size of pixels at target
 icxy2ad,x+1.,y,htemp,ra1,dec1         & icxy2ad,x,y+1.,htemp,ra2,dec2
 gcirc,1,ra1/15.,dec1,ra/15.,dec,dis1  & gcirc,1,ra2/15.,dec2,ra/15.,dec,dis2 
 px=dis1/3600.                         & py=dis2/3600.

; integerising 
 hxsize=psize/px/2. & hysize=psize/py/2.
 ix=fix([x-hxsize+0.5,x+hxsize+0.5])  & iy=fix([y-hysize+0.5,y+hysize+0.5]) 

 ix0=max([0,ix(0)]) & ix1=min([ixmax,ix(1)])
 iy0=max([0,iy(0)]) & iy1=min([iymax,iy(1)])


; checking target within image
  if ix(0) ge ixmax or ix(1) le 0 or $
     iy(0) ge iymax or iy(1) le 0 then begin
     print,'Target outside image'
     return
  endif

; if image at edge of frame make postage stamp extend inwards to
; keep it square.

if ix0 eq 0 then ix1=min([nint(hxsize*2),ixmax-1])
if iy0 eq 0 then iy1=min([nint(hysize*2),iymax-1])
if ix1 eq ixmax then ix0=max([0,ix1-nint(hxsize*2)])
if iy1 eq iymax then iy0=max([0,iy1-nint(hysize*2)])



print,ix0,ix1,iy0,iy1
 
im2=im1(ix0:ix1,iy0:iy1)


; setting up new header
 hd2=hd1

 sxaddpar,hd2,'NAXIS1',ix1-ix0+1
 sxaddpar,hd2,'NAXIS2',iy1-iy0+1

;  New astrometry

 extast,hd1,a1

 if(a1.ctype(0) eq 'RA---GSS')then begin 
;   gsss_stdast,hd2

; changing lower left corner of image (N.B. CNPIX in pixels not micron)
   a1.xll=a1.xll+ix0
   a1.yll=a1.yll+iy0
   sxaddpar,hd2,'CNPIX1',a1.xll
   sxaddpar,hd2,'CNPIX2',a1.yll

 endif else begin

   a1.crpix(0)=a1.crpix(0)-ix0
   a1.crpix(1)=a1.crpix(1)-iy0

   sxaddpar,hd2,'CRPIX1',a1.crpix(0)
   sxaddpar,hd2,'CRPIX2',a1.crpix(1)

 endelse


; 


END



