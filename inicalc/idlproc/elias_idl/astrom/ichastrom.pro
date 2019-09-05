pro ichastrom,oldim,oldhd,newim,newhd,refhd,MISSING=missing, INTERP = interp, $
              CUBIC = cubic, DEGREE = Degree, NGRID = Ngrid, disto=disto,dist_ref=dist_ref, delt=delt

;+
; NAME:
;	ICHASTROM
; PURPOSE:
;	Linear transformation of an image to align it with a reference image
; EXPLANATION:
;	A linear transformation is applied (using POLY_2D) to an image so that   
;	its astrometry is identical with that in a reference header.  This
;	procedure can be used to align two images.
;
; CALLING SEQUENCE:
;	ICHASTROM, oldim, oldhd, newim, newhd, refhd, [MISSING =, INTERP = ]
;		or
;	ICHASTROM, oldim, oldhd, refhd, [MISSING =, INTERP =, NGRID =, CUBIC =]
;
; INPUTS:
;	OLDIM - Image array to be manipulated.  If only 3 parameters are
;		supplied then OLDIM and OLDHD will be modified to contain 
;		the output image array and header
;	OLDHD - FITS header array for OLDIM, containing astrometry parameters
;	REFHD - Reference header, containing astrometry parameters.  OLDIM
;		will be rotated, shifted, and compressed or expanded until
;		its astrometry matches that in REFHD.
; OUTPUTS:
;	NEWIM - Image array after linear tranformation has been performed.
;		The dimensions of NEWIM will be identical to the NAXIS1 and 
;		NAXIS2 keywords specified in REFHD.  Regions on the reference 
;		image that do not exist in OLDIM can be assigned a value with
;		the MISSING keyword.
;	NEWHD - Updated FITS image header associated with NEWIM
;
; OPTIONAL INPUT KEYWORDS:
;	MISSING - Set this keyword to a scalar value which will be assigned
;		to pixels in the output image which do not correspond to 
;		existing imput images.  If not supplied then linear 
;		extrapolation is used.   See the IDL manual on POLY_2D.
;	INTERP - Scalar, one of -1, 1, or 2 determining type of interpolation
;		-1 nearest neighbor, 1 (default) bilinear interpolation, 
;		2 cubic interpolation.
;	CUBIC -  a scalar value between -1 and 0 specifying cubic interpolation
;		with the specified value as the cubic interpolation parameter.
;		(see poly_2d for info).    Setting CUBIC to a value greater 
;		than zero is equivalent to setting CUBIC = -1. 
;	NGRID -  Integer scalar specifying the number of equally spaced grid 
;		points on each axis to use to specify the transformation.   
;		Default is GRID = 3 (9 total grid points).     The value of
;		GRID must always be greater than DEGREE + 1
;	DEGREE - Integer scalar specifying the degree of the transformation.
;		See the routine POLYWARP for more info.   Default = 1
;		(linear transformation).
;
; NOTES:
;	(1) The 3 parameter calling sequence is less demanding on virtual 
;		memory.
;	(2) The astrometry in OLDHD will be precessed to match the equinox
;		given in REFHD.
;
; EXAMPLE:
;	Suppose one has an image array, IM, and an associated FITS header H.
;	One desires to warp the image array so that it is aligned with another
;	image with a FITS header, HREF.    Both headers contain astrometry info.
;	Set pixel values to 0 where there is no overlap between the input and
;	reference image, and use linear interpolation (default)
;
;           IDL> ichastrom, IM, H, HREF, MISSING = 0
;
; PROCEDURES USED:
;	zparcheck, check_FITS, sxpar(), get_EQUINOX(), hprecess, extast,
;	sxaddpar, sxaddhist, putast
;
; REVISION HISTORY:
;	Written  W. Landsman, STX Co.              Feb, 1989
;	Updated to CHECK_FITS                      Dec, 1991
;	New astrometry keywords                    Mar, 1994
;	Recognize GSSS header   W. Landsman        June, 1994
;	Added CUBIC keyword     W. Landsman        March, 1997
;	Converted to IDL V5.0   W. Landsman   September 1997
;       Fixed so INTERP=-1 uses nearest neighbour    Seb Oliver August 1998
;       Fixed so PUTAST is not called if GSS astrom. Seb Oliver August 1998
;       Disto keyword added to allow correction for CAM distortions Seb Oliver August 1998
;
; Seb Oliver December 1999
;       Optional Perturbation added to polywarp as it does not
;       seem to work correctly with small images 
;       the current fix appears to work going from big pixels to small
;       pixels (if delt=+0.5), but not the other way around which
;       appears to be sensitive to the roll angles.  A routine
;       for testing this is appended in case anyone else can solve
;       this problem
;
; EXAMPLE:
;
; IDL> im=readfits('AFGL_4463S.fits.gz',hd)
; IDL> hd2=hd
; IDL> im2=im+100
; IDL> GSSS_STDAST, hd2
; IDL> hrotate,im2,hd2,1
; IDL> ichastrom,im2,hd2,im3,hd3,hd,interp=-1
; IDL>  hrotate,im2,hd2,1
; IDL>  hrotate,im2,hd2,1
; IDL>  hrotate,im2,hd2,1
;
;
;-
   On_error,2                   ;Return to caller
   npar = N_params()

   if (npar LT 3) or (npar EQ 4) then begin ;3 parameter calling sequence?
      print,'Syntax:  ICHASTROM, oldim, oldhd, refhd
      print,'     or  ICHASTROM, oldim, oldhd, newim, newhd, refhd'
      print,'                 [ MISSING=, DEGREE=, INTERP=, NGRID=, CUBIC = ]'
      return
   endif  

   if not keyword_set(disto) then disto=0
   if not keyword_set(dist_ref) then dist_ref=0

   if ( npar EQ 3 ) then begin
      zparcheck, 'ICHASTROM', newim, 3, 7, 1, 'Reference FITS header'
      refhd = newim
   endif else  $
    zparcheck, 'ICHASTROM', refhd, 5, 7, 1, 'Reference FITS header'

   radeg = 180.D/!DPI           ;Double precision !RADEG

   check_FITS, oldim, oldhd, dimen
   if !ERR EQ -1 then message,'ERROR - Invalid image or FITS header array'

   if N_elements(dimen) NE 2 then message, $
    'ERROR - Input image array must be 2-dimensional'
   xsize_old = dimen[0]  &  ysize_old = dimen[1]

   xsize_ref = sxpar( refhd, 'NAXIS1' ) ;Get output image size
   ysize_ref = sxpar( refhd, 'NAXIS2' ) 
   if (xsize_ref LT 1) or (ysize_ref LT 1) then message, $
    'ERROR - Reference header must be for a 2-dimensional image'

;   Precess the header if necessary

   newhd = oldhd
   refeq = get_equinox( refhd, code)
   if code EQ -1 then message, $
    'WARNING - Equinox not specified in reference header',/CON else begin
      oldeq = get_equinox( oldhd, code)
      if code EQ -1 then message, $
       'WARNING - Equinox not specified in original header',/CON else $
       if oldeq NE refeq then hprecess, newhd, refeq
   endelse

; Extract CD, CRPIX and CRVAL value from image header and reference header

   extast, newhd, astr_old, par_old    
   extast, refhd, astr_ref, par_ref    
   if ( par_old LT 0 ) or ( par_ref LT 0 ) then $  
    message,'FITS Headers do not contain sufficient astrometry'

; Make a grid of points in the reference image to be used for the transformation

   if not keyword_set( DEGREE ) then degree = 1
   if not keyword_set(NGRID) then ngrid = 3 
   if not keyword_set(CUBIC) then begin 
      cubic = 0
      if not keyword_set(INTERP) then Interp = 1
      if interp eq -1 then interp=0
   endif

   nxdif = round( xsize_ref / (ngrid-1) ) + 1
   nydif = round( ysize_ref / (ngrid-1) ) + 1

   xref = lonarr(ngrid,ngrid) & yref = xref
   xrow = [ lindgen(ngrid-1)*nxdif, xsize_ref-1. ]
   yrow = [ lindgen(ngrid-1)*nydif, ysize_ref-1. ]

   nxdif = float(xsize_ref) / (ngrid-1.)
   nydif = float(ysize_ref) / (ngrid-1.)

   xref = fltarr(ngrid,ngrid) & yref = xref
   xrow = findgen(ngrid)*nxdif-0.5
   yrow = findgen(ngrid)*nydif-0.5


   for i=0,ngrid-1 do xref[0,i] =   xrow ;Four corners of image
   for i=0,ngrid-1 do yref[0,i] = replicate( yrow[i], ngrid)

; Find the position of the reference points in the supplied image

   case strmid(astr_ref.ctype[0],5,3) of
      'GSS': gsssxyad, astr_ref, xref, yref, raref, decref
      else: xy2ad, xref, yref, astr_ref, raref, decref,dist=dist_ref
   endcase

   case strmid(astr_old.ctype[0],5,3) of
      'GSS': gsssadxy, astr_old, raref, decref, x, y
      else: ad2xy, raref, decref, astr_old, x, y, disto=disto
   endcase

;   Stop


   IF !debug EQ 1 THEN BEGIN 
      wset,1
      icplot, fltarr(xsize_old, ysize_old), oldhd
      oplot,x,y, psym=4, color=4, symsize=2
      oplot,x,y,color=4
      xyouts, x, y, string(indgen(n_elements(x)))
      plotsym, 0, 2
      icellipse_oplot, oldhd, raref, decref, psym=8, color=2

      wset, 2
      icplot, fltarr(xsize_ref, ysize_ref), refhd
      icheader_oplot, refhd, oldhd

      oplot,xref,yref, psym=4, color=4, symsize=2
      oplot,xref,yref,color=4
      xyouts, xref, yref, string(indgen(n_elements(x)))
      icellipse_oplot, refhd, raref, decref, psym=8, color=2
   ENDIF


   if ( max(x) LT 0 ) or ( min(x) GT xsize_old ) or $
    ( max(y) LT 0 ) or ( min(y) GT ysize_old ) then begin
      message,'ERROR - No overlap found between original and reference images',/CON
      print,'Be sure you have the right headers and the right equinoxes'
      return
   endif


; it appears that the polywarp and poly_2d IDL intrinsic functions
; are NOT consistant with the IDL pixel numbering convention
; we thus need to perturb the x and y possitions which
; are correctly calculated by xy2ad, ad2xy combination

;   delt = +0.5
;   delt = 0
   IF NOT keyword_set(delt) THEN delt = 0.
   polywarp, x+delt, y+delt, xref+delt, yref+delt, degree, kx, ky ;Get coefficients
   
   if N_elements(missing) NE 1 then begin ;Do the warping

      if npar EQ 3 then $
       oldim = poly_2d( temporary(oldim), kx, ky, Interp, xsize_ref, ysize_ref, $
                        CUBIC = cubic) else $
       newim = poly_2d( oldim, kx, ky, Interp, xsize_ref, ysize_ref, CUBIC = cubic)

   endif else begin

      if npar EQ 3 then $
       oldim = poly_2d( temporary(oldim), kx, ky, Interp, xsize_ref, ysize_ref, $
                        MISSING=missing, CUBIC = cubic) $
      else $
       newim = poly_2d( oldim, kx, ky, Interp, xsize_ref, ysize_ref, $
                        MISSING=missing, CUBIC = cubic)

   endelse

   sxaddpar, newhd, 'NAXIS1', xsize_ref
   sxaddpar, newhd, 'NAXIS2', ysize_ref


   case strmid(astr_ref.ctype[0],5,3) of
      'GSS':  gsssputast, newhd, astr_ref
      else:   putast, newhd, astr_ref
   endcase



   label = 'ICHASTROM: ' + strmid(systime(),4,20)
   image = sxpar( refhd, 'IMAGE', Count = N_image)
   if N_image EQ 1 THEN sxaddhist,label+' Reference Image - ' + image,newhd
   sxaddhist,label+' Original Image Size X: ' + strtrim(xsize_old,2) + $
    ' Y: '  + strtrim(ysize_old,2), newhd
; Update BSCALE and BZERO factors in header if necessary

   bscale = sxpar( newhd, 'BSCALE')
   if ( !ERR NE -1 ) and ( bscale NE 1. ) then begin
      getrot, astr_old, rot, cdelt_old
      getrot, astr_ref, rot, cdelt_ref
      pix_ratio = ( cdelt_old[0]*cdelt_old[1]) / (cdelt_ref[0]*cdelt_ref[1] )
      sxaddpar, newhd, 'BSCALE', bscale/pix_ratio
      bzero = sxpar( newhd,'BZERO' )
      if bzero NE 0. then sxaddpar, newhd, 'BZERO', bzero/pix_ratio
   endif

   if npar LT 4 then oldhd = newhd

   return
end
PRO check_hastrom
; setting up blank 100,100 array
; and 3X3 image within
scale = 1
n=100*scale
im100=fltarr(n,n)
im3=fltarr(3,3)
im3(*)=[0.,1.,2.,3.,4.,5.,6.,7.,8.]
roll=80.+90+90+90
roll2=0.
cdelt1=-10./3600.
cdelt2=10./3600.
rac=ten(14,12,10.)*15 & decc=ten(30,10,10)
icmkhdr,n,n,cdelt1,cdelt2,rac,decc,roll2,h100
icmkhdr,3,3,cdelt1*8*scale,cdelt2*8*scale,rac,decc,roll,h3
icmkhdr,90,90,cdelt1*8*scale/30.,cdelt2*8*scale/30.,rac,decc,roll,h90



; plotting these two up

window, 1
icplot,im3,h3, /nosc

window, 2
icplot,im100,h100

icheader_oplot,h100,h3,color=color_over,thick=2

; distoting the sub image to the big image

im3b = im3
h3b = h3
ichastrom,im3b,h3b,h100,interp=-1, missing=0, delt=+0.5
temp = im3

;changeastrom,im3, temp, h3, im3b,temp2, h100


; distorting the big image back to its original size
im3c = im3b
h3c = h3b
ichastrom,im3c,h3c,h3, interp=-1, missing=0;, delt=+0.5


; distorting the big image back to its original size at higher res
im9 = im3b
h9 = h3b
ichastrom,im9,h9,h90, interp=-1, missing=0;, delt=+0.5
; plotting these all up

window, 2
icplot,im3b,h3b, /noscal
icheader_oplot,h3b,h3,color=color_over,thick=2


window, 1

icplot,im3,h3, /noscal

window, 3
icplot,im3c,h3c, /noscal

window, 4
icplot,im9, h9, /noscal
END
