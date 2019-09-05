pro ad2xy, a, d, astr, x, y,disto=disto
;+
; NAME:
;	AD2XY
; PURPOSE:
;	Compute X and Y from  RA and DEC and a FITS  astrometry structure
; EXPLANATION:
;	A tangent (gnomonic) projection is
;	computed directly; other projections are computed using WCSXY2SPH.  
;	AD2XY is meant to be used internal to other procedures.   For 
;	interactive purposes, use ADXY.
;
; CALLING SEQUENCE:
;	AD2XY, a ,d, astr, x, y   
;
; INPUTS:
;	A -     R.A. in DEGREES, scalar or vector
;	D -     Dec. in DEGREES, scalar or vector
;	ASTR - astrometry structure, output from EXTAST procedure containing:
;   	 .CD   -  2 x 2 array containing the astrometry parameters CD1_1 CD1_2
;		in DEGREES/PIXEL                                   CD2_1 CD2_2
;	 .CDELT - 2 element vector giving increment at reference point in
;		DEGREES/PIXEL
;	 .CRPIX - 2 element vector giving X and Y coordinates of reference pixel
;		(def = NAXIS/2)
;	 .CRVAL - 2 element vector giving R.A. and DEC of reference pixel 
;		in DEGREES
;	 .CTYPE - 2 element vector giving projection types 
;
; OUTPUTS:
;	X     - row position in pixels, scalar or vector
;	Y     - column position in pixels, scalar or vector
;
; REVISION HISTORY:
;	Converted to IDL by B. Boothman, SASC Tech, 4/21/86
;	Use astrometry structure,  W. Landsman      Jan. 1994	
;       Do computation correctly in degrees  W. Landsman       Dec. 1994
;	Only pass 2 CRVAL values to WCSSPH2XY   W. Landsman      June 1995
;	Don't subscript CTYPE      W. Landsman       August 1995	
;	Converted to IDL V5.0   W. Landsman   September 1997
;       disto(rtion) keyword addded Seb Oliver August 1998
;-
 On_error,2

 if N_params() lT 4 then begin
	print,'Syntax -- AD2XY, a, d, astr, x, y
	return
 endif

 if keyword_set(disto) then begin
     case n_elements(disto) of 
      1:   disto=read_distortion_file('/iso/arch/soft/lib/cia_NOV97/NOV97/tables/lw6asr3.dis')
     20:  
   else: message,'N elements of distortion must be 20' 
   endcase
 endif else disto=0

 radeg = 180.0D/!DPI                 ;Double precision !RADEG
 ctype = astr.ctype

 if  (strmid(ctype[0],5,3) EQ 'TAN') or (ctype[0] EQ '') then begin   

         crval = astr.crval/ radeg
	 radif = a/RADEG - crval[0]
         dec = d / radeg
	 h = sin(dec)*sin(crval[1]) + cos(dec)*cos(crval[1])*cos(radif)

	 xsi = cos(dec)*sin(radif)/h
	 eta = (sin(dec)*cos(crval[1]) -  cos(dec)*sin(crval[1])*cos(radif))/h
 
	 xsi = xsi*RADEG
	 eta = eta*RADEG

 endif else wcssph2xy, a, d, xsi, eta, CTYPE = ctype, PROJP1 = astr.projp1, $
	PROJP2 = astr.projp2, LONGPOLE = astr.longpole, CRVAL = astr.crval[0:1]

 xsi = xsi/astr.cdelt[0]
 eta = eta/astr.cdelt[1]

 crpix = astr.crpix
 cdinv = invert(astr.cd)
 x = ( cdinv[0,0]*xsi + cdinv[0,1]*eta + crpix[0] - 1 )
 y = ( cdinv[1,0]*xsi + cdinv[1,1]*eta + crpix[1] - 1 )


 IF  keyword_set(disto) then begin 
      compute_distortion, x,y, disto(*,0), disto(*,1), xd, yd,/invert
;      dx=xd-x
;      dy=yd-y
;      xd=x-dx
;      yd=y-dy
 endif else begin
      xd = x
      yd = y
 endelse
 x=xd
 y=yd
;stop

 return
 end
