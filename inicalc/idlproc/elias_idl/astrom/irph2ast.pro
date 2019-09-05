;***********************************************************************

PRO IRPH2AST ,irph ,astr, ra, dec, roll

;+
; NAME:
;	IRPH2AST
;
; PURPOSE:
;	Construct astrometry parameters from an IRPH file.
;	The output astrometry is in CD format.
;
; CALLING SEQUENCE:
;	irph2ast, irph, astr
;
; INPUTS:
;	irph - Filename of IRPH file
;
; OUTPUTS:
;	astr - Anonymous structure continaing astrometry info
;   	 .CD   -  2 x 2 array containing the astrometry parameters CD1_1 CD1_2
;		in DEGREES/PIXEL                                   CD2_1 CD2_2
;	 .CDELT - 2 element vector giving physical increment at reference pixel
;	 .CRPIX - 2 element vector giving X and Y coordinates of reference pixel
;		(def = NAXIS/2)
;	 .CRVAL - 2 element double precision vector giving R.A. and DEC of 
;		reference pixel in DEGREES
;	 .CTYPE - 2 element string vector giving projection types, default
;		['RA---TAN','DEC--TAN']
;	 .LONGPOLE - scalar longitude of north pole (default = 180) 
;        .PROJP1 - Scalar parameter needed in some projections
;	 .PROJP2 - Scalar parameter needed in some projections
;
;       ra, dec, roll - coordinates of the raster pointings
;
; RESTRICTIONS:
;	The ctype, longpole and projp parameters have hardwired values.
;
; EXAMPLE:
;	makeast,'irph.fit',a
;
; MODIFICATION HISTORY:
; 	Written by:	Nick Eaton  27/1/97
;       July 1999       Seb Oliver ra, dec & roll returned
;	July, 1994	Any additional mods get described here.
;-

; Return to caller for error conditions
on_error,2

; Check that there are sufficient input parameters
if ( n_params() lt 2 ) then begin
   print,'Usage: irph2ast,irph,astr [, ra,dec,roll]'
   return
endif

; Set up the constants
radeg = 180.0d0 / !dpi

; Open the IRPH file
fxbopen, u, irph, 1, hd

; Read the necessary items
fxbread, u, ra, 'ra'
hra = ra / 15.0
fxbread, u, dec, 'dec'
fxbread, u, ro, 'roll'
fxbread, u, rp, 'rpid'

; Close the file
fxbclose, u

; Extract the array size
n1 = max( rp( 0, * ) )
n2 = max( rp( 1, * ) )

; checking if raster point ids screwed up
; if so set some arbitrary n1 & n2
if n1 eq 0 and n2 eq 0 then begin
   message,'Raster Point IDs screwed',/inf
   cd=dblarr(2,2)
   cdelt=dblarr(2)
   crpix=fltarr(2)
   crval=fltarr(2)
   ctype=strarr(2)
   longpole=fltarr(2)
   projp1=fltarr(2)
   projp2=fltarr(2)
   goto,closing
endif
; Find the indices of three of the corners in the raster array
a1 = where( rp( 1, * ) eq 1 )
a2 = where( rp( 0, * ) eq n1 )
i1 = a1( where( rp( 0, a1 ) eq 1 ) )
i2 = a1( where( rp( 0, a1 ) eq n1 ) )
i3 = a2( where( rp( 1, a2 ) eq n2 ) )

; Calculate the pixel spacings from the ras and decs of these points
gcirc, 1, hra( i1 ), dec( i1 ), hra( i2 ), dec( i2 ), del1
gcirc, 1, hra( i2 ), dec( i2 ), hra( i3 ), dec( i3 ), del2
cdelt = [ del1 / float( n1 - 1 ) / 3.6d3 , del2 / float( n2 - 1 ) / 3.6d3 ]

; Create arrays for the astrometry structure from one of the points
crval = [ ra( i1 ), dec( i1 ) ]
crpix = [ float( rp( 0, i1 ) ), float( rp( 1, i1 ) ) ]
cr = ro( i1 ) / radeg
cosr = cos( cr )
sinr = sin( cr )
cd = [ [ cosr, -sinr ], [ sinr, cosr ] ]

; Define the other astrometry parameters here
ctype = [ 'RA---TAN', 'DEC--TAN' ]
longpole = 180.0
projp1 = -1.0
projp2 = -2.0

; Create the astrometry structure

closing:
astr = { CD: double(cd), CDELT: double(cdelt), $
         CRPIX: float(crpix), CRVAL:double(crval), $
         CTYPE: string(ctype), LONGPOLE: float( longpole(0)), $
         PROJP1: float(projp1(0)), PROJP2: float(projp2(0)) }

roll = ro

return
end

