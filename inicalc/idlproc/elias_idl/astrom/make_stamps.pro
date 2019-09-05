;***********************************************************************

PRO MAKE_STAMPS, IFILE, OFILE, PSIZE, RA, DEC

;+
; NAME:
;	MAKE_STAMPS
;
; PURPOSE:
;	Make postage stamp images from a FITS image and some positions.
;
; CALLING SEQUENCE:
;	make_stamps, ifile, ofile, psize, ra, dec
;
; INPUTS:
;	IFILE:	Name of FITS file containing image
;	OFILE:	Prefix name of output FITS files
;	PSIZE:	Size of postage stamps in degrees
;	RA:	List of right ascensions (J2000)
;	DEC:	List of declinations (J2000)
;
; SIDE EFFECTS:
;	The postage stamps are put into FITS files with names prefixed
;	by the OFILE parameter and ending '_##.fits' where ## is the
;	number of the position in the list of positions
;
; RESTRICTIONS:
;	Describe any "restrictions" here.
;
; EXAMPLE:
;	Please provide a simple example here.
;
; MODIFICATION HISTORY:
; 	Written by:	Nick Eaton,  15 Aug 1996
;	17th August, 1996 Check that image has been created before writing
;                         Seb Oliver
;-

; Return to caller for error conditions
;on_error, 2

; Check there are five parameters
if ( n_params() ne 5 ) then begin
   print,'Usage: make_stamps, ifile, ofile, psize, ra, dec'
   return
endif

; Get the number of positions
sr = size( ra )
sd = size( dec )
if ( sr( 0 ) ne 1 ) or ( sd( 0 ) ne 1 ) or ( sr( 1 ) ne sd( 1 ) ) then begin
   print,'Inappropriate vectors for ra and dec'
   return
endif
if ( sr( 1 ) ge 100 ) then begin
   print,'Too many positions'
   return
endif

; Open the input FITS file
i1 = readfits( ifile, h1 )

; Normalise to equinox 2000
eqn = get_equinox( h1, code )
if ( eqn ne 2000.0 ) then hprecess, h1, yearf = 2000.0

; Loop through the positions
for i = 0, sr( 1 ) - 1 do begin

; setting i2 to zero
   i2=0
; Extract the postage stamp
   postage, i1, h1, ra( i ), dec( i ), i2, h2, psize=psize

   if n_elements(i2) gt 1 then begin
; Create the output file name
     os = string( format='((a),"_",(i2.2),".fits")', ofile, i+1 )

; Make this into a FITS file
     print, 'Output number ', i+1, ' to ', os
     writefits, os, i2, h2

   endif
end

return
end

