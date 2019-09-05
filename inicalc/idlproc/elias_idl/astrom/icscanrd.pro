;***********************************************************************

PRO ICSCANRD ,im ,head ,ra ,dec ,scan

;+
; NAME:
;	ICSCANRD
;
; PURPOSE:
;	Construct a scan from an image and a list of RA's and Dec's.
;
; CALLING SEQUENCE:
;	icscanrd ,im ,head ,ra ,dec ,scan
;
; INPUTS:
;	im:	data array containing image (e.g. from readfits)
;	head:	FITS file header (e.g. from readfits)
;       ra:	array of Right Ascension's
;       dec:	array of Declination's
;
; OUTPUTS:
;	scan:	array containing the scan measurements
;
; SIDE EFFECTS:
;	The output array is zero where positions lie outside the image.
;
; RESTRICTIONS:
;	The Ra's and Dec's must be for the same equinox as the image
;
; EXAMPLE:
;	im = readfits( 'iras100.fit', head )    Read in fits file
;	icscanrd, im, head, ra, dec, scan       Extract scan given positions
;	plot, scan                              Plot scan
;
; MODIFICATION HISTORY:
; 	Written by:	Nick Eaton  30 Jun 1996
;       16th December 1996 allows multi dimensional coordinate input Seb Oliver
;-

; Return to caller for error conditions
on_error, 2
 
; Check that there are sufficient input parameters
np = n_params()
if ( np lt 5 ) then begin
   print, 'Usage: icscanrd ,im ,head ,ra ,dec ,scan
   return
endif

; Test image dimensions 
si = size( im )
if ( si( 0 ) ne 2 ) then message, 'Image must be 2-dimensional'

; Convert the RA's and Dec's to positions in the image array
adxy, head, ra, dec, x, y

; Convert the positions to integers
ix = fix( x + 0.5 )
iy = fix( y + 0.5 )

; Create a zeroed output array
su = size( ra )
scan = fltarr( su( 1 ) )

; this line added to allow multidimensional input Seb Oliver
scan= ra*0.

; Find where the pixels indices are within the array bounds
valid = where( ( ix ge 0 ) and ( ix lt si( 1 ) ) and $
               ( iy ge 0 ) and ( iy lt si( 2 ) ), count )

; Read off the values of the image at the scan positions
if ( count ne 0 ) then scan( valid ) = im( ix( valid ), iy( valid ) )

return
end

