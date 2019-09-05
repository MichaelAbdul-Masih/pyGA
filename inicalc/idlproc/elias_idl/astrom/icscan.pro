;***********************************************************************

PRO ICSCAN ,imf ,iiph ,scan ,time

;+
; NAME:
;	ICSCAN
;
; PURPOSE:
;	Construct a scan from a supplied image and the ISO pointing history.
;
; CALLING SEQUENCE:
;	icscan ,imf ,iiph ,scan [,time]
;
; INPUTS:
;	imf:	FITS file containing image
;	iiph:	FITS file containing ISO pointing history (IIPH).
;
; OUTPUTS:
;	scan:	Array containing the scan measurements
;
; OPTIONAL OUTPUTS:
;	time:	Array containing the time of the measurements
;
; RESTRICTIONS:
;	The image file must have an equinox defined
;
; EXAMPLE:
;	icscan, 'iras100.fit', 'iiph.fit', scan, time     Extract scan
;	plot, time, scan                                  Plot time vs scan
;
; MODIFICATION HISTORY:
; 	Written by:	Nick Eaton  28 Jun 1996
;-

; Return to caller for error conditions
on_error, 2
 
; Check that there are sufficient input parameters
np = n_params()
if ( np lt 3 ) then begin
   print, 'Usage: icscan ,imf ,iiph ,scan [,time]
   return
endif

; Open the image file
i = readfits( imf, hi )

; Check the equinox
eqn = get_equinox( hi, code )
if ( code lt 0 ) then message, 'No equinox in image file'

; Convert the image header to equinox 2000
if ( eqn ne 2000.0 ) then hprecess, hi, 2000.0

; Open the pointing history
fxbopen, u, iiph, 1, hh

; Extract the RA and Dec
fxbread, u, ra, 'RA'
fxbread, u, dec, 'DEC'

; Extract the time if requested
if ( np eq 4 ) then fxbread, u, time, 'UTK'

; Close the pointing history file
fxbclose, u

; Call icscanrd to extract the scan
icscanrd, i, hi, ra, dec, scan

return
end

