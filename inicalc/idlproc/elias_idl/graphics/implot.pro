;***********************************************************************

PRO IMPLOT, image, head, min=min, max=max

;+
; NAME:
;	IMPLOT
;
; PURPOSE:
;	Plots an image on current display device. If a FITS header is
;	supplied the RA, DEC coordinates are overlayed. The greyscale
;	levels can be adjusted with the min and max keywords.
;
; CALLING SEQUENCE:
;	IMPLOT ,image [,head ,/min ,/max]
;
; INPUTS:
;	image: data array containing image (e.g from readfits)
;
; OPTIONAL INPUTS:
;	head: FITS file header (e.g. from readfits)
;
; KEYWORD PARAMETERS:
;	min: minimum value to plot
;	max: maximum value to plot
;
; EXAMPLE:
;	im = readfits( '0026+1041.fit',head )    Reads in fits file
;	implot, im, head, min=0, max=100         Plot image on display
;
; MODIFICATION HISTORY:
; 	Written by:	Nick Eaton  10/05/96
;-

; Return to caller for error conditions
on_error, 2
 
; Check that there are sufficient input parameters 
if ( n_params() lt 1 ) then begin
   print, 'Usage: implot, image ,[head ,/min ,/max]'
   return
endif

; Test image dimensions 
s = size( image )
if ( s( 0 ) ne 2 ) then begin
   print, 'Image must be 2-dimensional'
   return
endif

; Check if device has scalable pixels
if ( !d.flags and 1 ) then scale = 1 else scale = 0

; If a FITS header has been supplied use imcontour to set window parameters
if keyword_set( head ) then begin
   imcontour, image, head, /nodata

; Get size of window for scaleable or non-scaleable devices
   if ( scale ) then begin
      px = !x.window
      py = !y.window
   endif else begin
      px = !x.window * !d.x_vsize
      py = !y.window * !d.y_vsize
   endelse

; Otherwise set the limits to match the array size to the display size
endif else begin
   rx = float( !d.x_vsize ) / float( s( 1 ) )
   ry = float( !d.y_vsize ) / float( s( 2 ) )
   r = min( [ rx, ry ] )
   if ( scale ) then begin
      px = [ 0.15 * r / rx, 0.95 * r / rx ]
      py = [ 0.15 * r / ry, 0.95 * r / ry ]
   endif else begin
      px = [ 0.15 * r * s( 1 ), 0.95 * r * s( 1 ) ]
      py = [ 0.15 * r * s( 2 ), 0.95 * r * s( 2 ) ]
   endelse
endelse

; Define size of plotted image
sx = px( 1 ) - px( 0 )
sy = py( 1 ) - py( 0 )
x0 = px( 0 )
y0 = py( 0 )

; Clear window
erase

; Set and report minimum and maximum greyscale levels
if keyword_set( min ) then min_p = min else min_p = min( image )
if keyword_set( max ) then max_p = max else max_p = max( image )
print, 'Minimum value for plot (black) is ', min_p
print, 'Maximum value for plot (white) is ', max_p
print, 'Number of levels is ', !d.table_size

; Plot pixel map scaled from min to max value into avalable colour map
; For scaleable devices use normalised-device coordinates
if ( scale ) then $
   tv, bytscl( image, min=min_p, max=max_p, top=!d.table_size ), $
   x0, y0, xsize = sx, ysize = sy, /norm $

; Otherwise construct a pixel map of the desired size
else $
   tv, congrid( bytscl( image, min=min_p, max=max_p, top=!d.table_size ), $
   sx, sy ), x0, y0

; Overlay axis labels but with no contours
if keyword_set( head ) then imcontour, image, head, /noerase, /type, /nodata

return

end

