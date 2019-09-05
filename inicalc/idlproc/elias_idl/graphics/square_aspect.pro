
pro square_aspect, xrange, yrange, _extra=e

;+
; NAME:
;	square_aspect
;
; PURPOSE:
;	Sets the graphics window to a square
;
; CATEGORY:
;	General graphics.
;
; CALLING SEQUENCE:
;	square_aspect, xrange, yrange	
;
; INPUTS:
;	A:	The two-dimensional array to display.
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;	No explicit outputs.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	The currently selected display is affected.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
; 	crappy. based around image_cont
;
; MODIFICATION HISTORY:
;	S Serjeant Nov 18th 1997
;-


aspect=1B
on_error,2                      ;Return to caller if an error occurs

a = [[0,0],[0,0]]
sz = size(a)

	;set window used by contour
contour,[[0,0],[1,1]],/nodata, xstyle=4, ystyle = 4

px = !x.window * !d.x_vsize	;Get size of window in device units
py = !y.window * !d.y_vsize
swx = px(1)-px(0)		;Size in x in device units
swy = py(1)-py(0)		;Size in Y
six = float(sz(1))		;Image sizes
siy = float(sz(2))
aspi = six / siy		;Image aspect ratio
aspw = swx / swy		;Window aspect ratio
f = aspi / aspw			;Ratio of aspect ratios

if (!d.flags and 1) ne 0 then begin	;Scalable pixels?
  if keyword_set(aspect) then begin	;Retain aspect ratio?
				;Adjust window size
	if f ge 1.0 then swy = swy / f else swx = swx * f
	endif

endif else begin	;Not scalable pixels	
   if keyword_set(window_scale) then begin ;Scale window to image?
	tvscl,a,px(0),py(0)	;Output image
	swx = six		;Set window size from image
	swy = siy
    endif else begin		;Scale window
	if keyword_set(aspect) then begin
		if f ge 1.0 then swy = swy / f else swx = swx * f
		endif		;aspect
	endelse			;window_scale
  endelse			;scalable pixels

mx = !d.n_colors-1		;Brightest color
colors = [mx,mx,mx,0,0,0]	;color vectors
if !d.name eq 'PS' then colors = mx - colors ;invert line colors for pstscrp
contour,a,/noerase,/xst,/yst,$	;Do the contour
	   pos = [px(0),py(0), px(0)+swx,py(0)+swy],/dev,$
	c_color =  colors, xrange=xrange, yrange=yrange,_extra=e
return
end
