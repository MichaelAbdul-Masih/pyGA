;***********************************************************************

PRO rad_prof,im,x0,y0,head,dist=dist

;+
; NAME:
;	icbackground
;
; PURPOSE:
;	gives the background from the IC models at and given positions
;       and dates.
;
; CATEGORY:
;	analysis
;
; CALLING SEQUENCE:
;	rad_prof,im,[x0,y0,head],dist=dist
;
; INPUTS:
;	im: image
;
; OPTIONAL INPUTS:
;	
;       x0: x position in image coordinates or RA if header passed
;       y0: y position in image coordinates or Dec if header passed
;         : if x0 and y0 not passed images maximum is used
;	
; KEYWORD PARAMETERS:
;	dist: the distance to each pixel
;
; OUTPUTS:
;	
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
;	
;
; PROCEDURE:
;	
;
; EXAMPLE:
;	im=readfits('filename',hd)
;       rad_prof,im,dist=dist,12.5d0,32.0,hd
;       plot,dist,im,psym=3,/ystyle
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver 13th Dec 1996
;	July, 1996	
;-


if n_params() lt 1 then message, 'calling sequence: rad_prof, dist=dist, im, [x0, y0 ,head]'
sz=size(im)
if sz(0) ne 2 then message,'Image must be 2-D'
if n_params() eq 2 then message, 'x and y must both be specified'
if n_params() eq 1 then begin
   print,'X and Y not specified using max'
   m=max(im,imax)
   whereimage,sz,imax,x,y
   x=x(0)
   y=y(0)
endif
if n_params() eq 3 then begin
  x=x0
  y=y0
endif

if n_params() eq 4 then begin
  ra=x0
  dec=y0
  icad2xy,ra,dec,head,x,y
endif

dist_circle,dist,sz(1:2),x,y



END

