;***********************************************************************

PRO iczoom, image, x,y, ra, dec,zoom=zoom, head=head,_extra=extra

;+
; NAME:
;	iczoom
;
; PURPOSE:
;	plots up an image and zooms in on a clicked pixel
;       then returns the position of a clicked pixel on 
;       the final image.
;
; CATEGORY:
;	graphics
;
; CALLING SEQUENCE:
;	iczoom, image, x,y,[ra, dec,zoom=zoom, head=head,_extra=extra]
;
; INPUTS:
;	image: 2D image
;    
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	head: fits header with astrometry
;       zoom: zoom factor, default 2
;       extra: any keywords accepted by icplot
;
; OUTPUTS:
;	x: x coordinate of final cursor position
;	y: y coordinate of final cursor position
;
; OPTIONAL OUTPUTS
;	ra: ra coordinate of final cursor position
;	dec: dec coordinate of final cursor position
;
; COMMON BLOCKS:
;	none
;
; SIDE EFFECTS:
;	displays in current graphics device
;
; RESTRICTIONS:
;	
;
; PROCEDURE:
;	
;
; EXAMPLE:
;	
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver 30th April 1997
;	July, 1996	
;-

if n_params() lt 3 then message,'Calling Sequence: iczoom, image, x,y,[ra, dec,zoom=zoom, head=head,_extra=extra]'


sz=size(image)

if sz(0) ne 2 then message,'image must be 2D'

if not keyword_set(zoom) then zoom =2
; guess pixels are 1" not important
pixel=1./60.^2

if not keyword_set(head) then begin
  icmkhdr,sz(1),sz(2),pixel,pixel,0.,0.,0.,head
  if n_params() gt 3 then print,'WARNING ra and dec meaningless without header'
endif


; diag as estimate of image size
xx=[0.,sz(1)]
yy=[0.,sz(2)]
icxy2ad,xx,yy,head,rr,dd
gcirc,1,rr(0)/15.,dd(0),rr(1)/15.,dd(1),real_size
real_size=real_size/60.^2/sqrt(2.)

; plotting up image with ellipse 5% of size as starting point

icplot,image,head,_extra=extra
tvellipse,sz(1)/20.,sz(2)/20.,x,y,/data


; getting position to zoom in on.
cursor,x,y
icxy2ad,x,y,head,ra,dec

; getting postage stamp around this position
postage,image,head,ra,dec,im2,hd2,psize=real_size/zoom

icad2xy,ra,dec,hd2,x,y
icplot,im2,hd2,_extra=extra
tvellipse,sz(1)/20./zoom,sz(2)/20./zoom,x,y,/data

; getting postion from this image
cursor,x,y
icxy2ad,x,y,hd2,ra,dec

; converting ra and dec back to x an y in original image
icad2xy,ra,dec,head,x,y


END

