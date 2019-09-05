;***********************************************************************

PRO ICHEADER_OPLOT, HD1, HD2,_extra=e,gal=gal,fill=fill

;+
; NAME:
;	ICHEADER_OPLOT
;
; PURPOSE:
;	Overplots Boundaries of FITS file described by header HD2 
;       on an image with astrometry defined by header HD1
;
; CATEGORY:
;	
;
; CALLING SEQUENCE:
;	ICHEADER_OPLOT, HD1, HD2
;
;
; INPUTS:
;	HD1:  FITS header of displayed image
;       HD2:  FITS header of overlayed boundaries
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;
;       fill: if set then uses polyfill rather than oplot	
;       takes any keywords to oplot (or polyfill)
;
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
;	Plots to current window
;
; RESTRICTIONS:
;	
;
; PROCEDURE:
;	
;
; EXAMPLE:
;	IDL> implot, im1, hd1
;       IDL> ICHEADER_OPLOT, hd1, hd2
;       
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver 24-May-1996
;	June, 1996	Rename ICHEADER_OPLOT
;      22-oct-1996      extra keyword added
;      27th Jan/ 1999.  Seb oliver  If hd1 has only one element then 
;                       assume that the co-oordinate system
;                       plotted is equatorial
;                       Keyword gal added to alter above to galactic
;                       above changes to help with e.g. aitoff maps.
;                        FILL keyword added
;-

;on_error,2

if n_elements(hd1) eq 1 then nohd1=1 else nohd1=0

naxis1=sxpar(hd2,'NAXIS1')
naxis2=sxpar(hd2,'NAXIS2')

; N.B. x and y are defined in IDL convention
; NOT FITS convention 
x=[-0.5, naxis1-0.5, naxis1-0.5, -0.5,        -0.5]
y=[-0.5, -0.5,        naxis2-0.5, naxis2-0.5, -0.5]


 if not nohd1  then extast, hd1, a1
 extast, hd2, a2

; Checking if GSS asstrometry on hd2
 if(a2.ctype(0) eq 'RA---GSS')then begin
    GSSSextast,hd2,a2
    gsssxyad,a2,x,y,r,d

 endif else begin


   xy2ad,x,y,a2,r,d


 endelse

 if nohd1 then begin
   if keyword_set(gal) then begin
      bprecess,r,d,r1,d1
      euler,r1,d1,l,b,1
      x=l & y=b
   endif else begin
      x=r & y=d
   endelse
 endif else begin


; Checking if GSS asstrometry on hd1
   if(a1.ctype(0) eq 'RA---GSS')then begin

      GSSSextast,hd1,a1
      gsssadxy,a1,r,d,x,y
   endif else begin

     ad2xy,r,d,a1,x,y

   endelse


 endelse

;stop
   if not keyword_set(fill) then begin
      oplot,x,y,_extra=e
   endif else begin
      polyfill,[x,x(0)],[y,y(0)],_extra=e
   endelse

END




