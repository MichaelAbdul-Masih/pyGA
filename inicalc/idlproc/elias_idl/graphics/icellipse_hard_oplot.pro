PRO ICELLIPSE_HARD_OPLOT, im, header, ra, dec, maj_ax, min_ax, pa0, _extra=e, text=text, alignment=alignment,thick=thick,mark=mark
;+ 
; NAME: 
;       ICELLIPSE_HARD_OPLOT
;
; PURPOSE: Overlays ellipses for point sources on existing map
;          also adjust the image to include these ellipses
;
; CATEGORY: VIII,????
;
; CALLING SEQUENCE: 
;    ICELLIPSE_HARD_OPLOT, im, header, ra, dec  [, maj_ax, min_ax, pa0] 
;
; INPUTS: 
;    im:   image
;    header: fits header of image
;    ra : ra  decimal degrees same equinox as fits frame 
;    dec: dec decimal degrees same equinox as fits frame 
;
; OPTIONAL INPUT PARAMETERS: 
;    maj_ax major-axis of ellipse in degrees (scalar or vector): if omitted crosses are used
;    min_ax minor-axis of ellipse in degrees (if ommited circles are used)
;    pa  position angle of ellipse in degrees (North to Major-axis eastwards, default 0)
;   
; KEYED INPUTS: 
;    none
;
;   text:  a vector of strings to be plotted by the positions
;   _extra: allows other keyword to pass down to tvellipse
;   text: text plotted on screed
;   alignment: alignment of text
;   thick:  thickness of elipses
;   mark:  value to set ellipses to in image
;
;
; OUTPUTS: 
;
; OPTIONAL OUTPUT PARAMETERS: 
;
; KEYED OUTPUT PARAMETERS: 
;      none
;
; EXAMPLE: ????
;
; ALGORITHM: 
;
; DEPENDENCIES: 
;
; COMMON BLOCKS: 
; 
;
; SIDE EFFECTS: 
;
; RESTRICTIONS: 
;        image plot must exist
;        ra,dec must be in same equinox as image header.
;        UNTESTED
;
; CALLED PROCEDURES AND FUNCTIONS: 
;
; SEE ALSO: ????
;
; MODIFICATION HISTORY: 
;     12-Apr-1996  written with TEMPLATE_GEN Seb Oliver (ICSTM)
;     17-June-1996 renamed ICELLIPSE_HARD_OPLOT 
;     Wed Aug 14  1996 Information about Equinox printed up
;     22-oct-1996 defaults to circles and single element ellipse 
;                 paramters added (Seb Oliver)
;     19-mar-1997 made counter in loop i over ellipses into a longward
;		  variable, to allow greater number of objects to be
;		  plotted (Bob Mann)
;     4-May-1997  hardplot version created which adjusts the pixel values 
;
; COPYRIGHT:  
;  

;   No warranties for installation/ support/ maintenance are given.
;
;-
 
; ------------------------------------------------------------
;  common blocks 
; ------------------------------------------------------------
 
;  environment parameters 

; ------------------------------------------------------------
;  on error conditions
; ------------------------------------------------------------
; ON_ERROR, 2
 
 
; ------------------------------------------------------------
;  parameters check
; ------------------------------------------------------------
 IF N_PARAMS() LT 4 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $ 
    'ICELLIPSE_HARD_OPLOT, im, header, ra, dec  [, maj_ax, min_ax, pa] '
   GOTO, CLOSING
 ENDIF
 if not keyword_set(mark) then mark=0B

 ; if no maj/minor axis are there set them all to zero
   if(n_elements(maj_ax) eq 0)then begin
	print,'maj and minor axis not set plotting crosses'
	maj_ax=fltarr(n_elements(ra)) & min_ax=maj_ax & pa0=maj_ax
   endif

   if(n_elements(min_ax) eq 0)then begin
	print,'minor axis not set plotting circles'
	min_ax=maj_ax & pa0=maj_ax
   endif

   if(n_elements(pa0) eq 0)then begin
	print,'PA not set default=0'
	pa0=maj_ax*0.
   endif

   if(n_elements(maj_ax) eq 1 and n_elements(ra) ne 1 )then begin
      print,'1 element in major axis extending to all major axis'
      maj_ax=replicate(maj_ax,n_elements(ra))
   endif

   if(n_elements(min_ax) eq 1 and n_elements(ra) ne 1 )then begin
      print,'1 element in minor axis extending to all minor axis'
      min_ax=replicate(min_ax,n_elements(ra))
   endif

   if(n_elements(pa0) eq 1 and n_elements(ra) ne 1)then begin
      print,'1 element in PA extending to all PAs'
      pa0=replicate(pa0,n_elements(ra))
   endif

   if(n_elements(dec) ne n_elements(ra) or $
      n_elements(maj_ax) ne n_elements(ra) or $
      n_elements(min_ax) ne n_elements(ra) or $
      n_elements(pa0) ne n_elements(ra) )then $
      message,'array sizes do not match'


; ------------------------------------------------------------
;  function body
; ------------------------------------------------------------
 

  equ=get_equinox(header,code)
  if(code eq -1) then begin
     print,'Warning Equinox not defined in header assuming same as Sources'
  endif else begin
     print,'Information: Image header has equinox:',equ
  endelse


; get astronometric header info
 extast,header,astr

; adding 90 to pa0 to conform to tvellipse convention
; measured from x-axis to major-axis  anti-clockwise
  pa=90-pa0


  nel=n_elements(ra)
; checking if any positions sent
  if (nel le 0l) then goto, closing
  
; coordinates of ellipse points into dx ,dy (degrees)

 majxy=cv_coord(from_polar=transpose([[pa],[maj_ax]]),/to_rect,/degree)
 minxy=cv_coord(from_polar=transpose([[pa+90.],[min_ax]]),/to_rect,/degree)

; points of ellipse into RA, DEC
 majra=ra+majxy(0,*)/cos(dec/180.*!pi) &  majdec=dec+majxy(1,*)
 minra=ra+minxy(0,*)/cos(dec/180.*!pi) &  mindec=dec+minxy(1,*)

;points of ellipse into x,y (pixels)
; & centre of ellipse in x,y

 if(astr.ctype(0) eq 'RA---GSS')then begin
    GSSSextast,header,astr
    gsssadxy,astr,majra,majdec,majx,majy
    gsssadxy,astr,minra,mindec,minx,miny
    gsssadxy,astr,ra,dec,x0,y0
 endif else begin
    ad2xy,majra,majdec,astr,majx,majy
    ad2xy,minra,mindec,astr,minx,miny
    ad2xy,ra,dec,astr,x0,y0
 endelse
    minxy=transpose([[minx],[miny]])
    majxy=transpose([[majx],[majy]])


; dx,dy
 majxy(0,*)=majxy(0,*)-x0 & majxy(1,*)=majxy(1,*)-y0
 minxy(0,*)=minxy(0,*)-x0 & minxy(1,*)=minxy(1,*)-y0

; maj and minor axies in pixels
 majpol=cv_coord(from_rect=majxy,/to_polar,/degree)
 minpol=cv_coord(from_rect=minxy,/to_polar,/degree)
;tv ellipses
 tvpa=majpol(0,*)
 tvmaj=majpol(1,*)
 tvmin=minpol(1,*)

; transforming ellipse in sky coordinates to ellipse in x and y 
; assuming PA is from x-axis  to major axis     


 xmax=sxpar(header,'naxis1')-1
 ymax=sxpar(header,'naxis2')-1

; size of image to be modified
;sz=size(im)
;; 2D arrays of x and y values for an image
;make_2d,findgen(sz(1)),findgen(sz(2)),xx,yy

;looping around each ellipse

 for i=0l,nel-1l do begin

           npoints=long(5*2.*!pi*tvmaj(i))+1
           if(x0(i) ge 0 and x0(i) le xmax and $
              y0(i) ge 0 and y0(i) le ymax )then $
;	     tvellipse,tvmaj(i),tvmin(i),x0(i),y0(i),tvpa(i),$
;                       /data,_extra=e,npoints=npoints

          for it=0,thick-1 do begin
; stuff taken from tvellipse and adapted to give ellipse positions in data coordinates
;stop
           phi = 2*!pi*(findgen(npoints)/(npoints-1))         ;Will connect 120 points
           x =  (tvmaj(i)+it)*cos(phi)              ;Parameterized equation of ellipse
           y =  (tvmin(i)+it)*sin(phi)
           ang = tvpa(i)/!RADEG               	;Position angle in radians
           cosang = cos(ang)
           sinang = sin(ang)
	   xprime = round(x0(i) + x*cosang - y*sinang)   	;Rotate to desired position angle
	   yprime = round(y0(i) + x*sinang + y*cosang)

           inimage=where(xprime ge 0 and xprime le xmax and $
                         yprime ge 0 and yprime le ymax,count )
           if count gt 0 then im(xprime(inimage),yprime(inimage))=mark
           endfor

;           dist_circle,dist,sz(1:2),x0(i),y0(i)
;           circle=where(dist ge tvmaj(i)-thick and dist le tvmaj(i)+thick and $
;                  xx ge 0 and xx le xmax and yy ge 0 and yy le ymax)
;          im(circle)=mark

 endfor

; plotting up cross at centre if maj_ax and minor_ax are zero
  zero=where(maj_ax eq 0. and min_ax eq 0. ,count)
  if count gt 0 then oplot,x0(zero),y0(zero),psym=1,_extra=e

; plotting up text strings
  if not keyword_set(alignment)then alignment=0
; had to remove extra keyword since xyouts did not accept same
; keywords as oplot
  if keyword_set(text) then xyouts,x0,y0,text,alignment=alignment

; ------------------------------------------------------------
;  closing
; ------------------------------------------------------------
 
 CLOSING:

  RETURN
 
 END






