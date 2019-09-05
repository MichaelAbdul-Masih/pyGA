PRO ICELLIPSE_OPLOT, header, ra0, dec0, maj_ax0, min_ax0, pa0, $
                     _extra=e, text=text, alignment=alignment,$
                     size=size,disto=disto,$
                     lines=lines,psym=psym,fill=fill,$
                     orient=orient,arc_frac=arc_frac,silent=silent
;+ 
; NAME: 
;       ICELLIPSE_OPLOT
;
; PURPOSE: Overlays ellipses for point sources on existing map
;
; CATEGORY: VIII,????
;
; CALLING SEQUENCE: 
;    ICELLIPSE_OPLOT, header, ra, dec  [, maj_ax, min_ax, pa0] 
;
; INPUTS: 
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
;   alignment: keyword passed to xyouts for text alignment
;   size: keyword passed to xyouts for text size
;
;   fill: 0 shapes are not filled
;       : 1 shapes are filled with lines
;       : 2 shapes are solid filled
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
;     17-June-1996 renamed ICELLIPSE_OPLOT 
;     Wed Aug 14  1996 Information about Equinox printed up
;     22-oct-1996 defaults to circles and single element ellipse 
;                 paramters added (Seb Oliver)
;     19-mar-1997 made counter in loop i over ellipses into a longword
;		  variable, to allow greater number of objects to be
;		  plotted (Bob Mann)
;      6-Aug-1998 Reformat ra0 and dec0 so that arrays of ra and dec can be plotted
;    06 aug 1998: disto keyword added to allow for field distortions Seb Oliver
;    06 mar 1999: silent keyword added
;    15 Nov 2001: various bugs 
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
   ON_ERROR, 2
   
   
; ------------------------------------------------------------
;  parameters check
; ------------------------------------------------------------
   IF N_PARAMS() LT 3 THEN BEGIN
      PRINT, 'CALLING SEQUENCE: ', $ 
       'ICELLIPSE_OPLOT, header, ra, dec  [, maj_ax, min_ax, pa] '
      GOTO, CLOSING
   ENDIF
   
;----------------------------------------------------------------------
; plotting keyword checking
;----------------------------------------------------------------------


   if n_elements(psym) eq 0 then psym=1 
   if n_elements(orient) eq 0 then orient=0 

   if n_elements(e) gt 0 then if tag_exist(e,'color') then color=e.color else color=0 else color=0
   if not keyword_set(size) then size=1
   IF not keyword_set(fill) THEN fill = 0

;----------------------------------------------------------------------
; possitional keyword checking
;----------------------------------------------------------------------

   if not keyword_set(disto) then disto=0

   if n_elements(ra0) gt 1 then begin
      ra=reform(ra0,n_elements(ra0))
      dec=reform(dec0,n_elements(dec0))
   endif else begin
      ra=ra0
      dec=dec0
   endelse
                                ; if no maj/minor axis are there set them all to zero
   if(n_elements(maj_ax0) eq 0)then begin
      if not keyword_set(silent) then print,'maj and minor axis not set plotting crosses'
      maj_ax=fltarr(n_elements(ra)) & min_ax=maj_ax & pa0=maj_ax
   endif else maj_ax=maj_ax0

   if(n_elements(min_ax0) eq 0)then begin
      if not keyword_set(silent) then print,'minor axis not set plotting circles'
      min_ax=maj_ax & pa0=maj_ax
   endif else min_ax=min_ax0

   if(n_elements(pa0) eq 0)then begin
      if not keyword_set(silent) then print,'PA not set default=0'
      pa0=maj_ax*0.
   endif

   if(n_elements(maj_ax) eq 1 and n_elements(ra) gt 1 )then begin
      if not keyword_set(silent) then print,'1 element in major axis extending to all major axis'
      maj_ax=replicate(maj_ax(0),n_elements(ra))
   endif

   if(n_elements(min_ax) eq 1 and n_elements(ra) gt 1 )then begin
      if not keyword_set(silent) then print,'1 element in minor axis extending to all minor axis'
      min_ax=replicate(min_ax(0),n_elements(ra))
   endif

   if(n_elements(pa0) eq 1 and n_elements(ra) gt 1)then begin
      if not keyword_set(silent) then print,'1 element in PA extending to all PAs'
      pa0=replicate(pa0(0),n_elements(ra))
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
      if not keyword_set(silent) then print,'Information: Image header has equinox:',equ
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
      ad2xy,majra,majdec,astr,majx,majy,disto=disto
      ad2xy,minra,mindec,astr,minx,miny,disto=disto
      ad2xy,ra,dec,astr,x0,y0,disto=disto
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


;looping around each ellipse
;stop
   for i=0l,nel-1l do begin
      if(x0(i) gt 0 and x0(i) le xmax and $
         y0(i) gt 0 and y0(i) le ymax )then $
       if not keyword_set(arc_frac) then begin
         tvellipse,tvmaj(i),tvmin(i),x0(i),y0(i),tvpa(i),$
          /data,_extra=e,xprime=xprime,yprime=yprime
         CASE fill OF
            1: polyfill,xprime,yprime,color=color,line_fill=1,$
             lines=lines,orient=orient,/device
            2: polyfill,xprime,yprime,color=color,/device
            0:
            ELSE: message, 'Fill not recognised'
         ENDCASE
      endif else begin
         tvarc,tvmaj(i),tvmin(i),x0(i),y0(i),tvpa(i),/data,_extra=e,arc_frac=arc_frac(i)
      endelse
   endfor

; plotting up cross at centre if maj_ax and minor_ax are zero
   zero=where(maj_ax eq 0. and min_ax eq 0. ,count)
   if count gt 0 then oplot,x0(zero),y0(zero),psym=psym,_extra=e
;stop



; plotting up lines if specified
   if n_elements(lines) GT 0 then BEGIN
      
;   if keyword_set (lines) then begin
;      stop
      CASE fill OF 
         2: polyfill,x0,y0,color=color
         1: polyfill,x0,y0,color=color,line_fill=1,lines=lines,orient=orient
         0:
         ELSE: message, 'Fill not recognised'
      ENDCASE
      oplot,x0,y0,lines=lines,color=color
   ENDIF

   
   


; plotting up text strings
   if not keyword_set(alignment)then alignment=0
; had to remove extra keyword since xyouts did not accept same
; keywords as oplot
   if keyword_set(text) then begin
;    stop
      xyouts,x0,y0,text,alignment=alignment,color=color,size=size
   endif

; ------------------------------------------------------------
;  closing
; ------------------------------------------------------------
   
   CLOSING:

   RETURN
   
END



