;***********************************************************************

PRO int_wfc_olay,he,ra,dec,expos,h1,h2,h3,h4,_extra=_extra,no_c2=no_c2,noplot=noplot,hires=hires,nomap=nomap,color=color

;+
; NAME:
;	int_wfc_olay
;
; PURPOSE:
;	Overplots int_wfc_olay
;       on an image with astrometry defined by header he
;
; CATEGORY:
;	
;
; CALLING SEQUENCE:
;	int_wfc_olay,he,ra,dec,[h1,h2,h3,h4]
;
;
; INPUTS:
;	HD:  FITS header of displayed image
;       ra: RA in degrees (J2000)
;      dec: DEC in degrees
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	noplot if set then no plotting is done
;
; OUTPUTS:
;	h1,h2,h3,h4, dummy header used for overplotting
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
;	IDL> 
;       IDL> 
;       
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver 2-May-1998
;	June, 1996	Rename ICHEADER_OPLOT
;       
;-

;on_error,2

if keyword_set(hires) then small=0 else small=1

if not keyword_set(color) then begin
 c1=2
 c2=3
 c3=4
 c4=5
endif else begin
 c1=color
 c2=color
 c3=color
 c4=color
endelse

; Get X and Y of 4 corners of the image

 if N_elements(xrange) NE 2 then begin    
      xmin = 0          & xmax =  sxpar(he,'NAXIS1') - 1
 ENDIF ELSE BEGIN
      xmin = xrange(0)  & xmax = xrange(1)
 ENDELSE

 if N_elements(yrange) NE 2 then BEGIN
        ymin=0          & ymax = sxpar(he,'NAXIS2') - 1
 ENDIF ELSE BEGIN
     ymin = yrange(0)   & ymax = yrange(1)
 ENDELSE

 x = [xmin, xmax, xmax, xmin]
 y = [ymin, ymin, ymax, ymax]

icxy2ad,x,y,he,ralim,declim
ramin = min(ralim)  & ramax = max(ralim)     ;Get max and min RA values
decmin = min(declim) & decmax = max(declim)  ;Get max and min Dec values
 if (ramax - ramin) GT 12 then begin    ;Does the RA cross 24 hours?
      newmax = ramin
      ramin = ramax
      ramax = 24.
      redo = 1
      message,'Sorry RA crosses 24hr not sorted'
endif else redo = 0
 
good=where(ramin le ra and ra le ramax and decmin le dec and dec le decmax,ngood)

for j=0,ngood-1 do begin
  i=good(j)


  int_wfc_heads,ra(i),dec(i),h1,h2,h3,h4,small=small


  if not keyword_set(noplot) then begin
    icheader_oplot,he,h1,lines=1,color=c1
    if not keyword_set(no_c2) then icheader_oplot,he,h2,lines=1,color=c2
    icheader_oplot,he,h3,lines=1,color=c3
    icheader_oplot,he,h4,lines=0,color=c4
  endif

  if not keyword_set(nomap) then begin
  naxis1=sxpar(h1,'naxis1')
  naxis2=sxpar(h1,'naxis2')
  expos1=replicate(1.,naxis1,naxis2)
  hastrom,expos1,h1,he,missing=0
  expos=expos+expos1

  if not keyword_set(no_c2) then begin
    naxis1=sxpar(h2,'naxis1')
    naxis2=sxpar(h2,'naxis2')
    expos2=replicate(1.,naxis1,naxis2)
    hastrom,expos2,h2,he,missing=0
    expos=expos+expos2
  endif

  naxis1=sxpar(h3,'naxis1')
  naxis2=sxpar(h3,'naxis2')
  expos3=replicate(1.,naxis1,naxis2)
  hastrom,expos3,h3,he,missing=0
  expos=expos+expos3

  naxis1=sxpar(h4,'naxis1')
  naxis2=sxpar(h4,'naxis2')
  expos4=replicate(1.,naxis1,naxis2)
  hastrom,expos4,h4,he,missing=0
  expos=expos+expos4

  endif
endfor

END

