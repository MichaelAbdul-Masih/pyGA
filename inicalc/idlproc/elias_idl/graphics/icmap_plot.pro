PRO ICMAP_PLOT, im1, hd1, sky=sky, log=log
;+ 
; NAME: 
;      ICMAP_PLOT
;
; PURPOSE: 
;      Allows some image processing before plotting up image with 
;      coordinate overlays
;
;
; CATEGORY: 
;
; CALLING SEQUENCE: 
;    ICMAP_PLOT, im1, hd1 [, /sky ,/log]
;
; INPUTS: 
;    im1: image array
;    hd1: fits header array from readfits command
;
; OPTIONAL INPUT PARAMETERS: 
;
;
; KEYED INPUTS: 
;
;      sky:   if set then sky noise is computed and sky-subtracted
;      log:   if set then log image is plotted
;
; OUTPUTS: 
;
; OPTIONAL OUTPUT PARAMETERS: 
;
; KEYED OUTPUT PARAMETERS: 
;
;
; EXAMPLE:
;
;  IDL> im1=readfits('0026+1041.fit',hd1)    Reads in in fits file
;  IDL> ICMAP_PLOT,im1,hd1                     Plots up map on display
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
;
;        very specific type of display levels could be made more general
;        should send min and max to icplot for speed
;        no guarentees
;
; CALLED PROCEDURES AND FUNCTIONS: 
;       ICPLOT
;
; SEE ALSO: 
;
;  ICELLIPSE_OVERLAY
;
; MODIFICATION HISTORY: 
;     12-Apr-1996  written with TEMPLATE_GEN Seb Oliver (ICSTM)
;     17-June-1996 rename to ICMAP_PLOT now calls ICPLOT
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
 IF N_PARAMS() LT 2 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $ 
   'ICMAP_PLOT, im1, hd1 [,/sky, /log]'
   GOTO, CLOSING
 ENDIF
 
; ------------------------------------------------------------
;  function body
; ------------------------------------------------------------
 


if keyword_set(sky)then begin

; getting and subtracting sky level
  mmm,im1,sky,skynoise & im2=im1-sky


endif else begin
  skynoise=1.
  im2=im1

endelse
if keyword_set(log) then begin
; logging sky-subtracted image
; setting -ve values to some small value (1/2 skynoise)
  p=where(float(im2) gt 0.) &  n=where(float(im2) le 0. ,ncount)
  if(ncount gt 0)then begin	
	  im2(p)=alog10(im2(p)) & im2(n)=alog10(skynoise/2.)
  endif else begin
	im2=alog10(im2)
  endelse
  min_p=alog10(skynoise/2.)
endif else begin
  min_p=min(im2)
endelse

icplot,im2,hd1

; ------------------------------------------------------------
;  closing
; ------------------------------------------------------------
 
 CLOSING:

  RETURN
 
 END
