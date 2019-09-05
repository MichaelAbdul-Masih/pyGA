;***********************************************************************

PRO  oplot_ph,he,list=list,_extra=_extra,noname=noname


;+
; NAME:
;       oplot_ph
;
; PURPOSE:
;
;	over plots boundaries of ISO observations
;       extracted from mission databases (and ph reports?)
;
;
; CATEGORY:
;	
;
; CALLING SEQUENCE:
;	oplot_ph,he,roll0,list=list,_extra=_extra,/noname
;
;
; INPUTS:
;	He:  FITS header of displayed image
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	list:     entries from databse to use
;       noname:   if set supresses plotting of names
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
;	databse must be open (NOT checked for)
;
; PROCEDURE:
;	
;
; EXAMPLE:
;	IDL> implot, im1, hd1
;       IDL> retangle, hd1, hd2
;       
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver 24-May-1996
;	June, 1996	Rename ICHEADER_OPLOT
;       
;-

on_error,2


if n_params() eq 0 then message,'Calling Sequence: oplot_ph,he,[roll0,list=list,_extra=_extra,noname=noname]'


if not keyword_set(list) then list=-1
if n_params() lt 2 then roll0=0.



dbext,list,'name,ra,dec,aot,m,n,dm,dn,roll',name,ra,dec,aot,m,n,dm,dn,roll0

ra=ra*15.
case n_elements(roll0) of
n_elements(ra):   roll=roll0
             1:  roll=replicate(roll0,n_elements(ra))
          else: message,'Number of elements of roll not match list'
endcase

sz0=fltarr(n_elements(m))
cam=where(strmid(aot,0,3) eq 'CAM',nc)
pht=where(strmid(aot,0,3) eq 'PHT',np)

;
;size of field of view (only works for campixels "6" and PHOT C100
;

;stop
if nc gt 0 then sz0(cam) = 192.
if np gt 0 then sz0(pht) = 135.5




for i=0,n_elements(ra)-1 do begin

  rectangle,he,ra(i),dec(i),roll(i),(sz0(i)+(m(i)-1)*dm(i))/3600.,$
                                 (sz0(i)+(n(i)-1)*dn(i))/3600.,_extra=_extra
endfor


if not keyword_set(noname) then icellipse_oplot,he,ra,dec,text=name,_extra=_extra



end
