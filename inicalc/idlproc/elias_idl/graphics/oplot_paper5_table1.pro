pro oplot_paper5_table1, header,colour=colour, size=size, thick=thick,$
                         nonumbers=nonumbers, radius=radius, offset=offset
;+
; NAME:
;       oplot_paper5_table1
;
; PURPOSE:
; 	Number the positions of the ISO-HDF sources in 
;
; CATEGORY:
;       image processing
;
; CALLING SEQUENCE:
;	oplot_hdf_mrr_table1, header,colour=colour, size=size
;
; INPUTS:
;	header: fits header string array
;
; OPTIONAL INPUTS:
;	colour: integer
;	size: float - size of characters
;
; KEYWORD PARAMETERS:
;	none
;
; OUTPUTS:
;	none
;	
; OPTIONAL OUTPUTS:
;	none
;
; COMMON BLOCKS:
;	none
;
; SIDE EFFECTS:
;	modified current plot
;
; RESTRICTIONS:
;	none
;
; PROCEDURE:
;
; EXAMPLE:
;	
;       oplot_paper5_table1, header,colour=3,size=1.5
;
; MODIFICATION HISTORY:
;	Written by Stephen Serjeant Wed Aug 13 1997
;       Fri Aug 22 1997: defaults for colour and size set and 
;                        lots of extra keywords added S Serjeant
;-
;On_error, 2

if ( n_params() lt 1 ) then begin
  message,'Usage: oplot_paper5_table1, header [,colour=colour, size=size]'
endif

if(not keyword_set(colour)) then colour=0
if(not keyword_set(size)) then size=1.0
if(not keyword_set(thick)) then thick=1.0
if(not keyword_set(offset)) then offset=[0,0]


mrr_ra= 1.0d0 * $
  [41.1,41.6,42.6, 42.9,43.0,43.7,43.9,46.4,48.1,48.4,49.7,51.5,58.9]
mrr_decm=1.0d0 * [11,11,12,13,11,12,11,14,14,12,13,13,12]
mrr_decs=1.0d0 * [29,42,10,9,52,55,30,6,27,15,15,57,48]
mrr_ra=12.0d0 + 36.0d0 / 60.0d0 + mrr_ra/3600.0d0
mrr_dec=62.0 + mrr_decm/60.0d0 + mrr_decs/3600.0d0
icad2xy,mrr_ra*15,mrr_dec,header,mrr_x,mrr_y
if(not keyword_set(nonumbers)) then begin
  for i=0,n_elements(mrr_ra)-1 do begin
      xyouts,mrr_x(i)+offset(0),mrr_y(i)+offset(1),strtrim(string(i+1),2),$
      color=colour, charsize=size,align=0.5, charthick=thick
  endfor
endif else begin
   if(keyword_set(radius)) then begin
      icellipse_oplot,header,mrr_ra*15,mrr_dec,$
      radius, color=colour, thick=thick
   endif else begin
      icellipse_oplot,header,mrr_ra*15,mrr_dec,$
      color=colour, thick=thick
   endelse
endelse


return
end
