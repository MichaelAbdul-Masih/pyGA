pro iiph_pointing_astrometry,iiph,raster,new_ra,new_dec,scd_list=scd_list,$
    radius=radius, history=history
;+
; NAME:
;       IIPH_POINTING_ASTROMETRY
;
; PURPOSE:
;       CALCULATES THE RA AND DEC OF A POINTING
;       FROM THE MEDIAN IIPH RA AND DEC WITHIN A GIVEN RADIUS
;       OF RASTER.INFO.RA
;       AND RASTER.INFO.DEC FOR THE POINTING
;
; CALLING SEQUENCE:
;       iiph_pointing_astrometry,iiph,raster,new_ra,new_dec,$
;        scd_list=scd_list,radius=radius
;
; INPUTS:
;       iiph: structure (obtained eg from read_extended_fits)
;       raster: structure
;
; OPTIONAL INPUTS:
;       scd_list: intarr - list of scds to find astrometry for
;       radius: search radius in arsec (default 40'') - for ISO-HDF use 3''
;
; KEYWORD PARAMETERS:
;       none
;
; EXAMPLE:
;       dir='/iso/pipe2/p0032282/23200251'
;       cd,dir
;       fitsname='iiph.fit'
;       read_extended_fits,FITSname ,dir= dir, header,iiph, ack=ok;
;       restore,'rassrc4232002510_97020323161000.cub'
; 
;       iiph_pointing_astrometry,iiph,raster,$
;       correct_ra,correct_dec,radius=30,scd_list=[0,1,2,3,229,236,243]
;
; MODIFICATION HISTORY:
;       Written by:     Stephen Serjeant  April 1997
;		25 Aug 1998: Added history keyword and param check S Serjeant
;-


if(n_params() lt 4) then begin
	print,'USAGE: iiph_pointing_astrometry,iiph,raster,new_ra,new_dec $'
	print,'  [,scd_list=scd_list,radius=radius, history=history]'
	return
endif

if(keyword_set(history)) then begin
 version='v1.0'
 help,calls=calls
 routine_name=strmid(calls(0),0,strpos(calls(0),'<'))
 history=history+'/'+routine_name+version 
 if(keyword_set(radius)) then history=history+' radius='+$
		strcompress(radius,/remove_all)
 if(keyword_set(scd_list)) then history=history+' scd list supplied '
endif


if(not keyword_set(scd_list)) then scd_list=indgen(raster.nscd)

new_ra=dblarr(n_elements(scd_list))
new_dec=dblarr(n_elements(scd_list))

if(not keyword_set(radius)) then radius=40.0

delta_dec=radius/3600.0
delta_ra=radius/(3600.0*cos(!pi*raster.dec_raster/180.0))

for i=0,n_elements(scd_list)-1 do begin
    scd=scd_list(i)
    index=where( $
       (iiph.ra lt raster.info.ra(scd)+delta_ra) and $
       (iiph.ra gt raster.info.ra(scd)-delta_ra) and $
       (iiph.dec lt raster.info.dec(scd)+delta_dec) and $
       (iiph.dec gt raster.info.dec(scd)-delta_dec), count)
    if(count gt 0) then begin
       ra_list=iiph(index).ra
       dec_list=iiph(index).dec
       ra_list=ra_list(sort(ra_list))
       dec_list=dec_list(sort(dec_list))
       new_ra(i)=ra_list(count/2)
       new_dec(i)=dec_list(count/2)
    endif else begin
       print,'No astrometry found for pointing ',scd
    endelse
endfor

return
end


