;***********************************************************************

PRO cam_src_plot,i,j,scd,raster,raster2,lead=lead,tail=tail,_extra=e

;+
; NAME:
;	cam_src_plot
;
; PURPOSE:
;	plots up a cam sources time profile
;
; CATEGORY:
;	analysis
;
; CALLING SEQUENCE:
;	cam_src_plot,i,j,scd,raster,[raster2,lead=lead,tail=tail]
;
; INPUTS:
;	i: i position
;	j: j position
;     scd: scd number
;    raster: name of raster structure
;
; OPTIONAL INPUTS:
;    raster2: name of raster structure to be overplotted
;
;	
; KEYWORD PARAMETERS:
;	
; lead: number of leadin pointing to be plotted	
; tail: number of tail pointing to be plotted	
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
;
; ICSTM CIA> ra=249.381
; ICSTM CIA> dec=40.8669
; ICSTM CIA> cam_src_locate, ra, dec, raster, i, j, scd
; ICSTM CIA> !p.multi=[0,1,2]
; ICSTM CIA> cam_src_plot,round(i(0)),round(j(0)),scd(0),raster
; ICSTM CIA> cam_src_plot,round(i(1)),round(j(1)),scd(1),raster
;	
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver 19th September 1996
;	September, 1996	
;-

on_error,2

if n_params() lt 4 then message,$
  'CALLING SEQUENCE: cam_src_plot,i,j,scd,raster,[lead=lead,tail=tail]'

if not keyword_set(lead) then lead=3
if not keyword_set(tail) then tail=3

start=raster.from(scd-lead)
end0=raster.to(scd+tail)
ymin=min(raster.cube(i,j,start:end0))
ymax=max(raster.cube(i,j,start:end0))
margin=(ymax-ymin)*0.1
yrange=[ymin-margin,ymax+margin]




readouts=indgen(end0-start+1)+start


plot,readouts,raster.cube(i,j,readouts),$
     title=raster.sscd_name+' Pixel: '+string(i)+' '+string(j),$
     xtitle='Readout', ytitle='ADU/sec/pixel',yrange=yrange,psym=10,$
     ystyle=1,xstyle=1,_extra=e,thick=2
if n_params() ge 5 then oplot,readouts,raster2.cube(i,j,readouts),$
                 psym=10

for slew=scd-tail,scd+lead do begin
   t=[raster.from(slew),raster.from(slew)]
   oplot,t,yrange,linestyle=1
   t=[raster.to(slew),raster.to(slew)]
   oplot,t,yrange,linestyle=1
endfor

oplot,[raster.from(scd),raster.from(scd)],yrange
oplot,[raster.to(scd),raster.to(scd)],yrange


END

