pro plot_n,models,star,obs=obs,ext=extin,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,hcopy=hcopy,ni=ni,scr=scr

if keyword_set(obs) then begin
  if obs ne 'oplot' then begin
; test for complete file_path
  if file_test(obs) then goto, cont
  obsfile='$HOME/Observations/optical/'+obs
;  obsfile='$HOME/Observations/optical/Gal_Bsg_Nevy/'+obs+'.sp'
  if file_test(obsfile) then begin
    obs=obsfile
    goto, cont
  endif else begin
    print,obsfile,' does not exist'
    return
  endelse
  endif  
endif

cont:
  
if keyword_set(hcopy) eq 1 and keyword_set(ni) eq 0 then begin

!x.thick=4.
!y.thick=4.
!p.charthick=4.
!p.thick=4.

set_plot,'ps'
device,file=star+'.eps',/color,xsize=19.7,ysize=25.,xoff=0.,yoff=0.

;niiprof,models,star,ext=extin,obscomp=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color
niiiprof,models,star,ext=extin,obscomp=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color


device,/close
set_plot,'x'

!x.thick=1.
!y.thick=1.
!p.charthick=1.
!p.thick=1.
endif

if keyword_set(hcopy) eq 1 and keyword_set(ni) eq 1  then begin

!x.thick=4.
!y.thick=4.
!p.charthick=4.
!p.thick=4.

set_plot,'ps'
device,file=star+'.eps',/color,xsize=19.7,ysize=25.,xoff=0.,yoff=0.

niiiprof,models,star,ext=extin,obscomp=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color
nivprof,models,star,ext=extin,obscomp=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color


device,/close
set_plot,'x'

!x.thick=1.
!y.thick=1.
!p.charthick=1.
!p.thick=1.
endif

;-------------------------------------


if (keyword_set(ni) eq 0 and keyword_set(scr) eq 0) then begin

window,0
hprof,models,star,ext=extin,obscomp=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color,/fcl
window,1
heiprof,models,star,ext=extin,obs=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color
window,2
heiiprof,models,star,ext=extin,obs=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color
window,3,xsize=800,ysize=800
niiprof,models,star,ext=extin,obscomp=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color
;window,4,xsize=1000,ysize=1000
;niiiprof,models,star,ext=extin,obscomp=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color
ntriplet,models,star,obs=obs,ext=extin,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,hcopy=hcopy
window,9,xsize=800,ysize=800
nivprof,models,star,ext=extin,obscomp=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color
window,10,xsize=800,ysize=800
nvprof,models,star,ext=extin,obscomp=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color

a=' '
read,a
wdelete,0
wdelete,1
wdelete,2
wdelete,3
wdelete,6
wdelete,8
wdelete,9
wdelete,10

endif

if (keyword_set(ni) eq 1 and keyword_set(scr) eq 0) then begin

;window,0
;hprof,models,star,ext=extin,obscomp=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color,/fcl
;window,1
;heiprof,models,star,ext=extin,obs=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color
;window,2
;heiiprof,models,star,ext=extin,obs=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color
;window,3
;niiiprof,models,star,ext=extin,obscomp=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color
window,4
nivprof,models,star,ext=extin,obscomp=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color


a=' '
read,a
wdelete,4

endif

return
end
