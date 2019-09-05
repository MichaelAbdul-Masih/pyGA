pro plot_n23,models,star,obs=obs,ext=extin,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,hcopy=hcopy

if keyword_set(hcopy) then begin

!x.thick=4.
!y.thick=4.
!p.charthick=4.
!p.thick=4.

set_plot,'ps'
device,file=star+'.eps',/color,xsize=19.7,ysize=25.,xoff=0.,yoff=0.

hprof,models,star,ext=extin,obs=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color,/fcl
heiprof,models,star,ext=extin,obs=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color
heiiprof,models,star,ext=extin,obs=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color
niiprof,models,star,ext=extin,obscomp=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color
niiiprof,models,star,ext=extin,obscomp=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color


device,/close
set_plot,'x'

!x.thick=1.
!y.thick=1.
!p.charthick=1.
!p.thick=1.
endif

;-------------------------------------


window,0
hprof,models,star,ext=extin,obscomp=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color,/fcl
window,1
heiprof,models,star,ext=extin,obs=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color
window,2
heiiprof,models,star,ext=extin,obs=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color
window,3
niiprof,models,star,ext=extin,obscomp=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color
window,4
niiiprof,models,star,ext=extin,obscomp=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,/color


a=' '
read,a
wdelete,0
wdelete,1
wdelete,2
return
end
