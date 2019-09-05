pro ntriplet,models,star,obs=obs,ext=extin,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,hcopy=hcopy

window,6,xsize=800,ysize=800
niiiprof,models,star,ext=extin,obscomp=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,trip=1,/color
window,8,xsize=800,ysize=800
niiiprof,models,star,ext=extin,obscomp=obs,vrad=vrad,vsini=vsini,vmacro=vmacro,resol=resol,restk=1,/color
return
end