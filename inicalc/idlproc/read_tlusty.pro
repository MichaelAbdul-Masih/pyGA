pro read_tlusty,file,dat

  openr,1,'$HOME/TLusty/depart/'+file
  readf,1,nd,nlev
  m=fltarr(nd)
  dep=fltarr(nlev,nd)

dat={name: '',  $
     rho: fltarr(nd), t: fltarr(nd), xne: fltarr(nd), m: fltarr(nd), $
       deph: fltarr(9,nd), dephei: fltarr(24,nd), depheii: fltarr(20,nd)}

  dat.name=file

  readf,1,m
  readf,1,dep
  close,1

  dat.m=m
  dat.t=dep(0,*)
  dat.xne=dep(1,*)
  dat.rho=dep(2,*)
  dat.deph=dep(3:11,*)
  dat.dephei=dep(13:36,*)
  dat.depheii=dep(37:56,*)
return
end
