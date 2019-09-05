pro readpaco, model, dat
  
  file=model
  nd=90
  openr,1,file
  dummy=''
  for i=1,13 do begin
  readf,1,dummy
  endfor

  r=fltarr(nd)
  readf,1,r
  rin=r(nd-1)
  r=r/rin
  
  v=r
  readf,1,dummy
  readf,1,v  

  dlnvdlnr=r
; dlnvdlnr-1!  
  readf,1,dummy
  readf,1,dlnvdlnr

  xne=r
  readf,1,dummy
  readf,1,xne

  t=r
  readf,1,dummy
  readf,1,t
  t=t*1.e4
  
  taur=r
  readf,1,dummy
  readf,1,taur
  
  tauf=r
  readf,1,dummy
  readf,1,tauf

  ndens=r
  readf,1,dummy
  readf,1,ndens

  iondens=r
  readf,1,dummy
  readf,1,iondens
  
  rho=r
  readf,1,dummy
  readf,1,rho

close,1
  
dat={name: '',r: fltarr(nd), v: fltarr(nd), xne: fltarr(nd), $
     rho: fltarr(nd), t: fltarr(nd), taur: fltarr(nd)}
   
dat.name=model
dat.r=r
dat.v=v
dat.xne=xne
dat.rho=rho
dat.t=t
dat.taur=taur

  return
end
