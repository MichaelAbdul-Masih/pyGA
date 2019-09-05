pro readtlusty,file,dat

 openr,1,'$HOME/Model_grids/tlusty/obstar_models_z_1_vt10/'+file+'.11'
  nd=50
  ncol=7
  data=fltarr(ncol,nd)

 readf,1,data
 close,1
  
  
 file1='$HOME/Model_grids/tlusty/obstar_fluxes_z_1_vt10/'+file+'.flux'
 if not file_test(file1) then begin
  print,' no flux available, download B-star fluxes'
  ifre=1
  lam=0.
  hnue=0.
  trad=0.
  goto, noflux
endif

 rspec,file1,lam,hnue

 lam=2.997925e18/lam

 b=where((lam gt 100.) and (lam lt 10000.))

 lam=lam(b)
 hnue=hnue(b)

 ifre=n_elements(lam)
 
 xic=4.d0*hnue
 trad=1.4388d8/lam/(alog(3.97d8/lam^3/xic+1.d0))

noflux: 
dat={name: '',  $
     rho: fltarr(nd), t: fltarr(nd), xne: fltarr(nd), m: fltarr(nd), $
     taur: fltarr(nd), opaross: fltarr(nd),$
     lam: dblarr(ifre), hnue: dblarr(ifre), trad: dblarr(ifre)}

  dat.name=file

  dat.lam=lam
  dat.hnue=hnue
  dat.trad=trad
  dat.m=data(1,*)
  dat.taur=data(2,*)
  dat.opaross=data(3,*)
  dat.t=data(4,*)
  dat.xne=data(5,*)
  dat.rho=data(6,*)
return
end
