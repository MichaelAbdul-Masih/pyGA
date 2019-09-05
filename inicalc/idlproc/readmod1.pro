pro readmod1,name,star,dat
file='out_'+name
openr,1,file
readf,1,nd,kel,kis,nlev,ifre
dat={name: '',  $
     r: fltarr(nd), v: fltarr(nd), dvdr: fltarr(nd), rho: fltarr(nd), $
     p: fltarr(nd), t: fltarr(nd), xne: fltarr(nd), taur: fltarr(nd), $
     m: fltarr(nd), levnam: strarr(nlev), dep: fltarr(nlev,nd),       $
     ifrac: fltarr(kis+1,kel,nd), lam: fltarr(ifre), fnue: fltarr(ifre), $
     trad: fltarr(ifre), tau911: fltarr(nd), taulya: fltarr(nd)}
dat.name=star
x=dat.r
readf,1,x
dat.r=x
x=dat.v
readf,1,x
dat.v=x
x=dat.dvdr
readf,1,x
dat.dvdr=x
x=dat.rho
readf,1,x
dat.rho=x
x=dat.p
readf,1,x
dat.p=x
x=dat.t
readf,1,x
dat.t=x
x=dat.xne
readf,1,x
dat.xne=x
x=dat.taur
readf,1,x
x(0)=x(1)/2.
dat.taur=x
x=dat.m
readf,1,x
x(0)=x(1)/2.
dat.m=x
x=dat.levnam
readf,1,x
dat.levnam=x
x=dat.dep
readf,1,x
dat.dep=x
x=dat.ifrac
readf,1,x
dat.ifrac=x
x=dat.lam
readf,1,x
dat.lam=x
x=dat.fnue
readf,1,x
dat.fnue=x
x=dat.trad
readf,1,x
dat.trad=x
close,1
file=name+'/tautau'
openr,1,file
readf,1,nd,nfreq,wlam
x=dat.tau911
readf,1,x
x(0)=x(1)/2.
dat.tau911=x
x=dat.taulya
readf,1,x
x(0)=x(1)/2.
dat.taulya=x
close,1
return
end




