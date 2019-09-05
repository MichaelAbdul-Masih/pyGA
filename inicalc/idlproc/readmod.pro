pro readmod,file,star,dat,err,rmax=rmax

on_ioerror,err
; additional Hnue, assuming rmax=120 srnom!!!!
err=0
openr,1,file
readf,1,nd,kel,kis,nlev,ifre
print,'file ',file,' successfully read'
dat={name: '',dir: '', $
     r: fltarr(nd), v: fltarr(nd), dvdr: fltarr(nd), rho: fltarr(nd), $
     p: fltarr(nd), t: fltarr(nd), xne: fltarr(nd), grad: fltarr(nd),$
     taur: fltarr(nd), m: fltarr(nd), $
     levnam: strarr(nlev), dep: fltarr(nlev,nd),       $
     ifrac: fltarr(kis+1,kel,nd), lam: fltarr(ifre), fnue: fltarr(ifre), $
     hnue: fltarr(ifre), trad: fltarr(ifre), rtau1: fltarr(ifre), fcl: fltarr(nd)}
dat.name=star
dat.dir=file_dirname(file)
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

if keyword_set(rmax) then begin
corr=alog10(float(rmax)^2/(4.*!Pi))
endif else begin
corr=alog10(120.^2/(4.*!Pi))
endelse

dat.hnue=x+corr
x=dat.trad
readf,1,x
dat.trad=x

hnue1=bnue(dat.trad,dat.lam)/4.
maxerr=max(abs(alog10(hnue1)-dat.hnue))

if maxerr gt 0.01 then begin
  print,'check rmax (presumably not 120)'
  close,1
  return
endif  

; for newer files
if not eof(1) then begin
x=dat.fcl
readf,1,x
dat.fcl=x
endif else begin
dat.fcl=1.
endelse

close,1

l=strpos(file,'/',/reverse_search)
file1=strmid(file,0,l+1)+'GRAD.OUT'
if file_test(file1) then begin
  readcol,file1,f='A,I,F',dummy,i,grad,/silent
  ndd=(size(grad))(1)
  if ndd ne nd then begin
    print,' error in dim(grad)'
    return
  endif
  dat.grad=grad
endif else begin
  dat.grad=0.
endelse  


file1=strmid(file,0,l+1)+'FLUXCONT'
openr,1,file1
char=''
readf,1,char
close,1
if strpos(char,'R(TAU=1)') eq -1 then begin
  dat.rtau1=0.
endif else begin
  readcol,file1,i,lam,logfnue,trad,trad1,rthin,lthin,rtau1,/silent
  ndd=(size(rtau1))(1)
  if ndd ne ifre then begin
    print,' error in dim(rtau1)'
    return
  endif
  dat.rtau1=rtau1
endelse

return

err: 
err=1
close,1
return

end
