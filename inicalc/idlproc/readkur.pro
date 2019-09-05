pro readkur, model, dat, abross

  file='~uh101aw/Kurucz/'+model+'_struct'
  if file_test(file) eq 0 then begin
    file=model
    if file_test(file) eq 0 then begin
      print,' file not found'
      return
    endif
  endif  
  openr,3,file
  openw,2,'tmp_kur_flux'
  a=' '
  readf,3,a
  while (strpos(a,'TEFF') eq -1) do begin
    readf,3,a
  endwhile
  print,a

  readf,3,a
  while (strpos(a,'FLUX') eq -1) do begin
    readf,3,a
  endwhile

  i=0
  while (strpos(a,'TEFF') eq -1) do begin
    i=i+1
    b=strmid(a,9)
    printf,2,b
    readf,3,a
  endwhile
  close,2

  i=i-1
  if(i ne 1221) then begin
  print,' nf ne 1221'
  stop
  endif
  
  rflux,'tmp_kur_flux',lam,hnue
  spawn,'rm tmp_kur_flux'
  lam=reverse(lam)
  hnue=reverse(hnue)
  xic=4.d0*10.d0^hnue
  trad=1.4388d8/lam/(alog(3.97d8/lam^3/xic+1.d0))
  
  openw,2,'tmp_kur_mod'
  readf,3,a
  while (strpos(a,'READ DECK') eq -1) do begin
    readf,3,a
  endwhile
  print,a
  adeck=a

  stri=str_sep(a,' ')
  b=where(stri ne '')
  str=stri(b)
  lineno=fix(str(2))

  i=0
  while (i ne lineno) do begin
    readf,3,a
    i=i+1
    printf,2,a
  endwhile
  close,2
  close,3
  
  col=7
  if strpos(adeck,'FLXCNV') ne -1 then col=10

  fmod='tmp_kur_mod'
  taur1=fltarr(lineno)
  taur1(*)=0.

  rtab,m,t,fmod,lineno,col,1,2
  rtab,p,xne,fmod,lineno,col,3,4
  rtab,m,abross,fmod,lineno,col,1,5
  rtab,m,grad,fmod,lineno,col,1,6
  spawn,'rm tmp_kur_mod'
  
  ctaur,m,abross,taur

  nd=lineno
  dim=size(lam)
  ifre=dim(1)

dat={name: '',  $
     p: fltarr(nd), t: fltarr(nd), xne: fltarr(nd), taur: fltarr(nd), $
     m: fltarr(nd), grad: fltarr(nd), $
     lam: fltarr(ifre), hnue: fltarr(ifre), trad: fltarr(ifre)}
     
dat.name=model
dat.p=p
dat.t=t
dat.xne=xne
dat.taur=taur
dat.m=m
dat.grad=grad
dat.lam=lam
dat.hnue=hnue
dat.trad=trad

  
return
end

pro rflux,file,lam,hnue
  rtab,x,y,file,1221,5,1,3
  lam=x*10.
  hnue=alog10(y)
  return
end  

pro ctaur,m,abross,taur
; calculated taur from Kurucz output
  d=size(m)
  nd=d(1)
  dm=fltarr(nd-1)
  dtaur=dm
  for i=1,nd-1 do begin
    dm(i-1)=m(i)-m(i-1)
    dtaur(i-1)=.5*(abross(i-1)+abross(i))*dm(i-1)
  endfor

  taur=m
  taur(0)=.4*m(0) ; simple approximation, accounting for e-scattering
  for i=1,nd-1 do begin
  taur(i)=taur(i-1)+dtaur(i-1)
  endfor
  return
  end

