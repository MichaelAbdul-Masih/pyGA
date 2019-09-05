  pro readcmfgen, model, dat, euv=euv, griem=griem, modelonly=modelonly

  file='~uh101aw/Model_grids/cmfgen_grid/'+model+'/PhysParam.dat'
  openr,3,file
  fmod='tmp_cmfgen_mod'
  openw,2,fmod
  a=' '
  readf,3,a

  
  for i=1,70 do begin
   readf,3,a
   printf,2,a
  endfor
  close,2
  close,3

  lineno=70
  col=6
  
  rtab,r,v,fmod,lineno,col,2,3
  rtab,rho,t,fmod,lineno,col,4,5
  rtab,t,taur,fmod,lineno,col,5,6
  spawn,'rm tmp_cmfgen_mod'

  if keyword_set(modelonly) then goto, cont
  
  if (not keyword_set(euv) and not keyword_set(griem)) then begin
  file='~uh101aw/Model_grids/cmfgen_grid/'+model+'/flux.dat'
  openr,3,file
  readf,3,a
  endif else begin  
    if keyword_set(euv) then begin
    file='~uh101aw/Model_grids/cmfgen_grid/'+model+'/EUV_flux.dat'
    openr,3,file
    readf,3,a
    endif
    if keyword_set(griem) then begin
    file='~Model_grids/cmfgen_grid/griem/'+model+'/flux_griem.dat'
    openr,3,file
    readf,3,a
    endif
  endelse
  
  stri=str_sep(a,' ')
  b=where(stri ne '')
  str=stri(b)
  str=strmid(str,1)
  nfl=long(str(0))
  nfc=long(str(1))

  if nfl lt nfc then begin
    print,' nfl < nfc!'
    return
  endif
    
  lam=fltarr(nfl)
  hnue=fltarr(nfl)
  lamc=fltarr(nfc)
  hnuec=fltarr(nfc)

  for i=0L,nfc-1L do begin
    readf,3,a
    stri=str_sep(a,' ')
    b=where(stri ne '')
    str=stri(b)
    lam(i)=str(0)
    hnue(i)=str(1)
    lamc(i)=str(2)
    hnuec(i)=str(3)
  endfor

  aux=fltarr(2,nfl-nfc)
  readf,3,aux
  lam(nfc:nfl-1L)=reform(aux(0,*))
  hnue(nfc:nfl-1L)=reform(aux(1,*))
  close,3
  
  lam=2.997925e18/lam
  lamc=2.997925e18/lamc
  xic=4.d0*hnue
  trad=1.4388d8/lam/(alog(3.97d8/lam^3/xic+1.d0))
  xic=4.d0*hnuec
  tradc=1.4388d8/lamc/(alog(3.97d8/lamc^3/xic+1.d0))

  hnuec1=interpol(hnuec,lamc,lam,/spline)
  profile=hnue/hnuec1
  ifre=nfl

  cont:
  nd=70

  if not keyword_set(modelonly) then begin  
   dat={name: '',r: fltarr(nd), v: fltarr(nd), $
     rho: fltarr(nd), t: fltarr(nd), taur: fltarr(nd), $
     lam: fltarr(ifre), hnue: fltarr(ifre), trad: fltarr(ifre), $
     prof: fltarr(ifre), $
     lamc:fltarr(nfc), hnuec: fltarr(nfc), tradc: fltarr(nfc)}
   endif else begin
   dat={name: '',r: fltarr(nd), v: fltarr(nd), $
     rho: fltarr(nd), t: fltarr(nd), taur: fltarr(nd)}
   endelse
   
dat.name=model
dat.r=r
dat.v=v
dat.rho=rho
dat.t=t
dat.taur=taur

if keyword_set(modelonly) then return

dat.lam=lam
dat.hnue=hnue
dat.trad=trad
dat.prof=profile
dat.lamc=lamc
dat.hnuec=hnuec
dat.tradc=tradc

file='~uh101aw/Model_grids/cmfgen_grid/'+model+'/'+model+'.spec'

if file_test(file) then return
openw,4,file

for i=0L,ifre-1 do begin
printf,4,lam(i),profile(i)
endfor

close,4

return
end
