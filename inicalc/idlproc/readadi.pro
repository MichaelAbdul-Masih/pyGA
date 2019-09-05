pro readadi,model,star,dat,rstar=rstar

model='$HOME/WM-basic/models/'+model+'/data'

if not keyword_set(rstar) then rstar=1.
  
  dummy = ''
  n     = 0L
  nd    = 41
  nx    = 0
  kel   = 30
  kis   = 9

  nit=1
  nf=nit
  nf1=nit
  openr,1,model+'/MERGESPEC'
  readf,1,nit,nf
  readf,1
  readf,1

  fluxarr=dblarr(4,nf)
  readf,1,fluxarr
  close,1

  adilam=reform(fluxarr(0,*))
  adihnue=reform(fluxarr(1,*))
  adihnue=adihnue/rstar^2
  adilam=reverse(adilam)
  
  for i=0,nf-2 do begin
    if(adilam(i+1) eq adilam(i)) then adilam(i+1)=adilam(i+1)+.001
  endfor
  adihnue=reverse(adihnue)

  nf1=0
  file=model+'/STOFLUX'
  if(file_test(file)) then begin
  openr,1,file
  readf,1,nf1
  readf,1
  readf,1

  fluxarr=dblarr(4,nf1)
  readf,1,fluxarr
  close,1

  xlam=reform(fluxarr(0,*))
  prof=reform(fluxarr(3,*))
  endif

  
  dat={name:'',r: fltarr(nd),v: fltarr(nd),rho: fltarr(nd),t: fltarr(nd), $
    xne: fltarr(nd),taur: fltarr(nd),ifrac: fltarr(kis,kel,nd), $
    lam: fltarr(nf), hnue: fltarr(nf), trad: fltarr(nf), $
    xlam: fltarr(nf1), prof: fltarr(nf1)}
    dat.name=star


  dat.lam=adilam
  dat.hnue=alog10(adihnue)
  trad=4.d0*adihnue
  dat.trad=1.4388d8/adilam/(alog(3.97d8/adilam^3/trad+1.d0))

  if nf1 ne 0 then begin
    dat.xlam=xlam
    dat.prof=prof
  endif  
  
  openr,1,model+'/FINFIELD'
  readf,1,teff,grav,rstar,yhe,nd
  print,teff,grav,rstar,yhe,nd

  for i=nd-1,0,-1 do begin
   readf,1,nx,x1,x2,x3,x4,x5,x6,x7
   
   dat.xne(i)=x2*(0.5*( 1.-sqrt(1.-1./(x3*x3) ) ))
   dat.r(i)=x3
   dat.v(i)=x4
   dat.rho(i)=x5
   dat.t(i)=x6
   dat.taur(i)=x7
 endfor
 close,1

  n1=0
  n2=0
  openr,1,model+'/IONTEST'
  while not eof(1) do begin
    readf,1,dummy
    if (strmid(dummy,1,1) eq 'D' and n2 eq 0) then begin
      n2=n1
      n1=n
    endif
    n=n+1L
  endwhile
  close,1
  openr,1,model+'/IONTEST'
  for i=0L,n-(41L*(n1-n2))-1L do begin
    readf,1,dummy
  endfor
  for i=nd-1,0,-1 do begin
    readf,1,dummy
    for j=0,n1-n2-2 do begin
      readf,1,nx,x1,x2,x3,x4,x5,x6,x7,x8,x9
      nx=nx-1
      dat.ifrac(0,nx,i) = x1
      dat.ifrac(1,nx,i) = x2
      dat.ifrac(2,nx,i) = x3
      dat.ifrac(3,nx,i) = x4
      dat.ifrac(4,nx,i) = x5
      dat.ifrac(5,nx,i) = x6
      dat.ifrac(6,nx,i) = x7
      dat.ifrac(7,nx,i) = x8
      dat.ifrac(8,nx,i) = x9
    endfor

  for nx=0,kel-1 do begin
    abund=total(dat.ifrac(*,nx,i))
    if (abund ne 0.) then dat.ifrac(*,nx,i)=dat.ifrac(*,nx,i)/abund
  endfor  

    
  endfor

  close,1

  
  return
end
