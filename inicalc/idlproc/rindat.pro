pro rindat,cat,teff,dat,silent=silent

dat={teff: 1.,logg: 1.,rstar: 1.,yhe: 1., hei: 1., vturb: 1.,z: 1.,$
     q: 1.,qchar:' ',mdot: 1.,vinf: 1.,beta: 1., vdiv: 1.,$
     clfac: 1.,clstart: 1.,clmax: 1., $
     qinf: 1.,q0: 1.,gamma: 1., $
     fx: 0., gammax: 0., mx: 0., rminx: 0., uinfx: 0., nitmax: 100}

  qinf=1.
  q0=1.
  gam=1.

  openr,1,cat+'/INDAT.DAT'
  line=strarr(13)
  line(*)=' '
  a=' '
  readf,1,a
  line(0)=a  ; model
  readf,1,a
  line(1)=a  ; iterations
  stri=str_sep(a,' ')
  b=where(stri ne '')
  str=stri(b)
  nitmax=str(3)
  readf,1,a
  line(2)=a  ; optmixed
  readf,1,teff,logg,rstar
;old version, does not work if there are blanks between the commas
;  line(3)=a  ; st-para
;  stri=str_sep(line(3),' ')
;  b=where(stri ne '')
;  str=stri(b)
;  teff=float(str(0))
;  logg=float(str(1))
;  rstar=float(str(2))
;  stop
  
  readf,1,a
  line(4)=a ; rmax, tmin_start
  stri=str_sep(line(4),' ')
  b=where(stri ne '')
  str=stri(b)
  tmin=float(str(1))
  rmax=float(str(0))
  if not keyword_set(silent) then print,rmax
  if rmax ne 120. then begin
    print,'rmax ne 120!!!'
  endif  
  
  readf,1,a
  line(5)=a ; wind para
  stri=str_sep(line(5),' ')
  b=where(stri ne '')
  str=stri(b)
  mdot=float(str(0))
  vinf=float(str(2))
  beta=float(str(3))
  vdiv=float(str(4))

  readf,1,a
  line(6)=a ; he info
  stri=str_sep(line(6),' ')
  b=where(stri ne '')
  str=stri(b)
  yhe=float(str(0))
  hei=float(str(1))

  readf,1,a
  line(7)=a ; control
  stri=str_sep(line(7),' ')
  b=where(stri ne '')
  str=stri(b)
  optlucy=(str(1))
  optlucy=strupcase(optlucy)
  
  readf,1,a
  line(8)=a ; z infor
  stri=str_sep(line(8),' ')
  b=where(stri ne '')
  str=stri(b)
  vturb=float(str(0))
  z=float(str(1))

  readf,1,a
  line(9)=a ; t control
  readf,1,a
  line(10)=a ; cl control
  if strtrim(a,2) eq 'THICK' then begin
    print,' OPTICALLY THICK CLUMPING'
    readf,1,a
    readf,1,a
    readf,1,a
    readf,1,a
    clfac=1.
    clstart=1.
    clmax=1.
  endif else begin
    stri=str_sep(line(10),' ')
    b=where(stri ne '')
    str=stri(b)
    clfac=float(str(0))
    clstart=float(str(1))
    clmax=float(str(2))
  endelse
  
  if optlucy eq 'F' then begin
  readf,1,a
  line(11)=a ; hopf-self
  stri=str_sep(line(11),' ')
  b=where(stri ne '')
  str=stri(b)
  optself=(str(0))
  optself=strupcase(optself)
  if optself eq 'F' then begin
  readf,1,a
  line(12)=a ; hopf para
  stri=str_sep(line(12),' ')
  b=where(stri ne '')
  str=stri(b)
  qinf=float(str(0))
  q0=float(str(1))
  gam=float(str(2))
  endif
  endif

  linex=''
  key=''
  while not eof(1) do begin
    readf,1,a
    linex=a     
    stri=str_sep(linex,' ')
    b=where(stri ne '')
    str=stri(b)
    key=(str(0))
    if key eq 'XRAYS' then begin
      fx=float(str(1))
      readf,1,a
      linex=a     
      stri=str_sep(linex,' ')
      b=where(stri ne '')
      str=stri(b)
      gammax=float(str(0))    
      mx=float(str(1))    
      rminx=float(str(2))    
      uinfx=float(str(3))    
    endif
  endwhile  
   
  q=alog10(mdot/(rstar*vinf)^1.5)
  char=['A','B','C','D','E','F','G','H','larger than H']
  qgrid=[-14.,-13.5,-13.15,-12.8,-12.45,-12.1,-11.75,-11.4]
  qgridm=qgrid
  for i=1,7 do begin
    qgridm(i-1)=qgrid(i)+.5*(qgrid(i-1)-qgrid(i))
  endfor
  
  for i=0,7 do begin
    if q le qgridm(i) then goto, lab
  endfor
  i=8

  lab: qchar=char(i)
  close,1

  if not keyword_set(silent) then begin

  print
  print,' Teff = ',strtrim(string(teff),2),' , logg = ',strtrim(string(logg),2), $
	 ' , YHe =',strtrim(string(yhe),2),' , Rstar = ',strtrim(string(rstar),2)
  print,' ',qchar,': log Q = ',strtrim(string(q),2)
  print,' vturb = ',strtrim(string(vturb),2),' , z = ',strtrim(string(z),2), $
	 ' , Tmin = ',strtrim(string(tmin),2)	 
  print,' Mdot = ',strtrim(string(mdot),2),' , vinf = ',strtrim(string(vinf),2), $
	 ' , beta = ',strtrim(string(beta),2)  
  if optlucy eq 'F' then begin
  print,' optself = ',optself
  if optself eq 'F' then begin
  print,' qinf = ',strtrim(string(qinf),2),' q0 = ',strtrim(string(q0),2), $
	 ' gamma = ',strtrim(string(gam),2)
  endif
  endif
  print
  endif
  
  dat.teff=teff
  dat.logg=logg
  dat.rstar=rstar
  dat.yhe=yhe
  dat.hei=hei
  dat.vturb=vturb
  dat.z=z
  dat.q=q
  dat.qchar=qchar
  dat.mdot=mdot
  dat.vinf=vinf
  dat.beta=beta
  dat.vdiv=vdiv
  dat.qinf=qinf
  dat.q0=q0
  dat.gamma=gam
  dat.clfac=clfac
  dat.clstart=clstart
  dat.clmax=clmax
  
  if key eq 'XRAYS'  then begin
    if not keyword_set(silent) then begin
     print,' Xray treatment with fx = ',fx
     print,' gammax = ',gammax    
     print,'     mx = ',mx    
     print,'  Rminx = ',rminx    
     print,'  uinfx = ',uinfx    
    endif
    dat.fx=fx
    dat.gammax=gammax
    dat.mx=mx
    dat.rminx=rminx
    dat.uinfx=uinfx
  endif	

  dat.nitmax=nitmax
  
  return
  end 
