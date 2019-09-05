pro analyze_spec,file

rspec,file,x,y  

window,0
plot,x,y  

print,' Give in wavelength range, wmin, wmax'
read,wmin,wmax
plot,x,y,xrange=[wmin,wmax],/ynozero

print,' Give in ionization stages for identifications, imin, imax'
print,' (astronomical convention, possible range 2...6)'
read,imin,imax

identify,imin,imax,x,y,wmin,wmax

choice:
print,' finished = 1'
print,' new wavelength range = 2'
print,' new ionisation stages = 3'
read,ok

if ok eq 1 then return

if ok eq 2 then begin
 plot,x,y  

 print,' Give in wavelength range, wmin, wmax'
 read,wmin,wmax
 plot,x,y,xrange=[wmin,wmax],/ynozero
 identify,imin,imax,x,y,wmin,wmax
 goto, choice

endif

if ok ne 3 then goto, choice
plot,x,y,xrange=[wmin,wmax],/ynozero

print,' Give in ionization stages for identifications, imin, imax'
print,' (astonomical convention, possible range 2...6)'
read,imin,imax

identify,imin,imax,x,y,wmin,wmax
goto, choice

return
end

pro identify,imin,imax,x,y,wmin,wmax

names=['C','N','O' ,'Ne' ,'Na' ,'Mg' ,'Si' ,'P' ,'S' ,'Ar' ,'K' ,'Ca']
weights=[12.,14.,16.,20.2,23.,24.3,28.1,31.,32,40.,39.1,40.1]
gfmin=-1.

  for i=imin,imax do begin
    if i eq 2 then io='II'
    if i eq 3 then io='III'
    if i eq 4 then io='IV'
    if i eq 5 then io='V'
    if i eq 6 then io='VI'
    if i eq imin then begin
      ions=[io]
    endif else begin
      ions=[ions,io]
    endelse
  endfor 
  print
  print,' ionization stages considered for ident:',ions
  no=imax-imin+1

  yr=!y.crange
  ymin=yr(0)
  ymax=yr(1)
  yt=ymin+(ymax-ymin)*.1

  for i=0,no-1 do begin
    read_reduced,'$HOME/line_ident/tables/'+ions(i),ioni,lami,gfi,configi
    if i eq 0 then begin
    ion=ioni
    lam=lami
    gf=gfi
    config=configi
    endif else begin
    ion=[ion,ioni]
    lam=[lam,lami]
    gf=[gf,gfi]
    config=[config,configi]
    endelse
  endfor  

  n=(size(lam))(1)

  replot:
  for i=0,n-1 do begin
      if(gf(i) gt gfmin) then begin
        oplot,[lam(i),lam(i)],yr,col=100
        xyouts,lam(i),yt,ion(i),charsize=2.
      endif
  endfor  

  ide:
  print,'OK=1, identify line = 2, measure vsini = 3'
  read,ok
  if ok eq 1 then return

  flag=0
  if ok eq 2 then xhair,x0,y0

  if ok eq 3 then begin
    print,' select line or central wavelength'
    xhair,x0,y0  
  endif
  
  if ok lt 1 or ok gt 3 then goto,ide
  
  b=where(lam ge x0-2. and lam le x0+2.,count)
  if count eq 0 and ok eq 3 then flag=1
 
  if flag eq 1 then begin
  print,' central wavelength at ',x0
  lam0=x0
  weight=20.
  goto, conti1
  endif
  
  if count eq 0 then begin
  print,' line missed, try once more?'
  goto, ide
  endif
  
  for i=0,(size(b))(1)-1 do begin
    if gf(b(i)) gt gfmin then $
    print,lam(b(i)),'  ',ion(b(i)),gf(b(i)),'  ',config(b(i))
  endfor

  if ok eq 2 then goto,ide

  conti1:
  print
  print,' Give in resolution'
  read,resol
;  print
;  print,' Give in approx. Teff'  
;  read,teff
; so far, we use vdop = 10 km/s (mean vturb)
  teff=40000.
  if flag eq 1 then goto,conti2
  
  gfmax=max(gf(b))
  for i=0,(size(b))(1)-1 do begin
    if gf(b(i)) eq gfmax then igfmax=i
  endfor

  i=b(igfmax)
  lam0=lam(i)
  name=strmid(ion(i),0,2)
  name=strtrim(name,2)
  j=where(name eq names,count)
  weight=weights(j)
  
  print,' selected line'
  print,' lambda0 = ',lam0,', ion = ',ion(i),', weight = ',weight 

  conti2:
  b=where(x gt lam0-15. and x le lam0+15.)
  vsini,x(b),y(b),lam0,resol,teff,weight
  window,0
  plot,x,y,xrange=[wmin,wmax],/ynozero
  goto,replot
  
return
end
  
pro read_reduced,file,ionis,lambda,gflog,config

lenline=72
line=' '

openr,1,file+'_reduced.txt'
readf,1,line
lam=strmid(line,3,9)
lambda=[float(lam)]

ion=strmid(line,18,6)
ionis=[strtrim(ion,2)]

conf=strmid(line,39,7)
config=[strtrim(conf,2)]

gf=strmid(line,lenline-7)
gflog=[float(gf)]

while not eof(1) do begin
readf,1,line
lam=strmid(line,3,9)
lamval=float(lam)
lambda=[lambda,lamval]

ion=strmid(line,18,6)
ionval=strtrim(ion,2)
ionis=[ionis,ionval]

conf=strmid(line,39,7)
config=[config,strtrim(conf,2)]

gf=strmid(line,lenline-7)
gfval=float(gf)
gflog=[gflog,gfval]

endwhile

close,1
return
end

pro vsini,x,yact,lam0,resol,teff,mass

y=yact  
window,1
plot,x,y,/ynozero  

ew,x,y,bi,ew

;vth=sqrt(2.*1.3806e-16*teff/(1.673e-24*mass))
print
print,' attention, vth assumed as 10 km/s; modify if A-stars analyzed!'
print
vth=10.e5
dl=vth*lam0/2.997925e10
a0=-ew/(sqrt(!pi)*dl)

w1=fltarr(301)
p1=w1


dl3=dl/3.
for i=0,300 do begin
  w1(i)=lam0-50.*dl+i*dl3
  dl1=lam0-w1(i)
  p1(i)=1.-a0*exp(-dl1*dl1/(dl*dl))
endfor

new:
plot,x(bi),y(bi),/ynozero

print,' give in try for vsini'

read,vrot
if keyword_set(resol) then begin
convol,w1,p1,w2,p2,vsini=vrot,resol=resol
endif else begin
convol,w1,p1,w2,p2,vsini=vrot,resol=resol
endelse
oplot,w2,p2,linestyle=1

print,' OK=1, newtry=0'
read,ok

if ok ne 1 then goto, new

return
end

pro ew,x,y,bi,ew

dim=(size(x))(1)

print,' re-rectify = 1?'
read,rect

if rect eq 1 then begin
rerect:
plot,x,y,/ynozero
print,' define begin and end of rectification'
print,' define start'
xhair,wmin,ymin
print,' define end'
xhair,wmax,ymax
dydx=(ymin-ymax)/(wmin-wmax)
const=ymin-dydx*wmin
y1=y
y1=dydx*x+const
oplot,x,y1,linestyle=1
print,' OK = 1?'
read,ok
if ok ne 1 then goto, rerect
y=y/y1
endif

print,' measure e.w.: automatic = 1, handish = 2' 
read, auto

if auto eq 1 then begin

newauto:
plot,x,y,/ynozero
print,' define begin and end of integration area'
print,' define w-start'
xhair,wmin,ymin
print,' define w-end'
xhair,wmax,ymax

bi=where(x ge wmin and x le wmax)
plot,x,y,xrange=[wmin,wmax],yrange=[min(y(bi)),max(y(bi))],xstyle=1
print,' OK = 1?'
read,ok
if ok ne 1 then goto, newauto
  
x1=[wmin-1.,x(bi),wmax+1.]
y1=[1.,y(bi),1.]
ew=int_tabulated(x1,y1-1.)
print
print,' ew=',ew
no=(size(x))(1)
bi=indgen(no)

return
endif

new:
plot,x,y,/ynozero  

print,' enlarge region'
print,' define wmin'
xhair,wmin,ymin
print,' define wmax'
xhair,wmax,ymax

bi=where(x ge wmin and x le wmax)
plot,x,y,xrange=[wmin,wmax],yrange=[min(y(bi)),max(y(bi))],xstyle=1
print,' OK = 1?'
read,ok
if ok ne 1 then goto, new

newtry0:

print,' click on continua in ascending order of wavelength!'

plot,x(bi),y(bi),yrange=[min(y(bi)),max(y(bi))]
measure1,a,b
sa=size(a)
na=sa(1)
amin=a(0)
amax=a(na-1)
na=na+4
a1=[wmin-1.,amin-1,a,amax+1.,wmax+1.]
b1=[1.,1.,b,1.,1.]
c=interpol(b1,a1,x)
oplot,x,c

print,' OK=1, newtry=0'
read,ok

if ok ne 1 then goto, newtry0
ew=int_tabulated(x,c-1.)
print,' ew=',ew

return
end

pro measure1,x1,y1,nd=nd
  ndim=100
  if keyword_set(nd) then ndim=nd
  x=fltarr(ndim)
  y=fltarr(ndim)
  
  dx1=1./(!x.crange(1)-!x.crange(0))
  dy1=1./(!y.crange(1)-!y.crange(0))

  i=0
  xhair,a,b
  x(i)=a
  y(i)=b
  repeat begin
    print,x(i),y(i)
    oplot,[x(i)],[y(i)],psym=2
    i=i+1
    if i gt ndim-1 then begin
      print
      print,' Too many points! Increase nd!' 
      goto,jump
    endif
    xhair,a,b
    x(i)=a
    y(i)=b
    errx=((x(i)-x(i-1))*dx1)
    erry=abs((y(i)-y(i-1))*dy1)
  endrep until errx lt 0.0 and erry lt 0.05  

  jump:
  b=where(x ne 0)
  x1=x(b)
  y1=y(b)
  return
  end
