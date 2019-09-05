pro prepff
;
; prepares tscm.exe
;
infile=' '
print,' which observed spectrum (IDL.sav-format)?'
print,' NO QUOTATION MARKS REQUIRED'
read,infile
print
;
restore,file=infile

ranges:
print, ' give in lower und upper boundary in lambda'
read,wmin,wmax
print
;
plot,wave,fluxrec,xrange=[wmin,wmax],xstyle=1
print, 'new wavelength ranges (yes = 1, no = 0)?'
print
read, yes
if yes eq 1 then goto, ranges

;
print,'measure fit-points with cursor'
print
;
  ndim=100
  if keyword_set(nd) then ndim=nd
  x=fltarr(ndim)
  y=fltarr(ndim)
  
  dx1=1./(!x.crange(0)-!x.crange(1))
  dy1=1./(!y.crange(0)-!y.crange(1))

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
      print,' Too many points! Increase nd! 
      goto,jump
    endif
    xhair,a,b
    x(i)=a
    y(i)=b
    errx=abs((x(i)-x(i-1))*dx1)
    erry=abs((y(i)-y(i-1))*dy1)
  endrep until errx lt 0.01 and erry lt 0.01  

  jump:
  b=where(x ne 0)
  x1=x(b)
  y1=y(b)
; 
; write files to smh/iter
;
  lineid=' '
  openw,1,'$HOME/smh/iter/obsitsp'
  print,' give in line-id (CAPITALS)'
  read,lineid
  printf,1,lineid 
  printf,1,i
  for j=0,i-1 do begin
    printf,1,x1(b(j)),y1(b(j))
  endfor
  close,1
;
  b=where(wave ge wmin and wave le wmax)
  nbs=size(b)
  nw=nbs(1)
  openw,1,'$HOME/smh/iter/obsspec'
  printf,1,lineid 
  printf,1,nw
  for i=0,nw-1 do begin
    printf,1,wave(b(i)),fluxrec(b(i))
  endfor
  close,1
;
  return
  end
