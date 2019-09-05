pro measure,x1,y1,nd=nd
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
      print,' Too many points! Increase nd!' 
      goto,jump
    endif
    xhair,a,b
    x(i)=a
    y(i)=b
    errx=abs((x(i)-x(i-1))*dx1)
    erry=abs((y(i)-y(i-1))*dy1)
  endrep until errx lt 0.05 and erry lt 0.05  

  jump:
  b=where(x ne 0)
  x1=x(b)
  y1=y(b)
  return
  end
