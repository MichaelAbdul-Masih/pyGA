pro ew,x,y,xr=xr

if keyword_set(xr) then begin
  plot,x,y,xrange=xr
endif else begin
  plot,x,y
endelse

new:  
print,'define avarage cont'
xhair,dummy,cont

print,'cont = ',cont

y1=y/cont
if keyword_set(xr) then begin
  plot,x,y1,xrange=xr
endif else begin
  plot,x,y1
endelse


print,'define wmin'
xhair,wmin,ymin
print,'define wmax'
xhair,wmax,ymax

bi=where(x ge wmin and x le wmax)

xx=x(bi)
yy=y1(bi)

oplot,xx,yy,color=100
print,'OK = 1?'
read,ok
if ok ne 1 then goto, new

integrate,ew,yy-1.,xx
print,'ew=',ew

return
end

pro integrate,x,a,b
  n=(size(a))(1)
  x=0.
  for i=1,n-1 do begin
    x=x+.5*(a(i)+a(i-1))*(b(i)-b(i-1))
  endfor
return
end
