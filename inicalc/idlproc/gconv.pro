pro gconv,x,y0,y1,ainst
;
;Approximate convolution with gaussian, fwhm of device = ainst
;assumes that resolution is high enough
       
;-----unconvolved profile y0
;-----  convolved profile y1
;
ndim=size(x)
n=ndim(1)
y1=fltarr(n)

if ainst lt 0.001 then begin
y1=y0
return
endif

div=2./ainst
fac=alog(2.)
n0=2

for i=1,n do begin
sum=0.
qad=0.
m0=n0
x0=x(i-1)

for j=m0,n do begin
xb=(x(j-2)-x0)*div
xf=(x(j-1)-x0)*div
if xb gt 4.0 then goto, jump3
if xf lt-4.0 then goto, jump1 
xb=max([-4.0,xb])
xf=min([4.0,xf])
pb= exp(-xb*xb*fac)
pf= exp(-xf*xf*fac)
sum=sum+(pb*y0(j-2)+pf*y0(j-1))*(xf-xb)
qad=qad+(pb+pf)*(xf-xb)
goto, jump2
jump1: n0=j
jump2:
endfor

jump3: y1(i-1)=sum/qad
endfor
return
end



