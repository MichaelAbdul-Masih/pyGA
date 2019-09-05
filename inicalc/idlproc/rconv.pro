pro rconv,x,y0,y1,vrot
;
;Rotational convolution vith vrot:= vsini in km/s
;       
;-----unrotated profile y0
;-----  rotated profile y1
;-----limb-darkening coefficient 1.5 used
;
ndim=size(x)
n=ndim(1)
y1=fltarr(n)

if vrot lt 0.1 then begin
y1=y0
return
endif

con=2.997925e5/vrot
pi1=0.31830
n0=2

for i=1,n do begin
sum=0.
qad=0.
m0=n0
x0=x(i-1)
div=con/x0

for j=m0,n do begin
xb=(x(j-2)-x0)*div
xf=(x(j-1)-x0)*div
if xb gt 1.0 then goto, jump3
if xf lt-1.0 then goto, jump1 
xb=max([-1.0,xb])
xf=min([1.0,xf])
pbx=1.0-xb*xb
pfx=1.0-xf*xf
pb=pi1*sqrt(pbx)+.375*pbx
pf=pi1*sqrt(pfx)+.375*pfx
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



