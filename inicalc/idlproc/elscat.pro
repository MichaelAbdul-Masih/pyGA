pro elscat,x,y,x1,y1,yout,exi
yt=dblarr(241)
xi1=yt
yi1=yt
for i=0,20 do yt(i)=-4.+i*.15
for i=21,220 do yt(i)=-1.+(i-20)*.01
for i=221,240 do yt(i)=1.+(i-220)*.15
rby,abs(yt),value1
;
n=size(x)
nf=n(1)
x1=x
y1=y
b=uniq(x1)
x1=x1(b)
y1=y1(b)
n=size(x1)
nf=n(1)
print,'new dimension (of unique wavelength grid) nf = ',nf
print,' '
yout=y1
;
print,'give in temperature, tau_el'
read,temp,tau
beta=1.84e-5*sqrt(temp)
xi=2.997925e18/x1
xi=alog(xi)
;
for i=0,nf-1 do begin
xii=xi(i)
xi1=xii-yt*beta
;
;for k=0,120 do begin
;if(xi1(k) gt xi(0) or xi1(k) lt xi(nf-1)) then begin
;yi1(k)=1.
;endif else begin
yi1=interpol(y1,xi,xi1)
;endelse
;endfor
;
value=value1*yi1
;
integ=0.
for k=0,239 do begin
dyt=xi1(k)-xi1(k+1)
integ=integ+.5*dyt*(value(k)+value(k+1))
endfor
;
yout(i)=integ/beta
print,i
endfor
;
exi=yout
integ=0.
integ1=0.
for k=0,nf-2 do begin
dxi=xi(k)-xi(k+1)
integ=integ+.5*dxi*(yout(k)+yout(k+1))
integ1=integ1+.5*dxi*(y1(k)+y1(k+1))
endfor
print,integ1,' ',integ
yout=tau*yout+(1.-tau)*y1
end


