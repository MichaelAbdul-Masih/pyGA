pro int,r,rho,m,rstar
rs=rstar*6.96e10
r1=r*rs
m=fltarr(47)
m(0)=0.
for i=1,46 do begin
dr=r1(i-1)-r1(i)
m(i)=m(i-1)+.5*(rho(i-1)+rho(i))*dr
endfor
return
end