pro halpha1,file,x,y,prof,vsini=vrot,ainst=ain,vinf=vin
prof=fltarr(161,2)

rtabprof,x,y,file,161,6,3,5,ew

y1=y
if keyword_set(vrot) then begin
rconv,x,y,y1,vrot
y=y1
endif

if keyword_set(ain) then begin
gconv,x,y,y1,ain
y=y1
endif

if keyword_set(vin) then begin
rtabprof,x,x1,file,161,6,2,2,ew
x=-x*vin
endif

for i=1,80 do begin
if abs(1.-y1(i)) gt .0015 then goto, lab1
endfor
lab1:lammax=x(i-1)
for i=159,81,-1 do begin
if abs(1.-y1(i)) gt .0015 then goto, lab2
endfor
lab2:lammin=x(i+1)
;lammax=-500.
;lammin=500.
!x.range=[lammax,lammin]

if keyword_set(vin) then begin
plot,x,y1,/ynozero,xtitle='v(km/s)',ytitle='emergent profile', $
xtick_get=xt,ytick_get=yt,xstyle=1
endif else begin
plot,x,y1,/ynozero,xtitle='lambda (A)',ytitle='emergent profile', $
xtick_get=xt,ytick_get=yt,xstyle=1
endelse

xs=size(xt)
nx=xs(1)
ys=size(yt)
ny=ys(1)
;xco=xt(0)+.05*(xt(nx-1)-xt(0))
xco=lammax+.05*(lammin-lammax)
;yco=yt(0)+.90*(yt(ny-1)-yt(0))
yco=yt(0)+.80*(yt(ny-1)-yt(0))
ewst=string(format='(f6.2)',ew)
;st='e.w. = '+ewst+' (A)'
st=ewst
dum=!p.linestyle
!p.linestyle=0
xyouts,/data,xco,yco,st,charsize=1.
!p.linestyle=dum
prof(*,0)=x
prof(*,1)=y
!x.range=0
return
end
