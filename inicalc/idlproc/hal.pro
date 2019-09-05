pro hal,file,prof,vsini=vrot
prof=fltarr(161,2)
rtabprof,x,y,file,161,6,3,5,ew
if keyword_set(vrot) then begin
rconv,x,y,y1,vrot
plot,x,y1,/ynozero,xtitle='lambda (A)',ytitle='emergent profile', $
xtick_get=xt,ytick_get=yt
endif else begin
plot,x,y,/ynozero,xtitle='lambda (A)',ytitle='emergent profile', $
xtick_get=xt,ytick_get=yt
endelse
xs=size(xt)
nx=xs(1)
ys=size(yt)
ny=ys(1)
xco=xt(0)+.05*(xt(nx-1)-xt(0))
yco=yt(0)+.90*(yt(ny-1)-yt(0))
ewst=string(format='(f5.2)',ew)
st='e.w. = '+ewst+' (A)'
xyouts,/data,xco,yco,st,charsize=.6
prof(*,0)=x
prof(*,1)=y
return
end