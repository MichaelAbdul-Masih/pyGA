pro irlines,file,x,y,vsini=vrot1,resol=resol, vmacro=vmacro, $
	     compmin=compmin,compmax=compmax, $
	     wo=wobs,po=pobs,color=color
prof=fltarr(161,2)
rtabprof,x,y,file,161,6,3,5,ew

vrot=0.
if keyword_set(vrot1) then vrot=vrot1

if keyword_set(vrot1) or keyword_set(resol) or keyword_set(vmacro) then begin
convol,x,y,x1,y1,resol=resol,vsini=vrot,vmacro=vmacro
x=x1/1.e4
y=y1
endif else begin
x=x/1.e4
endelse

min1=min(y)
max1=max(y)

if keyword_set(compmin) then begin
min1=min([min1,compmin])
endif
if keyword_set(compmax) then begin
max1=max([max1,compmax])
endif

if keyword_set(wobs) then begin
  xpobs=where(wobs gt !x.range(0) and wobs lt !x.range(1) and pobs ne 0.,count)  
  if count ne 0 then begin
  mino=min(pobs(xpobs))
  maxo=max(pobs(xpobs))
  min1=min([mino,min1,0.95])
  max1=max([maxo,max1,1.05])
  endif
endif

!y.range=[min1,max1]

plot,x,y,/ynozero,xtitle='lambda (mu)',ytitle='emergent profile', $
xticks=4,xtick_get=xt,ytick_get=yt,/xstyle
if keyword_set(color) then oplot,x,y,color=200

xs=size(xt)
nx=xs(1)
ys=size(yt)
ny=ys(1)
xco=xt(0)+.03*(xt(nx-1)-xt(0))
yco=yt(0)+.84*(yt(ny-1)-yt(0))
ewst=string(format='(f6.2)',ew)
;st='e.w. = '+ewst+' (A)'
st=ewst
xyouts,/data,xco,yco,st,charsize=0.85

!y.range=0

return
end
