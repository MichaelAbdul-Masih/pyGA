pro halpha,file,x,y,vsini=vrot1,resol=resol,vmacro=vmacro, $
	    compmin=compmin,compmax=compmax, $
	    xnoself=xnoself,wo=wobs,po=pobs,vdop=vdop
  
rtabprof,x,y,file,161,6,3,5,ew

vrot=0.
if keyword_set(vrot1) then vrot=vrot1

if keyword_set(vrot1) or keyword_set(resol) or keyword_set(vmacro) then begin
convol,x,y,x1,y1,resol=resol,vsini=vrot,vmacro=vmacro
x=x1
y=y1
endif
min1=min(y)
max1=max(y)

if keyword_set(compmin) then begin
min1=min([min1,compmin])
endif
if keyword_set(compmax) then begin
max1=max([max1,compmax])
endif

if not keyword_set(xnoself) then begin
idum=size(y)
dim=idum(1)
for i=1,dim-1 do begin
if abs(1.-y(i)) gt .0015 then goto, lab1
;if abs(1.-y(i)) gt .01 then goto, lab1
endfor
i=1 ; in case nothing has been found (too weak a line)
lab1:lammax=x(i-1)
for i=dim-2,1,-1 do begin
if abs(1.-y(i)) gt .0015 then goto, lab2
;if abs(1.-y(i)) gt .01 then goto, lab2
endfor
i=dim-2 ; in case nothing has been found (too weak a line) 
lab2:lammin=x(i+1)
!x.range=[lammax,lammin]
endif


if keyword_set(wobs) then begin
  xpobs=where(wobs gt !x.range(0) and wobs lt !x.range(1),count)  
  if count ne 0 then begin
  min1=min([min1,pobs(xpobs)])
  max1=max([max1,pobs(xpobs)])
  endif
endif

!y.range=[min1,max1]

plot,x,y,/ynozero,xtitle='lambda (A)',ytitle='emergent profile', $
xtick_get=xt,ytick_get=yt;,charsize=1.

xs=size(xt)
nx=xs(1)
ys=size(yt)
ny=ys(1)
xco=xt(0)+.05*(xt(nx-1)-xt(0))
yco=yt(0)+.90*(yt(ny-1)-yt(0))
ewst=string(format='(f8.3)',ew)
;st='e.w. = '+ewst+' (A)'
st=ewst
xyouts,/data,xco,yco,st,charsize=1.

!y.range=0
!x.range=0
return
end
