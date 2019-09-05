pro pldep,dat,m=m

ov=0
n=strtrim(dat.levnam)
lab0: print,n
print,''
print,'give in desired level'
lev=''
read,lev
i=where(n eq lev,count)
if(count eq 0) then begin
print,' level not found!!!'
goto,lab0
endif
print,' level no = ',i+1

if keyword_set(m) then begin
x=dat.m
xtit='column density'
endif else begin
x=dat.taur
xtit='tau_Ross'
endelse
ndim=size(x)
nd=ndim(1)
xmin=x(1)

y=dat.dep(i,*)

if(ov eq 0) then begin
lab2: print,' '
print,' bi, lin = 0, log = 1?'
read,q
if q ne 0 and q ne 1 then goto, lab2 
if q eq 0 then begin
lines=0
plot_oi,x,y,title=dat.name,xtitle=xtit,ytitle='dep. coeff.', $
xrange=[x(nd-1),xmin]
endif else begin
lines=0
plot_oo,x,y,title=dat.name,xtitle=xtit,ytitle='dep. coeff.', $
xrange=[x(nd-1),xmin]
endelse

endif else begin
lines=lines+1
if lines eq 6 then lines=0
oplot,x,y,linestyle=lines
endelse

lab3: print,' '
print,' new plot = 0 '
print,' overplot = 1 '
print,' end      = 2 '
read,ov
if ov eq 2 then return
if ov ne 0 and ov ne 1 then goto, lab3 
goto, lab0
 
end 
