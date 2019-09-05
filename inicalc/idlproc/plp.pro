pro plp,dat
char=' '
lab1: 	print,' give in x=axis: r, m or taur'
read,char
if char eq 'r' then begin
x=dat.r
ndim=size(x)
nd=ndim(1)
xmin=x(nd-2)-1.
plot_oo,x-1.,dat.p,title=dat.name,xtitle='r/Rstar -1',ytitle='p_gas', $
xrange=[xmin,x(0)]
endif else begin

if char eq 'm' then begin
x=dat.m
ndim=size(x)
nd=ndim(1)
xmin=x(1)
plot_oo,x,dat.p,title=dat.name,xtitle='m',ytitle='p_gas',xrange=[x(nd-1),xmin]
endif else begin

if char eq 'taur' then begin
x=dat.taur
ndim=size(x)
nd=ndim(1)
xmin=x(1)
plot_oo,x,dat.p,title=dat.name,xtitle='tau_Ross',ytitle='p_gas', $
xrange=[x(nd-1),xmin]
endif else begin
print,' Wrong Input'
goto, lab1
endelse
endelse
endelse
return
end

