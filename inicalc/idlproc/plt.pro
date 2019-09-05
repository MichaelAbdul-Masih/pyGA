pro plt,dat,taur=taur
char=' '
lab1: 	print,' give in x=axis: r, m or taur'
if keyword_set(taur) then begin
  char='taur'
endif else begin  
  read,char
endelse

if char eq 'r' then begin
x=dat.r
ndim=size(x)
nd=ndim(1)
xmin=x(nd-2)-1.
plot_oi,x-1.,dat.t,title=dat.name,xtitle='r/Rstar - 1',ytitle='Temp [K]', $
xrange=[xmin,x(0)],/ynozero
endif else begin

if char eq 'm' then begin
x=dat.m
ndim=size(x)
nd=ndim(1)
xmin=x(1)
plot_oi,x,dat.t,title=dat.name,xtitle='m',ytitle='Temp [K]', $
xrange=[x(nd-1),xmin],/ynozero
endif else begin

if char eq 'taur' then begin
x=dat.taur
ndim=size(x)
nd=ndim(1)
xmin=x(1)
plot_oi,x,dat.t,title=dat.name,xtitle='tau_Ross',ytitle='Temp [K]', $
xrange=[x(nd-1),xmin],/ynozero
endif else begin
print,' Wrong Input'
goto, lab1
endelse
endelse
endelse
return
end

