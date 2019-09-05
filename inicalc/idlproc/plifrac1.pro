pro plifrac1,dat
ndim=size(dat.ifrac)
natom=ndim(2)
nion=ndim(1)
print,' Number of atoms : ',natom
print,' max. no. of ions: ',nion
lab0: print,' '
print,' give in No. of atom'
read,na
if na gt natom then begin
print,' Wrong input'
goto, lab0
endif
lab1: print,' '
print,' give in min, max  ionization stage (astron. convention)'
read,imin,imax
if imax gt nion then begin
print,' Wrong input'
goto, lab1
endif

char=' '
print,' give in x=axis: r, v, xne or taur'
read,char
if char eq 'r' then begin
x=dat.r
x1=120./dat.r(0)
x=x*x1
endif else if char eq 'v' then begin
x=dat.v
print,' give in vinf (km/s)'
read,vinf
x=x*vinf
endif else if char eq 'xne' then begin
x=dat.xne
endif else begin
x=dat.taur
endelse

ndim=size(x)
nd=ndim(1)
xmin=x(1)

y=dat.ifrac(imin-1,na-1,*)
ymin=min(y)

if char eq 'taur' then begin
plot_oo,x,y,title=dat.name,xtitle='tau_Ross',ytitle='ioniz. fraction', $
xrange=[x(nd-1),xmin],yrange=[ymin,2]

endif else if char eq 'r' then begin
plot_oo,x,y,title=dat.name,xtitle='r/Rstar',ytitle='ioniz. fraction', $
xrange=[x(nd-1),xmin],yrange=[ymin,2],xstyle=1

endif else if char eq 'xne' then begin
plot_oo,x,y,title=dat.name,xtitle='n_e [cm^(-3)]',ytitle='ioniz. fraction', $
xrange=[x(nd-1),xmin],yrange=[ymin,2],xstyle=1

endif else begin
plot_io,x,y,title=dat.name,xtitle='v [km/s]',ytitle='ioniz. fraction', $
xrange=[x(nd-1),xmin],yrange=[ymin,2],xstyle=1
endelse
  
lines=0
for i=imin,imax-1 do begin
lines=lines+1
if lines gt 5 then lines=0
y=dat.ifrac(i,na-1,*)
oplot,x,y,linestyle=lines
endfor

lab3: print,' '
print,' new plot = 0 '
print,' end      = 2 '
read,ov
if ov eq 2 then return
goto, lab0

return
end
