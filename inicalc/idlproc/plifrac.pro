pro plifrac,dat
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

x=dat.taur
;x=dat.m
ndim=size(x)
nd=ndim(1)
xmin=x(1)

y=dat.ifrac(imin-1,na-1,*)
ymin=min(y)
plot_oo,x,y,title=dat.name,xtitle='tau_Ross',ytitle='ioniz. fraction', $
xrange=[x(nd-1),xmin],yrange=[ymin,2]

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
