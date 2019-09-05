pro pldepn,d,bhn,bhen
t=d.t
v=d.v
taur=d.taur
ndim=size(d.ifrac)
natom=ndim(2)
if(natom eq 1) then begin
print,' no helium'
endif
new=0

ndim=size(v)
nd=ndim(1)
bh=fltarr(4,nd)
bhe=fltarr(4,nd)
bhn=fltarr(4,nd)
bhen=fltarr(4,nd)
levh2='H12'
levh3='H13'
levh4='H14'
levh5='H15'
levhe4='HE24'
levhe6='HE26'
levhe8='HE28'
levhe10='HE210'
n=strtrim(d.levnam)
i=where(n eq levh2)
bh(0,*)=d.dep(i,*)
i=where(n eq levh3)
bh(1,*)=d.dep(i,*)
i=where(n eq levh4)
bh(2,*)=d.dep(i,*)
i=where(n eq levh5)
bh(3,*)=d.dep(i,*)
minbh=min(bh)
maxbh=max(bh)
relh=minbh/maxbh

log1=0
tit1='b_hydrogen(i=2,3,4,5)'
if relh lt .05 and maxbh gt 100. then begin
log1=1
tit1='log b_hydrogen(i=2,3,4,5)'
bh=alog10(bh)
minbh=alog10(minbh)
maxbh=alog10(maxbh)
endif

if(natom ge 2) then begin
i=where(n eq levhe4)
bhe(0,*)=d.dep(i,*)
i=where(n eq levhe6)
bhe(1,*)=d.dep(i,*)
i=where(n eq levhe8)
bhe(2,*)=d.dep(i,*)
i=where(n eq levhe10)
bhe(3,*)=d.dep(i,*)
minbhe=min(bhe)
maxbhe=max(bhe)
relhe=minbhe/maxbhe

log2=0
tit2='b_helium(i=4,6,8,10)'
if relhe lt .05 and maxbhe gt 100. then begin
log2=1
tit2='log b_helium(i=4,6,8,10)'
bhe=alog10(bhe)
minbhe=alog10(minbhe)
maxbhe=alog10(maxbhe)
endif

endif

lab0: print,' give in option'
print,'  actual departures = 0'
print,' normal. departures = 1'
print,'                END = 2'
read,norm
if norm gt 2 then goto, lab0
if norm eq 2 then return

if norm eq 1 and new eq 0 then begin
print,' give in Teff, alpha_t = Te/Teff'
read,teff,alphat
eh=fltarr(4)
ehe=fltarr(4)
dtinv=fltarr(nd)
tfac=fltarr(nd)

te=teff*alphat
tee=te^1.5
ce=1.43883

eh(0)=27419.7*ce
eh(1)=12186.5*ce
eh(2)=6854.9*ce
eh(3)=4387.1*ce

ehe(0)=27450.1*ce
ehe(1)=12210.9*ce
ehe(2)=6877.2*ce
ehe(3)=4408.5*ce

for i=0,nd-1 do begin
ttt=t(i)^1.5
dtinv(i)=1./t(i)-1./te
tfac(i)=tee/ttt

for j=0,3 do begin
if log1 eq 0 then begin
bhn(j,i)=bh(j,i)*tfac(i)*exp(eh(j)*dtinv(i))
endif else begin
bhn(j,i)=10.^bh(j,i)*tfac(i)*exp(eh(j)*dtinv(i))
endelse
if natom ge 2 then begin
if log2 eq 0 then begin
bhen(j,i)=bhe(j,i)*tfac(i)*exp(ehe(j)*dtinv(i))
endif else begin
bhen(j,i)=10.^bhe(j,i)*tfac(i)*exp(ehe(j)*dtinv(i))
endelse
endif
endfor
endfor

minbhn=min(bhn)
maxbhn=max(bhn)
relhn=minbhn/maxbhn

log3=0
tit3='b_hyd(NORM) (i=2,3,4,5)'
if relhn lt .05 and maxbhn gt 100. then begin
log3=1
tit3='log b_hyd(NORM) (i=2,3,4,5)'
bhn=alog10(bhn)
minbhn=alog10(minbhn)
maxbhn=alog10(maxbhn)
endif

if natom ge 2 then begin
minbhen=min(bhen)
maxbhen=max(bhen)
relhen=minbhen/maxbhen

log4=0
tit4='b_he(NORM) (i=4,6,8,10)'
if relhen lt .05 and maxbhen gt 100.then begin
log4=1
tit4='log b_he(NORM) (i=4,6,8,10)'
bhen=alog10(bhen)
minbhen=alog10(minbhen)
maxbhen=alog10(maxbhen)
endif
endif

new=1
endif

lab1: if natom ge 2 then begin 
print,' hydrogen = 0'
print,' helium   = 1'
read,elem
if elem gt 1 then goto, lab1

endif else begin
elem=0
endelse

lab2: print,' linear in v = 0'
print,' logar. in   = 1'
read,lin
if lin gt 1 then goto, lab2

if norm eq 0 then begin

if elem eq 0 then begin

if lin eq 0 then begin
plot,v,bh(0,*),title=d.name,xtitle='v/vinf',ytitle=tit1,$
yrange=[minbh,maxbh]
lines=0
for i=1,3 do begin
lines=lines+1
oplot,v,bh(i,*),linestyle=lines
endfor

endif else begin
plot_oi,v,bh(0,*),title=d.name,xtitle='v/vinf',ytitle=tit1,$
yrange=[minbh,maxbh]
lines=0
for i=1,3 do begin
lines=lines+1
oplot,v,bh(i,*),linestyle=lines
endfor
endelse

endif else begin
if lin eq 0 then begin
plot,v,bhe(0,*),title=d.name,xtitle='v/vinf',ytitle=tit2,$
yrange=[minbhe,maxbhe]
lines=0
for i=1,3 do begin
lines=lines+1
oplot,v,bhe(i,*),linestyle=lines
endfor

endif else begin
plot_oi,v,bhe(0,*),title=d.name,xtitle='v/vinf',ytitle=tit2,$
yrange=[minbhe,maxbhe]
lines=0
for i=1,3 do begin
lines=lines+1
oplot,v,bhe(i,*),linestyle=lines
endfor
endelse

endelse

endif else begin

if elem eq 0 then begin

if lin eq 0 then begin
plot,v,bhn(0,*),title=d.name,xtitle='v/vinf',ytitle=tit3,$
yrange=[minbhn,maxbhn]
lines=0
for i=1,3 do begin
lines=lines+1
oplot,v,bhn(i,*),linestyle=lines
endfor

endif else begin
plot_oi,v,bhn(0,*),title=d.name,xtitle='v/vinf',ytitle=tit3,$
yrange=[minbhn,maxbhn]
lines=0
for i=1,3 do begin
lines=lines+1
oplot,v,bhn(i,*),linestyle=lines
endfor

endelse

endif else begin

if lin eq 0 then begin
plot,v,bhen(0,*),title=d.name,xtitle='v/vinf',ytitle=tit4,$
yrange=[minbhen,maxbhen]
lines=0
for i=1,3 do begin
lines=lines+1
oplot,v,bhen(i,*),linestyle=lines
endfor

endif else begin
plot_oi,v,bhen(0,*),title=d.name,xtitle='v/vinf',ytitle=tit4,$
yrange=[minbhen,maxbhen]
lines=0
for i=1,3 do begin
lines=lines+1
oplot,v,bhen(i,*),linestyle=lines
endfor
endelse

endelse

endelse

goto,lab0
end 

