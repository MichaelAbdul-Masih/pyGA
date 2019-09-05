pro pldx,n,dat1,dat2,dat3,dat4,dat5,dat6

if n gt 6 then begin
print,' number of structures > 6, not implemented'
return
endif

print,'Give in level no (IDL nomenclature)'
read,nx

n1=0
n2=1
xlam=1215.
lines=0
dat=dat1
x=dat.taur
ndim=size(x)
nd=ndim(1)
xmin=x(1)

y=dat.dep(nx,*)
plot_oo,x,y,title=dat.name,xtitle='tau_ross',ytitle='dep. coeff.', $
xrange=[x(nd-1),xmin],linestyle=lines
b=where(dat.tau911 ge 1)
i1=b(0)
b=where(dat.taulya ge 1)
i2=b(0)
oplot,[x(i1)],[y(i1)],psym=2
oplot,[x(i2)],[y(i2)],psym=7
beta=x
for i=0,nd-1 do beta(i)=min([1.,1./(3.*dat.taulya(i))])
rscale=120./dat.r(0)
r1=dat.r*rscale
wr=r1
for i=0,nd-1 do begin
wr(i)=1.
if(r1(i) ge 1) then wr(i)=.5*(1.-sqrt(1.-1./r1(i)^2))/r1(i)
endfor
trad=interpol(dat.trad,dat.lam,xlam)
print,' radiation temperature at',xlam,'(A) = ',trad,'(K)'
bic=bnue(trad,xlam)
sl=bnue(dat.t,xlam,blow=dat.dep(n1,*),bup=dat.dep(n2,*))/bic(0)
z=beta*(1.-wr/sl(0,*))
i3=where(z eq min(z))
oplot,[x(i3)],[y(i3)],psym=4

if n eq 1 then return

for i=2,n do begin

if i eq 2 then dat=dat2
if i eq 3 then dat=dat3
if i eq 4 then dat=dat4
if i eq 5 then dat=dat5
if i eq 6 then dat=dat6

x=dat.taur
y=dat.dep(nx,*)
lines=lines+1
oplot,x,y,linestyle=lines
b=where(dat.tau911 ge 1)
i1=b(0)
b=where(dat.taulya ge 1)
i2=b(0)
oplot,[x(i1)],[y(i1)],psym=2
oplot,[x(i2)],[y(i2)],psym=7
beta=x
for j=0,nd-1 do beta(j)=min([1.,1./(3.*dat.taulya(j))])
rscale=120./dat.r(0)
r1=dat.r*rscale
wr=r1
for j=0,nd-1 do begin
wr(j)=1.
if(r1(j) ge 1) then wr(j)=.5*(1.-sqrt(1.-1./r1(j)^2))/r1(j)
endfor
trad=interpol(dat.trad,dat.lam,xlam)
print,' radiation temperature at',xlam,'(A) = ',trad,'(K)'
bic=bnue(trad,xlam)
sl=bnue(dat.t,xlam,blow=dat.dep(n1,*),bup=dat.dep(n2,*))/bic(0)
z=beta*(1.-wr/sl(0,*))
i3=where(z eq min(z))
oplot,[x(i3)],[y(i3)],psym=4

endfor
return
end 



