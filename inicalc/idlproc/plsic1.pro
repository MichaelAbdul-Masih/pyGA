pro plsic1,dat,sl1,bic
;pro plsic,dat,sl,bic
print,'input idl no of departure of lower level'
read,n1
print,'input idl no of departure of upper level'
read,n2
print,'give in wavelength in A'
read,xlam
bic=bnue(dat.t,xlam)
sl=bnue(dat.t,xlam,blow=dat.dep(n1,*),bup=dat.dep(n2,*))
x=dat.taur
ndim=size(x)
nd=ndim(1)
xmin=x(1)
sl1=sl/bic
plot_oi,x,sl1,title=dat.name,xtitle='tau_Ross',$
ytitle='S_Line/Bnue(Trad)',xrange=[x(nd-1),xmin]
y=x
y(*)=1.
oplot,x,y,linestyle=1
return
end

