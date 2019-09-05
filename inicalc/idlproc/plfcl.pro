pro plfcl,dat,plr=plr,plv=plv,pltaur=pltaur,comp=comp

if keyword_set(plr) then begin
x=dat.r
ndim=size(x)
nd=ndim(1)
x1=x(1)
x=x*120./x1
plot_oi,x,dat.fcl,title=dat.name,xtitle='r/R(taur=2/3)',ytitle='f_cl', $
xrange=[x(nd-1),x(0)],xs=1
if keyword_set(comp) then begin
  x1=comp.r(1)
  oplot,comp.r*120./x1,comp.fcl,color=200,lines=2
endif  
endif else begin

if keyword_set(plv) then begin
x=dat.v
ndim=size(x)
nd=ndim(1)
plot,x,dat.fcl,title=dat.name,xtitle='v(r)/vinf',ytitle='f_cl'
if keyword_set(comp) then begin
  oplot,comp.v,comp.fcl,color=200,lines=2
endif  
endif else begin

if keyword_set(pltaur) then begin
x=dat.taur
ndim=size(x)
nd=ndim(1)
xmin=x(1)
plot_oi,x,dat.fcl,title=dat.name,xtitle='tau_Ross',ytitle='f_cl', $
xrange=[1.,xmin]
if keyword_set(comp) then begin
  oplot,comp.taur,comp.fcl,color=200,lines=2
endif  
endif else begin
print,' Wrong Input'
return
endelse
endelse
endelse
return
end

