pro plot_profiles_combined, dat, wobs_opt, pobs_opt, wobs_ir, pobs_ir, $
    col, xranges = xranges, lband=lband

obscomp_opt=1
if wobs_opt(0) eq 0. then obscomp_opt=0

obscomp_ir=1
if wobs_ir(0) eq 0. then obscomp_ir=0


color=1
if col(0) eq -1 then color=0

no_mod=(size(dat.cat))(1)

no_lin=(size(dat.lines))(1)

nf=intarr(no_mod)
nf(0)=0

vsini=dat.vsini
vmacro=dat.vmacro
resol_opt=dat.resol_opt
resol_ir=dat.resol_ir

; loop over all lines
for i=0,no_lin-1 do begin

  li=dat.lines(i)
 
  band=dat.band(i) 

  if band eq 'opt' then begin
  resol1=resol_opt
  endif else begin
  resol1=resol_ir
  lam=wobs_ir
; sort out lband lines (for ir)
  if keyword_set(lband) then begin
   if li eq 'BRALPHA' or li eq 'BRALPHA1' or li eq 'PFGAMMA' then begin
   if keyword_set(resol_ir) then resol1=lband.resol
   lam=lband.wobs_ir1
  endif
  endif
  endelse
  
  !p.title=dat.star+dat.titles(i)

  for k=1,no_mod-1 do begin
  file=dat.cat(k)+'/OUT.'+dat.lines(i)+dat.ext(k)
  if not file_test(file) then begin
    print,file,' does not exist'
    return
  endif  

  rtabprof,x1,y1,file,161,6,3,5,ew
  if vsini ne 0 or vmacro ne 0 or resol1 ne 0 then begin
  convol,x1,y1,xp,yp,resol=resol1,vsini=vsini,vmacro=vmacro
  x1=float(xp)
  y1=float(yp)
  endif
  if band eq 'ir' then x1=x1/10000.
  nf(k)=nf(k-1)+(size(x1))(1)
  if k eq 1 then begin
    x=x1
    y=y1
  endif else begin
    x=[x,x1]
    y=[y,y1]
  endelse
  endfor

  if no_mod gt 1 then begin
  miny=min(y) 
  maxy=max(y)
  endif else begin
  miny=1.
  maxy=1.
  endelse

  file=dat.cat(0)+'/OUT.'+dat.lines(i)+dat.ext(0)
  if not file_test(file) then begin
    print,file,' does not exist'
    return
  endif  

; optical path (original values, resol1 and lam not needed)
  if band eq 'opt' then begin
  if xranges(0,i) ne 0. then begin
  !x.range=xranges(*,i)
  if obscomp_opt then begin
  halpha,file,vsini=vsini,resol=resol1,vmacro=vmacro, $
	compmin=miny,compmax=maxy,wo=wobs_opt,po=pobs_opt,/xnoself
  endif else begin
  halpha,file,vsini=vsini,resol=resol1,vmacro=vmacro, $
	compmin=miny,compmax=maxy,/xnoself
  endelse

  endif else begin
  if obscomp_opt then begin
  halpha,file,vsini=vsini,resol=resol1,vmacro=vmacro, $
	compmin=miny,compmax=maxy,wo=wobs_opt,po=pobs_opt
  endif else begin
  halpha,file,vsini=vsini,resol=resol1,vmacro=vmacro, $
	compmin=miny,compmax=maxy
  endelse
  endelse
  endif else begin
;ir path (with resol1 and lam) 
    
  !x.range=xranges(*,i)
  if obscomp_ir then begin
    irlines,file,vsini=vsini,resol=resol1,vmacro=vmacro, $
	compmin=miny,compmax=maxy,wo=lam,po=pobs_ir
  endif else begin
    irlines,file,vsini=vsini,resol=resol1,vmacro=vmacro, $
	compmin=miny,compmax=maxy  
  endelse
  endelse
  
  if color then begin
  for k=1,no_mod-1 do begin
     oplot,x(nf(k-1):nf(k)-1),y(nf(k-1):nf(k)-1),color=col(k)
  endfor
  endif else begin
  for k=1,no_mod-1 do begin
     oplot,x(nf(k-1):nf(k)-1),y(nf(k-1):nf(k)-1),lines=k
  endfor
  endelse

  if band eq 'opt' then begin
  if obscomp_opt then begin
  if not color then begin
    oplot,wobs_opt,pobs_opt,thick=2 
  endif else begin
    oplot,wobs_opt,pobs_opt,color=50
  endelse  
  endif
  endif else begin
  if obscomp_ir then begin
  if not color then begin
    oplot,lam,pobs_ir,thick=2 
  endif else begin
    oplot,lam,pobs_ir,color=50
  endelse  
  endif
  endelse
  
endfor  

return
end
