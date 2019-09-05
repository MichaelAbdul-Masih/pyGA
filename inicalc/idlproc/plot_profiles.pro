pro plot_profiles, dat, wobs, pobs, $
    col, xranges = xranges, ir=ir, lband=lband

obscomp=1
if wobs(0) eq 0. then obscomp=0

color=1
if col(0) eq -1 then color=0

no_mod=(size(dat.cat))(1)

no_lin=(size(dat.lines))(1)

nf=intarr(no_mod)
nf(0)=0

vsini=dat.vsini
vmacro=dat.vmacro
resol=dat.resol

; loop over all lines
for i=0,no_lin-1 do begin

  li=dat.lines(i)
  resol1=resol
  lam=wobs
; sort out lband lines (for ir)
  if keyword_set(lband) then begin
   if li eq 'BRALPHA' or li eq 'BRALPHA1' or li eq 'PFGAMMA' or li eq 'HEI370' then begin
   if keyword_set(resol) then resol1=lband.resol
   lam=lband.wobs1
  endif
  endif
  
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
  if keyword_set(ir) then x1=x1/10000.
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
    goto,cycle
  endif  

; optical path (original values, resol1 and lam not needed)
  if not keyword_set(ir) then begin
  if keyword_set(xranges) then begin
  !x.range=xranges(*,i)
  if obscomp then begin
  halpha,file,vsini=vsini,resol=resol,vmacro=vmacro, $
	compmin=miny,compmax=maxy,wo=wobs,po=pobs,/xnoself
  endif else begin
  halpha,file,vsini=vsini,resol=resol,vmacro=vmacro, $
	compmin=miny,compmax=maxy;,/xnoself
  endelse

  endif else begin
  if obscomp then begin
  halpha,file,vsini=vsini,resol=resol,vmacro=vmacro, $
	compmin=miny,compmax=maxy,wo=wobs,po=pobs
  endif else begin
  halpha,file,vsini=vsini,resol=resol,vmacro=vmacro, $
	compmin=miny,compmax=maxy
  endelse
  endelse
  endif else begin
;ir path (with resol1 and lam) 
    
  !x.range=xranges(*,i)
  if obscomp then begin
    irlines,file,vsini=vsini,resol=resol1,vmacro=vmacro, $
	compmin=miny,compmax=maxy,wo=lam,po=pobs
  endif else begin
    irlines,file,vsini=vsini,resol=resol1,vmacro=vmacro, $
	compmin=miny,compmax=maxy  
  endelse
  endelse
  
  if color then begin
  for k=1,no_mod-1 do begin
    oplot,x(nf(k-1):nf(k)-1),y(nf(k-1):nf(k)-1),color=col(k)
;    oplot,x(nf(k-1):nf(k)-1),y(nf(k-1):nf(k)-1),color=col(k),psym=1
  endfor
  endif else begin
  for k=1,no_mod-1 do begin
     oplot,x(nf(k-1):nf(k)-1),y(nf(k-1):nf(k)-1),lines=k
  endfor
  endelse

  if obscomp then begin
  if not color then begin
    oplot,lam,pobs,thick=2 
  endif else begin

    if dat.star eq 'ZP: ' and li eq 'HALPHA' then begin
      rspec,'/home/abbott/uh101aw/Observations/optical/HD_66811_ha_1',lamzp,pobszp
      lamzp=congrid(lamzp,200)
      pobszp=congrid(pobszp,200)
      oplot,lamzp,pobszp,color=80
    endif  

    oplot,lam,pobs,color=50
  endelse  
  endif

cycle:
endfor  

return
end
