pro irprof,mods,star,ext=extin, $
    vsini=vsini,vmacro=vmacro,resol=resol, $
    obscomp=obscomp, vrad=vrad, vrl=vrl, vacuum=vacuum,$
    lamcomp=lamcomp,profcomp=profcomp, $
    layout=layout,oldnames=oldnames, $
    color=color,ps=ps, $
    comp=comp,e1=e1,e2=e2, $
    lb_resol=lb_resol	      

if keyword_set(lb_resol) then begin
resol_lband=lb_resol
endif else begin
;resol_lband=6700. ;VLT-ISAAC
resol_lband=2500. ; SPEX
endelse

models=mods    
; last three keywords for compatibility with older versions
if keyword_set(comp) then begin
  dim1=(size(models,/dim))(0)
  dim2=(size(comp,/dim))(0)
  if dim1 ne 0 or dim2 ne 0 then begin
    print,'Inconsistent input for models and/or comp!!!'
    return
  endif  
  models=[models,comp]    
endif

if keyword_set(e1) then begin
  dim1=(size(e1,/dim))(0)
  if dim1 ne 0 then begin
    print,'Inconsistent input for e1!!!'
    return
  endif  
  extin=e1    
endif

if keyword_set(e2) then begin
  dim2=(size(e2,/dim))(0)
  if dim2 ne 0 or n_elements(extin) eq '' then begin
    print,'Inconsistent input for e2!!!'
    return
  endif  
  extin=[extin,e2]    
endif

; lines to be plotted  
nspec=12
if keyword_set(layout) then nspec=layout
if nspec ne 12 and nspec ne 8 and nspec ne 1 then begin
  print,'Wrong layout!'
  return
endif

; default values for layout=12
;lines=['BR12','BR11','BR10','BRGAMMA','PFGAMMA','BRALPHA', $
;       'HEI170','HEI205','HEI211','HEI370','HEII712','HEII218']

;titles=['Br12','Br11','Br10','Br_gamma','Pf_gamma','Br_alpha', $
;	'HeI1.70','HeI2.05','HeI2.11','HeI3.70','HeII1.69','HeII2.18']

; include HeI3.70 in FORMAL.IR
lines=['BR12','BR11','BR10','BRGAMMA','PFGAMMA','BRALPHA', $
       'HEI170','HEI205','HEI211','HEI6678','HEII712','HEII218']

titles=['Br12','Br11','Br10','Br_gamma','Pf_gamma','Br_alpha', $
	'HeI1.70','HeI2.05','HeI2.11','HeI6678','HeII1.69','HeII2.18']

xranges=fltarr(2,12)
xranges[*,0]=[1.63,1.65]
xranges[*,1]=[1.67,1.69]
xranges[*,2]=[1.726,1.746]
xranges[*,3]=[2.15,2.18]
xranges[*,4]=[3.7,3.78]
xranges[*,5]=[4.02,4.08]
xranges[*,6]=[1.695,1.706]
xranges[*,7]=[2.05,2.065]
xranges[*,8]=[2.107,2.118]
xranges[*,9]=[3.69,3.71]
xranges[*,10]=[1.688,1.697]
xranges[*,11]=[2.183,2.195]

!p.multi=[0,3,4]


if nspec eq 8 then begin
; define subset
lines1=['BR11','BR10','BRGAMMA', $
       'HEI170','HEI205','HEI211','HEII712','HEII218']
nl=8
xranges1=fltarr(2,nl)
titles1=strarr(nl)
for k=0,nl-1 do begin
  b=where(lines1(k) eq lines)
  titles1(k)=titles(b)
  xranges1(*,k)=xranges(*,b)
endfor
lines=lines1
xranges=xranges1
titles=titles1
!p.multi=[0,2,4]
endif

if nspec eq 1 then begin
; define subset
lines1=['BRGAMMA']
nl=1
xranges1=fltarr(2,nl)
titles1=strarr(nl)
for k=0,nl-1 do begin
  b=where(lines1(k) eq lines)
  titles1(k)=titles(b)
  xranges1(*,k)=xranges(*,b)
endfor
lines=lines1
xranges=xranges1
titles=titles1
!p.multi=0
endif

if keyword_set(oldnames) then begin
  ind=where(lines eq 'BR10')
  lines(ind)='BRZETA'
endif

if !p.charsize eq 1. then !p.charsize=1.5

no_lin=(size(lines))(1)
no_tit=(size(lines))(1)
if no_tit ne no_lin then begin
  print,'no_lin ne no_tit!' 
  return
endif  

; make and check arrays for model(s) and extension(s)
cat=models  
no_mod=(size(cat,/dimensions))(0)
if no_mod eq 0 then cat=[cat]

if keyword_set(extin) then begin
  ext=extin
  no_ext=(size(ext,/dimensions))(0)
  if no_ext ne no_mod then begin
    print,'Dimension of models and extensions not consistent!'
    return
  endif
  if no_ext eq 0 then ext=[ext]
endif

if no_mod eq 0 then no_mod=1

; color definition
if keyword_set(color) then begin
  loadct,12
  col=[0,200,100,80,150]
  if no_mod gt 4 then begin
    print,'More colors needed!'
    return
  endif  
endif else begin
  col=[-1] 
endelse


; modify extensions so that appropriate
if not keyword_set(extin) then begin
  ext=strarr(no_mod)
  for i=0,no_mod-1 do begin
    print,'model ',cat(i)
    print,'Which extensions shall be processed?'
    print,'standard [RET]  --  _ESC [e]  --  _VTxxx [xxx]  --  _ESC_VTxxx [exxx]'
    extension=''
    read,extension
    extension=strtrim(extension,2)
    dummy=strlen(extension)
    if (dummy eq 1) then extension='_ESC'
    if (dummy eq 3) then extension='_VT'+extension
    if (dummy eq 4) then extension='_ESC_VT'+strmid(extension,1,3)
    ext(i)=extension
  endfor
  endif else begin
  for i=0,no_mod-1 do begin
  if ext(i) eq '0' then begin
  extension=''
  endif else begin
  extension='_'+strtrim(ext(i),2)  
  endelse
  ext(i)=extension 
  endfor
endelse

print,cat(0)
rindat,cat(0),teff
if teff gt 35000. then begin
  ind=where(lines eq 'BRGAMMA')
  lines(ind)='BRGAMMA1'
  file=cat(0)+'/OUT.BRGAMMA1'+ext(0)
  if not file_test(file) then lines(ind)='BRGAMMA'
  ind=where(lines eq 'BRALPHA')
  if nspec eq 12 then begin
  lines(ind)='BRALPHA1'
  file=cat(0)+'/OUT.BRALPHA1'+ext(0)
  if not file_test(file) then lines(ind)='BRALPHA'
  endif
endif 

; observed/comparison profiles
if keyword_set(obscomp) then begin
  if(obscomp eq 'oplot') then begin
   wobs=lamcomp
   pobs=profcomp
  endif else begin

;  obsfile='$HOME/Observations/IR/HiRes/'+obscomp
;  obsfile='$HOME/Observations/IR/B-SG/'+obscomp
;  obsfile='$HOME/Observations/IR/GalCenOB/'+obscomp+'.dat'
  obsfile='$HOME/Observations/IR/O-SG/combined/'+obscomp
  
  if not file_test(obsfile) then begin
    print,obsfile,' does not exist'
    return
  endif  
  rspec,obsfile,wobs,pobs
  wobs=wobs/10000.
  endelse

  if keyword_set(vacuum) then begin
    if not keyword_set(lamcomp) then begin
      print,' keyword vacuum only in connection with "oplot"!'
      return
    endif  
    refrac,wobs
    wobs=wobs/10000.
  endif  

  if keyword_set(vrad) then wobs=wobs*(1.-vrad/2.99792e5)
  wobs1=wobs
  if keyword_set(vrl)  then wobs1=wobs*(1.-vrl/2.99792e5)

endif else begin
   wobs=[0.]
   pobs=[0.]
   wobs1=[0.]
endelse 

dat={star:'',  $
     cat:strarr(no_mod), ext:strarr(no_mod), $
     vsini:0., vmacro:0., resol:0., $
     lines: strarr(no_lin), titles: strarr(no_lin)}

if nspec eq 12 then begin
  nobs=(size(wobs1))(1)
  lband={resol: 0., wobs1: fltarr(nobs)}
  lband.resol=resol_lband
  lband.wobs1=wobs1
endif else begin
  lband=0
endelse  

if n_params() ne 1 then dat.star=star+': '
dat.cat=cat
dat.ext=ext
dat.lines=lines
dat.titles=titles

if keyword_set(vsini)  then dat.vsini=vsini
if keyword_set(vmacro) then dat.vmacro=vmacro
if keyword_set(resol)  then dat.resol=resol


if keyword_set(ps) then begin
set_plot,'ps'
;print,' Give in Output-filename'
outfile=ps
;read,outfile
if (nspec ne 1) then begin 
device,filename=outfile,ysize=20.,xoffset=1.,yoffset=6.5,/color
endif else begin
device,filename=outfile,/color,/landscape
endelse  
endif

; plot all profiles
plot_profiles, dat, wobs, pobs, col, xranges=xranges,/ir,lband=lband

if keyword_set(fcl) then begin
  readmod,dat.cat(0)+'/OUT_TOT',' ',a
  maxclf=max(a.fcl)
  for i=1,no_mod-1 do begin
    readmod,dat.cat(i)+'/OUT_TOT',' ',b
    maxclf=max([maxclf,b.fcl])
  endfor
  !y.range=[0,maxclf+1]  
  plfcl,a,/plv
  for i=1,no_mod-1 do begin
    readmod,dat.cat(i)+'/OUT_TOT',' ',a
    if keyword_set(color) then begin
      oplot,a.v,a.fcl,color=col(i)
    endif else begin
      oplot,a.v,a.fcl,line=i
    endelse
  endfor
endif  

if keyword_set(color) then loadct,0
!p.title=''
!p.charsize=1.
!p.multi=0
!x.range=0.
!y.range=0.


if keyword_set(ps) then begin
device,/close
set_plot,'x'
endif

return
end
