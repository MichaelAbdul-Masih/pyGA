pro combprof,mods,star,ext=extin, $
    vsini=vsini,vmacro=vmacro,resol_opt=resol_opt, resol_ir=resol_ir, $
    obscomp_opt=obscomp_opt, obscomp_ir=obscomp_ir, $
    vrad_opt=vrad_opt, vrad_ir=vrad_ir, vrl=vrl, vacuum=vacuum,$
    lamcomp=lamcomp,profcomp=profcomp, $
    layout=layout,oldnames=oldnames, $
    color=color,ps=ps,fcl=fcl, $
    comp=comp,e1=e1,e2=e2	      

resol_lband=6700. ;VLT-ISAAC

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
if nspec ne 12 then begin
  print,'Wrong layout!'
  return
endif

; default values for layout=12
lines=['HEI4471','HEI4922','HEI170','HEII4541','HEII4686', $
       'HGAMMA','BR10','BRGAMMA','HALPHA','PFGAMMA','BRALPHA']

titles=['HeI4471','HeI4922','HeI1.70','HeII4541','HeII4686',$
        'H_gamma','Br10','Br_gamma','H_alpha','Pf_gamma','Br_alpha']

band=['opt','opt','ir','opt','opt', $
      'opt','ir','ir','opt','ir','ir']

xranges=fltarr(2,11)
xranges[*,0]=[4460.,4480.]
xranges[*,1]=[4915.,4930.]
xranges[*,2]=[1.695,1.706]
xranges[*,3]=[0,0]
xranges[*,4]=[0,0]
xranges[*,5]=[0,0]
xranges[*,6]=[1.726,1.746]
xranges[*,7]=[2.15,2.18]
xranges[*,8]=[0,0]
xranges[*,9]=[3.7,3.78]
xranges[*,10]=[4.02,4.08]

!p.multi=[0,3,4]

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
;  col=[0,200,100,80,150]
  col=[0,200,40,100,150]
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
  ind=where(lines eq 'BRALPHA')
  if nspec eq 12 then begin
  lines(ind)='BRALPHA1'
  file=cat(0)+'/OUT.BRALPHA1'+ext(0)
  if not file_test(file) then lines(ind)='BRALPHA'
  endif
endif 


; observed/comparison profiles: optical
if keyword_set(obscomp_opt) then begin
  if(obscomp_opt eq 'oplot') then begin
   wobs_opt=lamcomp
   pobs_opt=profcomp
  endif else begin
  obsfile='$HOME/Observations/optical/'+obscomp_opt
;  obsfile='$HOME/Observations/optical/Gal_Bsg_Nevy/'+obscomp+'.sp'
;  obsfile=obscomp
  if not file_test(obsfile) then begin
    print,obsfile,' does not exist'
    return
  endif  
  rspec,obsfile,wobs_opt,pobs_opt
  endelse

  if keyword_set(vacuum) then refrac,wobs_opt

  if keyword_set(vrad_opt) then wobs_opt=wobs_opt*(1.-vrad_opt/2.99792e5)
endif else begin
   wobs_opt=[0.]
   pobs_opt=[0.]
endelse 


; observed/comparison profiles: ir
if keyword_set(obscomp_ir) then begin
  if(obscomp_ir eq 'oplot') then begin
   wobs_ir=lamcomp
   pobs_ir=profcomp
  endif else begin
  obsfile='$HOME/Observations/IR/O-SG/combined/'+obscomp_ir
  if not file_test(obsfile) then begin
    print,obsfile,' does not exist'
    return
  endif  
  rspec,obsfile,wobs_ir,pobs_ir
  wobs_ir=wobs_ir/10000.
  endelse

  if keyword_set(vacuum) then begin
    if not keyword_set(lamcomp) then begin
      print,' keyword vacuum only in connection with "oplot"!'
      return
    endif  
    refrac,wobs_ir
    wobs=wobs_ir/10000.
  endif  

  if keyword_set(vrad_ir) then wobs_ir=wobs_ir*(1.-vrad_ir/2.99792e5)
  wobs_ir1=wobs_ir
  if keyword_set(vrl)  then wobs_ir1=wobs_ir*(1.-vrl/2.99792e5)

endif else begin
   wobs_ir=[0.]
   pobs_ir=[0.]
   wobs_ir1=[0.]
endelse 

dat={star:'',  $
     cat:strarr(no_mod), ext:strarr(no_mod), $
     vsini:0., vmacro:0., resol_opt:0., resol_ir:0., $
     lines: strarr(no_lin), titles: strarr(no_lin), band: strarr(no_lin)}

nobs=(size(wobs_ir1))(1)
lband={resol: 0., wobs_ir1: fltarr(nobs)}
lband.resol=resol_lband
lband.wobs_ir1=wobs_ir1

if n_params() ne 1 then dat.star=star+': '
dat.cat=cat
dat.ext=ext
dat.lines=lines
dat.titles=titles
dat.band=band

if keyword_set(vsini)  then dat.vsini=vsini
if keyword_set(vmacro) then dat.vmacro=vmacro
if keyword_set(resol_opt)  then dat.resol_opt=resol_opt
if keyword_set(resol_ir)  then dat.resol_ir=resol_ir

if keyword_set(ps) then begin
set_plot,'ps'
;print,' Give in Output-filename'
outfile=ps
;read,outfile
device,filename=outfile,ysize=20.,xoffset=1.,yoffset=6.5,/color
endif

; plot all profiles
plot_profiles_combined, dat, wobs_opt, pobs_opt, wobs_ir, pobs_ir, col, xranges=xranges,lband=lband

!x.range=0
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
