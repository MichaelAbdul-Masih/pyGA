pro heiiprof,mods,star,ext=extin, $
    vsini=vsini,vmacro=vmacro,resol=resol, $
    obscomp=obscomp, vrad=vrad, vacuum=vacuum,$
    lamcomp=lamcomp,profcomp=profcomp, $
    layout=layout,color=color,ps=ps, $
    comp=comp,e1=e1,e2=e2	      

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
nspec=6
if keyword_set(layout) then nspec=layout
if nspec ne 4 and nspec ne 6 then begin
  print,'Wrong layout!'
  return
endif

if nspec eq 6 then begin
lines=['HEII4200','HEII4541','HEII4686','HEII6406','HEII6527','HEII6683']
titles=['HeII4200','HeII4541','HeII4686','HeII6406','HeII6527','HeII6683']

!p.multi=[0,2,3]
if !p.charsize eq 1. then !p.charsize=1.5

endif else begin
lines=['HEII4200','HEII4541','HEII4686']
titles=['HeII4200','HeII4541','HeII4686']

!p.multi=[0,2,2]
endelse


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

; observed/comparison profiles
if keyword_set(obscomp) then begin
  if(obscomp eq 'oplot') then begin
   wobs=lamcomp
   pobs=profcomp
  endif else begin
   obsfile=obscomp
   rspec,obsfile,wobs,pobs
  endelse

  if keyword_set(vacuum) then refrac,wobs

  if keyword_set(vrad) then wobs=wobs*(1.-vrad/2.99792e5)
endif else begin
   wobs=[0.]
   pobs=[0.]
endelse 

dat={star:'',  $
     cat:strarr(no_mod), ext:strarr(no_mod), $
     vsini:0., vmacro:0., resol:0., $
     lines: strarr(no_lin), titles: strarr(no_lin)}

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
device,filename=outfile,ysize=20.,xoffset=1.,yoffset=6.5,/color
endif

; plot all profiles
plot_profiles, dat, wobs, pobs, col

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
