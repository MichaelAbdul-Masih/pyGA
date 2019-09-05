pro heiiprof,cat,star,ps=ps,vsini=vrot1,vrad=vrad,resol=resol1, $
	      comp=comp,obscomp=obscomp,layout=layout,e1=ext,e2=ext_comp, $
  	   color=color,lamcomp=lamcomp,profcomp=profcomp,vacuum=vacuum

if not keyword_set(ext) then begin  
print,'Which files shall be processed?'
print,'standard [RET]  --  _ESC [e]  --  _VTxxx [xxx]  --  _ESC_VTxxx [exxx]'
extension=' '
read,extension
extension=strtrim(extension,2)
dummy=strlen(extension)
if (dummy eq 1) then extension='_ESC'
if (dummy eq 3) then extension='_VT'+extension
if (dummy eq 4) then extension='_ESC_VT'+strmid(extension,1,3)
endif else begin
  if ext eq '0' then begin
  extension=''
  endif else begin
  extension='_'+strtrim(ext,2)  
  endelse
endelse

if keyword_set(comp) then begin
if not keyword_set(ext_comp) then begin  
print,'Which COMPARISON files shall be processed?'
print,'standard [RET]  --  _ESC [e]  --  _VTxxx [xxx]  --  _ESC_VTxxx [exxx]'
extension_comp=' '
read,extension_comp
extension_comp=strtrim(extension_comp,2)
dummy=strlen(extension_comp)
if (dummy eq 1) then extension_comp='_ESC'
if (dummy eq 3) then extension_comp='_VT'+extension_comp
if (dummy eq 4) then extension_comp='_ESC_VT'+strmid(extension_comp,1,3)
endif else begin
  if ext eq '0' then begin
  extension=''
  endif else begin
  extension='_'+strtrim(ext,2)  
  endelse
endelse
endif

if keyword_set(obscomp) then begin
  if(obscomp eq 'oplot') then begin
   wobs=lamcomp
   pobs=profcomp
  endif else begin
  obsfile='$HOME/Observations/optical/'+obscomp
  rspec,obsfile,wobs,pobs
  endelse
  if keyword_set(vacuum) then begin
  refrac,wobs
  endif  
  if keyword_set(vrad) then begin
  wobs=wobs*(1.-vrad/2.99792e5)
  endif  
endif  

if n_params() eq 1 then  begin
  star=''
endif else begin
  star=star+': '
endelse

vrot=0.

if keyword_set(vrot1) then begin
vrot=vrot1
endif

resol=0.
if keyword_set(resol1) then begin
resol=resol1
endif

nspec=6
if keyword_set(layout) then nspec=layout

if nspec ne 4 and nspec ne 6 then begin
  print,' wrong layout!'
  return
  stop
endif
  
y1=[1.]
y2=y1
y3=y1
y4=y1
y5=y1
y6=y1

if keyword_set(comp) then begin
cat1=comp
file=cat1+'/OUT.HEII4200'+extension_comp
rtabprof,x1,y1,file,161,6,3,5,ew 
if keyword_set(vrot) or keyword_set(resol1) then begin
convol,x1,y1,xp,yp,resol=resol,vsini=vrot
x1=xp
y1=yp
endif

file=cat1+'/OUT.HEII4541'+extension_comp
rtabprof,x2,y2,file,161,6,3,5,ew 
if keyword_set(vrot) or keyword_set(resol1) then begin
convol,x2,y2,xp,yp,resol=resol,vsini=vrot
x2=xp
y2=yp
endif

file=cat1+'/OUT.HEII4686'+extension_comp
rtabprof,x3,y3,file,161,6,3,5,ew 
if keyword_set(vrot) or keyword_set(resol1) then begin
convol,x3,y3,xp,yp,resol=resol,vsini=vrot
x3=xp*(1.-9./2.99792e5)  ;error in restwavelength (4685.84 vs. 4685.7)
y3=yp
endif

if nspec eq 6 then begin

file=cat1+'/OUT.HEII6406'+extension_comp
rtabprof,x4,y4,file,161,6,3,5,ew 
if keyword_set(vrot) or keyword_set(resol1) then begin
convol,x4,y4,xp,yp,resol=resol,vsini=vrot
x4=xp
y4=yp
endif

file=cat1+'/OUT.HEII6527'+extension_comp
rtabprof,x5,y5,file,161,6,3,5,ew 
if keyword_set(vrot) or keyword_set(resol1) then begin
convol,x5,y5,xp,yp,resol=resol,vsini=vrot
x5=xp
y5=yp
endif

file=cat1+'/OUT.HEII6683'+extension_comp
rtabprof,x6,y6,file,161,6,3,5,ew 
if keyword_set(vrot) or keyword_set(resol1) then begin
convol,x6,y6,xp,yp,resol=resol,vsini=vrot
x6=xp
y6=yp
endif

endif

endif 

if keyword_set(color) then loadct,12

if keyword_set(ps) then begin
set_plot,'ps'
;print,' Give in Output-filename'
outfile=ps
;read,outfile
if keyword_set(color) then begin
device,filename=outfile,ysize=20.,xoffset=1.,yoffset=6.5,/color
endif else begin
device,filename=outfile,ysize=20.,xoffset=1.,yoffset=6.5
endelse
endif


!p.multi=[0,2,3]
if nspec eq 4 then !p.multi=[0,2,2]

!p.title=star+'HeII4200'
if keyword_set(obscomp) then begin
halpha,cat+'/OUT.HEII4200'+extension,vsini=vrot,resol=resol,compmin=min(y1), $
	 compmax=max(y1),wo=wobs,po=pobs
endif else begin
halpha,cat+'/OUT.HEII4200'+extension,vsini=vrot,resol=resol,compmin=min(y1), $
	 compmax=max(y1)
endelse

if keyword_set(comp) then oplot,x1,y1,linestyle=2

if keyword_set(obscomp) then begin
  if not keyword_set(color) then begin
    oplot,wobs,pobs,thick=2 
  endif else begin
    oplot,wobs,pobs,color=50
  endelse  
endif

!p.title=star+'HeII4541'
if keyword_set(obscomp) then begin
halpha,cat+'/OUT.HEII4541'+extension,vsini=vrot,resol=resol,compmin=min(y2), $
	 compmax=max(y2),wo=wobs,po=pobs
endif else begin     
halpha,cat+'/OUT.HEII4541'+extension,vsini=vrot,resol=resol,compmin=min(y2), $
	 compmax=max(y2)
endelse
if keyword_set(comp) then oplot,x2,y2,linestyle=2

if keyword_set(obscomp) then begin
  if not keyword_set(color) then begin
    oplot,wobs,pobs,thick=2 
  endif else begin
    oplot,wobs,pobs,color=50
  endelse  
endif

!p.title=star+'HeII4686'
if keyword_set(obscomp) then begin
halpha,cat+'/OUT.HEII4686'+extension,vsini=vrot,resol=resol,compmin=min(y3), $
	 compmax=max(y3),wo=wobs,po=pobs
endif else begin     
halpha,cat+'/OUT.HEII4686'+extension,vsini=vrot,resol=resol,compmin=min(y3), $
	 compmax=max(y3)
endelse
if keyword_set(comp) then oplot,x3,y3,linestyle=2

if keyword_set(obscomp) then begin
  if not keyword_set(color) then begin
    oplot,wobs,pobs,thick=2 
  endif else begin
    oplot,wobs,pobs,color=50
  endelse  
endif

if nspec eq 6 then begin

!p.title=star+'HeII6406'
if keyword_set(obscomp) then begin
halpha,cat+'/OUT.HEII6406'+extension,vsini=vrot,resol=resol,compmin=min(y4), $
	 compmax=max(y4),wo=wobs,po=pobs
endif else begin     
halpha,cat+'/OUT.HEII6406'+extension,vsini=vrot,resol=resol,compmin=min(y4), $
	 compmax=max(y4)
endelse
if keyword_set(comp) then oplot,x4,y4,linestyle=2

if keyword_set(obscomp) then begin
  if not keyword_set(color) then begin
    oplot,wobs,pobs,thick=2 
  endif else begin
    oplot,wobs,pobs,color=50
  endelse  
endif

!p.title=star+'HeII6527'
if keyword_set(obscomp) then begin
halpha,cat+'/OUT.HEII6527'+extension,vsini=vrot,resol=resol,compmin=min(y5), $
	 compmax=max(y5),wo=wobs,po=pobs
endif else begin     
halpha,cat+'/OUT.HEII6527'+extension,vsini=vrot,resol=resol,compmin=min(y5), $
	 compmax=max(y5)
endelse
if keyword_set(comp) then oplot,x5,y5,linestyle=2

if keyword_set(obscomp) then begin
  if not keyword_set(color) then begin
    oplot,wobs,pobs,thick=2 
  endif else begin
    oplot,wobs,pobs,color=50
  endelse  
endif

!p.title=star+'HeII6683'
if keyword_set(obscomp) then begin
halpha,cat+'/OUT.HEII6683'+extension,vsini=vrot,resol=resol,compmin=min(y6), $
	 compmax=max(y6),wo=wobs,po=pobs
endif else begin     
halpha,cat+'/OUT.HEII6683'+extension,vsini=vrot,resol=resol,compmin=min(y6), $
	 compmax=max(y6)
endelse
if keyword_set(comp) then oplot,x6,y6,linestyle=2

if keyword_set(obscomp) then begin
  if not keyword_set(color) then begin
    oplot,wobs,pobs,thick=2 
  endif else begin
    oplot,wobs,pobs,color=50
  endelse  
endif
endif

if keyword_set(color) then loadct,0
!p.title=''
!p.multi=0
!x.range=0.

if keyword_set(ps) then begin
device,/close
set_plot,'x'
endif

return
end
