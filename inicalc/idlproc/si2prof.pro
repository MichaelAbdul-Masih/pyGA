pro si2prof,cat,star,ps=ps,vsini=vrot1,vrad=vrad,resol=resol1,vmacro=vmacro, $
	     comp=comp,obscomp=obscomp,layout=layout,e1=ext,e2=ext_comp, $
	     color=color


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
  if ext_comp eq '0' then begin
  extension_comp=''
  endif else begin
  extension_comp='_'+strtrim(ext,2)  
  endelse
endelse
endif

if keyword_set(obscomp) then begin                               ;obscomp = name of the obs. spectrum
obsfile=obscomp
;obsfile='$HOME/Model_grids/phot_profiles/'+obscomp
;obsfile='$HOME/Observations/optical/Gal_Bsg_Nevy/'+obscomp+'.sp'
rspec,obsfile,wobs,pobs                                          ;reads the observed spectrum
if keyword_set(vrad) then begin                                  ; corects wl- scale for vrad
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

nspec=8                                                         ; number of the lines to be analysed
if keyword_set(layout) then nspec=layout

if nspec ne 8 and nspec ne 12 then begin                        ; extended number of lines
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
y7=y1
y8=y1
y9=y1
y10=y1
y11=y1
y12=y1

; reads the model profiles of all lines that have to be analysed and simultaneosly convolvs for vrot


if keyword_set(comp) then begin                           ; comp = the name of the used model
cat1=comp
file=cat1+'/OUT.SiII4128'+extension_comp                 
rtabprof,x1,y1,file,161,6,3,5,ew                          ; reads the model profile
if keyword_set(vrot) or keyword_set(resol1) or keyword_set(vmacro) then begin
convol,x1,y1,xp,yp,resol=resol,vsini=vrot,vmacro=vmacro   ; corects the model profile for vrot
x1=xp
y1=yp
endif

file=cat1+'/OUT.SiII4130'+extension_comp                 
rtabprof,x2,y2,file,161,6,3,5,ew                          ; reads the model profile of the next  line
if keyword_set(vrot) or keyword_set(resol1) or keyword_set(vmacro) then begin
convol,x2,y2,xp,yp,resol=resol,vsini=vrot,vmacro=vmacro
x2=xp
y2=yp
endif

file=cat1+'/OUT.SiIII4552'+extension_comp
rtabprof,x3,y3,file,161,6,3,5,ew 
if keyword_set(vrot) or keyword_set(resol1) or keyword_set(vmacro) then begin
convol,x3,y3,xp,yp,resol=resol,vsini=vrot,vmacro=vmacro
x3=xp
y3=yp
endif

file=cat1+'/OUT.SiIII4567'+extension_comp
rtabprof,x4,y4,file,161,6,3,5,ew 
if keyword_set(vrot) or keyword_set(resol1) or keyword_set(vmacro) then begin
convol,x4,y4,xp,yp,resol=resol,vsini=vrot,vmacro=vmacro
x4=xp
y4=yp
endif

file=cat1+'/OUT.SiIII4574'+extension_comp
rtabprof,x5,y5,file,161,6,3,5,ew
if keyword_set(vrot) or keyword_set(resol1) or keyword_set(vmacro) then begin
  convol,x5,y5,xp,yp,resol=resol,vsini=vrot,vmacro=vmacro
  x5=xp
  y5=yp
  endif
  
file=cat1+'/OUT.SiIII4813'+extension_comp
rtabprof,x6,y6,file,161,6,3,5,ew
if keyword_set(vrot) or keyword_set(resol1) or keyword_set(vmacro) then begin
  convol,x6,y6,xp,yp,resol=resol,vsini=vrot,vmacro=vmacro
  x6=xp
  y6=yp
  endif
  
file=cat1+'/OUT.SiIII4819'+extension_comp
rtabprof,x7,y7,file,161,6,3,5,ew
if keyword_set(vrot) or keyword_set(resol1) or keyword_set(vmacro) then begin
  convol,x7,y7,xp,yp,resol=resol,vsini=vrot,vmacro=vmacro
  x7=xp
  y7=yp
  endif
  
file=cat1+'/OUT.SiIII4829'+extension_comp
rtabprof,x8,y8,file,161,6,3,5,ew
if keyword_set(vrot) or keyword_set(resol1) or keyword_set(vmacro) then begin
  convol,x8,y8,xp,yp,resol=resol,vsini=vrot,vmacro=vmacro
  x8=xp
  y8=yp
  endif
  


if nspec eq 12 then begin                                ;adds 4 more lines to be analysed
file=cat1+'/OUT.SiII5041'+extension_comp
rtabprof,x9,y9,file,161,6,3,5,ew 
if keyword_set(vrot) or keyword_set(resol1) or keyword_set(vmacro) then begin
convol,x9,y9,xp,yp,resol=resol,vsini=vrot,vmacro=vmacro
x9=xp
y9=yp
endif


file=cat1+'/OUT.SiII5056'+extension_comp
rtabprof,x10,y10,file,161,6,3,5,ew 
if keyword_set(vrot) or keyword_set(resol1) or keyword_set(vmacro) then begin
convol,x10,y10,xp,yp,resol=resol,vsini=vrot,vmacro=vmacro
x10=xp
y10=yp
endif

file=cat1+'/OUT.SiIII4716'+extension_comp
rtabprof,x11,y11,file,161,6,3,5,ew
if keyword_set(vrot) or keyword_set(resol1) or keyword_set(vmacro) then begin
  convol,x11,y11,xp,yp,resol=resol,vsini=vrot,vmacro=vmacro
  x11=xp
  y11=yp
  endif
  
file=cat1+'/OUT.SiIII5739'+extension_comp
rtabprof,x12,y12,file,161,6,3,5,ew
if keyword_set(vrot) or keyword_set(resol1) or keyword_set(vmacro) then begin
  convol,x12,y12,xp,yp,resol=resol,vsini=vrot,vmacro=vmacro
  x12=xp
  y12=yp
  endif
  endif

endif 

;makes the plots

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

if keyword_set(color) then loadct,12

!p.multi=[0,3,4]                    ;????
if nspec eq 8 then !p.multi=[0,2,4] ;????

!p.title=star+'Si II 4128'                                             ;makes the title of the first plot
!x.range=[4124.,4132.]                                                ; fixs the lambda scale 
if keyword_set(obscomp) then begin
halpha,cat+'/OUT.SiII4128'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	compmin=min(y1), $
	compmax=max(y1),wo=wobs,po=pobs,/xnoself
endif else begin
halpha,cat+'/OUT.SiII4128'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	compmin=min(y1), $
	compmax=max(y1),/xnoself
endelse
if keyword_set(comp) then oplot,x1,y1,linestyle=2,color=200                     ;plots the model profile
if keyword_set(obscomp) then begin
  if not keyword_set(color) then begin
    oplot,wobs,pobs,thick=2 
  endif else begin
    oplot,wobs,pobs,color=50
  endelse  
endif

!p.title=star+'Si II 4130'
!x.range=[4126.,4136.]
if keyword_set(obscomp) then begin
halpha,cat+'/OUT.SiII4130'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	compmin=min(y2), $
	compmax=max(y2),wo=wobs,po=pobs,/xnoself
endif else begin
halpha,cat+'/OUT.SiII4130'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	compmin=min(y2), $
	compmax=max(y2),/xnoself
endelse
if keyword_set(comp) then oplot,x2,y2,linestyle=2,color=200
if keyword_set(obscomp) then begin
  if not keyword_set(color) then begin
    oplot,wobs,pobs,thick=2 
  endif else begin
    oplot,wobs,pobs,color=50
  endelse  
endif

if nspec eq 12 then begin
!p.title=star+'SiII5041'
!x.range=[5039.,5043.]
if keyword_set(obscomp) then begin
halpha,cat+'/OUT.SiII5041'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	compmin=min(y9), $
	compmax=max(y9),wo=wobs,po=pobs,/xnoself
endif else begin
halpha,cat+'/OUT.SiII5041'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	compmin=min(y9), $
	compmax=max(y9),/xnoself
endelse
if keyword_set(comp) then oplot,x9,y9,linestyle=2,color=200
if keyword_set(obscomp) then begin
  if not keyword_set(color) then begin
    oplot,wobs,pobs,thick=2 
  endif else begin
    oplot,wobs,pobs,color=50
  endelse  
endif

!p.title=star+'SiII5056'
!x.range=[5054.,5058.]
if keyword_set(obscomp) then begin
halpha,cat+'/OUT.SiII5056'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	compmin=min(y10), $
	compmax=max(y10),wo=wobs,po=pobs,/xnoself
endif else begin
halpha,cat+'/OUT.SiII5056'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	compmin=min(y10), $
	compmax=max(y10),/xnoself
endelse
if keyword_set(comp) then oplot,x10,y10,linestyle=2,color=200
if keyword_set(obscomp) then begin
  if not keyword_set(color) then begin
    oplot,wobs,pobs,thick=2 
  endif else begin
    oplot,wobs,pobs,color=50
  endelse  
endif

endif


!p.title=star+'Si III 4552'
!x.range=[4549.,4557.]
if keyword_set(obscomp) then begin
halpha,cat+'/OUT.SiIII4552'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	compmin=min(y3), $
	compmax=max(y3),wo=wobs,po=pobs,/xnoself
endif else begin
halpha,cat+'/OUT.SiIII4552'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	compmin=min(y3), $
	compmax=max(y3),/xnoself
endelse
if keyword_set(comp) then oplot,x3,y3,linestyle=2,color=200
if keyword_set(obscomp) then begin
  if not keyword_set(color) then begin
    oplot,wobs,pobs,thick=2 
  endif else begin
    oplot,wobs,pobs,color=50
  endelse  
endif

!p.title=star+'Si III 4567'
!x.range=[4564.,4572.]
if keyword_set(obscomp) then begin
halpha,cat+'/OUT.SiIII4567'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	compmin=min(y4), $
	compmax=max(y4),wo=wobs,po=pobs,/xnoself
endif else begin
halpha,cat+'/OUT.SiIII4567'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	compmin=min(y4), $
	compmax=max(y4),/xnoself
endelse
if keyword_set(comp) then oplot,x4,y4,linestyle=2,color=200
if keyword_set(obscomp) then begin
  if not keyword_set(color) then begin
    oplot,wobs,pobs,thick=2 
  endif else begin
    oplot,wobs,pobs,color=50
  endelse  
endif


!p.title=star+'Si III 4574'
!x.range=[4571.,4579.]
if keyword_set(obscomp) then begin
  halpha,cat+'/OUT.SiIII4574'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	  compmin=min(y5), $
	  compmax=max(y5),wo=wobs,po=pobs,/xnoself
endif else begin
  halpha,cat+'/OUT.SiIII4574'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	  compmin=min(y5), $
	  compmax=max(y5),/xnoself
  endelse
  if keyword_set(comp) then oplot,x5,y5,linestyle=2,color=200
if keyword_set(obscomp) then begin
  if not keyword_set(color) then begin
    oplot,wobs,pobs,thick=2 
  endif else begin
    oplot,wobs,pobs,color=50
  endelse  
endif
  
!p.title=star+'Si III 4813'
!x.range=[4809.,4817.]
if keyword_set(obscomp) then begin
  halpha,cat+'/OUT.SiIII4813'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	  compmin=min(y6), $
	  compmax=max(y6),wo=wobs,po=pobs,/xnoself
endif else begin
  halpha,cat+'/OUT.SiIII4813'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	  compmin=min(y6), $
	  compmax=max(y6),/xnoself
  endelse
  if keyword_set(comp) then oplot,x6,y6,linestyle=2,color=200
if keyword_set(obscomp) then begin
  if not keyword_set(color) then begin
    oplot,wobs,pobs,thick=2 
  endif else begin
    oplot,wobs,pobs,color=50
  endelse  
endif
  
!p.title=star+'Si III 4819'
!x.range=[4816.,4824.]
if keyword_set(obscomp) then begin
  halpha,cat+'/OUT.SiIII4819'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	  compmin=min(y7), $
	  compmax=max(y7),wo=wobs,po=pobs,/xnoself
endif else begin
  halpha,cat+'/OUT.SiIII4819'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	  compmin=min(y7), $
	  compmax=max(y7),/xnoself
  endelse
  if keyword_set(comp) then oplot,x7,y7,linestyle=2,color=200
if keyword_set(obscomp) then begin
  if not keyword_set(color) then begin
    oplot,wobs,pobs,thick=2 
  endif else begin
    oplot,wobs,pobs,color=50
  endelse  
endif
  
!p.title=star+'Si III 4829'
!x.range=[4825.,4833.]
if keyword_set(obscomp) then begin
  halpha,cat+'/OUT.SiIII4829'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	  compmin=min(y8), $
	  compmax=max(y8),wo=wobs,po=pobs,/xnoself
endif else begin
  halpha,cat+'/OUT.SiIII4829'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	  compmin=min(y8), $
	  compmax=max(y8),/xnoself
  endelse
  if keyword_set(comp) then oplot,x8,y8,linestyle=2,color=200
if keyword_set(obscomp) then begin
  if not keyword_set(color) then begin
    oplot,wobs,pobs,thick=2 
  endif else begin
    oplot,wobs,pobs,color=50
  endelse  
endif


if nspec eq 12 then begin
!p.title=star+'SiIII4716'
!x.range=[4714.,4718.]
if keyword_set(obscomp) then begin
  halpha,cat+'/OUT.SiIII4716'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	  compmin=min(y11), $
	  compmax=max(y11),wo=wobs,po=pobs,/xnoself
endif else begin
  halpha,cat+'/OUT.SiIII4716'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	  compmin=min(y11), $
	  compmax=max(y11),/xnoself
  endelse
  if keyword_set(comp) then oplot,x11,y11,linestyle=2,color=200
if keyword_set(obscomp) then begin
  if not keyword_set(color) then begin
    oplot,wobs,pobs,thick=2 
  endif else begin
    oplot,wobs,pobs,color=50
  endelse  
endif

  !p.title=star+'SiIII5739'
  !x.range=[5737.,5741.]
  if keyword_set(obscomp) then begin
    halpha,cat+'/OUT.SiIII5739'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	    compmin=min(y12), $
	    compmax=max(y12),wo=wobs,po=pobs,/xnoself
  endif else begin
    halpha,cat+'/OUT.SiIII5739'+extension,vsini=vrot,resol=resol,vmacro=vmacro, $
	    compmin=min(y12), $
	    compmax=max(y12),/xnoself
    endelse
if keyword_set(comp) then oplot,x12,y12,linestyle=2,color=200
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
