pro bprof,cat,star,ps=ps,outfile,vsini=vrot1,comp=comp
vrot=0.
if keyword_set(vrot1) then begin
vrot=vrot1
endif
if keyword_set(comp) then begin
print,' Give in filename and dimensions of photospheric profiles'
rtab,xw,yp
if keyword_set(vrot) then begin
rconv,xw,yp,y1,vrot
yp=y1
endif
endif 
if keyword_set(ps) then begin
set_plot,'ps'
device,filename=outfile,ysize=25.,xoffset=1.,yoffset=1.5
endif
!p.multi=[0,2,4]
!p.title=star+': H_Gamma'
halpha,cat+'/out.hgamma',vsini=vrot
if keyword_set(comp) then oplot,xw,yp,linestyle=1
!p.title=star+': H_Beta'
halpha,cat+'/out.hbeta',vsini=vrot
if keyword_set(comp) then oplot,xw,yp,linestyle=1
!p.title=star+': H_Alpha'
halpha,cat+'/out.halpha',vsini=vrot
if keyword_set(comp) then oplot,xw,yp,linestyle=1
!p.multi=[4,2,4]
!p.title=star+': HeI 4026'
halpha,cat+'/out.hei4026',vsini=vrot
if keyword_set(comp) then begin
xw=xw-1.
oplot,xw,yp,linestyle=1
xw=xw+1.
endif
!p.title=star+': HeI 4471'
halpha,cat+'/out.hei4471',vsini=vrot
if keyword_set(comp) then oplot,xw,yp,linestyle=1
!p.title=star+': HeI 4378'
halpha,cat+'/out.hei4387',vsini=vrot
if keyword_set(comp) then oplot,xw,yp,linestyle=1
!p.title=star+': HeI 4922'
halpha,cat+'/out.hei4922',vsini=vrot
if keyword_set(comp) then begin
xw=xw-1.5
oplot,xw,yp,linestyle=1
xw=xw+1.5
endif
!p.title=''
!p.multi=0
if keyword_set(ps) then begin
set_plot,'x'
endif
return
end
