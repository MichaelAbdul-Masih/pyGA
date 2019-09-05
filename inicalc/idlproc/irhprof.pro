pro irhprof,cat,star,ps=ps,vsini=vrot1,vrad=vrad,resol=resol1, $
	    comp=comp,obscomp=obscomp

print,'Which files shall be processed?'
print,'standard [RET]  --  _ESC [e]  --  _VTxxx [xxx]  --  _ESC_VTxxx [exxx]'
extension=' '
read,extension
extension=strtrim(extension,2)
dummy=strlen(extension)
if (dummy eq 1) then extension='_ESC'
if (dummy eq 3) then extension='_VT'+extension
if (dummy eq 4) then extension='_ESC_VT'+strmid(extension,1,3)

if keyword_set(comp) then begin
print,'Which COMPARISON files shall be processed?'
print,'standard [RET]  --  _ESC [e]  --  _VTxxx [xxx]  --  _ESC_VTxxx [exxx]'
extension_comp=' '
read,extension_comp
extension_comp=strtrim(extension_comp,2)
dummy=strlen(extension_comp)
if (dummy eq 1) then extension_comp='_ESC'
if (dummy eq 3) then extension_comp='_VT'+extension_comp
if (dummy eq 4) then extension_comp='_ESC_VT'+strmid(extension_comp,1,3)
endif

rindat,cat

if keyword_set(obscomp) then begin
  obsfile='$HOME/Observations/IR/HiRes/'+obscomp
  rspec,obsfile,wobs,pobs
  wobs=wobs/10000.
  if keyword_set(vrad) then begin
  wobs=wobs*(1.-vrad/2.99792e5)
  endif  
endif                                        

if n_params() eq 1 then  begin
  star=''
endif else begin
  star=star+': '
endelse

loadct,12

vrot=0.
y1=[1.]
y2=y1
y3=y1
y4=y1

if keyword_set(vrot1) then begin
vrot=vrot1
endif

resol=0.
if keyword_set(resol1) then begin
resol=resol1
endif

if keyword_set(comp) then begin
cat1=comp
print,' Parameters of comp. model'
rindat,cat1

file=cat1+'/OUT.BR12'+extension_comp
rtabprof,x1,y1,file,161,6,3,5,ew 
if keyword_set(vrot) or keyword_set(resol1) then begin
convol,x1,y1,xp,yp,resol=resol,vsini=vrot
x1=xp/1.e4
y1=yp
endif else begin
x1=x1/1.e4  
endelse

file=cat1+'/OUT.BR11'+extension_comp
rtabprof,x2,y2,file,161,6,3,5,ew 
if keyword_set(vrot) or keyword_set(resol1) then begin
convol,x2,y2,xp,yp,resol=resol,vsini=vrot
x2=xp/1.e4
y2=yp
endif else begin
x2=x2/1.e4  
endelse

file=cat1+'/OUT.BRZETA'+extension_comp
rtabprof,x3,y3,file,161,6,3,5,ew 
if keyword_set(vrot) or keyword_set(resol1) then begin
convol,x3,y3,xp,yp,resol=resol,vsini=vrot
x3=xp/1.e4
y3=yp
endif else begin
x3=x3/1.e4  
endelse

file=cat1+'/OUT.BRGAMMA'+extension_comp
rtabprof,x4,y4,file,161,6,3,5,ew 
if keyword_set(vrot) or keyword_set(resol1) then begin
convol,x4,y4,xp,yp,resol=resol,vsini=vrot
x4=xp/1.e4
y4=yp
endif else begin
x4=x4/1.e4  
endelse

endif 
outfile=' '
if keyword_set(ps) then begin
set_plot,'ps'
print,' Give in Output-filename'
read,outfile
device,filename=outfile,ysize=20.,xoffset=1.,yoffset=6.5,/color
endif

!p.multi=[0,2,2]

!p.title=star+'H4-12'
!x.range=[1.616,1.665]
if keyword_set(obscomp) then begin
irlines,cat+'/OUT.BR12'+extension,vsini=vrot,resol=resol,compmin=min(y1), $
;	 compmax=max(y1),wo=wobs,po=pobs
	 compmax=max(y1),/color
endif else begin
irlines,cat+'/OUT.BR12'+extension,vsini=vrot,resol=resol,compmin=min(y1), $
	 compmax=max(y1),/color
endelse
if keyword_set(comp) then oplot,x1,y1,linestyle=2
if keyword_set(obscomp) then oplot,wobs,pobs

!p.title=star+'H4-11'
!x.range=[1.656,1.706]
if keyword_set(obscomp) then begin
irlines,cat+'/OUT.BR11'+extension,vsini=vrot,resol=resol,compmin=min(y2), $
;	 compmax=max(y2),wo=wobs,po=pobs
	 compmax=max(y2),/color
endif else begin
irlines,cat+'/OUT.BR11'+extension,vsini=vrot,resol=resol,compmin=min(y2), $
	 compmax=max(y2),/color
endelse
if keyword_set(comp) then oplot,x2,y2,linestyle=2
if keyword_set(obscomp) then oplot,wobs,pobs


!p.title=star+'H4-10'
!x.range=[1.71,1.76]
if keyword_set(obscomp) then begin
irlines,cat+'/OUT.BRZETA'+extension,vsini=vrot,resol=resol,compmin=min(y3), $
;	 compmax=max(y3),wo=wobs,po=pobs
	 compmax=max(y3),/color
endif else begin
irlines,cat+'/OUT.BRZETA'+extension,vsini=vrot,resol=resol,compmin=min(y3), $
	 compmax=max(y3),/color
endelse
if keyword_set(comp) then oplot,x3,y3,linestyle=2
if keyword_set(obscomp) then oplot,wobs,pobs

!p.title=star+'H BR-GAM'
!x.range=[2.14,2.19]
if keyword_set(obscomp) then begin
irlines,cat+'/OUT.BRGAMMA'+extension,vsini=vrot,resol=resol,compmin=min(y4), $
;	 compmax=max(y4),wo=wobs,po=pobs
	 compmax=max(y4),/color
endif else begin
irlines,cat+'/OUT.BRGAMMA'+extension,vsini=vrot,resol=resol,compmin=min(y4), $
	 compmax=max(y4),/color
endelse
if keyword_set(comp) then oplot,x4,y4,linestyle=2
if keyword_set(obscomp) then oplot,wobs,pobs

!p.title=''
!p.multi=0
!x.range=0.

if keyword_set(ps) then begin
device,/close
set_plot,'x'
endif

return
end
