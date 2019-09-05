pro optlines,cat,vsini=vrot,vrad=vrad,resol=resol,comp=comp,obscomp=obscomp

print,'give in vturb-string (return or 015 etc)'
ext=' '
read,ext
if ext  eq '' then begin
  a='0'
endif else begin
  a='VT'+ext
endelse  

vrad1=0.
vrot1=0.
resol1=0.

if keyword_set(vrot) then vrot1=vrot
if keyword_set(vrad) then vrad1=vrad
if keyword_set(resol) then resol1=resol

if keyword_set(comp) and keyword_set(obscomp) then begin
window,2
hprof,cat,vsini=vrot1,vrad=vrad1,resol=resol1,comp=comp,obscomp=obscomp,e1=a
window,0
heiprof,cat,vsini=vrot1,vrad=vrad1,resol=resol1,comp=comp,obscomp=obscomp,e1=a
window,1
heiiprof,cat,vsini=vrot1,vrad=vrad1,resol=resol1,comp=comp,obscomp=obscomp,e1=a
endif

if keyword_set(obscomp) then begin
window,2
hprof,cat,vsini=vrot1,vrad=vrad1,resol=resol1,obscomp=obscomp,e1=a
window,0
heiprof,cat,vsini=vrot1,vrad=vrad1,resol=resol1,obscomp=obscomp,e1=a
window,1
heiiprof,cat,vsini=vrot1,vrad=vrad1,resol=resol1,obscomp=obscomp,e1=a
endif

if keyword_set(comp) then begin
window,2
hprof,cat,vsini=vrot1,vrad=vrad1,resol=resol1,comp=comp,e1=a
window,0
heiprof,cat,vsini=vrot1,vrad=vrad1,resol=resol1,comp=comp,e1=a
window,1
heiiprof,cat,vsini=vrot1,vrad=vrad1,resol=resol1,comp=comp,e1=a
endif

if not keyword_set(comp) and not keyword_set(obscomp) then begin
window,2
hprof,cat,vsini=vrot1,vrad=vrad1,resol=resol1,e1=a
window,0
heiprof,cat,vsini=vrot1,vrad=vrad1,resol=resol1,e1=a
window,1
heiiprof,cat,vsini=vrot1,vrad=vrad1,resol=resol1,e1=a
endif

return
end
