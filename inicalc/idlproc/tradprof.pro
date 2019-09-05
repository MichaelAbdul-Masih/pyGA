pro tradprof,file,rmax=rm
if not keyword_set(rm) then begin
rm=120.
endif
rtab,x,y,file,161,6,3,4
xlambda=x(0)
xic=y(0)*rm^2/!pi
TRAD=1.4388e8/XLAMBDA/(ALOG(3.97e8/XLAMBDA^3/XIC+1.))    
print,' Radiation temp. at ',xlambda,' (A) = ',trad,' (K)'
print,'(Rmax = ',rm,' nominal stellar radii)'
return
end