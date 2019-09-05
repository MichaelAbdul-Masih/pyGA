pro db,dat
;calculates Balmer- and Paschenjump
i=where(dat.lam le 3647.02)
i1=i(0)
i2=i1-1
i=where(dat.lam le 8205.83)
i3=i(0)
i4=i3-1
;
db=2.5*(dat.fnue(i2)-dat.fnue(i1))
dp=2.5*(dat.fnue(i4)-dat.fnue(i3))
print,' Balmer  jump = ',db,' at',dat.lam(i1),dat.lam(i2)  
print,' Paschen jump = ',dp,' at',dat.lam(i3),dat.lam(i4)    
return
end