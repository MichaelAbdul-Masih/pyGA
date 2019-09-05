pro wtest,dat
openw,1,'wtest.dat'
nd=size(dat.r)
nf=size(dat.lam)
printf,1,nd(1),nf(1)
printf,1,dat.r
printf,1,dat.taur
printf,1,dat.xne
printf,1,dat.t
printf,1,dat.lam
printf,1,dat.trad
close,1
return
end




