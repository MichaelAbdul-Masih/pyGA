pro plhnue,dat
xlam=dat.lam
print,' give in minimum wavelength (A)'
read,wmin
xr=[wmin,xlam(0)]
plot_oi,xlam,dat.hnue,title=dat.name,xtitle='lambda [A]', $
ytitle='log H_nue',xrange=xr
return
end

