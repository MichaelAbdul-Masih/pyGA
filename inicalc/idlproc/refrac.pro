pro refrac, lam

;input  lam(vacuum) [Angst]
;output lam(air)    [Angst]

sig2=(10000./lam)^2
n=643.28+294981./(146.-sig2)+2554./(41.-sig2)
n=1.d0+n*1.d-7
sig2=double(lam)
lam=sig2/n
return
end
