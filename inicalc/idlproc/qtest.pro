pro qtest,tau,t,qinf,q0,gamma
print,' give in teff'
read,teff
qtest=qinf+(q0-qinf)*exp(-gamma*tau)
t1=(.75*(tau+qtest))^.25*teff
plot_oi,tau,t1,/ynozero,xrange=[100,1.e-4]
lab1:print
print,'new try? '
read,new
if new ne 1 then return
print,'give in qinf,q0,gamma'
read,qinf,q0,gamma
qtest=qinf+(q0-qinf)*exp(-gamma*tau)
t1=(.75*(tau+qtest))^.25*teff
oplot,tau,t1
goto,lab1
return
end
