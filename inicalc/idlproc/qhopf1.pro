pro qhopf1,tau,t
print,' give in teff'
read,teff
q=(t/teff)^4*4./3.-tau
lab1: window,0 
plot_oo,tau,q,xrange=[100.,1.e-4],title='NLTE-HOPF FUNCTION'
print,'measure qinf at tau approx 10'
cursor,a,qinf,3
print,'qinf=',qinf
print,'measure q1 at tau approx 1'
cursor,tau1,q1,3
print,'measure q2 at tau approx .01'
cursor,tau2,q2,3
gamma=alog((q1-qinf)/(q2-qinf))/(tau1-tau2)
print,'gamma =',gamma
q0=(q1-qinf)*exp(-gamma*tau1)+qinf
print,'q0 = ',q0
qtest=qinf+(q0-qinf)*exp(gamma*tau)
oplot,tau,qtest,linestyle=1
window,1
plot_oi,tau,t,xrange=[100.,1.e-6]
t1=(.75*(tau+qtest))^.25*teff
oplot,tau,t1,linestyle=1
print
print,'new try? (yes=1, no=0)'
read,new
if new eq 1 then goto,lab1
print
print,' qinf=',qinf,' q0=',q0,' gamma=',gamma
print
window,0
relerr=abs(1.-t1/t)
plot_oi,tau,relerr,title='relative error'
errmax=max(relerr)
print,'maximum error=',errmax
return
end
