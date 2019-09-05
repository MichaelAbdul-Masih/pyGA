pro calc_q

print,'input Mdot (Msun/yr), vinf (km/s), Rstar Rrsun)'
read,mdot,vinf,rstar

logq=alog10(mdot/(vinf*rstar)^1.5)

qn=[-14.,-13.5,-13.15,-12.8,-12.45,-12.1,-11.75,-11.4]

ql=['A','B','C','D','E','F','G','H']

dq=abs(logq-qn)

b=where(dq eq min(dq))

print,'log Q = ',logq
print,format='("closest grid model = ",f7.2," corresp. to ",a1)',qn(b),ql(b)
return
end
