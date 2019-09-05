pro readkunze,file,star,dat
nd=90
ns=11
dat={name: '',  $
     p: fltarr(nd), t: fltarr(nd), xne: fltarr(nd), taur: fltarr(nd), $
     m: fltarr(nd)}
dat.name=star
rtab,x,p,file,nd,ns,1,5
dat.p=p
rtab,x,t,file,nd,ns,1,2
dat.t=t
rtab,x,xne,file,nd,ns,1,6
dat.xne=xne
rtab,x,taur,file,nd,ns,1,4
taur=10.^taur
dat.taur=taur
rtab,x,m,file,nd,ns,1,3
m=10.^m
dat.m=m
return
end




