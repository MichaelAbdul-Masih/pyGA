pro readkeith,file,star,dat
nd=90
ns=7
dat={name: '',  $
     t: fltarr(nd), xne: fltarr(nd), taur: fltarr(nd), $
     m: fltarr(nd), xna: fltarr(nd)}
dat.name=star
str=' '
openr,1,file
arr=dblarr(ns,nd)
readf,1,arr
dat.t=reform(arr(2,*))
dat.xne=reform(arr(4,*))
dat.taur=reform(arr(5,*))
dat.m=reform(arr(1,*))
dat.xna=reform(arr(3,*))
close,1
return
end
