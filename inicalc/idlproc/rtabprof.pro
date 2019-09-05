pro rtabprof,x,y,inpfile,nl,ncol,i1,i2,ew
arr=dblarr(ncol,nl)
x=fltarr(nl)
y=fltarr(nl)
openr,1,inpfile

readf,1,arr
x=reform(arr(i1-1,*))
y=reform(arr(i2-1,*))
readf,1,ew
close,1
return
end
