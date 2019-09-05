pro rtab,x,y,inpfile,nl,ncol,i1,i2
if (n_params() eq 2) then begin
inpfile=' '
print,' ** give in file-name'
read,inpfile
print,' ** give in column length, no. of columns'
read,nl,ncol
print,' ** give in col. position of x and y'
read,i1,i2
endif else begin
if(n_params() ne 7) then begin
print, ' ** too few input parameters given:'
print, ' ** required are either x,y'
print, ' or x,y,input-filename,nl,ncol,i1,i2'
return
endif
endelse

arr=dblarr(ncol,nl)
x=fltarr(nl)
y=fltarr(nl)
openr,1,inpfile

readf,1,arr
x=reform(arr(i1-1,*))
y=reform(arr(i2-1,*))
close,1
return
end
