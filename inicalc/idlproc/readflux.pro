pro readflux,file,x,y
line=' '
dim=0
openr,5,file
repeat begin
readf,5,z1,z2,z3,z4
dim=dim+1
end until eof(5)
close,5
;
openr,5,file
dim=dim*2
x=fltarr(dim)
y=x
for i=0,dim-1,2 do begin
readf,5,x1,y1,x2,y2
x(i)=x1
y(i)=y1
x(i+1)=x2
y(i+1)=y2
endfor
close,5
plot,x,y,xr=[6555,6567]
end