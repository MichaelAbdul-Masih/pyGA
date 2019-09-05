pro leep,file,dat,np=np

; procedure to read an FASTWIND line profile
; it returns, in a structure, the emergent profile
; and the EW

if (not keyword_set(np)) then np=161
openr,1,file

dat={wave: fltarr(np), flux_nor: fltarr(np), ew: fltarr(1)}

x1=fltarr(1)
x2=fltarr(1)
x3=x1
x4=x1
x5=x1
x6=x1
i=indgen(1)


for i=0,np-1 do begin
	readf,1,x1,x2,x3,x4,x5,x6
	dat.wave(i)=x3
	dat.flux_nor(i)=x5
endfor
readf,1,x1
dat.ew=x1
close,1
return
end
