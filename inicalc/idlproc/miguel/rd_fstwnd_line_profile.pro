pro rd_fstwnd_line_profile,file,dir=dir,dat 

; procedure to read a FASTWIND line profile
; it returns, in a structure, the emergent profile
; and the EW

if not keyword_set(dir) then dir = './'

i=0
sc=' '

file = dir + file

get_lun,u1
openr,u1,file
while not eof(u1) do begin
    readf,u1,sc
    aa=strtrim(sc,2)
    ii=strcmp(strmid(aa,0,5),'INVER')
    if (ii eq 0) then i=i+1
endwhile
close,u1

np = i-1    
dat={x: dblarr(np), y: dblarr(np), ew: 0.d0}

x1 = 0.d0
x2 = x1
x3 = x1
x4 = x1
x5 = x1
x6 = x1
i = 1

openr,u1,file
for i=0,np-1 do begin
	readf,u1,x1,x2,x3,x4,x5,x6
	dat.x(i)  = x3              ; lambda, AE
	dat.y(i)  = x5              ; normalized flux
endfor
readf,u1,x1
dat.ew = x1                         ; EW in AE (>0 means emission)
close,u1
free_lun,u1 

return
end
