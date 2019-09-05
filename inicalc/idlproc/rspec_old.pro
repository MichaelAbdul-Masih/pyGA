pro rspec,file,lam,spec

openr,1,file
on_ioerror, path2  

;path for paco-style files (starting with an integer denoting the number
;of following lines
ndata=1
readf,1,ndata

data=fltarr(2,ndata)

readf,1,data
close,1

print,' no of data points = ',ndata

lam=reform(data(0,*))
spec=reform(data(1,*))
return

;path for other files (e.g.,tadziu style)
path2:

ndata=0
a=' '

point_lun,1,0
readf,1,a
result=strpos(a,'ARTEMIO')
if result ne -1 then begin
;artemio style
close,1
rartemio,file,lam,spec
return
endif

point_lun,1,0
openw,2,'tmp.spec'

while not eof(1) do begin

readf,1,a
a=strtrim(a,1)
if a eq '' then goto, cont ;empty line
a1=strmid(a,0,1)
if a1 eq '#' then goto, cont;comment line

ndata=ndata+1
printf,2,a

cont:
endwhile
close,1
print,' no of data points = ',ndata

point_lun,2,0

data=fltarr(2,ndata)

readf,2,data
close,2
spawn,'rm tmp.spec'

lam=reform(data(0,*))
spec=reform(data(1,*))

return
end
