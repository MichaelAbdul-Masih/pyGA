pro rspec_dprec,file,lam,spec

openr,1,file
on_ioerror, path2  

;path for paco-style files (starting with an integer denoting the number
;of following lines) or keith-style files (just wlam,profile) 

ndata=1L
readf,1,ndata
; check whether paco (one integer) or keith style (2 reals)

point_lun,1,0
a=' '
readf,1,a
stri=str_sep(a,' ')
b=where(stri ne '')
str=stri(b)

if (size(str))(1) eq 1 then begin
; paco style

data=dblarr(2,ndata)

readf,1,data
close,1

print,' no of data points = ',ndata

lam=reform(data(0,*))
spec=reform(data(1,*))
return

endif else begin


 if (size(str))(1) eq 2 then begin
; keith style

 ndata=1L
 while not eof(1) do begin
  readf,1,a
  ndata=ndata+1L
 endwhile

 data=dblarr(2,ndata)
 point_lun,1,0

 readf,1,data
 close,1

 print,' no of data points = ',ndata

 lam=reform(data(0,*))
 spec=reform(data(1,*))
 return

 endif else begin
  print,' something wrong in philosophy'
  close,1
  return
 endelse

endelse

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

while not eof(1) do begin

readf,1,a
a=strtrim(a,1)
if a eq '' then goto, cont ;empty line
a1=strmid(a,0,1)
if a1 eq '#' then goto, cont;comment line

ndata=ndata+1

cont:
endwhile

print,' no of data points = ',ndata

lam=dblarr(ndata)
spec=lam

i=0
point_lun,1,0
while not eof(1) do begin

readf,1,a
ax=a
a=strtrim(a,1)
if a eq '' then goto, cont1 ;empty line
a1=strmid(a,0,1)
if a1 eq '#' then goto, cont1;comment line

stri=str_sep(ax,' ')
b=where(stri ne '')
str=stri(b)
  if (size(str))(1) ne 2 then begin
   print,' wrong number of entries'
   close,1
   stop
   return
  endif
lam(i)=float(str(0))
spec(i)=float(str(1))
  
i=i+1

cont1:
endwhile

close,1

return
end
