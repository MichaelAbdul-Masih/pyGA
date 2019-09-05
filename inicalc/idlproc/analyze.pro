pro analyze,name,model,pr=pr

readmod,name+'/OUT_TOT',' ',model  
plt,model,/taur

a=' '  
openr,2,name+'/'+'CONVERG'
iconv=0
while not eof(2) do begin
  readf,2,a
  iconv=iconv+1 
endwhile
close,2
print,' converged in ',iconv,' iterations'


iwhile=0
openr,2,name+'/'+'FLUXCONT'

  while not eof(2) do begin
    readf,2,a
    iwhile=iwhile+1
  endwhile    
  indx=iwhile
  iwhile=0
  close,2
  
  openr,2,name+'/'+'FLUXCONT'
  for j=1,indx-47 do begin
    readf,2,a
  endfor

arr=fltarr(4,47)
taur=fltarr(47)
ferr=fltarr(47)

readf,2,arr
close,2
ferr=reform(arr(3,*))
fmax=max(abs(ferr))
print,' max. flux error ',fmax

if keyword_set(pr) then begin
taur=reform(arr(2,*))
for i=0,46 do begin
  print,i+1,10.^taur(i),ferr(i)
endfor
endif
return
end
