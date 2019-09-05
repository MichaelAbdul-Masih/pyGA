pro compgrad,model

info=file_info(model+'/OUT_TOT') 

if info.exists ne 1 or info.size eq 0 then begin
  print,'model not present or corrupt'
  return
endif  

loadct,12  
readmod,model+'/OUT_TOT',' ',a
plot_oi,a.taur,a.grad

readcol,model+'/CHIBAR_H',format='I,F,F',i,chibar,gradact
oplot,a.taur,gradact,col=100

readcol,model+'/GRAD.OUT',format='A,I,F',type,i,grad

file=model+'/GRAD_START.OUT'
exist=file_test(file)

if exist then begin
;update performed  
oplot,a.taur,grad,col=200

readcol,file,format='A,I,F',type,i,gradstart
oplot,a.taur,gradstart,col=50
oplot,a.taur,gradstart,col=50,psym=1


print,'green = grad(start)'
print,'red   = grad(updated)'
print,'blue  = grad(actual)'

endif else begin
;no update performed
oplot,a.taur,grad,col=50
oplot,a.taur,grad,col=50,psym=1


print,'green = grad(start)'
print,'blue  = grad(actual)'
endelse

return
end
