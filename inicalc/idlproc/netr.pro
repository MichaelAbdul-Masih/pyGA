pro netr,file,tau
rate_r=dblarr(12,12,48)
rate_c=dblarr(12,12,48)
rate_1r=dblarr(9,48)
rate_1c=dblarr(9,48)
maxr=dblarr(48)
maxc=dblarr(48)
;
a={alltypes,ll:0,type:0,ilo:0,iup:0,dum1:0.0,dum2:0.0,netr:0.0}
openr,1,file
for i=1,5170 do begin
readf,1,a
if a.type eq 1 or a.type eq 2 then begin
if a.ilo le 3 then rate_r(a.ilo,a.iup,a.ll)=-a.dum1 
if a.ilo le 3 then rate_r(a.iup,a.ilo,a.ll)=a.dum2 
endif else begin
if a.ilo le 3 then rate_c(a.ilo,a.iup,a.ll)=-a.dum1 
if a.ilo le 3 then rate_c(a.iup,a.ilo,a.ll)=a.dum2 
endelse
endfor
close,1
rate_1r(1,*)=rate_r(1,2,*)
rate_1r(2,*)=rate_r(1,3,*)
for i=1,47 do begin
sum=0.
for k=4,10 do sum=sum+rate_r(1,k,i)
rate_1r(3,i)=sum
endfor
rate_1r(4,*)=rate_r(1,11,*)
rate_1r(5,*)=rate_r(2,1,*)
rate_1r(6,*)=rate_r(3,1,*)
for i=1,47 do begin
sum=0.
for k=4,10 do sum=sum+rate_r(k,1,i)
rate_1r(7,i)=sum
endfor
rate_1r(8,*)=rate_r(11,1,*)
;
rate_1c(1,*)=rate_c(1,2,*)
rate_1c(2,*)=rate_c(1,3,*)
for i=1,47 do begin
sum=0.
for k=4,10 do sum=sum+rate_c(1,k,i)
rate_1c(3,i)=sum
endfor
rate_1c(4,*)=rate_c(1,11,*)
rate_1c(5,*)=rate_c(2,1,*)
rate_1c(6,*)=rate_c(3,1,*)
for i=1,47 do begin
sum=0.
for k=4,10 do sum=sum+rate_c(k,1,i)
rate_1c(7,i)=sum
endfor
rate_1c(8,*)=rate_c(11,1,*)
for i=1,47 do maxr(i)=max(abs(rate_1r(*,i)))
for i=1,47 do maxc(i)=max(abs(rate_1c(*,i)))
for i=1,47 do rate_1r(*,i)=rate_1r(*,i)/max([maxr(i),maxc(i)])
for i=1,47 do rate_1c(*,i)=rate_1c(*,i)/max([maxr(i),maxc(i)])
set_plot,'ps'
plot_oi,tau,rate_1r(1,*),xrange=[100,1.e-6],yrange=[-1.1,1.1],$
title='rate_1_rad'
oplot,tau,rate_1r(5,*)
oplot,tau,rate_1r(2,*),linestyle=1
oplot,tau,rate_1r(6,*),linestyle=1
oplot,tau,rate_1r(3,*),linestyle=2
oplot,tau,rate_1r(7,*),linestyle=2
oplot,tau,rate_1r(4,*),linestyle=3
oplot,tau,rate_1r(8,*),linestyle=3
plot_oi,tau,rate_1c(1,*),xrange=[100,1.e-6],yrange=[-1.1,1.1],$
title='rate_1_coll'
oplot,tau,rate_1c(5,*)
oplot,tau,rate_1c(2,*),linestyle=1
oplot,tau,rate_1c(6,*),linestyle=1
oplot,tau,rate_1c(3,*),linestyle=2
oplot,tau,rate_1c(7,*),linestyle=2
oplot,tau,rate_1c(4,*),linestyle=3
oplot,tau,rate_1c(8,*),linestyle=3
;
rate_1r(1,*)=-rate_r(2,1,*)
rate_1r(2,*)=rate_r(2,3,*)
for i=1,47 do begin
sum=0.
for k=4,10 do sum=sum+rate_r(2,k,i)
rate_1r(3,i)=sum
endfor
rate_1r(4,*)=rate_r(2,11,*)
rate_1r(5,*)=-rate_r(1,2,*)
rate_1r(6,*)=rate_r(3,2,*)
for i=1,47 do begin
sum=0.
for k=4,10 do sum=sum+rate_r(k,2,i)
rate_1r(7,i)=sum
endfor
rate_1r(8,*)=rate_r(11,2,*)
;
rate_1c(1,*)=-rate_c(2,1,*)
rate_1c(2,*)=rate_c(2,3,*)
for i=1,47 do begin
sum=0.
for k=4,10 do sum=sum+rate_c(2,k,i)
rate_1c(3,i)=sum
endfor
rate_1c(4,*)=rate_c(2,11,*)
rate_1c(5,*)=-rate_c(1,2,*)
rate_1c(6,*)=rate_c(3,2,*)
for i=1,47 do begin
sum=0.
for k=4,10 do sum=sum+rate_c(k,2,i)
rate_1c(7,i)=sum
endfor
rate_1c(8,*)=rate_c(11,2,*)
for i=1,47 do maxr(i)=max(abs(rate_1r(*,i)))
for i=1,47 do maxc(i)=max(abs(rate_1c(*,i)))
for i=1,47 do rate_1r(*,i)=rate_1r(*,i)/max([maxr(i),maxc(i)])
for i=1,47 do rate_1c(*,i)=rate_1c(*,i)/max([maxr(i),maxc(i)])
plot_oi,tau,rate_1r(1,*),xrange=[100,1.e-6],yrange=[-1.1,1.1],$
title='rate_2_rad'
oplot,tau,rate_1r(5,*)
oplot,tau,rate_1r(2,*),linestyle=1
oplot,tau,rate_1r(6,*),linestyle=1
oplot,tau,rate_1r(3,*),linestyle=2
oplot,tau,rate_1r(7,*),linestyle=2
oplot,tau,rate_1r(4,*),linestyle=3
oplot,tau,rate_1r(8,*),linestyle=3
plot_oi,tau,rate_1c(1,*),xrange=[100,1.e-6],yrange=[-1.1,1.1],$
title='rate_2_coll'
oplot,tau,rate_1c(5,*)
oplot,tau,rate_1c(2,*),linestyle=1
oplot,tau,rate_1c(6,*),linestyle=1
oplot,tau,rate_1c(3,*),linestyle=2
oplot,tau,rate_1c(7,*),linestyle=2
oplot,tau,rate_1c(4,*),linestyle=3
oplot,tau,rate_1c(8,*),linestyle=3
;
rate_1r(1,*)=-rate_r(3,1,*)
rate_1r(2,*)=-rate_r(3,2,*)
for i=1,47 do begin
sum=0.
for k=4,10 do sum=sum+rate_r(3,k,i)
rate_1r(3,i)=sum
endfor
rate_1r(4,*)=rate_r(3,11,*)
rate_1r(5,*)=-rate_r(1,3,*)
rate_1r(6,*)=-rate_r(2,3,*)
for i=1,47 do begin
sum=0.
for k=4,10 do sum=sum+rate_r(k,3,i)
rate_1r(7,i)=sum
endfor
rate_1r(8,*)=rate_r(11,3,*)
;
rate_1c(1,*)=-rate_c(3,1,*)
rate_1c(2,*)=-rate_c(3,2,*)
for i=1,47 do begin
sum=0.
for k=4,10 do sum=sum+rate_c(3,k,i)
rate_1c(3,i)=sum
endfor
rate_1c(4,*)=rate_c(3,11,*)
rate_1c(5,*)=-rate_c(1,3,*)
rate_1c(6,*)=-rate_c(2,3,*)
for i=1,47 do begin
sum=0.
for k=4,10 do sum=sum+rate_c(k,3,i)
rate_1c(7,i)=sum
endfor
rate_1c(8,*)=rate_c(11,3,*)
for i=1,47 do maxr(i)=max(abs(rate_1r(*,i)))
for i=1,47 do maxc(i)=max(abs(rate_1c(*,i)))
for i=1,47 do rate_1r(*,i)=rate_1r(*,i)/max([maxr(i),maxc(i)])
for i=1,47 do rate_1c(*,i)=rate_1c(*,i)/max([maxr(i),maxc(i)])
plot_oi,tau,rate_1r(1,*),xrange=[100,1.e-6],yrange=[-1.1,1.1],$
title='rate_3_rad'
oplot,tau,rate_1r(5,*)
oplot,tau,rate_1r(2,*),linestyle=1
oplot,tau,rate_1r(6,*),linestyle=1
oplot,tau,rate_1r(3,*),linestyle=2
oplot,tau,rate_1r(7,*),linestyle=2
oplot,tau,rate_1r(4,*),linestyle=3
oplot,tau,rate_1r(8,*),linestyle=3
plot_oi,tau,rate_1c(1,*),xrange=[100,1.e-6],yrange=[-1.1,1.1],$
title='rate_3_coll'
oplot,tau,rate_1c(5,*)
oplot,tau,rate_1c(2,*),linestyle=1
oplot,tau,rate_1c(6,*),linestyle=1
oplot,tau,rate_1c(3,*),linestyle=2
oplot,tau,rate_1c(7,*),linestyle=2
oplot,tau,rate_1c(4,*),linestyle=3
oplot,tau,rate_1c(8,*),linestyle=3
set_plot,'x'
return
end




