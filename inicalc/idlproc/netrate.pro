pro netrate,file,rate1,rate2,rate3
rate1_11=dblarr(47)
rate1_2 =dblarr(47)
rate1_3=dblarr(47)
rate1_4=dblarr(47)
rate1_5=dblarr(47)
rate2_11=dblarr(47)
rate2_1 =dblarr(47)
rate2_3=dblarr(47)
rate2_4=dblarr(47)
rate2_5=dblarr(47)
rate3_11=dblarr(47)
rate3_1 =dblarr(47)
rate3_2=dblarr(47)
rate3_4=dblarr(47)
rate3_5=dblarr(47)
rate1_r=dblarr(47)
rate1_c=dblarr(47)
rate2_r=dblarr(47)
rate2_c=dblarr(47)
rate3_r=dblarr(47)
rate3_c=dblarr(47)
rate1_r(*)=0.
rate2_r(*)=0.
rate3_r(*)=0.
rate1_c(*)=0.
rate2_c(*)=0.
rate3_c(*)=0.
;
a={alltypes,ll:0,type:0,ilo:0,iup:0,dum1:0.0,dum2:0.0,netr:0.0}
openr,1,file
for i=1,5170 do begin
readf,1,a
if a.type eq 1 or a.type eq 2 then begin
if a.ilo eq 1 and a.iup eq 11 then rate1_11(a.ll-1)=a.netr
if a.ilo eq 2 and a.iup eq 11 then rate2_11(a.ll-1)=a.netr
if a.ilo eq 3 and a.iup eq 11 then rate3_11(a.ll-1)=a.netr
if a.ilo eq 1 and a.iup eq 2 then rate1_2(a.ll-1)=a.netr
if a.ilo eq 1 and a.iup eq 3 then rate1_3(a.ll-1)=a.netr
if a.ilo eq 1 and a.iup eq 4 then rate1_4(a.ll-1)=a.netr
if a.ilo eq 1 and a.iup eq 5 then rate1_5(a.ll-1)=a.netr
if a.ilo eq 1 and a.iup eq 2 then rate2_1(a.ll-1)=-a.netr
if a.ilo eq 2 and a.iup eq 3 then rate2_3(a.ll-1)=a.netr
if a.ilo eq 2 and a.iup eq 4 then rate2_4(a.ll-1)=a.netr
if a.ilo eq 2 and a.iup eq 5 then rate2_5(a.ll-1)=a.netr
if a.ilo eq 1 and a.iup eq 3 then rate3_1(a.ll-1)=-a.netr
if a.ilo eq 2 and a.iup eq 3 then rate3_2(a.ll-1)=-a.netr
if a.ilo eq 3 and a.iup eq 4 then rate3_4(a.ll-1)=a.netr
if a.ilo eq 3 and a.iup eq 5 then rate3_5(a.ll-1)=a.netr
if a.ilo eq 1 and a.iup gt 5 and a.iup ne 11 then $ 
  rate1_r(a.ll-1)=rate1_r(a.ll-1)+a.netr
if a.ilo eq 2 and a.iup gt 5 and a.iup ne 11 then $ 
  rate2_r(a.ll-1)=rate2_r(a.ll-1)+a.netr
if a.ilo eq 3 and a.iup gt 5 and a.iup ne 11 then $ 
  rate3_r(a.ll-1)=rate3_r(a.ll-1)+a.netr
endif else begin
if a.ilo eq 1 then begin
rate1_c(a.ll-1)=rate1_c(a.ll-1)+a.netr
l=a.ll-1
if(l ge 35 and l le 37) then print,l,a.ilo,a.iup,a.netr,rate1_c(l)
endif
if a.ilo eq 2 then rate2_c(a.ll-1)=rate2_c(a.ll-1)+a.netr
if a.ilo eq 3 then rate3_c(a.ll-1)=rate3_c(a.ll-1)+a.netr
if a.ilo eq 1 and a.iup eq 2 then rate2_c(a.ll-1)=rate2_c(a.ll-1)-a.netr
if a.ilo eq 1 and a.iup eq 3 then rate3_c(a.ll-1)=rate3_c(a.ll-1)-a.netr
if a.ilo eq 2 and a.iup eq 3 then rate3_c(a.ll-1)=rate3_c(a.ll-1)-a.netr
endelse
endfor
close,1
for i=0,46 do begin
max1=max([abs(rate1_11(i)),abs(rate1_2(i)),abs(rate1_3(i)),$
abs(rate1_4(i)),abs(rate1_5(i)),abs(rate1_r(i)),abs(rate1_c(i))])
rate1_11(i)=rate1_11(i)/max1
rate1_2(i)=rate1_2(i)/max1
rate1_3(i)=rate1_3(i)/max1
rate1_4(i)=rate1_4(i)/max1
rate1_5(i)=rate1_5(i)/max1
rate1_r(i)=rate1_r(i)/max1
rate1_c(i)=rate1_c(i)/max1
sum1=rate1_11(i)+rate1_2(i)+rate1_3(i)+rate1_4(i)+rate1_5(i)+$
 rate1_r(i)+rate1_c(i)
;
max2=max([abs(rate2_11(i)),abs(rate2_1(i)),abs(rate2_3(i)),$
abs(rate2_4(i)),abs(rate2_5(i)),abs(rate2_r(i)),abs(rate2_c(i))])
rate2_11(i)=rate2_11(i)/max2
rate2_1(i)=rate2_1(i)/max2
rate2_3(i)=rate2_3(i)/max2
rate2_4(i)=rate2_4(i)/max2
rate2_5(i)=rate2_5(i)/max2
rate2_r(i)=rate2_r(i)/max2
rate2_c(i)=rate2_c(i)/max2
sum2=rate2_11(i)+rate2_1(i)+rate2_3(i)+rate2_4(i)+rate2_5(i)+$
 rate2_r(i)+rate2_c(i)
;
max3=max([abs(rate3_11(i)),abs(rate3_1(i)),abs(rate3_2(i)),$
abs(rate3_4(i)),abs(rate3_5(i)),abs(rate3_r(i)),abs(rate3_c(i))])
rate3_11(i)=rate3_11(i)/max3
rate3_1(i)=rate3_1(i)/max3
rate3_2(i)=rate3_2(i)/max3
rate3_4(i)=rate3_4(i)/max3
rate3_5(i)=rate3_5(i)/max3
rate3_r(i)=rate3_r(i)/max3
rate3_c(i)=rate3_c(i)/max3
sum3=rate3_11(i)+rate3_1(i)+rate3_2(i)+rate3_4(i)+rate3_5(i)+$
 rate3_r(i)+rate3_c(i)
print,i,' ',sum1,' ',sum2,' ',sum3
endfor
rate1=dblarr(7,47)
rate1(0,*)=rate1_11(*)
rate1(1,*)=rate1_2(*)
rate1(2,*)=rate1_3(*)
rate1(3,*)=rate1_4(*)
rate1(4,*)=rate1_5(*)
rate1(5,*)=rate1_r(*)
rate1(6,*)=rate1_c(*)
rate2=dblarr(7,47)
rate2(0,*)=rate2_11(*)
rate2(1,*)=rate2_1(*)
rate2(2,*)=rate2_3(*)
rate2(3,*)=rate2_4(*)
rate2(4,*)=rate2_5(*)
rate2(5,*)=rate2_r(*)
rate2(6,*)=rate2_c(*)
rate3=dblarr(7,47)
rate3(0,*)=rate3_11(*)
rate3(1,*)=rate3_1(*)
rate3(2,*)=rate3_2(*)
rate3(3,*)=rate3_4(*)
rate3(4,*)=rate3_5(*)
rate3(5,*)=rate3_r(*)
rate3(6,*)=rate3_c(*)
return
end

