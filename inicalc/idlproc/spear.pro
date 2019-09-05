pro spear,data,n
spearman,data,cdata
rs=cdata(1,0)
fac=(1+rs)*(1-rs)
if (fac gt 0.) then begin
df=n-2
t=rs*sqrt(df/fac)
print,'t = ',t
probrs=2.*(1-student1_t(t,df))
endif else begin
probrs=0.
endelse
print,'significance = ',probrs
return
end




