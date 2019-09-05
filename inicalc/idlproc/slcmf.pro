pro slcmf,x,y,tau
plot_oi,x,y,xstyle=9,xrange=[.01,100],yrange=[0.,1.2],xtick_get=v,$
xtitle='v/vtherm',ytitle='r^2 Sline/Ic'
v1=alog10(interpol(tau,x,v))
ndim=size(v)
n=ndim(1)-1
s=string(v1,format='(f6.2)')
s=strtrim(s,2)
axis,xaxis=1,xrange=[.01,100],xstyle=1,xticks=n,xtickn=s,xtitle='log tau_sob'
return
end
