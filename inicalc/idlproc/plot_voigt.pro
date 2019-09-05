pro plot_voigt,astr

x=findgen(1001)
x=x/50.
x=x-10.
x=x*1.5

a=float(astr)
f=voigt(a,x)
f1=exp(-x^2)
f2=a/(sqrt(!pi)*x^2)

plot_io,x,f,title='Voigt profile with a = '+astr, $
	 xtitle='frequency displacement in doppler widths, v', $
	 ytitle='H(a,v)',charsize=1.5,xstyle=1

oplot,x,f1,linestyle=1
oplot,x,f2,linestyle=2
oplot,x,f1+f2,linestyle=3

return
end
