pro rby,y,value
; calculates elscat red.function rb(y) as in Rybicki & Hummer, AA 290, p.555
yabs2=.5*abs(y)
term1=1.1 + 0.4*y^2 + .05*Y^4
term1=term1/sqrt(!pi)*exp(-y^2/4.)
term2=1.5+.5*Y^2+.05*Y^4
erfc=1.-errorf(yabs2)
term2=term2*yabs2*erfc
value=term1-term2
return
end