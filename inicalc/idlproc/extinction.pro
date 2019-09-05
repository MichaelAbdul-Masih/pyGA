pro extinction, alam, lambda, R, EBV

; extinction law according to Cardelli et al., 1989, ApJ 345, 245
; wavelength in microns

    AV=R*EBV
  
    x=1./lambda
    outside=where(x lt 0.3 or x gt 10, count)
    if count ne 0 then begin
      print,' some frequencies ouside allowed range 1000 A < lam < 33333 A'
      print,' do not trust these values'
    endif
    alam=x
    alam(*)=0.
    
    ir=where(x lt 1.1,countir)
    opt=where(x ge 1.1 and x lt 3.3,countopt)
    uv=where(x  ge 3.3 and x lt 8. ,countuv)
    fuv=where(x ge 8.,countfuv)

    if countir ne 0 then begin
    a=0.574*x(ir)^1.61
    b=-0.527*x(ir)^1.61
    alam(ir)=AV*(a+b/R)
    endif

    if countopt ne 0 then begin
    y=x(opt)-1.82
    a=1.+0.17699*y-0.50447*y^2-0.02427*y^3+0.72085*y^4+ $
         0.01979*y^5-0.77530*y^6+0.32999*y^7
    b=   1.41338*y+2.28305*y^2+1.07233*y^3-5.38434*y^4- $
         0.62251*y^5+5.30260*y^6-2.09002*y^7
    alam(opt)=AV*(a+b/R)
    endif

    if countuv ne 0 then begin
    xx=x(uv)
    fa=xx
    fb=fa
    div1=where(xx lt 5.9,count1)
    div2=where(xx ge 5.9,count2)

    if (count1 ne 0) then begin
    fa(div1)=0.
    fb(div1)=0.
    endif

    if (count2 ne 0) then begin
    xxx=xx(div2)
    fa(div2)=-0.04473*(xxx-5.9)^2-0.009779*(xxx-5.9)^3
    fb(div2)= 0.2130 *(xxx-5.9)^2+0.1207  *(xxx-5.9)^3  
    endif

    a= 1.752-0.316*xx-0.104/((xx-4.67)^2+0.341)+fa
    b=-3.090+1.825*xx+1.206/((xx-4.62)^2+0.263)+fb
    alam(uv)=AV*(a+b/R)
    endif

    if countfuv ne 0 then begin
    y=x(fuv)-8.
    a=-1.073-0.628*y+0.137*y^2-0.070*y^3
    b=13.670+4.257*y-0.420*y^2+0.374*y^3
    alam(fuv)=AV*(a+b/R)
    endif

return    
END
