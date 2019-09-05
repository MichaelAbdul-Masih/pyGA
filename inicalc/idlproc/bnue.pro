function bnue,t,lam,blow=b1,bup=b2
if not keyword_set(b1) then begin 
b1=1.d0
b2=1.d0
endif
;a1=3.97297d8/lam^3
a1=3.973d8/lam^3
;a2=exp(1.4388563d8/(t*lam))
a2=exp(1.4388d8/(t*lam))
return,a1/(b1/b2*a2-1.d0)
end
