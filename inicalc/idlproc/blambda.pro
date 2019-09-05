function blambda,t,lam,blow=b1,bup=b2
if not keyword_set(b1) then begin 
b1=1.
b2=1.
endif
a1=3.973d8*2.997925d26/lam^5
a2=exp(1.4388d8/(t*lam))
return,a1/(b1/b2*a2-1.)
end
