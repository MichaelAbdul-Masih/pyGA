pro adicomp_hnue,my,adi,integ=integ,xrange=xrange

if not keyword_set(xrange) then xrange=[100,1.e4]  
loadct,12  

a=my
  
adilam=adi.lam
adihnue=adi.hnue

nf=(size(adilam))(1)
for i=0,nf-2 do begin
 if(adilam(i+1) eq adilam(i)) then adilam(i+1)=adilam(i+1)+.001
endfor

lam=reverse(a.lam)
hnue=10.d0^double(a.hnue)
hnue=reverse(hnue)

plot_oi,lam,alog10(hnue),xrange=xrange, xs=1, $
	 title=model,xtitle='Lambda',ytitle='log!D10!N H!7!Dm!N!X'

if not keyword_set(integ) then begin
oplot,adilam,adihnue,color=180
oplot,lam,alog10(hnue)
loadct,0
return
endif

adihnue=10.d0^double(adihnue)


b=where(lam le 2500.)
  nmax=(size(b))(1)
    
    
b=where(lam le 140.)
  is=(size(b))(1)-1
  print,lam(is)

  xint=hnue
  xint1=xint
  lint=lam
  ig=-1
  for il=is+1,nmax-1 do begin
      l1=lam(il-1)
      l2=lam(il)
      if (l2 - l1 lt 0.01) then goto,next 
      b=where(adilam ge l1 and adilam le l2)
      n1=b(0)
      nm=(size(b))(1)-1
      n2=b(nm)
      if(n2-n1 eq 0) then n2=n2+1
      ig=ig+1
      integ=0.
;tested, works
      for it=n1,n2-1 do begin
	integ=integ+0.5*(adihnue(it+1)+adihnue(it))* $
	      (adilam(it+1)-adilam(it))
      endfor
      xint(ig)=integ/(adilam(n2)-adilam(n1))
      lint(ig)=l2
      next:
   endfor  

  
   oplot,lint(0:ig),alog10(xint(0:ig)),color=180,thick=1.
   b=where(adilam le lam(is+1))
   oplot,adilam(b),alog10(adihnue(b)),color=180,thick=1.
   b=where(adilam gt lam(nmax-1))
   oplot,adilam(b),alog10(adihnue(b)),color=180,thick=1.
   oplot,lam,alog10(hnue)
 
loadct,0
return

end
