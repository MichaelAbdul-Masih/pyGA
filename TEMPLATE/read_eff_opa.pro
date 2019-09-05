PRO eo,file  

;reads effective opacity output 

openr,1,file
;'TCL_BG'
readf,1,ND,IFRE
opa_eff_rat = Fltarr(ND,IFRE) 
freq = Fltarr(ND,IFRE)
rad = Fltarr(nd,ifre) 
tross = rad 
opac = rad 
;ind =  intarr(nd,ifre) 
;replaced this by rho 
rho = rad 
for i=0,nd-1 do begin 
   for j=0,ifre-1 do begin 
      ;readf,1,eff_opa_rat(i,j),freq(i,j),rad(i,j) 
;      readf,1,effd,fred,radd,taurr,indd,opacc
      readf,1,effd,fred,radd,taurr,rhod,opacc
      opa_eff_rat(i,j) = effd
      freq(i,j) = fred
      rad(i,j) = radd      
      tross(i,j) = taurr      
;      ind(i,j) = indd 
      rho(i,j) = rhod
      opac(i,j) = opacc
   endfor
endfor
close,1


;save,filename='eff_opa.sav',opa_eff_rat,nd,ifre,freq,rad,tross,ind,opac
save,filename='eff_opa.sav',opa_eff_rat,nd,ifre,freq,rad,tross,rho,opac
END
