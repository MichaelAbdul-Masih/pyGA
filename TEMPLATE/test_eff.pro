PRO te

;reads in, and tets 

iter = 20

stn = 'OPA_EFF_RAT_'
j = 2 
file = stn+strtrim(string(j),1)
eo,file  
restore,'eff_opa.sav'
j = j+1 
openw,2,'test_eff_conv.dat'
printf,2,'      it,       lam,      rad,      taur,      conv,      opa,      opa_old'
printf,2,'    '
while (j le iter) do begin  

   opa_eff_rat_old = opa_eff_rat

   file = stn+strtrim(string(j),1)
   eo,file  
   restore,'eff_opa.sav'
;variables: freq, opa_eff_rat(nd,ifre), rad, tross 
;element-variables: nd,ifre

   conv = abs( 1.-opa_eff_rat_old/opa_eff_rat )
   ii = Where(conv eq max(conv)) 

   print,ii   
   print,'it:',j
   print,'lam:',freq(ii)
   print,'rad:',rad(ii)
   print,'taur:',tross(ii) 
   print,'-------------------'
   print,opa_eff_rat(ii),opa_eff_rat_old(ii)
   printf,2,format='(i10,2f15.5,e15.5,3f15.5)',j,freq(ii(0)),rad(ii(0)),tross(ii(0)),conv(ii(0)),opa_eff_rat(ii(0)),opa_eff_rat_old(ii(0))
   print,'conv:',conv(ii) 
   print,'XXXXXXXXXXXXXXXXXXXXXXxxx'
   j = j +1 

endwhile
Close,2

Return 

ii = Where(freq(0,*) ne 0.0)
ni = N_Elements(ii) 

zz = Fltarr(nd,ni)  
;yy = 1./freq(0,ii)*1.d8 
;already in Ang 
yy = freq(0,ii) 
yy = alog10(yy) 
;should now be angstroem  
xx = rad(*,0) 
xx = alog10(xx)
for i=0,nd-1 do zz(i,*) = opa_eff_rat(i,ii)  
 
ii = Where(yy gt lam_low and yy lt lam_high) 
yy_n = yy(ii)
xx_n = xx  
zz_n = Fltarr(nd,N_Elements(ii))
for i=0,nd-1 do zz_n(i,*) = zz(i,ii) 
yy = yy_n
xx = xx_n
zz = zz_n
;save
;Return

; Set up the data and colors for a filled contour plot.
levs = 10 
bb = 1 
cgLoadCT, 5, NColors=levs, Bottom=bb
c_colors = Indgen(levs)+1
position = [0.1, 0.1, 0.9, 0.75]

ii = Where(zz lt 1.0, count)
IF (count eq 0) THEN BEGIN
   ii = Where(zz gt 1.0, count)
   IF (count eq 0 ) THEN BEGIN 
      Print,'zz eq 1 everywhere!!!!!'
      Print,'That is -- NO reduction in OPA_EFF_RAT!!!'
   ENDIF ELSE BEGIN 
      Print,'zz ge 1 everywhere!!!!!'
      Print,'> 1 at points:'
      Print,ii
      Print,zz(ii)
   ENDELSE
ENDIF ELSE BEGIN 
   ran = [min(zz),max(zz)]
   cgContour, zz,xx,yy, NLevels=levs, /Cell_Fill, Position=position, C_Colors=c_colors,$
           xtit='log10 (r/Rstar)',ytit='log10(lambda)'
;cgContour, zz,xx,yy, NLevels=levs, Position=position, /Outline
   cgColorbar, Divisions=10, Range=ran, /discrete, $
               Position=[0.1, 0.90, 0.9, 0.94], NColors=levs, Bottom=bb
ENDELSE

;save

END

PRO eo,file  

;reads effective opacity output 

openr,1,file
;'TCL_BG'
readf,1,ND,IFRE
opa_eff_rat = Fltarr(ND,IFRE) 
freq = Fltarr(ND,IFRE)
rad = Fltarr(nd,ifre) 
tross = rad 
for i=0,nd-1 do begin 
   for j=0,ifre-1 do begin 
      ;readf,1,eff_opa_rat(i,j),freq(i,j),rad(i,j) 
      readf,1,effd,fred,radd,taurr
      opa_eff_rat(i,j) = effd
      freq(i,j) = fred
      rad(i,j) = radd      
      tross(i,j) = taurr      
   endfor
endfor
close,1


save,filename='eff_opa.sav',opa_eff_rat,nd,ifre,freq,rad,tross
END

