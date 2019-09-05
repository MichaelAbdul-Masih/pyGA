PRO pb,dir,l1,l2,yr=yr,rad=rad,xr=xr 

;Plot explicit elements 

IF N_PARAMS() eq 0 THEN BEGIN 
   Print,'syntax: dir (TWO dirs!),l1,l2 (STRINGS)'
   Print,'-------------------------------------'
   Print,'H He Basic Levels to choose from:'
   Print,'H11    H12    H13    H14    H15    H16    H17    H18    H19    H110   H111   H112   H113   H114   H115   H116   H117   H118   H119   H120   H21    HE11S1 HE12S3 HE12S1 HE12P3 HE12P1 HE13S3 HE13S1 HE13P3 HE13D3 HE13D1 HE13P1 HE14S3 HE14S1 HE14P3 HE14D3 HE14D1 HE14F3 HE14F1 HE14P1 HE15S3 HE15S1 HE15P3 HE15D3 HE15D1 HE15F3 HE15F1 HE15L5 HE15P1 HE16S3 HE16S1 HE16P3 HE16D3 HE16D1 HE16F3 HE16F1 HE16L5 HE16P1 HE17S3 HE17S1 HE17P3 HE17D3 HE17D1 HE17F3 HE17F1 HE17L5 HE17P1 HE18NS HE19NS HE10NS HE21   HE22   HE23   HE24   HE25   HE26   HE27   HE28   HE29 HE211  HE212  HE213  HE214  HE215  HE216  HE217  HE218  HE219  HE220  HE31'
   Return
ENDIF 


nd = N_Elements(dir) 
IF nd ne 2 THEN BEGIN 
   Print,'Must be TWO inpu dirs. (e.g. one thick, one thin)' 
   Return 
ENDIF

;if keeyword rad is set, plot on log radius scla,e 
;otherwise log taur 

;test plots for hydrogen for now (tired..) 

;FIRST DIR  
file = dir[0]+'/OUT_TOT'
readmod,file,' ',thin 

;Obs -- On what scale? 
IF Keyword_set(rad) THEN BEGIN 
;   x = alog10(thin.r-1.) 
   x = alog10(thin.r) 
;   xtit = 'alog10(r-1)'
   xtit = 'alog10(r)'
ENDIF ELSE BEGIN 
   x = alog10(thin.taur)
   xtit = 'alog10(taur)'
ENDELSE
;x = alog10(thin.r)  
ndim = size(x) 
nd = ndim(1) 

levs = strtrim(thin.levnam) 
ii=where(l1 eq levs,count)
if(count eq 0) then begin
   print,' level not found-1!!'
   Return
endif
LOW = ii 
;-------------------------
ii=where(l2 eq levs,count)
if(count eq 0) then begin
   print,' level not found-2!!!'
   Return
endif
UP = ii 

print,'first model plots dep. coef - LOW and UP:'
print,LOW,UP
print,l1,'   ',l2 

;OBS - ifrac and met_frac different in ion indexing!! 

;xr = [min(x),max(x)]
;yr = [-10,0.0]
th = 2

y = alog10( thin.dep(low,*) ) 
cgplot,x,y,linestyle=0,yr=yr,xr=xr,xs=1,ys=1,$
             thick=th,xtit=xtit,ytit='log10 Dep coef.' 
y = alog10( thin.dep(up,*) ) 
cgplot,x,y,linestyle=2,/overplot,thick=th


;FIRST DIR DONE, NOW SECOND 

file = dir[1]+'/OUT_TOT'
readmod,file,' ',thick

levs = strtrim(thin.levnam) 
ii=where(l1 eq levs,count)
if(count eq 0) then begin
   print,' level not found-3!!!'
   Return
endif
LOW = ii 
;-------------------------
ii=where(l2 eq levs,count)
if(count eq 0) then begin
   print,' level not found-4!!!'
   Return
endif
UP = ii 

y = alog10( thick.dep(low,*) ) 
cgplot,x,y,linestyle=0,/overplot,col=cgcolor('red') ;,thick=th
y = alog10( thick.dep(up,*) ) 
cgplot,x,y,linestyle=2,/overplot,col=cgcolor('red') ;,thick=th


print,'--------------------------'
print,'FIRST DIR is black, SECOND is red'
print,'--------------------------'
print,'--------------------------'
print,'LOW is solid, UP is dashed'
print,'--------------------------'


save

END
