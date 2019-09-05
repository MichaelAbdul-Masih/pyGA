PRO peo,file,lam_low,lam_high,opa=opa,levs=levs,xr=xr    

IF N_PARAMS() ne 3 THEN BEGIN 
   Print,'syntax: peo,file,lam_low,lam_high (LOG10 units!),opa=opa (key wordt if opac is to plotted instead)m, levs=levs number of contour levels (def levs=10)'
   Return
ENDIF 

;reads in, and then plots effective opacity maps etc 
;in log lambda range lam_low, lam_high 

;Note1 - for now I read into a save-set (enables furtehr testing by 
;simply restoring in from terminal)
;Note2 - variable freq is now really LAMBDA in Ang 
eo,file  
restore,'eff_opa.sav'
;variables: freq, opa_eff_rat(nd,ifre), rad, tross 
;element-variables: nd,ifre

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

if not keyword_set(opa) then begin 
   for i=0,nd-1 do zz(i,*) = opa_eff_rat(i,ii)  
endif else begin 
   for i=0,nd-1 do zz(i,*) = alog10(opac(i,ii)/rho(i,ii))  
;   for i=0,nd-1 do zz(i,*) = opac(i,ii)/rho(i,ii)
endelse 

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
if not keyword_set(levs) then levs = 10 
bb = 1 
cgLoadCT, 5, NColors=levs, Bottom=bb
c_colors = Indgen(levs)+1
position = [0.1, 0.1, 0.9, 0.75]

if not keyword_set(opa) then begin 
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
      if not keyword_set(xr) then xr=[min(xx),max(xx)]
      cgContour, zz,xx,yy, NLevels=levs, /Fill, Position=position, C_Colors=c_colors,$
           xtit='log10 (r/Rstar)',ytit=textoidl('log10(\lambda)'),xr=xr,xs=1
;cgContour, zz,xx,yy, NLevels=levs, Position=position, /Outline
      cgColorbar, Divisions=levs, Range=ran, /discrete, title=textoidl('\chi_{eff}/<\chi>'), $
               Position=[0.1, 0.90, 0.9, 0.94], NColors=levs, Bottom=bb
   ENDELSE
endif else begin 
   ran = [min(zz),max(zz)]
   if not keyword_set(xr) then xr=[min(xx),max(xx)]
   cgContour, zz,xx,yy, NLevels=levs, /Fill, Position=position, C_Colors=c_colors,$
           xtit='log10 (r/Rstar)',ytit=textoidl('log10(\lambda)'),xr=xr,xs=1
;cgContour, zz,xx,yy, NLevels=levs, Position=position, /Outline
   cgColorbar, Divisions=levs, Range=ran, /discrete, title=textoidl('alog10(\kappa = \chi/\rho)'),$
               Position=[0.1, 0.90, 0.9, 0.94], NColors=levs, Bottom=bb
endelse 

save

END

@read_eff_opa.pro 
