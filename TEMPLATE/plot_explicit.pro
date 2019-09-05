PRO pe,dir,atom_in,ion_min,ion_max,rad=rad,yr=yr,xr=xr   

;Plot explicit elements 

IF N_PARAMS() ne 4 THEN BEGIN 
   Print,'syntax:  pe,dir (2-array!),atom_in (number NOT IDL!),ion_min,ion_max, rad=rad'
   Return
ENDIF 
nd = N_Elements(dir) 
IF nd ne 2 THEN BEGIN 
   Print,'Must be TWO inpu dirs. (e.g. one thick, one thin)' 
   Return 
ENDIF

;if keeyword rad is set, plot on log radius scla,e 
;otherwise log taur 

;test plots for metal fractions 

atom = atom_in 

Print,'--------------------------------'
Print,'Plotting explicit ATOM:  ',atom
Print,'Ion Stages:',ion_min,ion_max
Print,'--------------------------------'

;FIRST DIR  
file = dir[0]+'/OUT_TOT'
readmod,file,' ',thin 

;Obs -- On what scale? 
IF Keyword_set(rad) THEN BEGIN 
   x = alog10(thin.r) 
   xtit = 'alog10(r)'
ENDIF ELSE BEGIN 
   x = alog10(thin.taur)
   xtit = 'alog10(taur)'
ENDELSE
;x = alog10(thin.r)  
ndim = size(x) 
nd = ndim(1) 

;file = dir+'thin/METAL_IDL'
;openr,1,file
;metal = Fltarr(10,30,nd) 
;readf,1,metal
;close,1
;NOTE: Following plifrac_met.pro 

;ifrac(J-1,K-1,L) 

;OBS - ifrac and met_frac different in ion indexing!! 

if not keyword_set(xr) then xr = [min(x),max(x)]
;yr = [-10,0.0]
th = 2

lines = 0 
for i=ion_min,ion_max do begin 
   lines = i - ion_min 
   y = alog10( thin.ifrac(i-1,atom-1,*) ) 
   if (i eq ion_min) then begin 
      cgplot,x,y,linestyle=lines,yr=yr,xr=xr,xs=1,ys=1,$
             thick=th,xtit=xtit,ytit='Ion frac.'  
   endif else begin 
      cgplot,x,y,linestyle=lines,/overplot,thick=th
   endelse
endfor 

;FIRST DIR DONE, NOW SECOND 

file = dir[1]+'/OUT_TOT'
readmod,file,' ',thick
;file = dir+'thick/METAL_IDL'
;openr,1,file
;metal = Fltarr(10,30,nd) 
;readf,1,metal
;close,1
;NOTE: Following plifrac_met.pro 

;metal(J,K-1,L) 

;Obs -- On what scale? 
;x = thick.r

IF  Keyword_set(rad) THEN BEGIN 
   x = alog10(thick.r) 
ENDIF ELSE BEGIN 
   x = alog10(thick.taur)
ENDELSE
lines = 0 
;th = 5.0
for i=ion_min,ion_max do begin 
   lines = i - ion_min 
   y = alog10( thick.ifrac(i-1,atom-1,*) ) 
   cgplot,x,y,linestyle=lines,/overplot,col=cgcolor('red');,thick=th
   print,'ion stage:',i,'  has idl linestyle',lines  
endfor 
print,'--------------------------'
print,'FIRST DIR is black, SECOND is red'
print,'--------------------------'

save

END
