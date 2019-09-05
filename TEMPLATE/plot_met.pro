PRO pmet,dir,atom_in,ion_min,ion_max,rad=rad, yr=yr   

IF N_PARAMS() ne 4 THEN BEGIN 
   Print,'syntax:  pmet,dirs (2-array!) ,atom_in (name!),ion_min,ion_max, rad=rad'
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

atom_name = ['H','HE','LI','BE','B','C', $
          'N','O','F','NE','NA','MG', $
          'AL','SI','P','S','CL','AR',$
          'K ','CA','SC','TI','V ','CR',$
          'MN','FE','CO','NI','CU','ZN']
;OBS -- COPIED FROM nlte_approx.f90-------------
;DATA NAMES /'H ','HE','LI','BE','B ','C ', &
;&          'N ','O ','F ','NE','NA','MG', &
;&          'AL','SI','P ','S ','CL','AR',&
;&          'K ','CA','SC','TI','V ','CR',&
;&          'MN','FE','CO','NI','CU','ZN'/
;-----------------------------------------

;dir = 's35_clf1_'
;-------------------------------
;atom = 15 
;ion_min = 4
;ion_max = 6 

ii = Where(atom_name eq atom_in, count)
IF (count ne 1) THEN BEGIN 
   Print,ii 
   Print,'Atom not found!'
   return
ENDIF ELSE BEGIN 
   atom = ii+1 
   ;NOTE - to be consistent with other notaition..
ENDELSE   


Print,'--------------------------------'
Print,'Plotting ATOM:  ',atom_name[atom-1]
Print,'Ion Stages:',ion_min,ion_max
Print,'--------------------------------'

;FIRST DIRECTORY 
;file = dir+'thin/OUT_TOT'
file = dir[0]+'/OUT_TOT'
readmod,file,' ',thin 

;Obs -- On what scale? 
IF Keyword_set(rad) THEN BEGIN 
   x = alog10(thin.r-1.)   
   xtit = 'alog10(r)'
ENDIF ELSE BEGIN 
   x = alog10(thin.taur)
   xtit = 'alog10(taur)'
ENDELSE
;x = alog10(thin.r)  
ndim = size(x) 
nd = ndim(1) 


;STILL FIRST DIRECTORY 
;file = dir+'thin/METAL_IDL'
file = dir[0]+'/METAL_IDL'
openr,1,file
metal = Fltarr(10,30,nd) 
readf,1,metal
close,1
;NOTE: Following plifrac_met.pro 

;metal(J,K-1,L) 

;xr = [min(x),max(x)]
;yr = [1.e-3,1.4]
th = 2
;IF KEYWORD_SET(yr) THEN yr=yr ELSE 
;Variable y-range unless keyword is set 

lines = 0 
for i=ion_min,ion_max do begin 
   lines = i - ion_min 
   y = alog10( metal(i,atom-1,*) ) 
   if (i eq ion_min) then begin 
      cgplot,x,y,linestyle=lines,yr=yr,xr=xr,xs=1,ys=1,$
             thick=th,xtit=xtit,ytit='Ion frac.'  
   endif else begin 
      cgplot,x,y,linestyle=lines,/overplot,thick=th
   endelse
endfor 

;FIRST DONE, NOW SECOND 

;file = dir+'thick/OUT_TOT'
file = dir[1]+'/OUT_TOT'
readmod,file,' ',thick
file = dir[1]+'/METAL_IDL'
openr,1,file
metal = Fltarr(10,30,nd) 
readf,1,metal
close,1
;NOTE: Following plifrac_met.pro 

;metal(J,K-1,L) 

;Obs -- On what scale? 
;x = thick.r

IF  Keyword_set(rad) THEN BEGIN 
   x = alog10(thick.r-1.) 
ENDIF ELSE BEGIN 
   x = alog10(thick.taur)
ENDELSE
lines = 0 
;th = 5.0
for i=ion_min,ion_max do begin 
   lines = i - ion_min 
   y = alog10( metal(i,atom-1,*) ) 
   cgplot,x,y,linestyle=lines,/overplot,col=cgcolor('red');,thick=th
   print,'ion stage:',i,'  has idl linestyle',lines  
endfor 
print,'--------------------------'
print,'FIRST dir is black, SECOND dir is red'
print,'--------------------------'

save

END
