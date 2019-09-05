PRO pmvse,dir,atom_in,ion_min,ion_max,rad=rad  

IF N_PARAMS() ne 4 THEN BEGIN 
   Print,'syntax:  pm,dir (!complete - either thin or thick!), atom_in (name!),ion_min,ion_max, rad=rad'
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
 
;OBS -- FROM nlte_approx.f90-------------
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

if (atom_in eq 'P') then begin 
   atom_exp = 2 
endif else if (atom_in eq 'H') then begin 
   atom_exp = 0 
endif else if (atom_in eq 'HE') then begin 
   atom_exp = 1
endif else if (atom_in eq 'N') then begin 
   atom_exp = 2
endif else begin 
   Print,'No explicit atom!!'
   Return
endelse 



Print,'--------------------------------'
Print,'Plotting ATOM:  ',atom_name[atom-1]
Print,'Ion Stages:',ion_min,ion_max
Print,'--------------------------------'

;First THIN 
file = dir+'/OUT_TOT'
readmod,file,' ',modd  

;Obs -- On what scale? 
IF Keyword_set(rad) THEN BEGIN 
   x = alog10(modd.r) 
   xtit = 'alog10(r)'
ENDIF ELSE BEGIN 
   x = alog10(modd.taur)
   xtit = 'alog10(taur)'
ENDELSE
;x = alog10(thin.r)  
ndim = size(x) 
nd = ndim(1) 

file = dir+'/METAL_IDL'
openr,1,file
metal = Fltarr(10,30,nd) 
readf,1,metal
close,1
;NOTE: Following plifrac_met.pro 

;metal(J,K-1,L) 

xr = [min(x),max(x)]
;yr = [0,1.4]
th = 2

lines = 0 
for i=ion_min,ion_max do begin 
   lines = i - ion_min 
   y = alog10( metal(i,atom-1,*) ) 
   y2 = alog10( modd.ifrac(i-1,atom_exp,*) ) 
   if (i eq ion_min) then begin 
      cgplot,x,y,linestyle=lines,yr=yr,xr=xr,xs=1,ys=1,$
             thick=th,xtit=xtit,ytit='Ion frac.' 
   endif 
   cgplot,x,y,linestyle=lines,/overplot,thick=th,col=cgcolor('red')    
   cgplot,x,y2,/overplot,col=cgcolor('blue'),linestyle=lines 
   print,'ion stage:',i,'  has idl linestyle',lines  
endfor 
print,'BLUE is EXPLICIT, black is BACKGROUND'

save

END
