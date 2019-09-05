;***********************************************************************

PRO elais_olay,dbase,hd,names=names,cross=cross,no_opt=no_opt,big=big,$
    ra0=ra0,dec0=dec0,errma0=errma0,errmi0=errmi0,errpa0=errpa0,elist=elist,$
    print=print,int=int,apm_ident=apm_ident

;,listo=listo
;+
; NAME:
;	elais_olay
;
; PURPOSE:
;	overlays elais sources on an existing plot
;
;      ---------
;       colours
;      ---------
;
;      elais databases source:  5 :  yellow
;      iras sources:            4 :  blue
;      first/nvss sources:      2 :  red
;      apm sources:             3 : green
;      apm id in database       7 : magenta
;      
;
; CATEGORY:
;	analysis
;
; CALLING SEQUENCE:
;	
;
; INPUTS:
;
;     dbase: name of elais source database	
;      hd: header of image to be overplotted
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	names:
;	cross:
;	no_opt:
;	big:
;	ra0:
;	dec0:
;	errma0:
;	errmi0:
;	errpa0:
;       elist: list of elais entry numbers to use
;       print: if set then association positions are printed
;
; OUTPUTS:
;	
;
; OPTIONAL OUTPUTS:
;	
;
; COMMON BLOCKS:
;	
;
; SIDE EFFECTS:
;	overplots onto current device
;       
;
;
; RESTRICTIONS:
;	
;
; PROCEDURE:
;	
;
; EXAMPLE:
;	
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver 28th January 1997
;       added in association numbers
;       apm_s1 databse location changed 2nd October 1997 Seb Oliver
;       elist keyword added 2nd October 1997 Seb Oliver
;	July, 1996	
;-

;on_error,2
on_error,3

colours
astrolib

doelais=1
if n_params() eq 1 then begin
   hd=dbase
   doelais=0
endif

; checking image is in J2000

equin=get_equinox(hd)

if equin ne 2000 then hprecess,hd,2000



;======================================================================
; ELAIS SOURCES
;======================================================================
if doelais eq 1 then begin
 print,'OK here'
 dbclose
 print,dbase,strlen(dbase)
 dbopen,dbase
 print,'OK here 2'
 if keyword_set(elist)then list=elist else imdbase,hd,dbase,list,cat=2000,/silent
print,'OK here 2'

dbext,list,'entry,name,ra,dec,errma,errmi,errpa,rel,apm_n1,apm_n2,apm_n3,apm_s1',$
      entry,name,ra,dec,err_maj,err_min,err_pa,rel,apm_n1,apm_n2,apm_n3,apm_s1
ra=ra*15.
err_maj=12./60./60.
err_min=12./60./60.
thick=2
if keyword_set(big) then begin
  err_maj=err_maj*2
  err_min=err_min*2
  if !d.name ne 'PS' then thick=1
endif

if keyword_set(names) then begin 
    icellipse_oplot,hd,ra,dec,err_maj,err_min,err_pa,$
               text=name,color=5,thick=thick
  endif else $
    icellipse_oplot,hd,ra,dec,err_maj,err_min,err_pa,$
           color=5,thick=thick

if keyword_set(cross)then icellipse_oplot,hd,ra,dec,color=5

endif


;======================================================================
; APM Associations in database
;======================================================================
if doelais eq 1 then begin
dbopen,'apm_n1'
apm=0

if min(apm_n2) lt 0 or apm_n1(0) gt 0 then begin
   apm=apm_n1
   if min(apm_n2) lt 0 then apm_ident=min(apm_n2)
   if apm_n1(0) gt 0 then apm_ident=-1
   case min(apm_n2) of
   0l: dbopen,'apm_n1'
  -1l: dbopen,'apm_n1'
  -2l: dbopen,'apm_n2'
  -3l: dbopen,'apm_n3'
  -4l: dbopen,'apm_s1'
  -5l: dbopen,'apm_s2'
  -6l: dbopen,'apm_x1'
  -7l: dbopen,'apm_x2'
  -8l: dbopen,'apm_x3' 
  -9l: dbopen,'apm_x4' 
 -10l: dbopen,'apm_x5' 
 -11l: dbopen,'apm_x6' 
   else: message,'Screwed up in elais_olay'
   endcase
endif


if apm_n2(0) gt 0 then begin
  dbopen,'apm_n2'
  apm=apm_n2
  apm_ident=-2
endif 

if apm_n3(0) gt 0 then begin
  dbopen,'apm_n3'
  apm=apm_n3
  apm_ident=-3
endif 


if apm_s1(0) gt 0 then begin
  dbopen,'apm_s1'
  apm=apm_s1
  apm_ident=-4
endif 



dbext,apm,'entry,ra,dec,bmajor,bminor,bangle',$
            entry,ra,dec,err_maj,err_min,pa
ra=ra*15.

err_maj=err_maj/60./60.
err_min=err_min/60./60.
icellipse_oplot,hd,ra,dec,err_maj,err_min,pa,$
               color=7
endif

;======================================================================
; IRAS FSC SOURCES
;======================================================================

dbopen,'fsc_v2'
imdbase,hd,'fsc_v2',list,cat=1950,/silent


if list(0) gt 0 then begin 

dbext,list,'entry,name,ra,dec,uncmaj,uncmin,posang',$
      entry,name,ra,dec,err_maj,err_min,err_pa

;stop
ra=ra*15.
jprecess,ra,dec,ra,dec

err_maj=err_maj/60./60.
err_min=err_min/60./60.

if keyword_set(names) then begin 
    icellipse_oplot,hd,ra,dec,err_maj,err_min,err_pa,$
               text=name,color=4
  endif else $
    icellipse_oplot,hd,ra,dec,err_maj,err_min,err_pa,$
               color=4
if keyword_set(cross)then icellipse_oplot,hd,ra,dec,color=4

endif

;======================================================================
; IRAS PSC SOURCES
;======================================================================

dbopen,'iras_psc'
imdbase,/silent,hd,'iras_psc',list,cat=1950


if list(0) gt 0 then begin
dbext,list,'entry,name,ra,dec,major,minor,posang',$
      entry,name,ra,dec,err_maj,err_min,err_pa
ra=ra*15.
jprecess,ra,dec,ra,dec

err_maj=err_maj/60./60.
err_min=err_min/60./60.

if keyword_set(names) then begin 
    icellipse_oplot,hd,ra,dec,err_maj,err_min,err_pa,$
               text=name,color=4
  endif else $
    icellipse_oplot,hd,ra,dec,err_maj,err_min,err_pa,$
               color=4
if keyword_set(cross)then icellipse_oplot,hd,ra,dec,color=4
endif


;----------------------------------------------------------------------
; FIRST CATALOGUE
;----------------------------------------------------------------------
dbopen,'first'
imdbase,/silent,hd,'first',list,cat=2000

if(list(0) gt 0 )then begin

  dbext,list,'entry,ra,dec,major_ax,minor_ax,posangle',entry,ra,dec,err_maj,err_min,pa

  ra=ra*15.
  err_maj=err_maj/60./60.
  err_min=err_min/60./60.

  if keyword_set(names) then begin 
    icellipse_oplot,hd,ra,dec,err_maj,err_min,pa,$
               text=string(entry,format='(i4)'),color=2
  endif else $
    icellipse_oplot,hd,ra,dec,err_maj,err_min,pa,$
               color=2
endif

dbclose

if not keyword_set(no_opt) then begin

;----------------------------------------------------------------------
; APM CATALOGUE
;----------------------------------------------------------------------
;----------------------------------------------------------------------
; N1
;----------------------------------------------------------------------

;if keyword_set(

dbopen,'apm_n1'
imdbase,/silent,hd,'apm_n1',list,cat=2000


if list(0) gt 0 then begin
dbext,list,'entry,ra,dec,rmajor,rminor,rangle',$
            entry,ra,dec,err_maj,err_min,pa
ra=ra*15.

err_maj=err_maj/60./60.
err_min=err_min/60./60.
icellipse_oplot,hd,ra,dec,err_maj,err_min,pa,$
               color=3,linestyle=1

if keyword_set(ra0) and keyword_set(dec0) and keyword_set(errma0) and $
   keyword_set(errmi0) and keyword_set(errpa0) then begin

; printing up numbers of most likely associations

;
; determining likelihood ratios 
     like_associate,2000.,ra0,dec0,'apm_n1',obj,dis,like,$
   '/iso/arch/soft/local/lib/associate/apm_n1_rhist',$
   '/iso/arch/soft/local/lib/associate/qf_unity_mag.dat',$
   errma0,errmi0,errpa0,radius=1.,$
   raname='ra',decname='dec',fluxname='rmag',/single,$
   majaxname='rmajor',minaxname='rminor',paname='rangle'

; reading histogram from random catalogue
 like_apmn1=fltarr(10000)
 restore,'/iso/arch/soft/local/lib/associate/randlike_apm_n1_r.idl'

; estimating probability of likelihood ratio (not used)
 prob1=fltarr(n_elements(like))
 for i=0L,n_elements(like)-1L do begin
    p_ran, like_apmn1,like(i),prob10
    prob1(i)=prob10
 endfor
 prob1=round(prob1*100)

 use=where(like gt 0. and prob1 le 99,l_count)
 if l_count gt 0 then begin
   like_names='   '+string(indgen(l_count)+1,format='(i2)')
   like_names(0)=like_names(0)+$
              ' ('+string(prob1(use(0)),format='(i3)')+'%)'
   list=obj(use)
   dbext,list,'entry,ra,dec,rmajor,rminor,rangle,rmag',$
            entry,ra,dec,err_maj,err_min,pa,rmag

   ra=ra*15.
   oc=!p.color
   !p.color=3
    icellipse_oplot,hd,ra,dec,$
               color=3,linestyle=1,text=like_names
   !p.color=oc
   if keyword_set(print) then begin
      for i=0,n_elements(list)-1 do print,entry(i),ra(i),dec(i),like_names(i)
   endif
 endif
endif


endif

dbclose

;----------------------------------------------------------------------
; N2
;----------------------------------------------------------------------

dbopen,'apm_n2'
imdbase,/silent,hd,'apm_n2',list,cat=2000


if list(0) gt 0 then begin
dbext,list,'entry,ra,dec,rmajor,rminor,rangle',$
            entry,ra,dec,err_maj,err_min,pa

ra=ra*15.

err_maj=err_maj/60./60.
err_min=err_min/60./60.
icellipse_oplot,hd,ra,dec,err_maj,err_min,pa,$
               color=3,linestyle=1
;----------------------------------------------------------------------
if keyword_set(ra0) and keyword_set(dec0) and keyword_set(errma0) and $
   keyword_set(errmi0) and keyword_set(errpa0) then begin

; printing up numbers of most likely associations

;
; determining likelihood ratios 
     like_associate,2000.,ra0,dec0,'apm_n2',obj,dis,like,$
   '/iso/arch/soft/local/lib/associate/apm_n2_rhist',$
   '/iso/arch/soft/local/lib/associate/qf_unity_mag.dat',$
   errma0,errmi0,errpa0,radius=1.,$
   raname='ra',decname='dec',fluxname='rmag',/single,$
   majaxname='rmajor',minaxname='rminor',paname='rangle'

; reading histogram from random catalogue
 like_apmn2=fltarr(10000)
 restore,'/iso/arch/soft/local/lib/associate/randlike_apm_n2_r.idl'

; estimating probability of likelihood ratio (not used)
 prob1=fltarr(n_elements(like))
 for i=0L,n_elements(like)-1L do begin
    p_ran, like_apmn2,like(i),prob10
    prob1(i)=prob10
 endfor
 prob1=round(prob1*100)

 use=where(like gt 0. and prob1 le 99,l_count)
 if l_count gt 0 then begin

   like_names='   '+string(indgen(l_count)+1,format='(i2)')
   like_names(0)=like_names(0)+$
              ' ('+string(prob1(use(0)),format='(i3)')+'%)'
   list=obj(use)

   dbext,list,'entry,ra,dec,rmajor,rminor,rangle,rmag',$
            entry,ra,dec,err_maj,err_min,pa,rmag

   ra=ra*15.
   oc=!p.color
   !p.color=3
    icellipse_oplot,hd,ra,dec,$
               color=3,linestyle=1,text=like_names
   !p.color=oc
   if keyword_set(print) then begin
      for i=0,n_elements(list)-1 do print,entry(i),ra(i),dec(i),like_names(i)
   endif


 endif
endif


;----------------------------------------------------------------------
endif

dbclose

;----------------------------------------------------------------------
; N3
;----------------------------------------------------------------------
dbopen,'apm_n3'
imdbase,/silent,hd,'apm_n3',list,cat=2000


if list(0) gt 0 then begin
dbext,list,'entry,ra,dec,rmajor,rminor,rangle',$
            entry,ra,dec,err_maj,err_min,pa

ra=ra*15.

err_maj=err_maj/60./60.
err_min=err_min/60./60.
icellipse_oplot,hd,ra,dec,err_maj,err_min,pa,$
               color=3,linestyle=1

;----------------------------------------------------------------------
if keyword_set(ra0) and keyword_set(dec0) and keyword_set(errma0) and $
   keyword_set(errmi0) and keyword_set(errpa0) then begin

; printing up numbers of most likely associations

;
; determining likelihood ratios 
     like_associate,2000.,ra0,dec0,'apm_n3',obj,dis,like,$
   '/iso/arch/soft/local/lib/associate/apm_n3_rhist',$
   '/iso/arch/soft/local/lib/associate/qf_unity_mag.dat',$
   errma0,errmi0,errpa0,radius=1.,$
   raname='ra',decname='dec',fluxname='rmag',/single,$
   majaxname='rmajor',minaxname='rminor',paname='rangle'

; reading histogram from random catalogue
 like_apmn3=fltarr(10000)
 restore,'/iso/arch/soft/local/lib/associate/randlike_apm_n3_r.idl'

; estimating probability of likelihood ratio (not used)
 prob1=fltarr(n_elements(like))
 for i=0L,n_elements(like)-1L do begin
    p_ran, like_apmn3,like(i),prob10
    prob1(i)=prob10
 endfor
 prob1=round(prob1*100)

 use=where(like gt 0. and prob1 le 99,l_count)
 if l_count gt 0 then begin

   like_names='   '+string(indgen(l_count)+1,format='(i2)')
   like_names(0)=like_names(0)+$
              ' ('+string(prob1(use(0)),format='(i3)')+'%)'
   list=obj(use)

   dbext,list,'entry,ra,dec,rmajor,rminor,rangle,rmag',$
            entry,ra,dec,err_maj,err_min,pa,rmag

   ra=ra*15.
   oc=!p.color
   !p.color=3
    icellipse_oplot,hd,ra,dec,$
               color=3,linestyle=1,text=like_names
   !p.color=oc
   if keyword_set(print) then begin
      for i=0,n_elements(list)-1 do print,entry(i),ra(i),dec(i),like_names(i)
   endif

 endif
endif

endif

dbclose

;----------------------------------------------------------------------
; S1
;----------------------------------------------------------------------

dbopen,'apm_s1'
imdbase,/silent,hd,'apm_s1',list,cat=2000

; converting speration to degrees
;

if list(0) gt 0 then begin
dbext,list,'entry,ra,dec,bmajor,bminor,bangle,bmag',$
            entry,ra,dec,err_maj,err_min,pa,bmag

ra=ra*15.


err_maj=err_maj/60./60.
err_min=err_min/60./60.
icellipse_oplot,hd,ra,dec,err_maj,err_min,pa,$
               color=3,linestyle=1

print,keyword_set(ra0), keyword_set(dec0), keyword_set(errma0),$
 keyword_set(errmi0),keyword_set(errpa0)

;----------------------------------------------------------------------
if keyword_set(ra0) and keyword_set(dec0) and keyword_set(errma0) and $
   keyword_set(errmi0) and keyword_set(errpa0) then begin

; printing up numbers of most likely associations

;----------------------------------------------------------------------
;----------------------------------------------------------------------

; determining likelihood ratios 
     like_associate,2000.,ra0,dec0,'apm_s1',obj,dis,like,$
   '/iso/arch/soft/local/lib/associate/apm_s1_bhist',$
   '/iso/arch/soft/local/lib/associate/qf_unity_mag.dat',$
   errma0,errmi0,errpa0,radius=10.,$
   raname='ra',decname='dec',fluxname='bmag',/single,$
   majaxname='bmajor',minaxname='bminor',paname='bangle'

; reading histogram from random catalogue
 like_apms1=fltarr(10000)
 restore,'/iso/arch/soft/local/lib/associate/randlike_apm_s1_b.idl'

; estimating probability of likelihood ratio (not used)

   prob1=fltarr(n_elements(like))
   for i=0L,n_elements(like)-1L do begin
    p_ran, like_apms1,like(i),prob10
    prob1(i)=prob10
 endfor
 prob1=round(prob1*100)

 use=where(like gt 0. and prob1 le 99,l_count)
 if l_count gt 0 then begin

   like_names='   '+string(indgen(l_count)+1,format='(i2)')
   like_names(0)=like_names(0)+$
              ' ('+string(prob1(use(0)),format='(i3)')+'%)'
   list=obj(use)

   dbext,list,'entry,ra,dec,rmajor,rminor,rangle,rmag',$
            entry,ra,dec,err_maj,err_min,pa,rmag

   ra=ra*15.
   oc=!p.color
   !p.color=3
    icellipse_oplot,hd,ra,dec,$
               color=3,linestyle=1,text=like_names
   !p.color=oc
   if keyword_set(print) then begin
      for i=0,n_elements(list)-1 do print,entry(i),ra(i),dec(i),like_names(i)
   endif


 endif


;----------------------------------------------------------------------


endif


endif

dbclose

endif

;----------------------------------------------------------------------
; VLA CATALOGUE
;----------------------------------------------------------------------

dbopen,'nrao_vla_ss'
imdbase,/silent,hd,'nrao_vla_ss',list,cat=2000
if(list(0) ne 0)then begin
  dbext,list,'entry,ra,dec,major_ax,minor_ax,posangle',$
              entry,ra,dec,err_maj,err_min,pa
  ra=ra*15.

  if keyword_set(names) then begin 
    icellipse_oplot,hd,ra,dec,err_maj,err_min,pa,$
              text=string(entry,format='(i4)'), color=2
  endif else $
    icellipse_oplot,hd,ra,dec,err_maj,err_min,pa,$
               color=2
endif

dbclose
;----------------------------------------------------------------------
; INT CCD CATALOGUE
;----------------------------------------------------------------------
if keyword_set(int) then begin
if int(0) gt 1 then int_list=int else int_list=-1
dbopen,'int_wfc_02'
imdbase,/silent,hd,'int_wfc_02',list,cat=2000; ,sublist=int_list
if(list(0) ne 0)then begin
  dbext,list,'entry,ra,dec,a_world,b_world,theta_world',$
              entry,ra,dec,err_maj,err_min,pa
  ra=ra*15.

  if keyword_set(names) then begin 
    icellipse_oplot,hd,ra,dec,err_maj,err_min,pa,$
              text=string(entry,format='(i4)'), color=8
  endif else $
    icellipse_oplot,hd,ra,dec,err_maj,err_min,pa,$
               color=8
endif

endif 

dbclose

end


