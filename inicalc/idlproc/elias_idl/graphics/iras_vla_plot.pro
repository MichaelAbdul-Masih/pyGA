PRO IRAS_VLA_PLOT, fits_name, ra_in, dec_in, siz
;+ 
; NAME: 
;     IRAS_VLA_PLOT
;
; PURPOSE: reads in a fits image plots it and overlays any 
;          IRAS-PSC sources IRAS-FSC_V2
;          and NRAO_VLA sources & FIRST survey sources
;
;
; CATEGORY: VIII,????
;
; CALLING SEQUENCE: 
;
;  IRAS_VLA_PLOT, fits_name
;
; INPUTS:
;
;    fits_name: string containg fits image name
;    ra_in:   ra of any additional sources to overplot in decimal degrees
;    dec_in:   dec of any additional sources to overplot
;
; OPTIONAL INPUT PARAMETERS: 
;
; KEYED INPUTS: 
;    none
;
; OUTPUTS: 
;
; OPTIONAL OUTPUT PARAMETERS: 
;
; KEYED OUTPUT PARAMETERS: 
;      none
;
; EXAMPLE: ????
;
;  IDL> astrolib 
;  IDL> dbopen,'first'
;  IDL> dbopen,'nrao_vla_ss'
;  IDL> dbopen,'iras_psc'
;  IDL> dbopen,'fsc_v2'
; 
; ALGORITHM: 
;
; DEPENDENCIES: 
;
;  Needs the first, iras_psc and nrao_vla_ss databases open
;
; COMMON BLOCKS: 
; 
;
; SIDE EFFECTS:
;
;  produces graphics on display device
;
; RESTRICTIONS: 
;
;     assumes image is in J2000 coordinates
;
;        UNTESTED
;
; CALLED PROCEDURES AND FUNCTIONS:
;
;  astrolib routines
;  map_plot
;  ellipse_oplot
;
; SEE ALSO: ????
;
; MODIFICATION HISTORY: 
;     12-Apr-1996  written with TEMPLATE_GEN Seb Oliver (ICSTM)
;
; COPYRIGHT:  
;  
;   No warranties for installation/ support/ maintenance are given.
;
;-
 
; ------------------------------------------------------------
;  common blocks 
; ------------------------------------------------------------
 
;  environment parameters 

; ------------------------------------------------------------
;  on error conditions
; ------------------------------------------------------------
 ON_ERROR, 2
 
 
; ------------------------------------------------------------
;  parameters check
; ------------------------------------------------------------
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $ 
    'IRAS_VLA_PLOT, fits_name'
   GOTO, CLOSING
 ENDIF
 
; ------------------------------------------------------------
;  function body
; ------------------------------------------------------------
 
; readin fits file

 im1=readfits(fits_name,hd1)

; plot map
 
 map_plot,im1,hd1,/sky,/log

; plotting up addditional sources
 nel=n_elements(ra_in)
 if (nel gt 0) then begin
 maj_ax=fltarr(nel)+siz
 min_ax=fltarr(nel)+siz
 pa=fltarr(nel)
 print,maj_ax,min_ax,pa
 ellipse_oplot,hd1,ra_in,dec_in,maj_ax,min_ax,pa
 endif

; ------------------------------------------------------------
;
; check IRAS PSC data base

 imdbase,hd1,'iras_psc',list
 if (n_elements(list) gt 1 or list(0) ne 0  )then begin
	dbprint,list,'*'
    	dbext,list,'ra,dec,major,minor,posang',ra,dec,maj_ax,min_ax,pa

; converting axies to degrees
	maj_ax=maj_ax/3600.
	min_ax=min_ax/3600.
	ra=ra*15.0d0
;precessing to J2000
	jprecess,ra,dec,ra2,dec2

; IRAS ellipse is thicker than VLA

	ellipse_oplot,hd1,ra2,dec2,maj_ax,min_ax,pa,thick=2

 endif

; ------------------------------------------------------------
;
; check IRAS FSC data base
;
 imdbase,hd1,'fsc_v2',list
 if (n_elements(list) gt 1 or list(0) ne 0  )then begin
	dbprint,list,'*'
    	dbext,list,'ra,dec,uncmaj,uncmin,posang',ra,dec,maj_ax,min_ax,pa

; converting axies to degrees
	maj_ax=maj_ax/3600.
	min_ax=min_ax/3600.
	ra=ra*15.0d0
;precessing to J2000
	jprecess,ra,dec,ra2,dec2

; IRAS ellipse is thicker than VLA
;
	ellipse_oplot,hd1,ra2,dec2,maj_ax,min_ax,pa,thick=2

 endif

; ------------------------------------------------------------
;
;  check VLA database

 imdbase,hd1,'nrao_vla_ss',list
 if (n_elements(list) gt 1 or list(0) ne 0  )then begin
	 dbprint,list,'*'
    	dbext,list,'ra,dec,major_ax,minor_ax,posangle',ra,dec,maj_ax,min_ax,pa

; hours to degrees
 	ra2=ra*15.d0
 	dec2=dec

 	ellipse_oplot,hd1,ra2,dec2,maj_ax,min_ax,pa

 endif
; ------------------------------------------------------------
;
;  check first database

 imdbase,hd1,'first',list,cat_year=2000.
 if (n_elements(list) gt 1 or list(0) ne 0  )then begin
	dbprint,list,'*'
    	dbext,list,'ra,dec,major_ax,minor_ax,posangle',ra,dec,maj_ax,min_ax,pa

; hours to degrees
 	ra2=ra*15.d0
 	dec2=dec
; converting axies to degrees
	
	maj_ax=maj_ax/3600. 
	min_ax=min_ax/3600.
	zero=where(maj_ax lt 0.,count) & if count gt 0 then maj_ax(zero)=0.
	zero=where(min_ax lt 0.,count) & if count gt 0 then min_ax(zero)=0.
 	ellipse_oplot,hd1,ra2,dec2,maj_ax,min_ax,pa
 endif

; ------------------------------------------------------------
;
;  check quasar_agn database

 imdbase,hd1,'quasar_agn',list
 if (n_elements(list) gt 1 or list(0) ne 0  )then begin
	dbprint,list,'ra_2000,dec_2000,v,v_phot,ub,bv'
    	dbext,list,'ra,dec',ra,dec

; hours to degrees
 	ra=ra*15.d0
	jprecess,ra,dec,ra2,dec2

; converting axies to degrees
	
	maj_ax=0.*ra
	min_ax=0.*ra
	pa=0.*ra
 	ellipse_oplot,hd1,ra2,dec2,maj_ax,min_ax,pa
 endif

;
; finally putting up cursor
;
;
;   curval,hd1,im1 ,x0=x0,y0=y0
;

; ------------------------------------------------------------
;  closing
; ------------------------------------------------------------
 
 CLOSING:

  RETURN
 
 END
