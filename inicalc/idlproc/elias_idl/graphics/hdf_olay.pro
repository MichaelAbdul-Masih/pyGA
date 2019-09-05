;***********************************************************************

PRO hdf_olay,hd,names=names,cross=cross,listo=listo,flisto=flisto,$
    bright=bright,do_z=do_z,do_assoc=do_assoc,list_ifa=list_ifa,_extra=e

;+
; NAME:
;	
;
; PURPOSE:
;	ovlays hdf sources on an existing plot
; overplots various hdf catalogues on an image 
; (Image is assumed to be in Williams et al reference frame)
;
;       cat.      color
;
;       isohdf15    : 2
;       isohdf15sup : 2
;       isohdf7     : 2
;       isohdf7sup  : 2
;       isohdf7sup  : 2
;       hdf_stsci   : 3
;       hdf_flank   : 3
;       hdf_ifa     : 4
;       hdf_vla     : 5
;       hdf_z       : 8
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
;      hd: header of image to be overplotted
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	
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
;	
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
; 	Written by:	Seb Oliver 24th October 1996
;	July, 1996	
;-

;on_error,2
list=-1

dbopen,'isohdf15'
;imdbase,hd,'isohdf15',list,cat=2000
dbext,list,'entry,name,ra,dec,err_maj,err_min,err_pa,stsci_no,stsci_prob_i,flank_no,flank_prob',$
      entry,name,ra,dec,err_maj,err_min,err_pa,stsci_no,stsci_prob_i,flank_no,flank_prob
stsci_no(where(stsci_prob_i gt flank_prob))=-1
flank_no(where(flank_prob ge stsci_prob_i))=-1

assoc1=stsci_no
flank_assoc1=flank_no

ra=ra*15.
if keyword_set(names) then begin 
    icellipse_oplot,_extra=e,hd,ra,dec,err_maj,err_min,err_pa,$
               text=name,color=2
  endif else $
    icellipse_oplot,_extra=e,hd,ra,dec,err_maj,err_min,err_pa,$
           color=2

 if keyword_set(cross)then icellipse_oplot,_extra=e,hd,ra,dec,color=2

dbopen,'isohdf15sup'
;imdbase,hd,'isohdf15sup',list,cat=2000

dbext,list,'entry,name,ra,dec,err_maj,err_min,err_pa,stsci_no,stsci_prob_i,flank_no,flank_prob',$
      entry,name,ra,dec,err_maj,err_min,err_pa,stsci_no,stsci_prob_i,flank_no,flank_prob
stsci_no(where(stsci_prob_i gt flank_prob))=-1
flank_no(where(flank_prob ge stsci_prob_i))=-1

assoc1=[assoc1,stsci_no]
flank_assoc1=[flank_assoc1,flank_no]

ra=ra*15.
if keyword_set(names) then begin 
    icellipse_oplot,_extra=e,hd,ra,dec,err_maj,err_min,err_pa,$
               text=name,color=2
  endif else $
    icellipse_oplot,_extra=e,hd,ra,dec,err_maj,err_min,err_pa,$
           color=2

 if keyword_set(cross)then icellipse_oplot,_extra=e,hd,ra,dec,color=2





dbopen,'isohdf7'
;imdbase,hd,'isohdf7',list,cat=2000

dbext,list,'entry,name,ra,dec,err_maj,err_min,err_pa,stsci_no,stsci_prob_i,flank_no,flank_prob',$
      entry,name,ra,dec,err_maj,err_min,err_pa,stsci_no,stsci_prob_i,flank_no,flank_prob
stsci_no(where(stsci_prob_i gt flank_prob))=-1
flank_no(where(flank_prob ge stsci_prob_i))=-1

assoc1=[assoc1,stsci_no]
flank_assoc1=[flank_assoc1,flank_no]

ra=ra*15.
if keyword_set(names) then begin 
    icellipse_oplot,_extra=e,hd,ra,dec,err_maj,err_min,err_pa,$
               text=name,color=2
  endif else $
    icellipse_oplot,_extra=e,hd,ra,dec,err_maj,err_min,err_pa,$
               color=2
if keyword_set(cross)then icellipse_oplot,_extra=e,hd,ra,dec,color=2


dbopen,'isohdf7sup'
;imdbase,hd,'isohdf7sup',list,cat=2000

dbext,list,'entry,name,ra,dec,err_maj,err_min,err_pa,stsci_no,stsci_prob_i,flank_no,flank_prob',$
      entry,name,ra,dec,err_maj,err_min,err_pa,stsci_no,stsci_prob_i,flank_no,flank_prob
stsci_no(where(stsci_prob_i gt flank_prob))=-1
flank_no(where(flank_prob ge stsci_prob_i))=-1

assoc1=[assoc1,stsci_no]
flank_assoc1=[flank_assoc1,flank_no]

ra=ra*15.
if keyword_set(names) then begin 
    icellipse_oplot,_extra=e,hd,ra,dec,err_maj,err_min,err_pa,$
               text=name,color=2
  endif else $
    icellipse_oplot,_extra=e,hd,ra,dec,err_maj,err_min,err_pa,$
               color=2
if keyword_set(cross)then icellipse_oplot,_extra=e,hd,ra,dec,color=2


;----------------------------------------------------------------------
; ASSOC1IATIONS
;----------------------------------------------------------------------
assoc1=assoc1(where(assoc1 gt 0))
flank_assoc1=flank_assoc1(where(flank_assoc1 gt 0))


;----------------------------------------------------------------------
; STSCI ASSOC1IATIONS
;----------------------------------------------------------------------
if keyword_set(do_assoc) then begin

  dbopen,'hdf_stsci'
  dbext,assoc1,'entry,name,ra,dec',entry,name,ra,dec
  ra=ra*15.
  icellipse_oplot,_extra=e,hd,ra,dec,1.2/60./60., color=3

endif

dbclose


;----------------------------------------------------------------------
; STSCI CATALOGUE
;----------------------------------------------------------------------
dbopen,'hdf_stsci'
if not keyword_set(listo)then imdbase,hd,'hdf_stsci',listo,cat=2000
if keyword_set(bright) then listo=dbfind('vmagt<22',listo)

if(listo(0) gt 0 )then begin
  dbext,listo,'entry,ra,dec,rad1,axial,pa,name',entry,ra,dec,rad1,ax,pa,namehdf
;  ra=(ra-0.089/60./60.)*15.
;  dec=dec+1.03/60./60.

  ra=ra*15.
  errmaj=rad1/60./60.
  errmin=errmaj*ax

  if keyword_set(names) then begin 
    icellipse_oplot,_extra=e,hd,ra,dec,errmaj,errmin,pa,$
               text=namehdf,color=3
  endif else $
    icellipse_oplot,_extra=e,hd,ra,dec,errmaj,errmin,pa,$
               color=3
endif

dbclose

;----------------------------------------------------------------------
; FLANKING FIELD ASSOC1IATIONS
;----------------------------------------------------------------------

if keyword_set(do_assoc) then begin

  dbopen,'hdf_flank'
  dbext,flank_assoc1,'entry,ra,dec',entry,ra,dec

  ra=ra*15.
  icellipse_oplot,_extra=e,hd,ra,dec,1.2/60./60., color=3

endif

dbclose

;----------------------------------------------------------------------
; FLANKING FIELD CATALOGUE
;----------------------------------------------------------------------

dbopen,'hdf_flank'
if not keyword_set(flisto)then imdbase,hd,'hdf_flank',flisto,cat=2000
if keyword_set(bright) then flisto=dbfind('mag<22',flisto) 
if(flisto(0) gt 0 )then begin
  dbext,flisto,'entry,ra,dec',entry,ra,dec

ra=ra*15.0
;  ra=(ra-0.089/60./60.)*15.
;  dec=dec+1.03/60./60.
;  errmaj=rad1/60./60.
;  errmin=errmaj*ax

  if keyword_set(names) then begin 
    icellipse_oplot,_extra=e,hd,ra,dec,1.2/60./60.,text=strcompress('F'+string(entry,format='(i4)'),/remove_all),color=3
  endif else $
    icellipse_oplot,_extra=e,hd,ra,dec,1.2/60./60.,color=3
endif

dbclose


;----------------------------------------------------------------------
; HAWAII CATALOGUE
;----------------------------------------------------------------------
dbopen,'hdf_ifa'
if not keyword_set(list_ifa) then imdbase,hd,'hdf_ifa',list_ifa,cat=2000
if keyword_set(bright) then list_ifa=dbfind('v<22',list_ifa) 

if(list(0) ne 0)then begin
  dbext,list_ifa,'entry,ra,dec',entry,ra,dec

  ra=(ra+0.089/60./60.)*15.
  dec=dec-1.03/60./60.

 if keyword_set(names) then begin 
     icellipse_oplot,_extra=e,hd,ra,dec,1./60./60.,$
         text=string(entry-1,format='(i4)'),color=4
  endif else $
     icellipse_oplot,_extra=e,hd,ra,dec,1./60./60.,$
               color=4
endif

dbclose

;----------------------------------------------------------------------
; VLA CATALOGUE
;----------------------------------------------------------------------

dbopen,'hdf_vla'
imdbase,hd,'hdf_vla',list,cat=2000
if(list(0) ne 0)then begin
  dbext,list,'entry,ra,dec,ra_err,dec_err,',$
              entry,ra,dec,ra_err,dec_err
  ra=ra*15.
  emaj=dec_err/60./60.
  emin=ra_err*15.*0.47/60./60.
  pa=fltarr(n_elements(emaj))

  if keyword_set(names) then begin 
    icellipse_oplot,_extra=e,hd,ra,dec,emaj,emin,pa,$
              text=string(entry,format='(i4)'), color=5
  endif else $
    icellipse_oplot,_extra=e,hd,ra,dec,emaj,emin,pa,$
               color=5
endif

;----------------------------------------------------------------------
; VLA CATALOGUE '98
;----------------------------------------------------------------------

dbopen,'hdf_vla98'
imdbase,hd,'hdf_vla98',list,cat=2000
if(list(0) ne 0)then begin
  dbext,list,'entry,ra,dec,raerr,decerr,',$
              entry,ra,dec,raerr,decerr
  ra=ra*15.
  emaj=decerr/60./60.
  emin=raerr*15.*0.47/60./60.
  pa=fltarr(n_elements(emaj))

  if keyword_set(names) then begin 
    icellipse_oplot,_extra=e,hd,ra,dec,emaj,emin,pa,$
              text=string(entry,format='(i4)'), color=5,thick=2
  endif else $
    icellipse_oplot,_extra=e,hd,ra,dec,emaj,emin,pa,$
               color=5,thick=2
endif

;----------------------------------------------------------------------
; REDSHIFT CATALOGUE
;----------------------------------------------------------------------
if keyword_set(do_z) then begin
dbopen,'hdf_z'
imdbase,hd,'hdf_z',list,cat=2000
if(list(0) ne 0)then begin
  dbext,list,'entry,ra,dec',$
              entry,ra,dec
  ra=ra*15.

  if keyword_set(names) then begin 
    icellipse_oplot,_extra=e,hd,ra,dec,5.e-5,$
              text=string(entry,format='(i4)'), color=8,alignment=1.
  endif else $
    icellipse_oplot,_extra=e,hd,ra,dec,1./60./60.,$
               color=8
endif
endif


end


