;***********************************************************************

PRO cpp_counts,band,lgs,lgn,type=type,diff=diff

;+
; NAME:
;	cpp_counts
;
; PURPOSE:
;	reads in Chris Pearsons tables of sources counts
;
; CATEGORY:
;	Put a category (or categories) here.  For example: Widgets.
;
; CALLING SEQUENCE:
;	cpp_counts,band,lgs,lgn,type=type,diff=diff
;
; INPUTS:
;	band:  band
;
;
; OPTIONAL INPUTS:
;	
;       lgs:  If lgs is not zeros then counts interpolated at these fluxes
;             otherwise default table is returned
;	
; KEYWORD PARAMETERS:
;	type:  Galaxy Type
;            = 'all'   :   total of below (Defualt)
;            = 'normal':     normal galaxies
;            = 'starburst':     starburst galaxies
;            = 'hyper':     hyper-luminous (F10214 like) galaxies
;            = 'sey1':     Seyfert 1s
;            = 'sey2':     Seyfert 2s
;       diff: If set then differential counts are calculated rather than
;             integral counts
;
; OUTPUTS:
;	lgs:  log10(flux/Jy)
;       lgn:   log10( N/sq deg > flux)  [if diff not set]
;       lgn:   log10( -dN/ds / sq deg  /Jy )  [if diff set]
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
; RESTRICTIONS:
;	Only reads in whole table could do differential counts
;       or counts above given lgs limit
;
; PROCEDURE:
;	
;
; EXAMPLE:
;	
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver 5th August 1996
;	July, 1994	Any additional mods get described here.
;-

on_error,2
data_dir='/starpc07_data/sjo/arch_cats/sims/counts/'

data_dir = !elais_dir+data/cpp_sims/'

case band of

7:  begin
       dir=data_dir+'6.7um/'
       file1='RES6-7'
    end
12: begin
       dir=data_dir+'12um/'
       file1='RES12'
    end
15: begin
       dir=data_dir+'15um/'
       file1='RES15'
    end
50: begin 
       dir=data_dir+'50um/'
       file1='RES50'
    end
90: begin
       dir=data_dir+'90um/'
       file1='RES90'
    end
200: begin
       dir=data_dir+'200um/'
       file1='RES200'
     end
else: message,'Band must be 7, 12, 15, 50, 90 or 200'

endcase

if not keyword_set(type) then type='all'
count=0
if n_elements(lgs) gt 0 then begin
  temp=where(lgs ne 0,count)
  if count gt 0 then lgs0=lgs
endif

case strlowcase(strmid(type,0,4)) of

   'norm': file2='C.DAT'
   'star': file2='S.DAT'
   'hype': file2='F.DAT'
   'sey1': file2='S1.DAT'
   'sey2': file2='S2.DAT'
   else: file2='um.DAT'

endcase

fmt='(2e)'
readcol,dir+file1+file2,lgs,lgn,/silent

; dN   d(Log10 N)   N
; -- = ---------- * -
; dS   d(Log10 s)   S

if keyword_set(diff) then begin
  del_lgn=ts_diff(lgn,1)
  del_lgs=ts_diff(lgs,1)
  n1=n_elements(lgn)
  s1=n_elements(lgs)
  del_lgn(n1-1)=del_lgn(n1-2)
  del_lgs(s1-1)=del_lgs(s1-2)

  lgn=lgn(1:*)+del_lgn/2.
  lgs=lgs(1:*)+del_lgs/2.

  lgdnds=lgn-lgs+alog10(-del_lgn/del_lgs)

  lgn=lgdnds

endif

; interpolating if necessary
if count gt 0 then begin

  lgn=interpol(lgn,lgs,lgs0)
  lgs=lgs0
endif


END




