;***********************************************************************

PRO af_counts,band,lgs,lgn,type=type,diff=diff,euclid=euclid,order=order

;+
; NAME:
;	af_counts
;
; PURPOSE:
;	reads in Alberto Franceschini's tables of sources counts
;
; CATEGORY:
;	Put a category (or categories) here.  For example: Widgets.
;
; CALLING SEQUENCE:
;	af_counts,band,lgs,lgn,[type=type,/diff,/euclid,/order
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
;            = 'spiral':     spiral galaxies 
;            = 'elliptical':     elliptical galaxies (or E+s0 at 6,7)
;            = 's0':     s0 galaxies (or E+s0 at 6,7)
;            = 'starburst':     starburst galaxies (not at 6.7)
;            = 'agn':      AGN
;       diff: If set then differential counts are calculated rather than
;             integral counts
;       euclid: If set then euclidean differential counts are calculated rather than
;             integral counts
;
; OUTPUTS:
;	lgs:  log10(flux/Jy)
;       lgn:   log10( N/sq deg > flux)  [if diff not set]
;       lgn:   log10( -dN/ds / sr /Jy )  [if diff set]
;       lgn:   log10( -S^2.5 dN/ds / sr /Jy )  [if euclid set]
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
; 	Written by:	Seb Oliver 20th November 1996
;	July, 1994	Any additional mods get described here.
;-
if n_params() lt 3 then message,$
'Calling Sequence: af_counts,band,lgs,lgn,[type=type,/diff,/euclid,/order'

on_error,2

count=0


if n_elements(lgs) gt 0 then begin
  temp=where(lgs ne 0,count)
  if count gt 0 then lgs0=lgs
endif

dir='/scratch/sjo/icstm_idl/general/'
dir=!elais_dir+'data/'

case band of

7:  begin

       file1='af_7.dat'
       readfmt,dir+file1,$
       '(3x,d12.5,4x,d11.5,4x,d11.5,4x,d11.5,4x,d11.5,4x,d11.5)',$
       lgs,lgn_s,lgn_e,lgn_sb,lgn_agn,lgn_tot,/silent
       scale=1.
    end

15: begin

       file1='af_15.dat'
       readfmt,dir+file1,$
       '(2x,d12.5,2x,d11.5,2x,d11.5,2x,d11.5,2x,d11.5,2x,d11.5,2x,d11.5)',$
       lgs,lgn_s,lgn_e,lgn_s0,lgn_sb,lgn_agn,lgn_tot,/silent
       scale=600.
    end

else: message,'Band must be 7, or 15'

endcase

if not keyword_set(type) then type='all'



case strlowcase(strmid(type,0,4)) of

   'spir': lgn=lgn_s
   'elli': lgn=lgn_e
   's0':   if band eq 15 then begin
                lgn=lgn_s0 
           endif else lgn=lgn_e
   'star': if band eq 15 then begin
             lgn=lgn_sb
         endif else lgn=lgn_tot
   'agn': lgn=lgn_agn
   'all': lgn=lgn_tot
    else: lgn=lgn_tot

endcase

; dN   d(Log10 N)   N
; -- = ---------- * -
; dS   d(Log10 s)   S

if keyword_set(diff) or keyword_set(euclid) then begin

 lgn=lgn(0:49)
 lgs=lgs(0:49)
 lgn=alog10(lgn*scale)
 if not keyword_set(euclid)then  lgn=lgn-2.5*lgs

endif else begin
;   stop
   lgn=lgn(50:99)
   lgs=lgs(50:99)
   lgn(where(lgn le 0.))=min(lgn(where(lgn gt 0.))/100000.)
;
; Franceschin's integral counts are in /sq deg *pi^2/180^2  (oops!)
; CONVERTING THEM BACK TO /SQ DEG
   lgn=lgn*!radeg^2
   lgn=alog10(lgn)
endelse

; interpolating if necessary
if count gt 0 then begin

  lgn=interpol(lgn,lgs,lgs0)
  lgs=lgs0
endif

; sorting into numerical order
if keyword_set(order) then begin
  order=sort(lgs)
  lgs=lgs(order)
  lgn=lgn(order)
endif
END




