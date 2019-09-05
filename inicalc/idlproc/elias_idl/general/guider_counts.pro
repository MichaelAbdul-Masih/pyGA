;***********************************************************************

PRO guider_counts,band,lgs,lgn,type=type,diff=diff

;+
; NAME:
;	guider_counts
;
; PURPOSE:
;	reas in Guiderdoni's tables of sources counts
;
; CATEGORY:
;	Put a category (or categories) here.  For example: Widgets.
;
; CALLING SEQUENCE:
;	guider_counts,band,lgs,lgn,type=type,diff=diff
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
;            = 'A'   :   model A
;            = 'E':     model E
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
; 	Written by:	Seb Oliver 19th January 1999 Updated from old version
;	July, 1994	Any additional mods get described here.
;-

;on_error,2

if not keyword_set(type) then type='A'
if not keyword_set(diff) then diff=0

;stop
case type of 
'E': readcol,!ELAIS_DIR+'data/guiderdoni/counts_E',flux,n,/silent
'A': readcol,!ELAIS_DIR+'data/guiderdoni/counts_A',flux,n,/silent
else: message,'Model not recognised'

endcase

;----------------------------------------------------------------------
;check if lgs is already set
;----------------------------------------------------------------------

count=0
if n_elements(lgs) gt 0 then begin
  temp=where(lgs ne 0,count)
  if count gt 0 then lgs0=lgs
endif

;----------------------------------------------------------------------

start=where(flux eq -7.9)
finish=shift(start-1,-1)

nband=n_elements(start)
finish(nband-1)=n_elements(flux)-1

bands=[15, 20, 30, 40, 60, 80, 90,100,150,175,200,250,300,$
350,400,450,550,650,750,850,1000,1380,2000]

if n_elements(bands) ne nband then message,'Not right number of bands'

iband=where(band eq bands,nok)

;----------------------------------------------------------------------
; two options, either this is one of the bands dealt with by
; Guiderdoni and is straight-forward or we have to try and interpolate
;----------------------------------------------------------------------


if nok ne 1 then BEGIN
  ; message,'Not set up for this band: trying to interpolate', /inf
  band_diff = bands-band
  sign = band_diff/abs(band_diff)
  diff_sign = ts_diff(sign, 1)
  i0 = where(diff_sign NE 0, ndif)
  IF ndif EQ 0 THEN BEGIN 
     message, 'Band outside range', /inf
     lgn = 0
     lgs = 0
     return
 ENDIF

  i0 = i0[0]

; use lgs0 as this is either what was passed down or is unset and
; so will be returned set
  guider_counts,bands[i0],lgs0,lgn0,type=type,diff=diff
  guider_counts,bands[i0+1],lgs0,lgn2,type=type,diff=diff
  lgs = lgs0

  n0 = 10.^lgn0
  n2 = 10.^lgn2
  b0 = bands[i0]
  b1 = band
  b2 = bands[i0+1]

  n1 = (n0*(b2-b1)+n2*(b1-b0))/(b2-b0)
  lgn = alog10(n1)
 ;plot,lgs0,lgn  
 ;oplot,lgs0,lgn0,color=2
 ;oplot,lgs0,lgn2,color=3

ENDIF ELSE BEGIN

iband=iband(0)

lgs=flux(start(iband):finish(iband))
lgn=n(start(iband):finish(iband))


;----------------------------------------------------------------------
; converting to /sq deg units instead of /sr

lgn=lgn+2.*alog10(!dtor)


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


;----------------------------------------------------------------------
; interpolating if necessary
if count gt 0 then begin

  lgn=interpol(lgn,lgs,lgs0)
  lgs=lgs0
endif

ENDELSE


END




