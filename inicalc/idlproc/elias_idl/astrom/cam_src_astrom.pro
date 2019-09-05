;***********************************************************************

PRO cam_src_astrom, raster, source,disto=disto, history=history

;+
; NAME:
;	cam_src_astrom
;
; PURPOSE:
;	Takes the i, j and scd positions of a detected source
;       and calculates the RA and DEC
;
; CATEGORY:
;	
;
; CALLING SEQUENCE:
;	cam_src_astom, raster, source
;
; INPUTS:
;	source.x:    cam_src structure (x)
;	source.y:    cam_src structure (y)
;       raster:       raster structure in which sources detected
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	
;
; OUTPUTS:
;
;	source.ra:  array decimal degrees
;       source.dec: decimal degrees
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
;	
;
; PROCEDURE:
;	
;
; EXAMPLE:
;	
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver, 16th May 1996
;	27 May 1997: Raster.history updated S Serjeant
;  v1.1 06 aug 1998: disto keyword added to allow for field distortions Seb Oliver
;	25 Aug 1998: history string changed instead of raster.history
;		SSerjeant (v1.2)
;
;-

; ------------------------------------------------------------
;  on error conditions
; ------------------------------------------------------------
 ON_ERROR, 2
 
if(keyword_set(history)) then begin
 version='v1.2'
 help,calls=calls
 routine_name=strmid(calls(0),0,strpos(calls(0),'<'))
 history=history+'/'+routine_name+version 
endif
 
; ------------------------------------------------------------
;  parameters check
; ------------------------------------------------------------
 IF N_PARAMS() LT 2 THEN BEGIN
   MESSAGE, 'CALLING SEQUENCE:  cam_src_astrom, raster, source'
 ENDIF

 if not keyword_set(disto) then disto=0

;

nsource=n_elements(source)


for i=0,nsource-1 do begin
   iccam_mkhdr, raster.info.ra(source(i).iscd),$
                raster.info.dec(source(i).iscd),$
                raster.info.roll(source(i).iscd),hd
   extast,hd,ast
   xy2ad,source(i).x,source(i).y,ast,ra0,dec0,disto=disto
   source(i).ra=ra0 & source(i).dec=dec0

endfor

CLOSING:

END


