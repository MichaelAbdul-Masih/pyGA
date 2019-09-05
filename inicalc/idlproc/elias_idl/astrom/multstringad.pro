;***********************************************************************

PRO multstringad, sexi, ra, dec

;+
; NAME:
;	multstringad
;
; PURPOSE:
;	runs stringad for vector input
;
; CATEGORY:
;	analysis
;
; CALLING SEQUENCE:
;	multstringad, sexi, ra, dec
;
; INPUTS:
;	
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
; 	Written by:	Seb Oliver 15th Aug 1996
;       18-Nov-1996:    Replaces : with spaces
;	10-Jul-1998:	works for long lists (Bob Mann)
;-

;on_error,2

if n_params() lt 3 then message,$
'CALLING SEQUENCE: multstringad, sexi, ra, dec'


n=n_elements(sexi)

; allowing for : as seperators
sexi_space=repchr(sexi,':',' ')
; replacing d, m s, " ' etc characters with spaces
sexi_space=repchr(sexi_space,'h',' ')
sexi_space=repchr(sexi_space,'d',' ')
sexi_space=repchr(sexi_space,'m',' ')
sexi_space=repchr(sexi_space,'s',' ')
sexi_space=repchr(sexi_space,"'",' ')
sexi_space=repchr(sexi_space,'"',' ')



; below doesn't work might now
; ensuring a spce in front of sign
;sexi_space=repchr(sexi_space,'+',' +')
;sexi_space=repchr(sexi_space,'-',' -')




ra=dblarr(n)
dec=ra

for i=0l, n-1l do begin


  stringad, sexi_space(i),ra0,dec0
  ra(i)=ra0 & dec(i)=dec0
endfor


END

