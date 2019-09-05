;***********************************************************************

function isohdf_name,ra,dec,str,south=south

;+
; NAME:
;	isohdf_name
;
; PURPOSE:
;	generates official ISOHDF source names from a
;       list of RAs and DECs
;
; CATEGORY:
;	analysis
;
; CALLING SEQUENCE:
;	isohdf_name(ra,dec[,inst])
;
; INPUTS:
;	ra:   RA(J2000) in degrees
;       dec: DEC(J2000) in degrees
;
; OPTIONAL INPUTS:
;	inst: character string specifying instrument (Default 'CAM')
;	
; KEYWORD PARAMETERS:
;	
;
; OUTPUTS:
;	isohdf_name: vector of offical isohdf names
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
;	follows IAU namining convention as in
;
; <a href="http://cdsweb.u-strasbg.fr/iau-spec.html"><http://cdsweb.u-strasbg.fr/iau-spec.html</a>
;
; EXAMPLE:
;	
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver 24th September 1996
;	July, 1996	
;-

on_error,2
if n_params() lt 2 then message,'CALLING SEQUENCE:isohdf_name(ra,dec[,inst])'
if n_params() lt 3 then str='3C'

prefix='ISOHDF'+str+'_J'
if keyword_set(south)then prefix='ISOHDFS'+str+'_J'

multadstring,ra,dec,sexi,prec=1

; finding seperation between RA and DEC
sep_plus=strpos(sexi,'+')
sep_minu=strpos(sexi,'-')
len=strlen(sexi)


north=where( sep_plus gt sep_minu,nc)
south=where( sep_plus lt sep_minu,sc)
sep=sep_plus
if nc gt 0 then sep(north)=sep_plus(north)
if sc gt 0 then sep(south)=sep_minu(south)

; truncating fractions of seconds of time of seconds of arc
for i=0L,n_elements(ra)-1L do begin
  raend=strpos(sexi(i),'.',0)+2
  decend=strpos(sexi(i),'.',sep(i))
  newsex=strmid(sexi(i),0,raend)+strmid(sexi(i),sep(i),decend-sep(i))
  sexi(i)=newsex
endfor


; stripping out blanks and adding prefix
isohdf_name=prefix+strcompress(sexi,/remove_all)

return,isohdf_name

END

