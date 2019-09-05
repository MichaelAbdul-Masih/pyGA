;***********************************************************************

function elais_name,ra,dec,band, prefix=prefix

;+
; NAME:
;	elais_name
;
; PURPOSE:
;	generates official ELAIS source names from a
;       list of RAs and DECs
;
; CATEGORY:
;	analysis
;
; CALLING SEQUENCE:
;	elais_name(ra,dec[,band])
;
; INPUTS:
;	ra:   RA(J2000) in degrees
;       dec: DEC(J2000) in degrees
;
; OPTIONAL INPUTS:
;	band: character string specifying bandwidth (default C15)
;	
; KEYWORD PARAMETERS:
;	
;
; OUTPUTS:
;	elais_name: vector of offical elais names
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
;       march '97: fixed bug if only one element supplied (SJO)
;       name prefix convention changed in accordance with consortium
;       meeting (SJO) 29th June 1998.
;       prefix keyword added (Seb 20th Aug 1999)
;	July, 1996	
;-

on_error,2
if n_params() lt 2 then message,'CALLING SEQUENCE:elais_name(ra,dec[,band])'
if n_params() lt 3 then band='C15'

IF NOT keyword_set(prefix) THEN prefix='ELAIS'+strupcase(band)+'_J'

multadstring,ra,dec,sexi,prec=0

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
  raend=strpos(sexi(i),'.',0)
  decend=strpos(sexi(i),'.',sep(i))
  newsex=strmid(sexi(i),0,raend)+strmid(sexi(i),sep(i),decend-sep(i))
  sexi(i)=newsex
endfor


; stripping out blanks and adding prefix
elais_name0=prefix+strcompress(sexi,/remove_all)

; changing to scalar if single element
if n_elements(elais_name0) eq 1 then elais_name0=elais_name0(0)

return,elais_name0

END

