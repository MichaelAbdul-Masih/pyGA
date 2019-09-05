;***********************************************************************

PRO multadstring, ra, dec, sexi,prec=prec,hms=hms,html=html

;+
; NAME:
;	multadstring
;
; PURPOSE:
;	runs adstring for vector input
;
; CATEGORY:
;	analysis
;
; CALLING SEQUENCE:
;	multadstring, ra, dec, sexi [,prec=prec,/hms]
;
; INPUTS:
;	ra: vector of RAs (degrees)
;      dec: vector of DECs  "
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	
;     prec:  precision of string as for adstring function
;      hms:  if set then an 00h00m00.0s +00d00m00.0s format is returned
;
; OUTPUTS:
;    sexi:   sexigesimal formatted output	
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
;       10 th dec 1996  hms keword added (doesn't works)
;       4th June 1997 hms keyword fixed (but not precise)
;	July, 1996	
;-


on_error,2

if n_params() lt 3 then message,$
'CALLING SEQUENCE: multadstring, ra, dec, sexi [,prec=prec,/hms]'

if not keyword_set(prec) then prec=1
n=n_elements(ra)
sexi=strarr(n)

ra_split=dblarr(n,3)
dec_split=dblarr(n,3)

for i=0, n-1 do begin
  sexi(i)=adstring(ra(i),dec(i),prec,/zero)
  ra_split(i,*)=sixty(ra(i)/15.)
  dec_split(i,*)=sixty(dec(i))
endfor



if keyword_set(hms) then begin
; removing spaces
;strcompress(sexi,/remove_all)

for i=0, n-1 do begin
; finding seperation between RA and DEC
  sexi0=sexi(i)+' '
  sep_plus=strpos(sexi0,'+')
  sep_minu=strpos(sexi0,'-')
  len=strlen(sexi0)

  north=where( sep_plus gt sep_minu,nc)
  south=where( sep_plus lt sep_minu,sc)
  sep=sep_plus

  strput,sexi0,'h',3
  strput,sexi0,'m',6
  strput,sexi0,'s',sep-2

  strput,sexi0,'d',sep+3
  strput,sexi0,'m',sep+6
  strput,sexi0,'s',len-1
  
  sexi(i)=strtrim(sexi0,2)
; actually that all totally screws up for 0 hours
; quick fix by truncating
  ras=fix(ra_split(i,2))
  decs=fix(dec_split(i,2))

  sexi(i)=string(fix(ra_split(i,0)),format='(i2.2)')+'h'+$
          string(fix(ra_split(i,1)),format='(i2.2)')+'m'+$
          string(ras,format='(i2.2)')+'.'+$
          string(fix((ra_split(i,2)-ras)*100),format='(i2.2)')+'s '+$
          string(fix(dec_split(i,0)),format='(i3.2)')+'d'+$
          string(fix(dec_split(i,1)),format='(i2.2)')+'m'+$
          string(decs,format='(i2.2)')+'.'+$
          string(fix((dec_split(i,2)-decs)*10),format='(i1.1)')+'s'

endfor
endif


if keyword_set(html)then begin

 for i=0,n_elements(sexi)-1 do begin
   mid=strpos(sexi(i),'+')
   if mid le 0 then mid=strpos(sexi(i),'-')
   rastr=strmid(sexi(i),0,mid-1)
   decstr=strmid(sexi(i),mid,strlen(sexi(i))-mid)
   rastr0=repchr(rastr,' ','+')
   if dec(0) lt 0 then decstr0=decstr
   if dec(0) ge 0 then decstr0='%2B'+strmid(decstr,1,strlen(decstr)-1)
   decstr0=repchr(decstr0,' ','+')
   sexi(i)='RA='+rastr0+'&Dec='+decstr0
 endfor

endif

END

